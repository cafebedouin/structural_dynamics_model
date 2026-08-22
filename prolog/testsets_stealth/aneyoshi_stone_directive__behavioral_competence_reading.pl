% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Aneyoshi Tsunami Stone Directive (behavioral competence reading, 1933-2011)
 *   domain: disaster anthropology / institutional memory / land-use governance
 *
 * SUMMARY:
 *   After the 1854 Ansei tsunami, and re-validated by the 1896 and 1933
 *   Sanriku events, villages on the Japanese ria coast carved stones above
 *   the reached inundation line bearing directives not to build below them.
 *   This story instantiates the behavioral_competence_reading of that
 *   standing arrangement: across the 78 years from the 1933 Showa Sanriku
 *   tsunami to the 2011 Tohoku tsunami — an interval containing no validating
 *   local catastrophe — the directive retained binding behavioral force, and
 *   communities sited dwellings above the marked line anyway. The
 *   arrangement's structure is that of a physical limit recorded in an
 *   artifact: the stone marks a boundary fixed by wave physics and valley
 *   topography, compliance costs the governed households only the forgone
 *   seaward land they themselves would have used, and no party collects from
 *   anyone else's compliance. Metrics are authored independently of the
 *   mountain claim — negligible extraction (0.06), minimal suppression
 *   (0.10), low theater (0.08) — and the engine computes each seat's
 *   classification from the structural data; any divergence between the
 *   claimed type and a computed seat type is the measurement, not an error to
 *   reconcile. Assumptions of record: the 78-year interval is anchored 1933
 *   to 2011, and the reading is authored at the level of the
 *   transmission-intact villages, with site-level heterogeneity carried as an
 *   omega rather than averaged away.
 *
 * KEY AGENTS:
 *   - descendant_coastal_households: governed compliers (moderate/constrained) — bear the siting restriction and receive its protection; cost and benefit land on the same households
 *   - village_elder_transmitters: transmission stewards (moderate/identity_locked) — administer the directive's behavioral force; their village standing is constituted by the stewardship itself
 *   - seaward_development_interests: excluded parties (organized/mobile) — would contest the line's placement or bindingness if given a seat; kept outside the transmission structure that adjudicates the directive
 *   - disaster_ethnography_observers: analytical observer (analytical/analytical) — documented the stones and siting patterns before validation and matched them to the run-up after
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__behavioral_competence_reading, 0.06).
domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, 0.1).
domain_priors:theater_ratio(aneyoshi_stone_directive__behavioral_competence_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_stone_directive__behavioral_competence_reading, "Aneyoshi Tsunami Stone Directive (behavioral competence reading, 1933-2011)").
narrative_ontology:topic_domain(aneyoshi_stone_directive__behavioral_competence_reading, "disaster anthropology / institutional memory / land-use governance").

domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__behavioral_competence_reading, '058c040f-35b2-4fb0-8b07-8d339c529af4').
narrative_ontology:cs_kernel_codification('058c040f-35b2-4fb0-8b07-8d339c529af4', fixed_text).
narrative_ontology:cs_authority_grounding('058c040f-35b2-4fb0-8b07-8d339c529af4', lineage).
narrative_ontology:cs_interpretation_layer_present('058c040f-35b2-4fb0-8b07-8d339c529af4').
narrative_ontology:cs_reading_relation('058c040f-35b2-4fb0-8b07-8d339c529af4', aneyoshi_stone_directive__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('058c040f-35b2-4fb0-8b07-8d339c529af4', foundational, ancestral_testimony_binding_absent_living_memory).
narrative_ontology:cs_axiom_status(ancestral_testimony_binding_absent_living_memory, holdable).
narrative_ontology:cs_axiom_grounding('058c040f-35b2-4fb0-8b07-8d339c529af4', ancestral_testimony_binding_absent_living_memory, instrumental).
narrative_ontology:cs_axiom('058c040f-35b2-4fb0-8b07-8d339c529af4', secondary, marked_line_equals_physical_runup).
narrative_ontology:cs_axiom_status(marked_line_equals_physical_runup, holdable).
narrative_ontology:cs_axiom_grounding('058c040f-35b2-4fb0-8b07-8d339c529af4', marked_line_equals_physical_runup, empirically_contingent).
narrative_ontology:cs_reference_frame('058c040f-35b2-4fb0-8b07-8d339c529af4', ancestral_directive_in_force).
narrative_ontology:cs_drift_state('058c040f-35b2-4fb0-8b07-8d339c529af4', validation_free_interval_end, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('058c040f-35b2-4fb0-8b07-8d339c529af4', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__behavioral_competence_reading, descendant_coastal_households).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__behavioral_competence_reading, descendant_coastal_households).
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive__behavioral_competence_reading, hazard_line_tracks_validated_runup).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fisher and farming households on the ria coast who site their dwellings at or above the line the stone marks. They give up the flat, port-adjacent ground seaward of it — the most convenient buildable land in a steep valley — and in exchange their houses stand above the inundation their great-grandparents measured. Leaving means leaving the fishery, the terraced fields, and the kin network, so the realistic choice is where on the slope to build, not whether to be here. The cost of compliance and the protection it buys land on the same households; nothing is collected from anyone.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, descendant_coastal_households, beneficiary,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_directive__behavioral_competence_reading, descendant_coastal_households, payer).

% Older villagers, household heads, and in some hamlets temple keepers who maintain the stone, walk children to it, and retell the founding inundation at memorial observances and before rebuilding seasons. Their standing in the village rests on keeping the transmission intact; an elder who let the practice lapse would lose the role that organizes their late-life identity. They cannot hand off the duty and leave — the duty is the position.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, village_elder_transmitters, agenda_setter,
    moderate, generational, identity_locked, local).

% Post-war fish processors, cold-storage operators, port businesses, and municipal planners who want the flat seaward ground for plants, warehouses, and public buildings. They sit outside the transmission structure that gives the stone its force: no seat at the memorial, no voice in where the line sits. Their options are to build below the line and carry the risk themselves, to petition for engineered protection that would let them ignore it, or to relocate inland at cost.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, seaward_development_interests, excluded,
    organized, biographical, mobile, regional).

% Researchers and survey teams who recorded the stones and the siting patterns before 2011 and matched them against the run-up afterward. They hold no stake in the villages' land and can see both the transmission practice and the physical record that eventually checked it.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, disaster_ethnography_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_directive__behavioral_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_stone_directive__behavioral_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stores a measured hazard boundary outside living memory: the stone encodes the inundation limit validated by actual catastrophes so each generation can site dwellings safely without re-experiencing one. It solves the rare-event memory problem that unassisted oral tradition fails at — Sanriku tsunamis recur on centennial scales, longer than living transmission reliably spans.
% TRANSFER_FUNCTION: Moves no wealth to any party. What moves is hazard knowledge — from the founding survivors to each subsequent generation — and a reciprocal obligation: each generation accepts the siting restriction and passes the boundary on intact. The forgone seaward land value stays with the households that forgo it; the protection accrues to the same households and their successors.
% ABSENT_VOICES: Seaward development interests — processors, port businesses, municipal planners — would contest the line's placement or its bindingness if they had a seat; they were never part of the transmission structure that adjudicates the directive. Post-war planners who relocated or ignored stones in neighboring towns made exactly this objection and were answered only in 2011.
% DISAPPEARANCE_RATIONALE: Delete the directive's behavioral force in, say, 1940, and the post-war boom develops the flat seaward ground exactly where development pressure pointed — dwellings, processors, schools below the measured line. By 2011 the run-up reaches structures that stone-keeping villages did not have in the inundation zone; mortality, village layout, and the coastal economy all rearrange. The transmission practice, the elders' role, and the commemorative calendar disappear with it.
% FOUNDING_PROBLEM: After the 1854 Ansei tsunami destroyed the coastal villages, survivors needed a way to make a validated inundation boundary outlast living memory, so that descendants who had never seen the sea at full run would still build above the line.
% FOUNDING_PROBLEM_CORROBORATION: The physical record corroborates from outside any beneficiary party: tsunami-deposit sediment research and post-2011 municipal hazard reconstruction independently reproduce the marked line and confirm that centennial-scale inundation recurs beyond unaided memory. The 2011 run-up itself attested both the founding problem and the line's accuracy; no party that benefits needed to assert it.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__behavioral_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_stone_directive__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_directive__behavioral_competence_reading, 0.06, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is negligible (0.06) because nothing is collected: the directive's only cost is the seaward land value the complying households forgo, and that cost stays with them as the price of siting above a measured inundation line. Suppression is minimal (0.10) because the alternative — building below the line — remains physically open throughout; the directive holds by transmission and hazard salience, with mild social sanction, not coercion. Theater is low (0.08) because the artifact's function is behavioral: the stone is consulted when ground is chosen, and the memorial rites around it are secondary to that use. Accessibility collapse is high but not absolute (0.82): for anyone who accepts the evidence the seaward-dwelling alternative collapses — the 2011 run-up confirmed the line almost exactly — but the alternative never becomes physically impossible, which keeps this just under the natural-law band. Resistance is low (0.10): individual deviations and development pressure occurred, but no organized resistance within the transmission-intact villages. The measurement series share one grid (t = 0, 15, 30, 45, 60, 70, 78). The suppression_requirement series is authored because this story specifically traces enforcement-capacity change: the mild rise tracks the handoff from living memory — the 1933 witness generation died off around t=45 — to normative instruction carried by the elders; it never approaches coercive levels, which is why the story-level suppression scalar sits at 0.10. fixing_cost is authored 'prohibitive' on its own evidence: the constraint's content cannot be repealed by any agent — moving or removing the stone does not move the sea — and abandoning the protection was priced in 2011. That is impossibility, not the administrator-will-not-fix cost asymmetry of an inertial leftover; the theater level and the absence of any capturing seat distinguish the two cells.
 *
 * PERSPECTIVAL GAP:
 *   The elder seat and the household seat compute differently from the same artifact. Elders experience the directive as a duty they administer: their standing, their late-life role, and their memory of the telling are bound to it, so from that seat the arrangement looks like an obligation that must be kept whether or not it is ever tested. Household heads experience it as a siting rule that costs convenient land and returns survival — a trade they can evaluate against their own valley's geography. The excluded development interests, never admitted to the transmission structure, experience the same stone as an unaccountable prohibition: no explanation is owed to them, no seat exists where their objection could land, and from their position the line's authority looks arbitrary rather than earned. The analytical observer sees both the practice and the physical record that eventually checked it, which no participant seat can. The engine computes these per-seat classifications from power, horizon, and exit data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared in base_properties, deliberately: no party collects from the constraint's operation. The protection compliance buys accrues to the governed households themselves, symmetrically — the structural signature the mountain claim rests on. Per seat: the coastal households sit near symmetric, since the forgone seaward land (cost) and the siting protection (benefit) land on the same households, despite their bearing the arrangement's entire material cost. The elder transmitters sit toward the beneficiary end — they collect standing and role from stewarding the directive while bearing little material cost — but what they collect is authority within the practice, not a rent skimmed from other parties, which is why no seat is named as the extraction's recipient. The excluded development interests sit nearest the target end: the directive bars them from land they could otherwise use and returns them nothing, with no seat in which to contest it. Spatial scope is deliberately local — each stone governs its own valley — so the scope-side amplification of effective extraction has almost nothing to work with on an epsilon this small. No directionality overrides are authored: every seat differs from the others on power or exit options, so the derivation chain has what it needs.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy risk in this story runs in the unusual direction: the failure mode is not mistaking coordination for extraction but mistaking a live constraint for a dead one — reading 78 years without a validating catastrophe as evidence that the directive's function atrophied into ceremony. The behavioral_competence_reading guards against that error with the theater metric rather than elapsed time: theater stays at 0.08 because the stone's use remained behavioral (siting decisions consulted it), and the founding problem — rare-catastrophe memory outlasting unaided transmission — never lapsed, because Sanriku tsunamis recur on centennial scales and no seawall or hazard map had made the encoded line redundant during the interval. founding_problem_status is therefore 'live' and mandatrophy is not resolved. The classification also prevents the reverse mislabel: because no party collects from compliance, the arrangement cannot be read as a coordination cover for extraction, and the absence of declared beneficiaries keeps the false-summit machinery from firing on what is, under this reading, an encoded physical limit rather than a constructed rule serving identifiable interests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_underdetermination_behavioral_vs_husk,
    'Is this story''s behavioral_competence_reading the correct instantiation of the aneyoshi_stone_directive kernel for the 1933-2011 interval, or does the sibling commemorative_husk_reading describe the directive''s actual status?',
    'Village-level land-use records across the interval and post-2011 siting outcome comparisons: if dwellings in stone-marked villages clustered above the marked line throughout the validation-free decades, behavioral force was retained; if seaward construction proceeded as though the stones were memorials only, force lapsed.',
    'If the sibling reading is correct, this constraint reclassifies toward an inertial leftover retained by ceremony — high theater_ratio, atrophied function — and the mountain structure claimed here is a false summit; the epsilon referent shifts from a live rule to a dead letter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_underdetermination_behavioral_vs_husk, empirical, 'Which reading of the stone-directive kernel describes the inter-catastrophe period.').

omega_variable(
    encoded_physical_limit_vs_transmitted_norm,
    'Is the directive''s binding force the physical-geography limit the stone encodes, or the community transmission practice that keeps that limit salient across generations?',
    'Compare villages that lost, relocated, or stopped maintaining their stones against villages that retained them, holding hazard knowledge roughly constant: if siting behavior tracked the hazard wherever knowledge persisted regardless of the artifact''s fate, the operative constraint is the physics; if behavior decayed with the artifact, the practice is the constraint.',
    'If the transmission practice is the operative constraint, the structure is a constructed coordination with real maintenance cost rather than an encoded natural limit, emerges_naturally becomes false, and epsilon rises modestly to reflect the practice''s maintenance burden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(encoded_physical_limit_vs_transmitted_norm, empirical, 'Whether the stone encodes a natural limit or constitutes a maintained norm.').

omega_variable(
    village_level_heterogeneity_of_force,
    'Did the directive retain behavioral force uniformly across stone-marked villages, or did force persist at transmission-intact sites and lapse elsewhere, so that neither kernel reading is uniformly true of the coast?',
    'Per-site siting records 1933-2011 plus oral-history work distinguishing villages with intact transmission chains from villages where the chain broke; the historical coast contains both kinds of site.',
    'If force was heterogeneous, the kernel decomposes into per-site constraints with different epsilon and type profiles — this story''s classification holding for the intact subset, the sibling''s for the failed subset — rather than one coast-wide verdict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(village_level_heterogeneity_of_force, empirical, 'Whether behavioral force was uniform or site-partitioned.').

omega_variable(
    absence_of_validation_epistemics,
    'Does 78 years without an intervening tsunami evidence the directive''s robustness, or merely the absence of a test — could a mid-interval event have found the transmission already broken?',
    'Document transmission-chain continuity (witness generations, teaching practice, memorial calendar) against the known recurrence interval of Sanriku tsunamis, using the shorter 1896-to-1933 gap as the comparative case where validation did arrive.',
    'If the interval was merely test-free rather than proof of competence, the persistence claim weakens toward the sibling reading''s and this story''s confidence drops; if transmission continuity is independently documented, the reading stands without needing the 2011 validation as its sole warrant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(absence_of_validation_epistemics, empirical, 'Whether the validation-free interval evidences robustness or only absence of a test.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the kernel of this commitment system the stone inscription itself (fixed text adjudicated by lineage authority), or the community''s siting practice that the inscription anchors (implicit codification, practice authority)?',
    'Examine what disputes over the directive actually appealed to — the inscription''s wording or what the community did — and whether relocated or re-carved stones retained authority, which would indicate the practice rather than the text carries the kernel.',
    'Under the practice framing, kernel_codification shifts from fixed_text to implicit and authority_grounding from lineage to practice, changing the commitment-system classification; the arrangement''s structural classification in this story is unchanged.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel is the inscription or the practice it anchors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__behavioral_competence_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_behavioral_tr_t0, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t15, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t30, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 30, 0.06).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t45, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 45, 0.06).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t60, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 60, 0.07).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t70, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 70, 0.08).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t78, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 78, 0.08).

% Extraction over time
narrative_ontology:measurement(aneyoshi_behavioral_be_t0, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 0, 0.04).
narrative_ontology:measurement(aneyoshi_behavioral_be_t15, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 15, 0.04).
narrative_ontology:measurement(aneyoshi_behavioral_be_t30, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 30, 0.05).
narrative_ontology:measurement(aneyoshi_behavioral_be_t45, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 45, 0.05).
narrative_ontology:measurement(aneyoshi_behavioral_be_t60, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 60, 0.06).
narrative_ontology:measurement(aneyoshi_behavioral_be_t70, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 70, 0.06).
narrative_ontology:measurement(aneyoshi_behavioral_be_t78, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 78, 0.06).

% Suppression requirement over time
narrative_ontology:measurement(aneyoshi_behavioral_su_t0, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(aneyoshi_behavioral_su_t15, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 15, 0.08).
narrative_ontology:measurement(aneyoshi_behavioral_su_t30, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 30, 0.09).
narrative_ontology:measurement(aneyoshi_behavioral_su_t45, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 45, 0.1).
narrative_ontology:measurement(aneyoshi_behavioral_su_t60, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 60, 0.1).
narrative_ontology:measurement(aneyoshi_behavioral_su_t70, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 70, 0.11).
narrative_ontology:measurement(aneyoshi_behavioral_su_t78, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 78, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_directive__behavioral_competence_reading, information_standard).
narrative_ontology:affects_constraint(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_directive__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the aneyoshi tsunami stones' covers two structurally distinct claims about the same standing arrangement during the inter-catastrophe period: that the directive retained binding behavioral force (this story — negligible extraction, mountain structure claimed, low theater) and that it lost behavioral force and persisted as a commemorative husk (sibling story — atrophied function, persistence by ceremony and inertia). The two readings share the epsilon referent — the directive as it actually stood, 1933-2011 — and diverge because they assess it by different lights, so they are authored as separate epsilon-invariant constraints linked here rather than one story with a measurement parameter. The 2011 run-up record is the shared evidentiary hinge both stories cite.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
