% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__catastrophe_validation_axis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__catastrophe_validation_axis, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: tsunami_stone_commitment__catastrophe_validation_axis
 *   human_readable: 2011 Tohoku Tsunami as Decisive Binary Test of the Stone Commitment
 *   domain: disaster_anthropology/commitment_system_analysis/institutional_memory
 *
 * SUMMARY:
 *   On the Sanriku coast, inscribed stone markers record the high-water lines
 *   of earlier tsunamis and warn against building below them. This file
 *   instantiates ONE reading of that kernel — the catastrophe_validation_axis
 *   reading — under which the 2011 Tohoku tsunami constitutes the kernel's
 *   decisive empirical test: a physical constraint serving as a
 *   commitment-test mechanism whose outcome distribution sorted settlements
 *   into survival and loss along the compliance gradient. Per the
 *   epsilon-referent rule, epsilon is authored for the standing arrangement
 *   under contest (the stone-commitment system) as THIS reading assesses it:
 *   a faithful encoding of a self-executing physical limit, hence low
 *   extraction. The sibling readings — behavioral_competence_reading and
 *   commemorative_husk_reading — are separate constraints in separate files,
 *   linked through network.affects_constraints; their contests are not
 *   averaged into this file's epsilon.
 *
 * KEY AGENTS:
 *   - - inundation_zone_communities: Primary target (moderate/constrained) — occupies the tested side of the boundary; bore the 2011 losses
 *   - - high_ground_descendant_villages: Arrangement-level beneficiary (moderate/mobile) — compliance position converted transmitted warning into survival margin
 *   - - stone_erector_lineages: Agenda-setter of the arrangement under contest (organized/identity_locked) — cut the adjudication reference into stone and bound descendants to it
 *   - - municipal_planners_postwar: Dual-positioned (payer primary, agenda_setter secondary) — authorized below-line development, then bore its destruction
 *   - - jogan_tsunami_survivor_communities: Excluded voice — the original witnesses, present only as sediment and inscription
 *   - - disaster_researchers: Analytical observer — reads the outcome distribution as evidence and adjudicates its reach
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__catastrophe_validation_axis, 0.15).
domain_priors:suppression_score(tsunami_stone_commitment__catastrophe_validation_axis, 0.08).
domain_priors:theater_ratio(tsunami_stone_commitment__catastrophe_validation_axis, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, extractiveness, 0.15).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, resistance, 0.06).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__catastrophe_validation_axis, mountain).
narrative_ontology:human_readable(tsunami_stone_commitment__catastrophe_validation_axis, "2011 Tohoku Tsunami as Decisive Binary Test of the Stone Commitment").
narrative_ontology:topic_domain(tsunami_stone_commitment__catastrophe_validation_axis, "disaster_anthropology/commitment_system_analysis/institutional_memory").

domain_priors:emerges_naturally(tsunami_stone_commitment__catastrophe_validation_axis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__catastrophe_validation_axis, 'a3129dd0-2b32-451b-9a8c-f7356e75c69f').
narrative_ontology:cs_kernel_codification('a3129dd0-2b32-451b-9a8c-f7356e75c69f', fixed_text).
narrative_ontology:cs_authority_grounding('a3129dd0-2b32-451b-9a8c-f7356e75c69f', self_enforcing).
narrative_ontology:cs_reading_relation('a3129dd0-2b32-451b-9a8c-f7356e75c69f', tsunami_stone_commitment__behavioral_competence_reading, influences).
narrative_ontology:cs_reading_relation('a3129dd0-2b32-451b-9a8c-f7356e75c69f', tsunami_stone_commitment__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('a3129dd0-2b32-451b-9a8c-f7356e75c69f', foundational, catastrophic_outcome_binary_adjudication).
narrative_ontology:cs_axiom_status(catastrophic_outcome_binary_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('a3129dd0-2b32-451b-9a8c-f7356e75c69f', catastrophic_outcome_binary_adjudication, empirically_contingent).
narrative_ontology:cs_axiom('a3129dd0-2b32-451b-9a8c-f7356e75c69f', secondary, outcome_sorting_tracks_compliance_gradient).
narrative_ontology:cs_axiom_status(outcome_sorting_tracks_compliance_gradient, holdable).
narrative_ontology:cs_axiom_grounding('a3129dd0-2b32-451b-9a8c-f7356e75c69f', outcome_sorting_tracks_compliance_gradient, empirically_contingent).
narrative_ontology:cs_reference_frame('a3129dd0-2b32-451b-9a8c-f7356e75c69f', empirically_adjudicated_hazard_commitment).
narrative_ontology:cs_drift_state('a3129dd0-2b32-451b-9a8c-f7356e75c69f', post_2011_tohoku_validation, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a3129dd0-2b32-451b-9a8c-f7356e75c69f', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_victim(tsunami_stone_commitment__catastrophe_validation_axis, inundation_zone_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__catastrophe_validation_axis, high_ground_descendant_villages).
narrative_ontology:constraint_victim(tsunami_stone_commitment__catastrophe_validation_axis, municipal_planners_postwar).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__catastrophe_validation_axis, paleotsunami_recurrence_modeling).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__catastrophe_validation_axis, jogan_earthquake_recurrence_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Occupy and work the coastal plain below the elevation of the older inscribed markers — port towns, rice paddies, and postwar industrial estates on the Sendai plain. In March 2011 the water reached kilometers inland across these holdings; roughly eighteen thousand people died, most in settlements sitting under the run-up limit an earlier generation had been warned about. Leaving means abandoning ports, fields, and family graves; staying means betting on the length of the next interval.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, inundation_zone_communities, payer,
    moderate, biographical, constrained, regional).

% Villages whose houses, shrines, and fields sit above the inscribed high-water lines their ancestors cut after earlier inundations. They kept the markers clear, walked children past them, and treated the low road as somewhere to visit rather than to live. In 2011 their settlements stood above the water; they lost relatives and trading partners below the line but not their homes. They could relocate at ordinary moving cost; they choose not to.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, high_ground_descendant_villages, beneficiary,
    moderate, generational, mobile, regional).

% Village elders, priests, and survivors of the 1896 and 1933 waves who commissioned and carved the markers — 'Remember the calamity of the great tsunami; do not build homes below this point' — and bound their descendants to maintain them. Their names are on the stones; abandoning the markers would erase the only monument their dead received. The obligation passed down as family duty rather than contract.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, stone_erector_lineages, agenda_setter,
    organized, generational, identity_locked, regional).

% Postwar prefectural and national agencies that zoned the Sendai plain for industry, housing, and expressways, treating the inscribed lines as relics of a pre-engineering age superseded by concrete seawalls. They set the siting agenda that moved thousands below the old markers, and in 2011 their projects, offices, and constituents took the losses. Their exit now runs through buyouts and managed retreat that they themselves must authorize.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, municipal_planners_postwar, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tsunami_stone_commitment__catastrophe_validation_axis, municipal_planners_postwar, agenda_setter).

% The communities that lived through the 869 Jogan inundation, whose experience is the deepest layer of evidence — preserved as sand sheets beneath the plain and, distantly, in the tradition the later stones renewed. They left no institutional seat; their testimony reaches the present only through geology and carved copies of copies. They would attest what the water did and how quickly forgetting began.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, jogan_tsunami_survivor_communities, excluded,
    powerless, civilizational, trapped, regional).

% Paleotsunami geologists, seismologists, and engineers who mapped the Jogan deposits, modeled trench rupture, and published recurrence warnings before 2011; afterward they compiled run-up surveys, sorted survival by elevation and compliance, and adjudicate what the outcome distribution does and does not prove. They hold no stake in land or lineage; their currency is whether the evidence holds.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, disaster_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tsunami_stone_commitment__catastrophe_validation_axis, diffuse).
narrative_ontology:fixing_cost_class(tsunami_stone_commitment__catastrophe_validation_axis, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves and transmits hard-won hazard knowledge across generations when living memory fails: the inscribed markers fix the observed inundation limit as a public, durable reference standard that calibrates settlement decisions against a rare, high-consequence, recurring hazard.
% TRANSFER_FUNCTION: Moves survival probability across generations at the price of foregone lowland development value: ancestors accept siting costs and transmit warnings; descendants receive elevated-safety margins. The 2011 event settled the outstanding accounts — compliance converted transmitted warnings into survival, abandonment converted them into losses.
% ABSENT_VOICES: The original witnesses — the 869 Jogan communities and the 1896 dead — are present only as sediment layers and carved characters; they cannot testify to what the markers meant when cut. Pre-2011 hazard researchers who had mapped Jogan sand deposits and published recurrence warnings sat largely outside the planning conversation that zoned the plain below the old lines.
% DISAPPEARANCE_RATIONALE: If the subduction-zone hazard vanished overnight, the arrangement loses its object: the markers become answers to a question no one asks, settlement reclaims the plain to the waterline within a generation, and the kernel contest among the three readings dissolves for lack of a test. The 2011 sorting, the stones, and the dispute itself all depend on the water arriving.
% FOUNDING_PROBLEM: After the 869 Jogan tsunami, and again after 1896, surviving communities faced the problem that lethal hazard knowledge outlives the lifespans of those who hold it: recurrence intervals of decades to centuries exceed generational memory, so each generation must be retaught by people who never saw the water. The inscribed markers were built to solve intergenerational transmission of existential siting knowledge.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside any benefiting party by the geological sediment record (Jogan-era sand sheets mapped by independent paleotsunami researchers), by plate-tectonic modeling of the Japan trench that predicts recurrence without reference to any stone, and by the 2011 instrumental record itself. What remains contested between the readings is not whether the hazard recurs but what the 2011 outcome distribution proves about the transmission apparatus.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__catastrophe_validation_axis, world_rearranges).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__catastrophe_validation_axis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__catastrophe_validation_axis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tsunami_stone_commitment__catastrophe_validation_axis, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__catastrophe_validation_axis, 0.15, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__catastrophe_validation_axis_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, ExtMetricName, E),
    domain_priors:suppression_score(tsunami_stone_commitment__catastrophe_validation_axis, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(tsunami_stone_commitment__catastrophe_validation_axis),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(tsunami_stone_commitment__catastrophe_validation_axis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the referent arrangement transfers almost nothing to anyone: the event's costs are catastrophic but uncollected — destruction without receipt. The reading indexes epsilon to the validation function, under which the event's destructive capacity is precisely what makes it evidentiary. Suppression is very low (0.08) because the adjudicator is self-executing: no enforcement machinery maintains wave physics, and any suppression measured on the Sanriku coast would belong to the behavioral sibling's enforcement claims, not to this constraint. Theater ends at 0.12: the measurement series shows a catastrophe-driven sawtooth — theater accumulates during quiet decades (moss-grown stones, heritage plaques, school visits), then each event (1896, 1933, 1960, 2011) re-functionalizes the markers overnight, collapsing performance back into function. Accessibility collapse is high (0.85) because once run-up physics is understood, the alternative of safe occupation below the line closes almost completely — 2011 demonstrated that engineered seawalls only partially reopen it. Resistance is near zero (0.06): denial and neglect are non-compliance, not resistance; the physics is unperturbed by either. The claimed type (mountain) and the metrics are authored independently: the claim rests on the no-collector structure of the adjudicator, and the engine computes per-seat classifications from the structural data. gain_flow is authored as 'diffuse' as an affirmative checked claim: every named seat was examined and none receives the event's yields — the high-ground villages receive protection through their own positioning relative to the boundary, not transfers flowing from the event's operation. fixing_cost is 'prohibitive': the adjudicator is a subduction-zone process; no seat could remove it at any cost comparable to the benefit. Note the receipt-surface cell (diffuse + prohibitive) is shaped like the piton cell, but the disanalogy is stated openly: a piton persists by inertia around an atrophied function, whereas this constraint's function is intact and was exercised in 2011 — the engine adjudicates. suppression_requirement measurements are deliberately omitted: the enforcement picture is static (self-executing physics), so the scalar in base_properties carries the whole story.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different types from identical structural data. From the inundation_zone_communities seat — moderate power, constrained exit, full-target directionality — the event is experientially indistinguishable from predation, and effective extraction amplifies accordingly. From the high_ground_descendant_villages seat the same boundary operates as protection they inherit. From the disaster_researchers seat it is a clean experiment. The story-level mountain claim stands on the absence of any collector; the per-seat divergence is the measurement this corpus exists to take, not an inconsistency to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The victims declaration drives inundation_zone_communities toward the full-target end of directionality, amplified by constrained exit (livelihoods, ports, and graves tie them to the exposed plain) and by regional scope. high_ground_descendant_villages hold role=beneficiary at the arrangement level but are deliberately NOT entered in base_properties.beneficiaries: nothing is collected from the adjudicator's operation, and entering them would falsely trigger false-summit treatment of a genuinely natural limit. Their directionality sits near the beneficiary end through compliance positioning. stone_erector_lineages are identity_locked: their legacy is constituted by the markers' meaning, so the adjudication's outcome bears on them regardless of material flow. disaster_researchers are analytical and near-symmetric. No directionality_overrides are used: the derivation from the victims declaration plus exit options produces the operative asymmetry, and the available override keys (power atoms) would collide — both village seats are moderate — risking cross-contamination of their d values. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The adjudicator has no mandate to atrophy — physics does not expire. The surrounding arrangement's mandate (transmit the warning) was renewed rather than retired by 2011. The classification work here is separation: without it, husk-side observations (weathered stones, ceremonial maintenance) would drag the physical adjudicator toward an inertial reading, while validation-side observations would let the behavioral sibling claim enforced-coordination status for what is actually self-executing nature. The sawtooth theater series records catastrophe-driven renewal, not an institutional lifecycle: mandatrophy keys on a mandate outliving its function, and the mandate cannot die while the hazard recurs. The founding-problem mismatch consumer should find status=live paired with verdict=world_rearranges — no zombie flag — which is the correct signature for a constraint whose function was exercised, not abandoned, at interval end.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the tsunami_stone_commitment kernel — the catastrophe_validation_axis reading, under which the 2011 tsunami operates as a decisive binary adjudication of the commitment''s content. What would the sibling readings change structurally, and where exactly does the disagreement bite?',
    'Cross-reading comparison on the same outcome distribution: behavioral_competence_reading attributes the survival sorting to enforced intergenerational transmission; commemorative_husk_reading attributes it to geography and coincidence. Locate the disputed element — whether the inscriptions carried behavioral force in 2011 — against evacuation-era testimony and marker-maintenance records.',
    'If the behavioral reading is right, the arrangement is a live enforced norm and this reading''s adjudication supplies its validation; if the husk reading is right, the 2011 sorting validates the physics but not the apparatus, and this reading reduces to testing geology. The disagreement is located at the transmission mechanism, not at the hazard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Which reading of the stone-commitment kernel the 2011 outcome distribution actually supports.').

omega_variable(
    binary_validation_sufficiency,
    'Does a single catastrophic event supply decisive binary validation evidence, or is the 2011 outcome distribution confounded by seawall height, evacuation timing, time of day, and local bathymetry?',
    'Replicate the compliance-elevation sorting across the 1896, 1933, and 1960 events and against paleotsunami deposit boundaries; if the sorting holds across events with differing confound structures, the binary claim strengthens; if it appears only in 2011, the decisive-test framing fails.',
    'If confounded, this reading loses adjudicative force over the kernel contest and the dispute returns to the siblings'' home terrain; if replicated, the 2011 event stands as the kernel''s decisive experiment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binary_validation_sufficiency, empirical, 'Whether one catastrophe can serve as binary adjudication or the sorting is confound-driven.').

omega_variable(
    boundary_calibration_natural_vs_constructed,
    'Is the adjudicating boundary a natural limit (physically determined maximum run-up) or a constructed threshold (stone lines marking merely the largest event their erectors had witnessed, given that 2011 run-up locally exceeded some markers)?',
    'Compare inscribed elevations against modeled Jogan-scale maximum run-up and the 2011 surveyed run-up distribution; markers set at historical-maximum elevations that were overtopped in 2011 indicate inductive calibration rather than a physically fixed limit.',
    'If constructed, part of what the 2011 test measured was the calibration quality of an inherited estimate rather than nature itself, and the arrangement drifts toward transitional-support classification; if the physics fixes the boundary independently of the stones, the mountain claim stands undiluted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_calibration_natural_vs_constructed, conceptual, 'Whether the tested boundary is fixed by physics or induced from historical maxima.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__catastrophe_validation_axis, 1896, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsc_catastrophe_val_tr_t1896, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1896, 0.12).
narrative_ontology:measurement_basis(tsc_catastrophe_val_tr_t1896, observed).
narrative_ontology:measurement(tsc_catastrophe_val_tr_t1910, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1910, 0.2).
narrative_ontology:measurement_basis(tsc_catastrophe_val_tr_t1910, observed).
narrative_ontology:measurement(tsc_catastrophe_val_tr_t1933, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1933, 0.15).
narrative_ontology:measurement_basis(tsc_catastrophe_val_tr_t1933, observed).
narrative_ontology:measurement(tsc_catastrophe_val_tr_t1960, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1960, 0.28).
narrative_ontology:measurement_basis(tsc_catastrophe_val_tr_t1960, observed).
narrative_ontology:measurement(tsc_catastrophe_val_tr_t1990, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1990, 0.44).
narrative_ontology:measurement_basis(tsc_catastrophe_val_tr_t1990, observed).
narrative_ontology:measurement(tsc_catastrophe_val_tr_t2011, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 2011, 0.12).
narrative_ontology:measurement_basis(tsc_catastrophe_val_tr_t2011, observed).

% Extraction over time
narrative_ontology:measurement(tsc_catastrophe_val_be_t1896, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1896, 0.08).
narrative_ontology:measurement_basis(tsc_catastrophe_val_be_t1896, observed).
narrative_ontology:measurement(tsc_catastrophe_val_be_t1910, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1910, 0.09).
narrative_ontology:measurement_basis(tsc_catastrophe_val_be_t1910, observed).
narrative_ontology:measurement(tsc_catastrophe_val_be_t1933, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1933, 0.1).
narrative_ontology:measurement_basis(tsc_catastrophe_val_be_t1933, observed).
narrative_ontology:measurement(tsc_catastrophe_val_be_t1960, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1960, 0.11).
narrative_ontology:measurement_basis(tsc_catastrophe_val_be_t1960, observed).
narrative_ontology:measurement(tsc_catastrophe_val_be_t1990, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1990, 0.13).
narrative_ontology:measurement_basis(tsc_catastrophe_val_be_t1990, observed).
narrative_ontology:measurement(tsc_catastrophe_val_be_t2011, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 2011, 0.15).
narrative_ontology:measurement_basis(tsc_catastrophe_val_be_t2011, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(tsunami_stone_commitment__catastrophe_validation_axis, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__catastrophe_validation_axis, information_standard).
narrative_ontology:affects_constraint(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment__behavioral_competence_reading).
narrative_ontology:affects_constraint(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the tsunami stones were proved right in 2011' conflates three structurally distinct claims with different epsilon values, decomposed per the epsilon-invariance principle. This file carries the validation-axis claim (the event as binary adjudication; negligible extraction, no collector). behavioral_competence_reading carries the enforcement-force claim (a live transmitted norm; its epsilon turns on whether enforcement machinery existed). commemorative_husk_reading carries the decay claim (symbolic residue; its epsilon turns on theater and inertia). Upstream/downstream: this reading sits mid-chain — the 2011 outcome distribution feeds the behavioral claim's credibility (validated content strengthens the transmission tradition) and pressures the husk claim (a decisive test is difficult to reconcile with coincidental compliance). Each member links the others; no member averages across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
