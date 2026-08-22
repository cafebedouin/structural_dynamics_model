% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__catastrophe_validation_axis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
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
    narrative_ontology:measurement_basis/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: tsunami_stone_commitment__catastrophe_validation_axis
 *   human_readable: 2011 Tsunami as Decisive Commitment Test (Catastrophe Validation Axis)
 *   domain: disaster anthropology / institutional memory
 *
 * SUMMARY:
 *   On the Sanriku coast, stone stelae erected and re-erected after the 1896
 *   and 1933 tsunamis carve recorded inundation heights and injunctions
 *   against building below them. This story instantiates ONE reading of the
 *   tsunami-stone commitment kernel: the 2011 Tohoku tsunami as the
 *   commitment's decisive empirical test - a party-free physical axis on
 *   which the commitment's validity was adjudicated by observed outcomes. The
 *   mechanism (run-up versus marked line) is natural fact: it would operate
 *   identically regardless of who defended the stones, collects nothing from
 *   its operation, and cannot be opposed, only complied with or died of. Its
 *   evidentiary output - above-line settlement survival alongside run-up
 *   exceedance of carved heights at other surveyed points - is the raw
 *   material both sibling readings consume. KEY AGENTS (by structural
 *   relationship): intentionally NONE. This axis is the kernel's only
 *   seat-free component, which is precisely what qualifies it to arbitrate
 *   between the seated readings; the party-bearing structure (transmitting
 *   elders, compliant and non-compliant settlements, ceremonial committees,
 *   reconstruction authorities) is authored in the sibling stories. No
 *   beneficiaries or victims are declared here, and accordingly no
 *   stakeholders are authored - the exempt genuine-mountain case.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__catastrophe_validation_axis, 0.04).
domain_priors:suppression_score(tsunami_stone_commitment__catastrophe_validation_axis, 0.52).
domain_priors:theater_ratio(tsunami_stone_commitment__catastrophe_validation_axis, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, extractiveness, 0.04).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, accessibility_collapse, 0.93).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__catastrophe_validation_axis, mountain).
narrative_ontology:human_readable(tsunami_stone_commitment__catastrophe_validation_axis, "2011 Tsunami as Decisive Commitment Test (Catastrophe Validation Axis)").
narrative_ontology:topic_domain(tsunami_stone_commitment__catastrophe_validation_axis, "disaster anthropology / institutional memory").

domain_priors:emerges_naturally(tsunami_stone_commitment__catastrophe_validation_axis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__catastrophe_validation_axis, '895a6909-5fad-44a1-90ee-58a0b0cf0433').
narrative_ontology:cs_kernel_codification('895a6909-5fad-44a1-90ee-58a0b0cf0433', fixed_text).
narrative_ontology:cs_authority_grounding('895a6909-5fad-44a1-90ee-58a0b0cf0433', lineage).
narrative_ontology:cs_interpretation_layer_present('895a6909-5fad-44a1-90ee-58a0b0cf0433').
narrative_ontology:cs_reading_relation('895a6909-5fad-44a1-90ee-58a0b0cf0433', tsunami_stone_commitment__behavioral_competence_reading, influences).
narrative_ontology:cs_reading_relation('895a6909-5fad-44a1-90ee-58a0b0cf0433', tsunami_stone_commitment__commemorative_husk_reading, influences).
narrative_ontology:cs_axiom('895a6909-5fad-44a1-90ee-58a0b0cf0433', foundational, catastrophe_outcomes_adjudicate_commitment).
narrative_ontology:cs_axiom_status(catastrophe_outcomes_adjudicate_commitment, holdable).
narrative_ontology:cs_axiom_grounding('895a6909-5fad-44a1-90ee-58a0b0cf0433', catastrophe_outcomes_adjudicate_commitment, empirically_contingent).
narrative_ontology:cs_axiom('895a6909-5fad-44a1-90ee-58a0b0cf0433', foundational, marked_line_binds_maximum_credible_event).
narrative_ontology:cs_axiom_status(marked_line_binds_maximum_credible_event, holdable).
narrative_ontology:cs_axiom_grounding('895a6909-5fad-44a1-90ee-58a0b0cf0433', marked_line_binds_maximum_credible_event, empirically_contingent).
narrative_ontology:cs_reference_frame('895a6909-5fad-44a1-90ee-58a0b0cf0433', falsifiable_hazard_boundary_claim).
narrative_ontology:cs_drift_state('895a6909-5fad-44a1-90ee-58a0b0cf0433', post_2011_runup_survey, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('895a6909-5fad-44a1-90ee-58a0b0cf0433', '2026-08-12T09:30:00Z').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__catastrophe_validation_axis, locational_abstinence_above_carved_line_reduces_mortality).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__catastrophe_validation_axis, carved_inscription_outlives_generational_memory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Anchors settlement-location decisions to a durable record of past inundation extents: the carved line carries hazard knowledge across generations whose living memory cannot reach the last catastrophic test. Stated without evaluation: it coordinates land use against a stochastic hazard recurring on a multi-generational interval.
% TRANSFER_FUNCTION: Moves habitable land off the shorefront - foregone shorefront value borne by would-be builders in exchange for reduced mortality exposure for later generations. Each test firing additionally moves evidentiary weight: survival and destruction outcomes flow into the community's credibility accounting for the commitment.
% ABSENT_VOICES: The generations erased by the 1896 and 1933 waves cannot attest; the stones speak in their place and their detailed testimony is unrecoverable. Post-2011, households economically bound to the shore (fisheries, processing, port trade) object to locational restriction through compensation politics and relocation litigation rather than through the commitment's own forum - represented, not present.
% DISAPPEARANCE_RATIONALE: Nothing is organized to maintain, defend, or administer the physical axis; it is self-executing physics. If it ceased - if no great wave ever came again - no enforcement machinery would wind down and no service would lapse; the stones would complete their drift into heritage, and settlement would creep seaward over generations. The rearrangement would be epistemic (the kernel loses its adjudicator and the sibling readings become permanently unfalsifiable) rather than material.
% FOUNDING_PROBLEM: Recurrent catastrophic amnesia: Sanriku tsunamis recur on intervals longer than reliable oral transmission (869 Jogan, 1611 Keicho, 1896, 1933), so each generation resettles the inundation zone unless an external anchor carries the line forward. The stelae were erected as durable injunctions pinning the remembered waterline against memory decay.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside any benefiting party: the paleotsunami sediment record (Jogan-era sand sheets beneath the Sendai plain, mapped before 2011), plate-coupling seismology on the Japan trench, and the 2011 joint-survey run-up data all attest that the hazard and its recurrence are real independently of anyone invested in the stones' authority. No party attests the founding problem is dead.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__catastrophe_validation_axis, world_unchanged).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__catastrophe_validation_axis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__catastrophe_validation_axis, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tsunami_stone_commitment__catastrophe_validation_axis, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__catastrophe_validation_axis, 0.04, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is 0.04: the commitment's compliance cost (foregone shorefront) prices as coordination cost, not rent collection; no seat accrues the test's output. Suppression is 0.52 at interval end, matching the series: the post-test state ratchet (disaster-danger-zone designations and rebuilding restrictions imposed within months of the 2011 inundation) rebuilt enforcement machinery after five decades of decay. Theater is 0.08: the test's firing surged functional activity (run-up surveying, relocation planning) and displaced the ceremonial accretion that had grown around the markers through the late interval. Accessibility collapse is 0.93: hydrodynamics admits no negotiated alternative once the mechanism is understood. Resistance is 0.05: the physical axis meets no opposition; what looks like resistance (shorefront development pressure) targets the commitment layer, not the physics. Claim/metric independence is deliberate: the claimed mountain describes the physical axis per the expected structural delta, while the metrics describe the standing arrangement including its enforcement shell - the divergence is diagnostic, showing the kernel is a composite (mountain core, enforced commitment shell) rather than tuning the claim to a predicted verdict. No boltzmann coordination_type is declared: the axis itself performs no coordination, and attributing one would import the sibling dispute over whether the commitment still coordinates. Temporal pattern is punctuated equilibrium, not smooth drift: enforcement ratchets after each test firing (1896, 1933, 2011) and decays between firings under generational turnover and growth-era economics, while theater accretes between firings and drops when a test refocuses function. The oscillation is not noise - the inter-test decay phase is exactly what the husk reading observes, and the post-test ratchet is exactly what the competence reading observes; the cycle's phases ARE the siblings' evidence. All series share one six-point grid (0=1896, 37=1933, 64=1960, 100=1996, 113=2009, 115=2011); base_properties scalars match interval-end values.
 *
 * PERSPECTIVAL GAP:
 *   Within this reading there are no seats to diverge - that absence is the design. The gap lives BETWEEN readings: from the test axis, the commitment's status is an open empirical question settled only by outcomes; from the competence seat, it is a lived practice whose enforcement is directly witnessed; from the husk seat, it is a relic whose compliance was ambient. Same coastline, three constraints. The engine computes per-seat classifications where seats are authored; this story authors the seat-free axis so that the between-reading divergence has a common measuring stick rather than three incommensurable vantages.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionalities are derived because no beneficiaries or victims are declared: the physical axis subsidizes no one and extracts from no one, d is undefined for all indices, and effective extraction collapses to base epsilon. Suppression remains a raw structural property of the arrangement (unscaled); extractiveness would be scaled by directionality and scope in the engine's computation, but with no positional atoms authored there is nothing to scale - any chi computation reduces to epsilon. The mechanism operates regionally (Sanriku margin), but scope atoms ride the stakeholder surface, which this exempt mountain omits. Where beneficiary and victim structure exists, it belongs to the sibling stories: transmitting elders and above-line settlements on the competence side, ceremonial maintenance bodies on the husk side.
 *
 * MANDATROPHY ANALYSIS:
 *   The mountain claim protects the adjudication axis from two misclassifications: as rope (the axis coordinates nothing itself - it verifies) and as piton (its dormancy between tests is not atrophy; the mechanism is fully operational when the test fires, however rarely, and the low theater ratio confirms maintenance is not merely performative). Conversely, the omega structure prevents the mountain claim from laundering the commitment layer's constructed enforcement history as natural law: the suppression series documents ratcheting human enforcement wrapped around a natural core, and omega enforcement_vehicle_substitution keeps the vehicle-attribution question open. Mandatrophy is not resolved: the founding problem (multi-generational hazard amnesia) is live, no sunset clause applies to physics, and the axis cannot retire while the hazard persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexical,
    'This constraint is one reading of kernel tsunami_stone_commitment (reading: catastrophe_validation_axis). Does instantiating the 2011 event as a party-free binary test mechanism capture the kernel''s commitment structure, or does the test-axis framing smuggle in retrospective selection?',
    'Comparative classification across the three sibling stories: the behavioral_competence_reading should yield an enforced-norm arrangement profile (transmitting elders as agenda_setters, compliant settlements as beneficiaries), the commemorative_husk_reading should yield an inertial/performance profile, and this reading should certify the physical axis as mountain. Convergence on the axis plus divergence on the commitment layer confirms the decomposition.',
    'Sibling readings relocate the classification of the commitment layer entirely; this story''s mountain classification holds only for the physical adjudication axis. If the test-axis framing is itself retrospective, the axis loses its claim to neutrality and the kernel needs re-decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexical, conceptual, 'Committer structure: which kernel, which reading, what siblings would change, where the disagreement is located.').

omega_variable(
    binary_evidence_partial_exceedance,
    'Was the 2011 test actually binary? At least one settlement built above its stone''s recorded height survived the 2011 inundation intact, while at multiple surveyed points the 2011 run-up exceeded every carved historical height and some markers were toppled or washed out. Does the evidence constitute binary validation or a mixed verdict?',
    'Point-by-point comparison of stone-recorded heights against the 2011 joint-survey run-up dataset, classifying each marker as vindicated, exceeded, or destroyed.',
    'If validation is partial, this reading''s decisive-binary claim degrades to graded evidence: the commitment''s logic is vindicated (abstinence above the line worked where the line held) while specific markers'' adequacy is falsified (the historical envelope did not bound the maximum credible event). Graded evidence weakens this axis''s adjudicative authority and feeds the husk reading''s coincidence thesis for the exceeded markers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(binary_evidence_partial_exceedance, empirical, 'Whether the 2011 outcome distribution supports binary validation or mixed partial validation.').

omega_variable(
    hindsight_selection_of_decisive_test,
    'Decisive is a post hoc designation: ex ante, the 2011 event was one of many possible tests, and both sibling readings can absorb its outcome narratively (vindication narrative versus coincidence narrative). Can this axis adjudicate between the siblings at all, or does every test merely accumulate confirmatory material for whichever reading survives?',
    'Prospective pre-registration of rival predictions for the next major test (Nankai-trough-class event): what each reading predicts for enforcement persistence, compliance differentials, and marker adequacy before the wave arrives.',
    'Determines whether the catastrophe-validation axis is a genuine adjudication device feeding both siblings or an unfalsifiable confirmation machine; if the latter, this reading''s structural delta (adjudication device) fails and the kernel contest is resolved by persuasion rather than evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hindsight_selection_of_decisive_test, conceptual, 'Whether post hoc designation of a decisive test preserves adjudicative force.').

omega_variable(
    enforcement_vehicle_substitution,
    'The suppression series spans distinct enforcement vehicles: village-level norm enforcement (1896-1933), engineered seawall substitution (1960 onward), and state hazard-zone rebuilding restrictions (post-2011). Is the post-2011 ratchet enforcement OF the stone commitment, or of a successor modeled-hazard arrangement that replaced it?',
    'Trace legal continuity: whether post-2011 rebuilding restrictions cite or incorporate the inherited marked line, or rest exclusively on newly modeled inundation projections that supersede it.',
    'If the ratchet enforces a successor arrangement, this constraint''s end-state suppression reverts toward the pre-ratchet baseline (approximately 0.12), the mountain profile certifies cleanly, and the ratchet belongs to a separate downstream constraint story linked via the network edge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vehicle_substitution, conceptual, 'Vehicle continuity versus substitution in the commitment''s enforcement history.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__catastrophe_validation_axis, 0, 115).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsc_catastrophe_validation_tr_t0, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 0, 0.02).
narrative_ontology:measurement_basis(tsc_catastrophe_validation_tr_t0, observed).
narrative_ontology:measurement(tsc_catastrophe_validation_tr_t37, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 37, 0.05).
narrative_ontology:measurement_basis(tsc_catastrophe_validation_tr_t37, observed).
narrative_ontology:measurement(tsc_catastrophe_validation_tr_t64, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 64, 0.08).
narrative_ontology:measurement_basis(tsc_catastrophe_validation_tr_t64, observed).
narrative_ontology:measurement(tsc_catastrophe_validation_tr_t100, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 100, 0.13).
narrative_ontology:measurement_basis(tsc_catastrophe_validation_tr_t100, observed).
narrative_ontology:measurement(tsc_catastrophe_validation_tr_t113, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 113, 0.16).
narrative_ontology:measurement_basis(tsc_catastrophe_validation_tr_t113, observed).
narrative_ontology:measurement(tsc_catastrophe_validation_tr_t115, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 115, 0.08).
narrative_ontology:measurement_basis(tsc_catastrophe_validation_tr_t115, observed).

% Extraction over time
narrative_ontology:measurement(tsc_catastrophe_validation_be_t0, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 0, 0.03).
narrative_ontology:measurement_basis(tsc_catastrophe_validation_be_t0, observed).
narrative_ontology:measurement(tsc_catastrophe_validation_be_t37, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 37, 0.04).
narrative_ontology:measurement_basis(tsc_catastrophe_validation_be_t37, observed).
narrative_ontology:measurement(tsc_catastrophe_validation_be_t64, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 64, 0.04).
narrative_ontology:measurement_basis(tsc_catastrophe_validation_be_t64, observed).
narrative_ontology:measurement(tsc_catastrophe_validation_be_t100, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 100, 0.04).
narrative_ontology:measurement_basis(tsc_catastrophe_validation_be_t100, observed).
narrative_ontology:measurement(tsc_catastrophe_validation_be_t113, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 113, 0.04).
narrative_ontology:measurement_basis(tsc_catastrophe_validation_be_t113, observed).
narrative_ontology:measurement(tsc_catastrophe_validation_be_t115, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 115, 0.04).
narrative_ontology:measurement_basis(tsc_catastrophe_validation_be_t115, observed).

% Suppression requirement over time
narrative_ontology:measurement(tsc_catastrophe_validation_su_t0, tsunami_stone_commitment__catastrophe_validation_axis, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(tsc_catastrophe_validation_su_t0, observed).
narrative_ontology:measurement(tsc_catastrophe_validation_su_t37, tsunami_stone_commitment__catastrophe_validation_axis, suppression_requirement, 37, 0.44).
narrative_ontology:measurement_basis(tsc_catastrophe_validation_su_t37, observed).
narrative_ontology:measurement(tsc_catastrophe_validation_su_t64, tsunami_stone_commitment__catastrophe_validation_axis, suppression_requirement, 64, 0.26).
narrative_ontology:measurement_basis(tsc_catastrophe_validation_su_t64, observed).
narrative_ontology:measurement(tsc_catastrophe_validation_su_t100, tsunami_stone_commitment__catastrophe_validation_axis, suppression_requirement, 100, 0.16).
narrative_ontology:measurement_basis(tsc_catastrophe_validation_su_t100, observed).
narrative_ontology:measurement(tsc_catastrophe_validation_su_t113, tsunami_stone_commitment__catastrophe_validation_axis, suppression_requirement, 113, 0.12).
narrative_ontology:measurement_basis(tsc_catastrophe_validation_su_t113, observed).
narrative_ontology:measurement(tsc_catastrophe_validation_su_t115, tsunami_stone_commitment__catastrophe_validation_axis, suppression_requirement, 115, 0.52).
narrative_ontology:measurement_basis(tsc_catastrophe_validation_su_t115, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment__behavioral_competence_reading).
narrative_ontology:affects_constraint(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the tsunami stones worked' conflates three structurally distinct claims, decomposed per the epsilon-invariance principle into a three-story family: (1) the physical adjudication axis (this story - mountain, epsilon approximately 0.04, no parties); (2) the live behavioral-enforcement arrangement (sibling story - expected enforced-norm profile with transmitting elders as agenda_setters and above-line settlements as beneficiaries); (3) the commemorative residue (sibling story - expected inertial/performance profile). Each story carries its own epsilon, beneficiaries, and classification; edges link the family. The physical axis is upstream: its test firings supply the evidence both siblings interpret, which is why both reading_relations from this story are influences. This story's epsilon refers to the standing arrangement under contest - the stone commitment - assessed by the validation reading's own lights, never to the endorsed alternative arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
