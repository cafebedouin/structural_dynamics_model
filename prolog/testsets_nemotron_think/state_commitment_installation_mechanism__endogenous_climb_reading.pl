% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__endogenous_climb_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: state_commitment_installation_mechanism__endogenous_climb_reading
 *   human_readable: Endogenous Climb Legitimacy Mechanism
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint story instantiates the endogenous_climb_reading of the
 *   state_commitment_installation_mechanism kernel. The reading claims that
 *   new commitments gain legitimacy primarily by originating at institutional
 *   fringes, demonstrating superior problem-solving, and gradually climbing
 *   to systemic acceptance. The mechanism coordinates the absorption of
 *   innovation without systemic rupture but extracts demonstration costs from
 *   fringe actors and transition costs from apex institutions. The
 *   claim/metric independence is maintained: the reading claims this is a
 *   genuine coordination mechanism (rope-like), while the authored metrics
 *   reveal substantial extractive and suppressive dimensions consistent with
 *   tangled_rope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__endogenous_climb_reading, 0.42).
domain_priors:suppression_score(state_commitment_installation_mechanism__endogenous_climb_reading, 0.38).
domain_priors:theater_ratio(state_commitment_installation_mechanism__endogenous_climb_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__endogenous_climb_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__endogenous_climb_reading, "Endogenous Climb Legitimacy Mechanism").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__endogenous_climb_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__endogenous_climb_reading, '4637bd13-bb2c-45bd-b752-f1fa927782f8').
narrative_ontology:cs_kernel_codification('4637bd13-bb2c-45bd-b752-f1fa927782f8', distributed).
narrative_ontology:cs_authority_grounding('4637bd13-bb2c-45bd-b752-f1fa927782f8', practice).
narrative_ontology:cs_interpretation_layer_present('4637bd13-bb2c-45bd-b752-f1fa927782f8').
narrative_ontology:cs_reading_relation('4637bd13-bb2c-45bd-b752-f1fa927782f8', state_commitment_installation_mechanism__exogenous_imposition_reading, coexists_with).
narrative_ontology:cs_reading_relation('4637bd13-bb2c-45bd-b752-f1fa927782f8', state_commitment_installation_mechanism__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('4637bd13-bb2c-45bd-b752-f1fa927782f8', foundational, legitimacy_requires_demonstrated_superiority).
narrative_ontology:cs_axiom_status(legitimacy_requires_demonstrated_superiority, holdable).
narrative_ontology:cs_axiom_grounding('4637bd13-bb2c-45bd-b752-f1fa927782f8', legitimacy_requires_demonstrated_superiority, empirically_contingent).
narrative_ontology:cs_axiom('4637bd13-bb2c-45bd-b752-f1fa927782f8', foundational, fringe_as_primary_legitimacy_source).
narrative_ontology:cs_axiom_status(fringe_as_primary_legitimacy_source, holdable).
narrative_ontology:cs_axiom_grounding('4637bd13-bb2c-45bd-b752-f1fa927782f8', fringe_as_primary_legitimacy_source, empirically_contingent).
narrative_ontology:cs_reference_frame('4637bd13-bb2c-45bd-b752-f1fa927782f8', historical_legitimacy_contests).
narrative_ontology:cs_drift_state('4637bd13-bb2c-45bd-b752-f1fa927782f8', contemporary_state_formation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4637bd13-bb2c-45bd-b752-f1fa927782f8', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_reformers).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, grassroots_advocates).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, apex_institutions).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, failed_fringe_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, grassroots_advocates).
narrative_ontology:constraint_vindicates(state_commitment_installation_mechanism__endogenous_climb_reading, legitimacy_through_demonstrated_superiority).
narrative_ontology:constraint_vindicates(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_as_innovation_source).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actors at institutional margins who develop new commitments and demonstrate their superiority through pilot projects, local governance, or intellectual work. They gain a legitimate pathway to systemic adoption but bear the full cost of demonstration — resources, reputation, time — with no guarantee of success. Exit means abandoning the commitment or seeking patronage from existing power.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_reformers, beneficiary,
    moderate, biographical, constrained, national).

% Movements and networks that amplify fringe demonstrations into broader legitimacy claims. They benefit when their adopted commitments climb, but their identity fuses with the commitment — exit means abandoning a core constituency and narrative. They invest organizing capital that is lost if the climb fails.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, grassroots_advocates, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__endogenous_climb_reading, grassroots_advocates, payer).

% Central state bodies, established churches, or dominant professional orders that initially resist fringe commitments. They bear the cost of resistance (legitimacy erosion, repression apparatus) and eventually the transition cost (restructuring, personnel replacement, doctrinal revision). They cannot exit the mechanism — they are the terrain on which the climb occurs — but they can shape its terms through gatekeeping.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, apex_institutions, payer,
    institutional, generational, constrained, national).

% Fringe actors whose commitments fail the demonstration test — their pilots collapse, their ideas prove inferior, or they are outcompeted. They bear the full demonstration cost with zero legitimacy return. No institutional exit exists; they return to marginality or are absorbed by the apex as cautionary examples.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, failed_fringe_actors, payer,
    powerless, immediate, trapped, local).

% Religious hierarchies, hereditary elites, or customary law councils whose legitimating monopoly is eroded by the climb mechanism. They would object that legitimacy derives from tradition or divine mandate, not demonstrated superiority. Their exclusion is structural — the mechanism defines them as the old order to be climbed past.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, traditional_legitimating_authorities, excluded,
    organized, civilizational, identity_locked, national).

% Scholars who study the mechanism across cases and eras. They see the full structure — the fringe beneficiaries, the apex payers, the failed actors — but neither collect from nor pay into the mechanism. Their analysis can influence how the mechanism is understood and whether it is institutionalized.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legitimate, non-violent pathway for new commitments to gain systemic acceptance by requiring them to prove superior problem-solving capacity at smaller scales before scaling up — solving the 'legitimacy gap' between innovation and institutionalization.
% TRANSFER_FUNCTION: Moves legitimacy upward from demonstrated local success to systemic authority; moves demonstration costs (resources, risk, organizing labor) to fringe actors; moves transition costs (restructuring, legitimacy repair) to apex institutions.
% ABSENT_VOICES: Traditional legitimating authorities (religious, hereditary, customary) are structurally excluded — they would claim legitimacy flows from continuity and mandate, not demonstration. Populations subject to new commitments without participatory voice in the demonstration process are also absent — the mechanism assumes demonstration speaks for them.
% DISAPPEARANCE_RATIONALE: If the endogenous climb mechanism vanished overnight, new commitments would have no legitimate path to systemic adoption except exogenous imposition (conquest, revolution, decree) or hybrid cascade (apex installation with fringe validation). The legitimacy economy would reorganize around force or fiat, increasing violent contested succession and decreasing experimental policy innovation.
% FOUNDING_PROBLEM: How can a polity legitimate new commitments — administrative reforms, rights expansions, doctrinal updates — without each one triggering a legitimacy crisis that threatens the whole order? The climb mechanism was built to channel innovation through a controlled, demonstrative filter rather than letting it rupture the system.
% FOUNDING_PROBLEM_CORROBORATION: Historical sociologists outside the benefiting parties (Tilly on state formation, Mann on IEMP model, Fukuyama on political order) attest that the problem of legitimate innovation absorption persists across eras. The mechanism's beneficiaries (reformers, advocates) claim the problem is live; apex institutions often claim it is dead (their legitimacy is self-sustaining). Independent corroboration comes from comparative history showing recurrent climb patterns in Meiji Japan, Progressive Era US, post-colonial state building.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__endogenous_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__endogenous_climb_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__endogenous_climb_reading_tests).
:- end_tests(state_commitment_installation_mechanism__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the real costs borne by fringe actors (demonstration investment with high failure rate) and apex institutions (resistance and transition costs). Suppression (0.38) captures the gatekeeping that excludes non-demonstrated commitments and the structural exclusion of traditional legitimating authorities. Theater ratio (0.28) measures the gap between the mechanism's declared meritocratic logic and its actual operation — demonstration criteria often reflect apex preferences, and successful climbs frequently depend on patronage rather than pure superiority. Accessibility collapse (0.55) is moderate: alternative legitimacy paths (exogenous imposition, hybrid cascade) exist but are structurally disadvantaged. Resistance (0.62) is high because apex institutions actively shape demonstration criteria and fringe actors contest the mechanism's fairness.
 *
 * PERSPECTIVAL GAP:
 *   From the fringe reformer seat, the mechanism is a genuine rope — it coordinates their access to systemic legitimacy. From the apex institution seat, it is a snare — it forces costly transitions and erodes their legitimating monopoly. From the failed fringe actor seat, it is a snare with no coordination benefit. The engine computes this divergence from the structural data; the authored claim (tangled_rope) reflects the analyst's view that all three seats experience the same structure differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Fringe reformers and grassroots advocates are structural beneficiaries (d ≈ 0.2-0.3) — they gain a path to legitimacy that would not exist otherwise, though they pay demonstration costs. Apex institutions are payers (d ≈ 0.7-0.8) — they bear transition costs and legitimacy erosion during the climb, and their gatekeeping power is the enforcement mechanism. Failed fringe actors are full targets (d ≈ 0.9) — they pay demonstration costs with zero return and have no exit. Traditional legitimating authorities are excluded (d not computed) — their exclusion is the mechanism's boundary condition. Historical sociologists are analytical observers (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimate innovation absorption) remains live — polities still need to adopt new commitments without rupture. However, the mechanism's extraction has accumulated: demonstration costs have professionalized (think tanks, pilot programs, metric regimes) while apex transition costs have grown (bureaucratic entrenchment, veto players). The mechanism persists not because it solves the founding problem efficiently but because no institutional actor can unilaterally replace it — exogenous imposition is illegitimate, hybrid cascade requires the same demonstration infrastructure. This is mandatrophy: the mechanism's mandate (legitimate innovation) has outlived its efficient form, but the constraint remains due to institutional inertia and the absence of a legitimate alternative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demonstration_criteria_objectivity,
    'Are the criteria for ''demonstrated superiority'' objectively measurable problem-solving metrics, or are they socially constructed by apex institutions to filter threats?',
    'Comparative case analysis: if commits with objectively superior outcomes (health, wealth, stability) consistently climb across diverse polities, criteria track objectivity; if climb correlates with apex ideological alignment, criteria are constructed.',
    'If criteria are constructed, the mechanism''s coordination function is compromised — it becomes a legitimacy filter for apex-preferred innovations, increasing its extractive classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demonstration_criteria_objectivity, empirical, 'Objectivity of the demonstration test that gates the climb.').

omega_variable(
    committer_structure_ambiguity,
    'Is the endogenous climb a descriptive historical pattern (how legitimacy has worked) or a prescriptive institutional design (how it should work)?',
    'Discourse analysis of the reading''s proponents: do they describe the mechanism as an empirical regularity to be studied, or as a normative principle to be institutionalized?',
    'If prescriptive, the constraint is an institutional design with engineered extraction; if descriptive, it is an emergent pattern whose extraction is incidental. Affects whether mandatrophy_resolved applies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_structure_ambiguity, conceptual, 'Descriptive vs. prescriptive status of the endogenous climb claim.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the endogenous climb reading logically foreclose the exogenous imposition reading within a single polity''s legitimacy framework?',
    'Theoretical analysis: can a polity simultaneously hold that legitimacy comes from fringe demonstration AND from top-down mandate? Historical cases (Meiji Japan, Atatürk''s Turkey) suggest hybridity is possible.',
    'If forecloses, the readings are mutually exclusive frameworks; if coexists_with, they are competing explanations for different cases or phases. Determines cs_structure.reading_relations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical relationship between endogenous climb and exogenous imposition readings.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional gatekeeping, legal barriers) or internalized (fringe actors self-censor to match demonstrable criteria)?',
    'Post-exit suppression trajectory: if fringe actors who leave the climb mechanism still frame their commitments in demonstration terms, suppression is partially internalized.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint''s categories colonize the fringe''s imagination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the climb mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__endogenous_climb_reading, 1750, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1750, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 1750, 0.15).
narrative_ontology:measurement(stat_tr_t1800, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 1800, 0.18).
narrative_ontology:measurement(stat_tr_t1850, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 1850, 0.22).
narrative_ontology:measurement(stat_tr_t1900, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 1900, 0.25).
narrative_ontology:measurement(stat_tr_t1950, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 1950, 0.27).
narrative_ontology:measurement(stat_tr_t2000, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(stat_tr_t2025, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(stat_be_t1750, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 1750, 0.25).
narrative_ontology:measurement(stat_be_t1800, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 1800, 0.3).
narrative_ontology:measurement(stat_be_t1850, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 1850, 0.35).
narrative_ontology:measurement(stat_be_t1900, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 1900, 0.38).
narrative_ontology:measurement(stat_be_t1950, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement(stat_be_t2000, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 2000, 0.41).
narrative_ontology:measurement(stat_be_t2025, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1750, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 1750, 0.45).
narrative_ontology:measurement(stat_su_t1800, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 1800, 0.42).
narrative_ontology:measurement(stat_su_t1850, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 1850, 0.4).
narrative_ontology:measurement(stat_su_t1900, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 1900, 0.38).
narrative_ontology:measurement(stat_su_t1950, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 1950, 0.35).
narrative_ontology:measurement(stat_su_t2000, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 2000, 0.36).
narrative_ontology:measurement(stat_su_t2025, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__endogenous_climb_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(state_commitment_installation_mechanism__endogenous_climb_reading, 0.08).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism__exogenous_imposition_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the kernel 'state_commitment_installation_mechanism' into three structurally distinct legitimacy mechanisms. The endogenous climb reading (this story) has ε=0.42 (tangled_rope) because demonstration costs are real and asymmetrically borne. The exogenous imposition reading has lower ε (rope) — the mandate authority bears coordination cost. The hybrid cascade reading has higher ε (tangled_rope→snare) — it combines apex extraction with fringe validation costs. The ε values differ because each reading names a different constraint with different beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_commitment_installation_mechanism__endogenous_climb_reading, organized, 0.35).
constraint_indexing:directionality_override(state_commitment_installation_mechanism__endogenous_climb_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
