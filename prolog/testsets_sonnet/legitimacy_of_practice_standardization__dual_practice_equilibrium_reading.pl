% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, []).

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
 *   constraint_id: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
 *   human_readable: Domain-Partitioned Practice Legitimacy (Dual Practice Equilibrium)
 *   domain: political_history/modernization_studies/institutional_change
 *
 * SUMMARY:
 *   This story instantiates the dual practice equilibrium reading of the
 *   practice-standardization legitimacy kernel: state authority and
 *   traditional authority partition legitimacy by domain — Gregorian calendar
 *   and Western dress govern the public/administrative sphere, lunar calendar
 *   and traditional dress govern the private/ritual sphere — with no
 *   expectation of eventual convergence. This is not a transitional stage
 *   toward full state absorption (the exogenous_override_reading) nor a story
 *   of organic bottom-up practice change (the
 *   endogenous_displacement_reading); it is a stable, negotiated bifurcation
 *   in which both authorities retain uncontested jurisdiction over their
 *   respective domains and compliance in either domain is strategic rather
 *   than internalized. The three readings describe structurally distinct
 *   claims about the same underlying phenomenon (practice-legitimacy
 *   transition) and are deliberately NOT reconciled here — each is its own
 *   constraint with its own ε, beneficiary/victim structure, and persistence
 *   logic.
 *
 * KEY AGENTS:
 *   - state_bureaucracy: agenda_setter (institutional/arbitrage) — administers and enforces the public/administrative domain
 *   - traditional_ritual_authorities: agenda_setter/beneficiary (organized/constrained) — administers and enforces the private/ritual domain
 *   - households_managing_dual_calendars: beneficiary/payer (moderate/constrained) — bears the dual-tracking cost but retains flexibility
 *   - rural_agricultural_households: payer (powerless/trapped) — bears the sharpest cross-domain penalty with no appeal
 *   - cross_domain_workers: payer (moderate/constrained) — absorbs personal switching costs across employment and family life
 *   - minority_practice_communities: payer (powerless/trapped) — falls outside both recognized domains entirely
 *   - colonial_or_international_observers: excluded (institutional/analytical) — evaluates only the public-facing domain
 *   - modernization_historians: observer (analytical/analytical) — documents the equilibrium as stable rather than transitional
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.38).
domain_priors:suppression_score(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.42).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "Domain-Partitioned Practice Legitimacy (Dual Practice Equilibrium)").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "political_history/modernization_studies/institutional_change").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, '5a9fda0d-2926-422b-90a0-7ba7d06d6b42').
narrative_ontology:cs_kernel_codification('5a9fda0d-2926-422b-90a0-7ba7d06d6b42', distributed).
narrative_ontology:cs_authority_grounding('5a9fda0d-2926-422b-90a0-7ba7d06d6b42', distributed).
narrative_ontology:cs_reading_relation('5a9fda0d-2926-422b-90a0-7ba7d06d6b42', legitimacy_of_practice_standardization__endogenous_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('5a9fda0d-2926-422b-90a0-7ba7d06d6b42', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('5a9fda0d-2926-422b-90a0-7ba7d06d6b42', foundational, legitimacy_is_domain_indexed_not_universal).
narrative_ontology:cs_axiom_status(legitimacy_is_domain_indexed_not_universal, holdable).
narrative_ontology:cs_axiom_grounding('5a9fda0d-2926-422b-90a0-7ba7d06d6b42', legitimacy_is_domain_indexed_not_universal, conventional).
narrative_ontology:cs_axiom('5a9fda0d-2926-422b-90a0-7ba7d06d6b42', foundational, permanent_bifurcation_is_a_stable_terminus_not_a_transition).
narrative_ontology:cs_axiom_status(permanent_bifurcation_is_a_stable_terminus_not_a_transition, holdable).
narrative_ontology:cs_axiom_grounding('5a9fda0d-2926-422b-90a0-7ba7d06d6b42', permanent_bifurcation_is_a_stable_terminus_not_a_transition, empirically_contingent).
narrative_ontology:cs_reference_frame('5a9fda0d-2926-422b-90a0-7ba7d06d6b42', dual_domain_partition_settlement).
narrative_ontology:cs_drift_state('5a9fda0d-2926-422b-90a0-7ba7d06d6b42', contemporary_globalized_administration, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5a9fda0d-2926-422b-90a0-7ba7d06d6b42', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_bureaucracy).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_ritual_authorities).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, households_managing_dual_calendars).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, rural_agricultural_households).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, cross_domain_workers).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, minority_practice_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, households_managing_dual_calendars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers taxation, schooling, courts, and civil registration on the Gregorian calendar and Western dress code, and enforces this domain boundary through licensing, official recognition, and bureaucratic gatekeeping. Does not attempt to displace lunar calendar or kimono use in private or ritual contexts, because the partition itself — rather than full convergence — is what keeps administrative costs low and avoids provoking resistance from traditional authorities.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).

% Retain undisputed jurisdiction over festival timing, agricultural ritual, marriage, and funeral practice under the lunar calendar and traditional dress. Their authority persists precisely because the state ceded the private/ritual domain rather than contesting it; they collect deference, fees, and social standing from administering that domain and have no incentive to press for convergence.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_ritual_authorities, agenda_setter,
    organized, civilizational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_ritual_authorities, beneficiary).

% Run two parallel systems: they file taxes and interact with schools on Gregorian dates and in Western dress, then observe festivals, planting/harvest timing, and rites of passage on the lunar calendar in traditional dress. The dual-tracking is workable for urban salaried households with calendar apps and closets for both wardrobes, but it is a standing administrative and cognitive cost, not a convenience.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, households_managing_dual_calendars, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, households_managing_dual_calendars, payer).

% Depend on the lunar calendar for planting, harvest, and irrigation-sharing rituals but must also meet Gregorian-dated tax deadlines, loan repayment schedules, and school enrollment windows set by a bureaucracy that does not adjust to agricultural timing. Missing a Gregorian deadline because it fell during a lunar-calendar labor peak carries real penalties; there is no authority to appeal to that spans both systems.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, rural_agricultural_households, payer,
    powerless, biographical, trapped, local).

% Civil servants, teachers, and factory workers who must present in Western dress and operate on Gregorian time for employment while their extended families and home communities expect participation in lunar-calendar rites and traditional dress obligations. They absorb the switching cost personally — leave requests denominated in a calendar their employer does not recognize, wardrobe expenses for both registers, and social penalties in either domain if they get the code-switch wrong.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, cross_domain_workers, payer,
    moderate, biographical, constrained, national).

% Hold practices that fit neither the state's administrative calendar/dress code nor the dominant traditional authority's ritual domain (e.g. a third calendar tradition, a distinct dress convention). The dual-partition is built around the majority traditional authority, so minority practice has no recognized domain at all — it must either assimilate into the dominant lunar/kimono register to gain any recognition, or persist unrecognized in both domains.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, minority_practice_communities, payer,
    powerless, generational, trapped, regional).

% International bodies, trade partners, and historical colonial administrations that evaluate the state by its public/administrative face (Gregorian calendar, Western dress, formal legal codes) and treat the private/ritual domain as invisible or irrelevant to modernization scorecards. Their assessments shape aid, trade terms, and diplomatic standing without ever registering the costs the partition imposes on rural and minority populations.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, colonial_or_international_observers, excluded,
    institutional, generational, analytical, global).

% Study the partition as a stable institutional equilibrium rather than a transitional phase, documenting how domain-bifurcation persists across generations without either side's practice absorbing the other, and how strategic (not internalized) compliance in both domains sustains the split.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, modernization_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, diffuse).
narrative_ontology:fixing_cost_class(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Partitioning legitimacy by domain lets the state govern administration (taxes, courts, schooling) with a single, verifiable standard while traditional authority retains uncontested control of ritual and private life — avoiding the coordination failure that would result if either authority tried to fully displace the other's domain.
% TRANSFER_FUNCTION: Moves administrative-compliance costs onto anyone whose life crosses the domain boundary — rural households whose agricultural calendar is lunar but whose obligations are Gregorian-dated, cross-domain workers who must maintain two registers, and minority communities whose practices fit neither recognized domain — while state bureaucrats and traditional authorities each collect legitimacy and standing within their own uncontested sphere.
% ABSENT_VOICES: Rural agricultural households and minority practice communities bear the switching costs of the partition but have no seat at the table where the domain boundary itself is drawn or adjusted — that boundary is negotiated between state officials and traditional authorities, not with the populations who must live across it.
% DISAPPEARANCE_RATIONALE: If the domain partition collapsed — either through full state absorption of ritual life or full traditionalization of administration — households would lose the option of strategic compliance in whichever domain currently costs them least; either bureaucracy would have to accommodate lunar timing (raising administrative costs) or traditional authorities would lose their uncontested private-domain jurisdiction (losing standing and revenue). Both institutional actors currently benefit from the boundary holding, so its removal would force a real renegotiation of authority, not merely a symbolic change.
% FOUNDING_PROBLEM: At the point of state modernization, direct confrontation between the new administrative apparatus and entrenched traditional/ritual authority threatened to be destabilizing or unenforceable; partitioning legitimacy by domain let the state secure administrative control without a costly fight over private and ritual life.
% FOUNDING_PROBLEM_CORROBORATION: Modernization historians and international observers corroborate that the domain split remains actively maintained rather than a residual transitional artifact — administrative enforcement in the public domain and ritual authority's grip on the private domain are both still exercised, not merely tolerated out of inertia. No corroboration from outside the two authorities has been offered for the claim that this partition serves the populations who must operate across both domains; rural households and minority communities are not on record affirming the arrangement's necessity.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).
:- end_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because the partition itself is a genuine coordination solution for the two authorities but imposes real, uncompensated switching costs on households and workers who cross domain boundaries; it is well below snare-level because most households benefit from the reduced administrative burden of NOT having to fully convert either domain. Suppression is moderate (0.42) and declining slightly over the interval as the equilibrium stabilizes and enforcement becomes more routine/predictable rather than actively contested — this is a story of institutional settling, not escalating coercion, hence the falling suppression_requirement trajectory. Theater ratio rises modestly (0.15→0.28) as bureaucratic and ritual gatekeeping increasingly perform boundary-maintenance (certifying which domain a given transaction belongs to) rather than solving new coordination problems. All three series share the same time grid (0/10/20/30/40/50/60).
 *
 * DIRECTIONALITY LOGIC:
 *   State bureaucracy and traditional ritual authorities are both structural beneficiaries: each retains uncontested jurisdiction over a domain and neither bears the cost of maintaining the boundary — households and workers do. Households managing dual calendars sit near symmetric (real coordination benefit from not having to fully convert, real cost of dual-tracking). Rural agricultural households and minority practice communities are the clearest targets: trapped exit options, no domain that fully fits their situation, and no appeal mechanism spanning both systems. This differs sharply from the exogenous_override reading, where the state would be pursuing full displacement (making traditional authority the sole victim), and from the endogenous_displacement reading, where no enforced boundary exists at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — avoiding a costly confrontation between new administrative authority and entrenched traditional authority — remains live: both authorities still actively enforce their respective domains rather than merely coasting on inertia. This distinguishes the dual_practice_equilibrium reading from a piton: the boundary is maintained because both agenda-setters still extract standing from it, not because no one has gotten around to dismantling a dead arrangement. Classifying this as tangled_rope (rather than snare) captures that genuine coordination value exists for the median household even as rural and minority populations pay a disproportionate, uncompensated cost through the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_boundary_stability,
    'Is the public/private domain boundary itself stable over civilizational timescales, or does it slowly migrate (state absorbing more of ''private'' life, or traditional authority reasserting claims over administrative domains) such that the dual_practice_equilibrium_reading is actually a slow-motion instance of one of the sibling readings?',
    'Longitudinal tracking of which activities are classified as public/administrative vs private/ritual across multiple generations; a stable boundary supports this reading, a monotonically shifting boundary supports either endogenous_displacement or exogenous_override depending on the direction of shift.',
    'If the boundary is genuinely stable, the dual_practice_equilibrium reading is structurally distinct and should remain a permanent classification. If it drifts, this reading may itself be a snapshot of a slower version of one of the sibling processes, and would need re-decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_boundary_stability, empirical, 'Whether the domain partition is a stable equilibrium or a slow-drift instance of a sibling reading.').

omega_variable(
    sibling_reading_coexistence_or_contest,
    'Do the three kernel readings (dual_practice_equilibrium, endogenous_displacement, exogenous_override) describe genuinely coexisting positions held by different parties within the same society, or are they mutually exclusive descriptions where only one can be structurally true of a given case at a given time?',
    'Cross-case comparison: identify societies/periods where multiple readings are simultaneously asserted by different actors (state claims exogenous_override legitimacy while traditional authorities live the dual_practice_equilibrium reality) versus cases where the readings are sequential (endogenous_displacement giving way to exogenous_override).',
    'If coexisting, all three readings should be generated as parallel, permanently linked constraint-family members. If mutually exclusive per case, network edges should carry directional/temporal annotations rather than symmetric coexistence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_coexistence_or_contest, conceptual, 'Whether the kernel''s three readings coexist across parties or are mutually exclusive per case.').

omega_variable(
    minority_practice_recognition_gap,
    'Is the absence of any recognized domain for minority_practice_communities a design feature of the dual partition (the boundary is drawn by and for the two dominant authorities) or an unintended gap that could be closed by adding a third domain category?',
    'Examine whether any historical case has successfully added a third recognized domain (e.g. a minority religious calendar granted parallel administrative standing) without collapsing the underlying two-domain structure.',
    'If a third domain can be added without collapsing the equilibrium, the minority community''s victim status is contingent rather than structural to this reading. If the two-domain structure is load-bearing and cannot accommodate a third category, minority exclusion is intrinsic to this reading''s persistence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_practice_recognition_gap, empirical, 'Whether minority-practice exclusion is structural to the two-domain partition or a fixable gap.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 50, 0.27).
narrative_ontology:measurement(legi_tr_t60, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 30, 0.36).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 40, 0.37).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 50, 0.375).
narrative_ontology:measurement(legi_be_t60, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 60, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 30, 0.44).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 40, 0.43).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement(legi_su_t60, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 60, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.1).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization__endogenous_displacement_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the legitimacy_of_practice_standardization kernel. dual_practice_equilibrium_reading claims a permanent, stable domain-bifurcation of legitimacy (state governs public/administrative, tradition governs private/ritual, no convergence expected). endogenous_displacement_reading claims legitimacy tracks voluntary, utility-driven adoption. exogenous_override_reading claims legitimacy tracks state decree for collective benefit. The three are linked bidirectionally to preserve family cohesion; each carries its own ε, its own claimed_type, and its own stakeholder/beneficiary/victim structure per the ε-invariance principle. This reading is authored as tangled_rope; the sibling readings may compute to different types given their different structural claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
