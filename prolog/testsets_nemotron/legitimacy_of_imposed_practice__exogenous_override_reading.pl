% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__exogenous_override_reading, []).

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
 *   constraint_id: legitimacy_of_imposed_practice__exogenous_override_reading
 *   human_readable: Exogenous Override Reading — Legitimacy of Imposed Practice
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   This constraint story instantiates the exogenous_override_reading of the
 *   legitimacy_of_imposed_practice kernel. It models the claim that state
 *   decree authority alone suffices to displace prior calendar and dress
 *   practices, with compliance following from legal mandate regardless of
 *   internalization. The structural delta is asymmetric: calendar
 *   displacement is near-total in law but incomplete in practice (rural
 *   non-compliance, parallel reckoning); dress displacement is partial and
 *   coercively enforced. The state modernization agenda and its
 *   bureaucratic/military apparatus are beneficiaries; rural populations bear
 *   the adjustment costs without consultation. This reading treats the
 *   mandate as a genuine coordination mechanism (unified time and appearance
 *   for administration) that simultaneously extracts asymmetric costs from
 *   populations whose practices are displaced — a classic tangled_rope
 *   structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, 0.68).
domain_priors:suppression_score(legitimacy_of_imposed_practice__exogenous_override_reading, 0.72).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__exogenous_override_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__exogenous_override_reading, "Exogenous Override Reading — Legitimacy of Imposed Practice").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__exogenous_override_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__exogenous_override_reading, 'd4e7e603-9521-4b44-83d4-f48942ac189b').
narrative_ontology:cs_kernel_codification('d4e7e603-9521-4b44-83d4-f48942ac189b', formalized).
narrative_ontology:cs_authority_grounding('d4e7e603-9521-4b44-83d4-f48942ac189b', extraction).
narrative_ontology:cs_interpretation_layer_present('d4e7e603-9521-4b44-83d4-f48942ac189b').
narrative_ontology:cs_reading_relation('d4e7e603-9521-4b44-83d4-f48942ac189b', legitimacy_of_imposed_practice__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('d4e7e603-9521-4b44-83d4-f48942ac189b', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('d4e7e603-9521-4b44-83d4-f48942ac189b', foundational, state_decree_sufficiency_for_displacement).
narrative_ontology:cs_axiom_status(state_decree_sufficiency_for_displacement, holdable).
narrative_ontology:cs_axiom_grounding('d4e7e603-9521-4b44-83d4-f48942ac189b', state_decree_sufficiency_for_displacement, conventional).
narrative_ontology:cs_axiom('d4e7e603-9521-4b44-83d4-f48942ac189b', foundational, internalization_not_required_for_compliance).
narrative_ontology:cs_axiom_status(internalization_not_required_for_compliance, holdable).
narrative_ontology:cs_axiom_grounding('d4e7e603-9521-4b44-83d4-f48942ac189b', internalization_not_required_for_compliance, empirically_contingent).
narrative_ontology:cs_reference_frame('d4e7e603-9521-4b44-83d4-f48942ac189b', sovereign_legislative_monopoly).
narrative_ontology:cs_drift_state('d4e7e603-9521-4b44-83d4-f48942ac189b', midcentury_administrative_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d4e7e603-9521-4b44-83d4-f48942ac189b', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, central_bureaucracy).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, military_administration).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, rural_populations).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_calendar_practitioners).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, local_religious_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, urban_merchant_class).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__exogenous_override_reading, state_decree_sufficiency_doctrine).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__exogenous_override_reading, legal_mandate_compliance_theory).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__exogenous_override_reading, modernization_via_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drives the decree-based displacement of traditional calendar and dress practices as the central engine of modernization. Issues legal mandates, allocates enforcement resources, and defines the administrative categories through which compliance is measured. Its authority derives from the state's monopoly on legitimate coercion and the ideological commitment to rationalization.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda, agenda_setter,
    institutional, generational, arbitrage, national).

% Implements the decrees through administrative circulars, inspection regimes, and penalty schedules. Gains budgetary allocations, personnel expansion, and institutional prestige from the enforcement mission. Staff are career civil servants whose advancement depends on measurable compliance metrics.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, central_bureaucracy, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__exogenous_override_reading, central_bureaucracy, beneficiary).

% Provides the coercive backbone for dress-code enforcement in garrison towns and frontier zones. Gains operational justification, supply priority, and political visibility from the mandate. Officers rotate through postings, making their stake temporal but the institution's stake durable.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, military_administration, beneficiary,
    organized, biographical, constrained, regional).

% Bear the full adjustment costs of calendar and dress mandates without consultation. Calendar disruption desynchronizes agricultural labor, market cycles, and ritual calendars; dress mandates require cash expenditure on unfamiliar garments and expose wearers to climate and occupational mismatch. Exit is identity-locked: the practices are constitutive of communal self-understanding, and abandonment is experienced as cultural erasure rather than policy adaptation.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, rural_populations, payer,
    powerless, biographical, identity_locked, local).

% Hold the embodied knowledge of the displaced calendar system — intercalation rules, festival timing, agricultural markers. Their authority collapses when the state declares their reckoning illegal. They maintain parallel reckoning in private but cannot transmit it publicly without risk. Exit is constrained: they can teach covertly but cannot reclaim public legitimacy.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_calendar_practitioners, payer,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_calendar_practitioners, excluded).

% Lose control over the ritual calendar that structures communal worship and life-cycle events. The state's civil calendar replaces the liturgical year as the public timeframe. They retain private adherence but their public authority is legally displaced. Exit is constrained: they can resist through pastoral accommodation but cannot challenge the decree openly.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, local_religious_authorities, payer,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__exogenous_override_reading, local_religious_authorities, excluded).

% Gains predictability in commercial scheduling, contract enforcement, and cross-regional trade from the unified civil calendar. Adopts the new dress code as a marker of modern commercial identity. Their exit is mobile: they can relocate to jurisdictions with favorable regimes, but the national scope of the mandate limits this.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, urban_merchant_class, beneficiary,
    moderate, biographical, mobile, regional).

% Observe the constraint from outside its operational field. Their stake is epistemic: understanding whether decree-based displacement produces stable coordination or brittle compliance. They do not collect from the constraint nor pay into it.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, historical_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Imposes a unified civil calendar and standardized dress code to enable centralized administration, synchronized commerce, and legible citizenship across a heterogeneous territory. The coordination problem is real: multiple overlapping calendars and regionally variable dress impede tax collection, military conscription, market integration, and bureaucratic communication.
% TRANSFER_FUNCTION: Moves adjustment costs (temporal dislocation, garment expenditure, ritual disruption, knowledge devaluation) from the state apparatus onto rural populations and traditional authorities. Moves administrative simplicity, commercial predictability, and symbolic modernity to the state modernization agenda and urban commercial classes.
% ABSENT_VOICES: Village elders, women's collectives managing domestic ritual calendars, nomadic groups whose mobility patterns are calendar-dependent, and minority religious communities whose liturgical calendars are not recognized in the civil code. They are absent because the decree process includes no consultation mechanism and enforcement targets public performance, not private belief.
% DISAPPEARANCE_RATIONALE: If the decree authority vanished overnight, the civil calendar would lose its legal monopoly; rural communities would revert to traditional reckoning for agricultural and ritual purposes; urban commerce would maintain the civil calendar as a coordination standard but without coercive enforcement; dress codes would fragment along class, regional, and occupational lines. The administrative machinery built around the mandate (inspection regimes, penalty schedules, compliance reporting) would lose its operational basis.
% FOUNDING_PROBLEM: The state inherited a territory where timekeeping and bodily presentation were fragmented across religious, ethnic, and regional lines, making centralized administration, taxation, conscription, and market integration unreliable. The founding problem was administrative illegibility: the state could not see, count, or move its population efficiently.
% FOUNDING_PROBLEM_CORROBORATION: State archives and reformist journals of the period attest the administrative illegibility problem as live and urgent. Rural petitions, missionary reports, and ethnographic records from the same period attest that local populations experienced the mandates as foreign imposition solving no local problem, and that traditional calendars continued to govern agricultural and ritual life de facto. The corroboration from outside the benefiting parties (rural petitions, missionary reports) supports the contested reading.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the substantial transfer of adjustment costs onto rural populations who neither consented nor benefit. Suppression (0.72) is high because the constraint's persistence depends on active enforcement: inspection regimes, penalty schedules, and the legal abolition of alternative calendars. Theater ratio (0.38) is moderate: the coordination function (administrative legibility) is real and valued by urban commercial classes, but a growing share of enforcement activity serves symbolic modernization rather than functional coordination. Accessibility collapse (0.55) is partial: rural populations maintain parallel calendar reckoning and covert dress practices, so alternatives are not fully collapsed. Resistance (0.58) is significant: non-compliance, covert maintenance, and periodic uprisings demonstrate that the constraint is not self-sustaining.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (state modernization), the constraint is a rope: genuine coordination solving administrative illegibility. From the payer seats (rural populations, traditional practitioners), it is a snare: coercive extraction displacing lived practice. The engine computes this divergence from the structural data — the same constraint yields different types at different seats. The claimed type (tangled_rope) acknowledges the hybrid structure: real coordination function + asymmetric extraction + active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   The state modernization agenda and central bureaucracy are structural beneficiaries (d near 0.0): they set the agenda, collect administrative simplicity, and control enforcement resources. The military administration is a secondary beneficiary (d ~ 0.2): it gains operational justification but bears deployment costs. Urban merchants are near-symmetric beneficiaries (d ~ 0.4): they gain commercial predictability but adopt the dress code as identity signaling. Rural populations are full targets (d ~ 0.9): they bear adjustment costs, have identity-locked exit, and face coercive enforcement. Traditional practitioners and religious authorities are targets with constrained exit (d ~ 0.8): they lose public authority but retain private practice. Historical analysts sit at d = 0.5 (analytical seat).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (administrative illegibility) was live at inception. By the interval's end, the problem is contested: urban administration achieved legibility, but rural populations never experienced the founding problem as theirs. The arrangement persists beyond its founding justification because the state apparatus that benefits from it has no incentive to sunset it. The mandate has not resolved its mandatrophy: it continues to extract from populations for whom the coordination problem was never real, maintained by institutional inertia and the symbolic value of visible modernization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decree_sufficiency_boundary,
    'Is state decree authority structurally sufficient to displace embedded practices, or does the observed displacement depend on unstated ideological, economic, or military reinforcement?',
    'Compare jurisdictions with similar decrees but different reinforcement capacities (ideological messaging, economic incentives, military occupation). If displacement correlates with reinforcement, decree alone is insufficient.',
    'If decree alone is insufficient, the exogenous_override_reading''s core premise is false; the constraint is misclassified as tangled_rope (coordination + extraction) when its coordination function is actually parasitic on unacknowledged reinforcement — a snare with a coordination cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decree_sufficiency_boundary, empirical, 'Whether the exogenous override mechanism operates without hidden reinforcement.').

omega_variable(
    calendar_dress_decoupling,
    'Are calendar displacement and dress displacement the same constraint, or two constraints with different enforcement logics and beneficiary structures?',
    'Trace the administrative genealogy: were the calendar and dress mandates issued by the same authority, under the same legal instrument, with the same enforcement machinery? Measure compliance trajectories independently.',
    'If they are distinct constraints, this story conflates two ε values. Calendar (near-total legal displacement, high parallel practice) and dress (partial enforcement, visible compliance) may have different types — e.g., calendar as piton (atrophied function, theatrical maintenance) and dress as tangled_rope (active enforcement, real coordination for urban commerce).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(calendar_dress_decoupling, conceptual, 'Whether the calendar and dress mandates constitute one constraint or a constraint family.').

omega_variable(
    internalization_measurement_gap,
    'How should compliance be measured when public performance and private practice diverge — and does the reading''s claim (compliance follows from mandate regardless of internalization) make internalization empirically irrelevant or structurally constitutive?',
    'Longitudinal ethnographic data on private calendar reckoning and dress practices across generations. If private practice persists without public performance, the constraint''s extraction is higher than compliance metrics suggest.',
    'If internalization is structurally constitutive (the constraint creates the divergence between public and private), the theater_ratio is underestimated: the gap between performative compliance and functional coordination is the extraction mechanism. If internalization is irrelevant, the constraint is a cleaner tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_measurement_gap, conceptual, 'Whether the public/private compliance gap is a measurement artifact or a structural feature of the constraint.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the ''legitimacy_of_imposed_practice'' kernel frame the dispute at the right level, or does it conflate analytically distinct questions: (a) whether decree displaces practice, (b) whether displaced practice loses legitimacy, (c) whether the displacement is normatively justified?',
    'Decompose the kernel into sub-kernels: decree_effectiveness, legitimacy_transfer, normative_justification. Test whether the three sibling readings map cleanly to positions on each sub-kernel or whether they bundle different positions.',
    'If the kernel bundles distinct questions, the three readings are not genuine siblings but different answers to different questions. The constraint family decomposition would need restructuring, and the reading_relations authored here would be invalid.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel itself is a coherent analytical unit or a conflation of distinct disputes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__exogenous_override_reading, 1910, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legitimacy_imposed_override_tr_t1910, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1910, 0.25).
narrative_ontology:measurement(legitimacy_imposed_override_tr_t1920, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1920, 0.3).
narrative_ontology:measurement(legitimacy_imposed_override_tr_t1930, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1930, 0.34).
narrative_ontology:measurement(legitimacy_imposed_override_tr_t1940, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1940, 0.36).
narrative_ontology:measurement(legitimacy_imposed_override_tr_t1950, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1950, 0.38).

% Extraction over time
narrative_ontology:measurement(legitimacy_imposed_override_be_t1910, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1910, 0.45).
narrative_ontology:measurement(legitimacy_imposed_override_be_t1920, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1920, 0.52).
narrative_ontology:measurement(legitimacy_imposed_override_be_t1930, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1930, 0.58).
narrative_ontology:measurement(legitimacy_imposed_override_be_t1940, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1940, 0.63).
narrative_ontology:measurement(legitimacy_imposed_override_be_t1950, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1950, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(legitimacy_imposed_override_su_t1910, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1910, 0.55).
narrative_ontology:measurement(legitimacy_imposed_override_su_t1920, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1920, 0.62).
narrative_ontology:measurement(legitimacy_imposed_override_su_t1930, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1930, 0.68).
narrative_ontology:measurement(legitimacy_imposed_override_su_t1940, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1940, 0.7).
narrative_ontology:measurement(legitimacy_imposed_override_su_t1950, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1950, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimacy_of_imposed_practice__exogenous_override_reading, 0.12).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice__endogenous_climb_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the legitimacy_of_imposed_practice constraint family. The three readings instantiate distinct constraints with different ε values and stakeholder structures, linked by network.affects_constraints. The exogenous_override_reading has the highest extractiveness (0.68) and suppression (0.72) because it treats decree authority as sufficient, requiring active enforcement to maintain displacement. The endogenous_climb_reading would have lower extractiveness (coordination without coercion) but higher accessibility_collapse (practices disappear when internalization fails). The hybrid_scaffolding_reading would show intermediate values with ideological_messaging as a coordination mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_of_imposed_practice__exogenous_override_reading, moderate, 0.35).
constraint_indexing:directionality_override(legitimacy_of_imposed_practice__exogenous_override_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
