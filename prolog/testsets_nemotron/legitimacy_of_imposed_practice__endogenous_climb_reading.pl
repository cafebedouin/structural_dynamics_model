% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__endogenous_climb_reading, []).

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
 *   constraint_id: legitimacy_of_imposed_practice__endogenous_climb_reading
 *   human_readable: Imposed Practice Displacement Requires Endogenous Internalization
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   This constraint captures the endogenous_climb_reading of the contested
 *   kernel 'legitimacy_of_imposed_practice': the claim that practice
 *   displacement — whether calendar reform, dress codes, linguistic
 *   standardization, or ritual substitution — fundamentally requires
 *   internalization by the affected population. Top-down mandates without
 *   bottom-up adoption pathways fail; the historical record shows lunar
 *   calendar observance persisting decades after official solar imposition,
 *   and dress reforms achieving public compliance but private retention. The
 *   constraint is a tangled rope: it coordinates social legitimacy (genuine
 *   function) while extracting compliance costs from state modernization
 *   timelines (asymmetric extraction), requiring active enforcement to
 *   maintain the imposition. Beneficiaries are autonomous communities and
 *   traditional authorities who preserve cultural continuity; victims are
 *   state bureaucracies whose reform timelines stall and whose enforcement
 *   resources are consumed by non-compliance.
 *
 * KEY AGENTS:
 *   - autonomous_communities: Primary beneficiary (organized/constrained) — preserves cultural autonomy through non-internalization
 *   - traditional_authorities: Beneficiary (organized/identity_locked) — maintains institutional relevance when imposed practices fail to displace endogenous ones
 *   - cultural_preservation_networks: Beneficiary (moderate/mobile) — gains legitimacy from documenting and supporting resistance to imposition
 *   - state_modernization_bureaucracy: Primary victim (institutional/trapped) — bears timeline delays, enforcement costs, legitimacy erosion
 *   - central_administration_timeline: Victim (institutional/constrained) — reform schedules slip, political capital expended on failed displacements
 *   - reform_implementation_officials: Victim (organized/trapped) — directly enforce failing mandates, face community resistance daily
 *   - historical_analyst: Observer (analytical/analytical) — sees full structural pattern across cases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.62).
domain_priors:suppression_score(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.58).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__endogenous_climb_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__endogenous_climb_reading, "Imposed Practice Displacement Requires Endogenous Internalization").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__endogenous_climb_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__endogenous_climb_reading, '6955c325-12dc-43d9-ae10-ac2949618f1d').
narrative_ontology:cs_kernel_codification('6955c325-12dc-43d9-ae10-ac2949618f1d', distributed).
narrative_ontology:cs_authority_grounding('6955c325-12dc-43d9-ae10-ac2949618f1d', practice).
narrative_ontology:cs_interpretation_layer_present('6955c325-12dc-43d9-ae10-ac2949618f1d').
narrative_ontology:cs_reading_relation('6955c325-12dc-43d9-ae10-ac2949618f1d', legitimacy_of_imposed_practice__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('6955c325-12dc-43d9-ae10-ac2949618f1d', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('6955c325-12dc-43d9-ae10-ac2949618f1d', foundational, internalization_necessary_for_legitimate_displacement).
narrative_ontology:cs_axiom_status(internalization_necessary_for_legitimate_displacement, holdable).
narrative_ontology:cs_axiom_grounding('6955c325-12dc-43d9-ae10-ac2949618f1d', internalization_necessary_for_legitimate_displacement, empirically_contingent).
narrative_ontology:cs_axiom('6955c325-12dc-43d9-ae10-ac2949618f1d', foundational, bottom_up_adoption_pathway_is_structural_requirement).
narrative_ontology:cs_axiom_status(bottom_up_adoption_pathway_is_structural_requirement, holdable).
narrative_ontology:cs_axiom_grounding('6955c325-12dc-43d9-ae10-ac2949618f1d', bottom_up_adoption_pathway_is_structural_requirement, empirically_contingent).
narrative_ontology:cs_reference_frame('6955c325-12dc-43d9-ae10-ac2949618f1d', pre_modern_practice_autonomy).
narrative_ontology:cs_drift_state('6955c325-12dc-43d9-ae10-ac2949618f1d', high_modernist_imposition_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6955c325-12dc-43d9-ae10-ac2949618f1d', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, autonomous_communities).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, traditional_authorities).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, cultural_preservation_networks).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_bureaucracy).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, central_administration_timeline).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, reform_implementation_officials).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__endogenous_climb_reading, internalization_necessary_for_legitimacy).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__endogenous_climb_reading, bottom_up_adoption_pathway_required).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__endogenous_climb_reading, imposed_commitment_failure_without_internalization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities maintain endogenous practices (lunar calendar, traditional dress, vernacular language) despite official imposition. They benefit from the constraint's operation: the failure of top-down displacement preserves their cultural autonomy and social cohesion. Exit from the constraint would mean accepting the imposed practice — which they resist through dual practice, private retention, and intergenerational transmission. Their power is collective but constrained by state capacity.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, autonomous_communities, beneficiary,
    organized, generational, constrained, regional).

% Religious leaders, tribal elders, and customary law holders retain authority when imposed practices fail to displace endogenous ones. They actively coordinate resistance (agenda-setting) and benefit from the constraint's persistence (beneficiary). Their identity is fused with the endogenous practices — exit would mean relinquishing the authority that derives from being the custodians of those practices. They cannot exit without dissolving their institutional self-concept.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, traditional_authorities, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__endogenous_climb_reading, traditional_authorities, agenda_setter).

% Intellectuals, ethnographers, and advocacy organizations document and support endogenous practice retention. They benefit from the constraint's existence — failed impositions validate their mission and attract resources. Their exit options are mobile: they could shift to other preservation causes, but the constraint's persistence aligns with their professional identity and funding streams.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, cultural_preservation_networks, beneficiary,
    moderate, biographical, mobile, national).

% Central ministries, reform commissions, and implementation agencies bear the extraction: decades of enforcement expenditure, timeline slippage, and political capital burned on displacements that fail without internalization. They are trapped — the modernization mandate is their institutional raison d'être; abandoning it means institutional dissolution. They cannot exit the constraint without exiting their institutional role.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_bureaucracy, payer,
    institutional, biographical, trapped, national).

% The state's reform schedule itself is a victim: five-year plans extend to decades, political transitions reset the clock, and each failed displacement consumes legitimacy capital for the next. Exit is constrained — the timeline could be abandoned (regime change, policy reversal), but the structural pressure to modernize persists across administrations.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, central_administration_timeline, payer,
    institutional, generational, constrained, national).

% Local administrators, inspectors, and enforcement personnel directly enforce failing mandates. They face daily community resistance, dual compliance reporting, and career risk from both non-compliance (punished by center) and over-enforcement (triggers unrest). They are trapped: rotation is possible but the structural pattern repeats at each posting; the constraint follows the role.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, reform_implementation_officials, payer,
    organized, biographical, trapped, local).

% Sees the full structural pattern across cases: calendar reforms (Ottoman Rumi vs. lunar, Soviet revolutionary calendar, Meiji calendar), dress codes (Kemalist hat law, Soviet unveiling campaigns, Chinese queue abolition), linguistic impositions (Russification, Turkification, Sinicization). The pattern is consistent — internalization is the gate; without it, extraction accumulates and displacement fails.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, historical_analyst, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social legitimacy around endogenous practices: communities maintain shared temporal, sartorial, and linguistic frameworks that enable trust, reciprocity, and collective identity without state mediation. The constraint protects this coordination function from displacement by external authority.
% TRANSFER_FUNCTION: Moves enforcement capacity, political legitimacy, and reform timeline from state modernization bureaucracy to autonomous communities — the state pays the cost of failed displacement; communities retain their endogenous coordination infrastructure. The transfer is asymmetric: state extracts effort and receives non-compliance; communities pay minimal cost and retain autonomy.
% ABSENT_VOICES: Populations in frontier zones where state capacity was too weak to attempt imposition — they would object to being categorized as 'resistant' when they were never reached. Diaspora communities maintaining endogenous practices under different state regimes — their experience of the constraint differs by host state but is absent from the single-state frame. Future generations who inherit either the endogenous practice or the imposed one — their voice on legitimacy is structurally absent.
% DISAPPEARANCE_RATIONALE: If the internalization requirement vanished overnight — if state decree alone could displace endogenous practices — calendar reforms would succeed in years not decades, dress codes would achieve full private adoption, linguistic standardization would erase vernaculars in a generation. State modernization timelines would compress dramatically; enforcement resources would be freed; traditional authorities would lose their custodial relevance. The world rearranges because the constraint's operation currently structures the timeline and cost of every cultural imposition attempt.
% FOUNDING_PROBLEM: Early modern and modern states needed to standardize time, appearance, and language across heterogeneous populations to enable taxation, conscription, bureaucracy, and national markets. The founding problem was coordination at scale: how to make diverse populations legible and interoperable for state administration.
% FOUNDING_PROBLEM_CORROBORATION: State modernization bureaucracies attest the founding problem is live (legibility, interoperability, fiscal/military coordination still require standardization). Autonomous communities and cultural preservation networks attest the problem is dead or transformed — modern communication and transport enable coordination without cultural erasure; the founding problem was specific to pre-digital state capacity. Independent historical sociologists (Tilly, Scott, Hobsbawm) corroborate that the original coordination problem has been technologically superseded, but the imposition mandate persists as institutional inertia.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__endogenous_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the real cost imposed on state modernization: decades of enforcement expenditure, timeline slippage, and political capital burned on displacements that fail without internalization. Suppression (0.58) is moderate-high: the state deploys legal mandates, inspection regimes, and penalty structures, but these are structurally limited by the internalization barrier — you can enforce public compliance but not private belief. Theater ratio (0.42) captures the growing performative aspect: later-stage enforcement increasingly targets visible symbols (public dress, official calendars) while private practice persists, making enforcement theater. Accessibility collapse (0.35) is low: alternatives (endogenous practices) remain fully accessible and actively maintained. Resistance (0.71) is high: communities actively resist through non-compliance, dual practice, and cultural preservation — the constraint meets sustained opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the community seat, the constraint appears as a protective rope (coordination of cultural continuity against state intrusion). From the state seat, it appears as a snare (extraction of modernization capacity with no coordination benefit). The engine computes this seat divergence from the declared structural data — the endogenous_climb_reading itself does not adjudicate between these perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Autonomous communities and traditional authorities are beneficiaries (d ~0.2): they gain cultural continuity and institutional relevance from the constraint's operation — the failure of imposition preserves their autonomy. State modernization bureaucracy and reform officials are victims (d ~0.85): they bear the extraction (timeline, resources, legitimacy) and are structurally trapped — they cannot exit the modernization mandate without career/institutional consequences. Cultural preservation networks sit nearer symmetric (d ~0.5): they benefit from the constraint's existence (documentation mission) but also pay costs (advocacy effort). The engine derives these from beneficiary/victim declarations + exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (successful practice displacement through imposition) has atrophied — historical evidence shows it fails without internalization. Yet states repeatedly attempt it (mandatrophy unresolved: the mandate persists despite known failure mode). This is not a piton (no theatrical maintenance of a dead function) but a recurring category error: the state treats internalization as optional when it is structurally necessary. The classification prevents mislabeling the state's repeated attempts as coordination — they are extraction attempts that fail the coordination gate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Is this constraint one reading of the contested kernel ''legitimacy_of_imposed_practice''?',
    'This is a structural fact about the authoring frame: the constraint instantiates the endogenous_climb_reading of the kernel. Sibling readings are exogenous_override_reading and hybrid_scaffolding_reading.',
    'Establishes that ε, beneficiaries, and victims are reading-indexed; other readings instantiate different constraints with different structural profiles.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'This constraint is the endogenous_climb_reading of the legitimacy_of_imposed_practice kernel').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (state enforcement capacity) or internalized (communities accepting the imposition''s framing)?',
    'Post-imposition trajectory: if communities resist despite formal compliance, suppression is structural; if resistance dissolves after formal withdrawal, internalized component was significant.',
    'If internalized, the constraint''s effective suppression is higher than structural measure suggests — the target carries the suppression after formal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in cultural imposition').

omega_variable(
    natural_law_vs_constructed_legitimacy,
    'Does the endogenous climb principle reflect a genuine sociological law, or a constructed constraint that benefits communities preserving autonomy?',
    'Comparative historical analysis: do ALL successful practice displacements show endogenous internalization, or do some exogenous overrides succeed under specific conditions (hybrid_scaffolding_reading)?',
    'If genuine law, the constraint is rope/tangled_rope with coordination function; if constructed, it may be a snare protecting community autonomy against state capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_legitimacy, conceptual, 'Whether the internalization requirement is a natural sociological constraint or a constructed beneficiary protection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__endogenous_climb_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(legi_tr_t5, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(legi_tr_t15, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(legi_be_t5, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(legi_be_t15, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 20, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(legi_su_t5, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(legi_su_t15, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 15, 0.56).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__endogenous_climb_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.08).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% This constraint (endogenous_climb_reading) and its two siblings form the legitimacy_of_imposed_practice constraint family. All three share the kernel but instantiate different structural claims: this reading asserts internalization is necessary (tangled_rope); exogenous_override_reading asserts decree suffices (likely mountain/false_summit candidate); hybrid_scaffolding_reading asserts scaffolded imposition achieves partial displacement (scaffold candidate). The ε values differ substantially: this reading's ε=0.62 reflects measured state extraction; exogenous_override would author low ε (decree works); hybrid_scaffolding would author moderate ε with sunset dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_of_imposed_practice__endogenous_climb_reading, institutional, 0.85).
constraint_indexing:directionality_override(legitimacy_of_imposed_practice__endogenous_climb_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
