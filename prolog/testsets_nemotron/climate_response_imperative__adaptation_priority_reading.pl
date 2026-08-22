% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__adaptation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__adaptation_priority_reading, []).

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
 *   constraint_id: climate_response_imperative__adaptation_priority_reading
 *   human_readable: Adaptation-Priority Climate Response Imperative
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   The adaptation_priority_reading of the climate_response_imperative kernel
 *   holds that climate response should be primarily resilience-building and
 *   damage reduction in exposed regions, with mitigation treated as
 *   aspirational rather than binding. This reading emerged from the UNFCCC
 *   process (Rio 1992, Kyoto 1997, Copenhagen 2009, Paris 2015) where
 *   mitigation commitments were repeatedly deferred while adaptation finance
 *   remained structurally inadequate. The constraint operates as a
 *   tangled_rope: it coordinates genuine adaptation action (early warning
 *   systems, coastal defenses, agricultural resilience) while extracting
 *   capital from developing nations through debt-based finance
 *   conditionalities, and suppresses alternatives (mitigation-first pathways,
 *   degrowth transformations) through institutional inertia and finance
 *   architecture. The victim set centers present-day developing nations who
 *   face immediate capital requirements for adaptation they cannot meet
 *   without borrowing on terms set by Global North-controlled institutions —
 *   creating a vicious circle where those least responsible for historical
 *   emissions bear the highest immediate costs. The claimed type is
 *   tangled_rope because the coordination function (adaptation is necessary
 *   and urgent) is real, but the extraction function (deferred mitigation
 *   costs loaded onto the vulnerable via finance terms) is structural and
 *   asymmetric.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__adaptation_priority_reading, 0.78).
domain_priors:suppression_score(climate_response_imperative__adaptation_priority_reading, 0.65).
domain_priors:theater_ratio(climate_response_imperative__adaptation_priority_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__adaptation_priority_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__adaptation_priority_reading, "Adaptation-Priority Climate Response Imperative").
narrative_ontology:topic_domain(climate_response_imperative__adaptation_priority_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__adaptation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__adaptation_priority_reading, 'ef7ef005-ca2e-4223-9164-086a65dda244').
narrative_ontology:cs_kernel_codification('ef7ef005-ca2e-4223-9164-086a65dda244', formalized).
narrative_ontology:cs_authority_grounding('ef7ef005-ca2e-4223-9164-086a65dda244', extraction).
narrative_ontology:cs_interpretation_layer_present('ef7ef005-ca2e-4223-9164-086a65dda244').
narrative_ontology:cs_reading_relation('ef7ef005-ca2e-4223-9164-086a65dda244', climate_response_imperative__degrowth_reading, forecloses).
narrative_ontology:cs_reading_relation('ef7ef005-ca2e-4223-9164-086a65dda244', climate_response_imperative__mitigation_priority_reading, coexists_with).
narrative_ontology:cs_axiom('ef7ef005-ca2e-4223-9164-086a65dda244', foundational, adaptation_is_primary_obligation).
narrative_ontology:cs_axiom_status(adaptation_is_primary_obligation, holdable).
narrative_ontology:cs_axiom_grounding('ef7ef005-ca2e-4223-9164-086a65dda244', adaptation_is_primary_obligation, conventional).
narrative_ontology:cs_axiom('ef7ef005-ca2e-4223-9164-086a65dda244', foundational, mitigation_deferral_is_pragmatic).
narrative_ontology:cs_axiom_status(mitigation_deferral_is_pragmatic, holdable).
narrative_ontology:cs_axiom_grounding('ef7ef005-ca2e-4223-9164-086a65dda244', mitigation_deferral_is_pragmatic, instrumental).
narrative_ontology:cs_reference_frame('ef7ef005-ca2e-4223-9164-086a65dda244', unfccc_1992_differentiated_responsibility).
narrative_ontology:cs_drift_state('ef7ef005-ca2e-4223-9164-086a65dda244', paris_2015_implementation_gap, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ef7ef005-ca2e-4223-9164-086a65dda244', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__adaptation_priority_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, global_north_governments).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, multilateral_development_banks).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, private_finance_firms).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, domestic_elites_in_exposed_regions).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, present_day_developing_nations).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, low_income_populations_in_exposed_regions).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, future_generations_in_global_south).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, climate_vulnerable_communities_without_finance_access).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, domestic_elites_in_exposed_regions).
narrative_ontology:constraint_vindicates(climate_response_imperative__adaptation_priority_reading, adaptation_is_primary_response).
narrative_ontology:constraint_vindicates(climate_response_imperative__adaptation_priority_reading, mitigation_is_aspirational).
narrative_ontology:constraint_vindicates(climate_response_imperative__adaptation_priority_reading, resilience_building_justifies_deferred_mitigation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the global climate agenda through UNFCCC, control MDB capital and governance, define adaptation finance terms. Avoid binding mitigation commitments that would require domestic economic transformation. Capture political benefit from adaptation rhetoric while preserving consumption patterns. Can shift between adaptation/mitigation framings as political winds change.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, global_north_governments, agenda_setter,
    institutional, generational, arbitrage, global).

% Administer adaptation finance (GCF, CIFs, national MDB windows). Design loan terms, conditionalities, blended finance structures. Earn fees and maintain institutional relevance through climate finance volumes. Their mandate requires lending volume; adaptation priority guarantees demand. Not accountable to victims of finance terms.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, multilateral_development_banks, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__adaptation_priority_reading, multilateral_development_banks, beneficiary).

% Deploy capital into adaptation via blended finance, green bonds, resilience bonds. First-loss guarantees from MDBs de-risk returns. Extract fees at origination, servicing, and exit. No obligation to remain if returns falter — can reallocate capital globally. Benefit from adaptation-priority framing that expands their addressable market.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, private_finance_firms, beneficiary,
    powerful, biographical, mobile, global).

% Capture adaptation contracts (construction, consulting, implementation). Mediate finance flows from MDBs to projects. Pay some costs through taxation but capture disproportionate benefits. Constrained exit: wealth tied to national jurisdiction, but can move assets internationally. Their interests align with adaptation-priority because it channels resources through capture-able channels.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, domestic_elites_in_exposed_regions, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__adaptation_priority_reading, domestic_elites_in_exposed_regions, payer).

% Face immediate adaptation capital requirements (coastal protection, agricultural transition, water infrastructure) they cannot meet from domestic revenue. Must borrow from MDBs and private markets on terms set by Global North. Sovereign identity is fused with development trajectory — cannot 'exit' being a developing nation. Debt service on adaptation loans crowds out health, education, mitigation investment. The vicious circle: more adaptation need → more debt → less fiscal space → more vulnerability.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, present_day_developing_nations, payer,
    moderate, biographical, identity_locked, global).

% Bear physical climate impacts (heat, floods, crop failure) without finance access. Pay through lost livelihoods, health costs, displacement. No voice in finance terms or project selection. Trapped by geography, poverty, and lack of documentation/land title that would enable finance access. Their adaptation is autonomous (migration, informal settlement upgrading) and uncounted.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, low_income_populations_in_exposed_regions, payer,
    powerless, immediate, trapped, local).

% Inherit the compounded debt from adaptation borrowing AND the escalated climate impacts from mitigation deferral. Cannot consent to debts incurred today. Identity-locked: their existence is constituted by the choices of present actors. No exit from the physical and fiscal trajectory. The constraint's persistence guarantees their victimization.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, future_generations_in_global_south, payer,
    powerless, generational, identity_locked, global).

% Indigenous communities, informal settlements, stateless populations — structurally excluded from adaptation finance because they lack recognized governance structures, land title, or bankable projects. Would object to the finance architecture if present. Their adaptation needs are invisible to the constraint's coordination function.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, climate_vulnerable_communities_without_finance_access, excluded,
    powerless, immediate, trapped, local).

% Document the finance gap, advocate for grants not loans, push for loss and damage finance, challenge mitigation deferral. Can mobilize diplomatic pressure (COP walkouts, legal cases) but cannot alter finance architecture directly. Mobile: operate transnationally, but constrained by funding dependence on Global North foundations.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, global_south_civil_society_and_climate_justice_movements, observer,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global mobilization of adaptation resources (finance, technology, capacity) for exposed regions. Solves the collective action problem: no single nation can adapt alone; shared climate risk requires pooled response. The constraint creates the institutional architecture (UNFCCC adaptation track, GCF, NAP process) that channels resources to adaptation projects.
% TRANSFER_FUNCTION: Moves capital from Global North public balance sheets and private markets → through MDBs and blended finance structures → to adaptation projects in developing nations, with debt service and fees flowing back to creditors and intermediaries. Net transfer direction is contested: gross flows to projects vs. net flows after debt service, fees, conditionalities, and profit extraction. The arrangement transfers mitigation obligation from Global North (deferred) to Global South (future adaptation burden).
% ABSENT_VOICES: Climate-vulnerable communities without finance access (indigenous, informal, stateless) are structurally excluded. Future generations in the Global South cannot be present. Would-be alternative market mechanisms (debt-for-nature swaps at fair value, grant-based adaptation funds, polluter-pays mitigation finance) are excluded by the same finance architecture the constraint rides on.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority imperative vanished overnight, the UNFCCC adaptation architecture (NAPs, GCF, adaptation communications) would collapse. Developing nations would face adaptation costs with no institutional finance channel — forcing either sovereign default, radical domestic resource mobilization, or unmanaged climate impacts. Global North governments would lose the primary coordination framework that legitimates deferred mitigation. Private finance would lose the de-risked adaptation asset class. The world would rearrange around either a mitigation-first emergency or uncoordinated autonomous adaptation.
% FOUNDING_PROBLEM: In 1992 (Rio), the founding problem was coordinating a global response to climate change that recognized common but differentiated responsibilities. The adaptation-priority framing emerged as a pragmatic compromise: mitigation was politically difficult for Global North, so adaptation became the actionable track for Global South. The arrangement was built to unlock immediate action for vulnerable regions while buying time for mitigation.
% FOUNDING_PROBLEM_CORROBORATION: Global North governments and MDBs attest the founding problem is still live — adaptation needs are growing, finance is the bottleneck, the architecture is the best available mechanism. Developing nation coalitions (G77, AOSIS, LDCs), climate justice movements, and independent analysts (IPCC WGII, UNEP Adaptation Gap Report) attest the founding problem has shifted: the architecture now primarily manages the symptoms of mitigation failure while extracting capital from the vulnerable. The corroboration from outside the beneficiary set (vulnerable nations, civil society, independent science) supports the shifted-function reading.
narrative_ontology:disappearance_verdict(climate_response_imperative__adaptation_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__adaptation_priority_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__adaptation_priority_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(climate_response_imperative__adaptation_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__adaptation_priority_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__adaptation_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__adaptation_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__adaptation_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint's operation transfers adaptation costs to those least able to pay via debt instruments and conditionalities, while mitigation deferral preserves Global North consumption patterns. Suppression (0.65) is substantial because the institutional architecture (UNFCCC, MDBs, Paris Agreement) actively marginalizes mitigation-first and degrowth alternatives — they are not merely unfunded but structurally excluded from serious negotiation. Theater ratio (0.42) is moderate-high: adaptation rhetoric and reporting frameworks (NAPs, NDCs, GCF) perform coordination while the finance gap widens and mitigation stalls. Accessibility collapse (0.72) is high because the finance architecture makes alternatives (grant-based adaptation, rapid mitigation, structural transformation) practically inaccessible to developing nations. Resistance (0.55) is moderate: climate justice movements, vulnerable nation coalitions (AOSIS, LDCs), and some legal challenges push back but have not shifted the structural trajectory.
 *
 * PERSPECTIVAL GAP:
 *   From the Global North government / MDB seat, the constraint appears as a rope: genuine coordination on adaptation, collective action problem solved through finance mobilization. From the developing nation seat, it is a snare: adaptation is non-negotiable (physics), but the terms are extractive and alternatives are suppressed. From the private finance seat, it is a rope with upside: new asset classes, de-risked returns. From the climate-vulnerable community seat, it is a mountain: the climate impacts are physical reality, but the finance constraint is constructed. The engine computes these seat divergences from the structural data — the claimed_type does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Global North governments and multilateral development banks are structural beneficiaries: they avoid mitigation costs that would disrupt domestic economies, control finance terms, and capture returns via private finance intermediation. Private finance firms are beneficiaries through blended finance structures that socialize risk and privatize returns. Domestic elites in exposed regions are secondary beneficiaries — they capture adaptation contracts and mediate finance flows. Present-day developing nations are primary victims: they borrow for adaptation at market-adjacent rates while mitigation deferral guarantees escalating future needs. Low-income populations in exposed regions bear the physical impacts without finance access. Future generations in the Global South inherit the compounded debt and climate burden. Climate-vulnerable communities without finance access are excluded entirely. Directionality derives from this beneficiary/victim structure plus exit options: developing nations are identity_locked (sovereign identity fused with development trajectory) to constrained (some diplomatic maneuvering but no exit from climate physics or finance architecture).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordinating global climate response) was live in 1992. By 2024, the founding problem is contested: mitigation deferral has made the original coordination problem harder, not easier. The arrangement persists not because it solves the problem but because the beneficiaries (Global North governments, finance actors) extract value from its persistence, and the victims (developing nations) are trapped by sovereign identity and finance architecture. The mandate has atrophied — the constraint no longer primarily serves its declared coordination function — but it is not a piton because active enforcement and extraction continue. It is a tangled_rope whose coordination cover has thinned while its extraction core has thickened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the adaptation_priority_reading a structurally distinct constraint from the mitigation_priority_reading and degrowth_reading, or are they merely emphases of a single climate_response_imperative?',
    'Test whether each reading generates a different beneficiary/victim structure, different extractiveness profile, and different coordination function. If the victim set for adaptation_priority_reading includes present-day developing nations facing immediate capital requirements they cannot meet — while the mitigation_priority_reading''s victims are primarily future generations and the degrowth_reading''s victims are current Global North consumers — they are distinct constraints.',
    'If distinct, each reading gets its own ε, its own classification, and its own seat divergences. The ε-invariance principle requires decomposition: one constraint label cannot cover observably different extractive structures. Link via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s declared readings instantiate distinct constraints with independent ε values').

omega_variable(
    natural_law_vs_constructed_priority,
    'Is the prioritization of adaptation over mitigation a genuine natural-law constraint (physics of committed warming) or a constructed political choice that benefits identifiable agents?',
    'Separate the physical commitment (already-locked-in warming requiring adaptation) from the policy choice (treating mitigation as aspirational rather than binding). If the latter is a choice that concentrates costs on developing nations while Global North governments and finance actors avoid mitigation costs, the prioritization is a false summit — a constructed constraint presented as natural law.',
    'If constructed, the constraint is a false_summit_mountain candidate (claimed mountain but beneficiaries exist) or a tangled_rope with extraction. The FSM signature would trigger reclassification. The omega documents the irreducible ambiguity for the corpus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_priority, conceptual, 'Whether adaptation-priority framing reflects physical necessity or political construction').

omega_variable(
    finance_conditionalities_as_extraction_mechanism,
    'Do adaptation finance conditionalities (structural adjustment, debt instruments, private equity terms) function as an extraction mechanism that transfers value from developing nations to Global North financial actors?',
    'Trace the full capital flow: adaptation loans at market rates, blended finance structures with first-loss guarantees for private investors, debt-for-climate swaps that retire sovereign debt at discount to creditors. Measure net resource transfer direction over the interval. If developing nations pay more in debt service on adaptation finance than they receive in grant-equivalent adaptation value, the finance mechanism is extractive.',
    'If extractive, the coordination function (resilience building) is the cover for a transfer function (capital extraction). This would confirm tangled_rope classification and identify the specific extraction pathway. If not extractive, the constraint may be a genuine rope with implementation failures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(finance_conditionalities_as_extraction_mechanism, empirical, 'Whether adaptation finance operates as a coordination mechanism or an extraction mechanism').

omega_variable(
    mitigation_deferral_irreversibility,
    'Does treating mitigation as aspirational create irreversible commitment to higher warming trajectories that ultimately increase adaptation costs beyond what developing nations can ever finance?',
    'Model the feedback: adaptation priority → deferred mitigation → higher peak warming → exponentially rising adaptation costs → finance gap widens → more debt/conditionalities → less fiscal space for adaptation. If the loop is structurally inevitable given carbon cycle dynamics and finance architecture, the constraint creates a trap. If mitigation can be re-accelerated without catastrophic cost, the trap is political not physical.',
    'If irreversible, the constraint is a snare for future generations and a tangled_rope for present developing nations (coordination now, extraction later). If reversible, it is a scaffold with a missing sunset clause — the aspirational mitigation was meant to be temporary but became permanent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mitigation_deferral_irreversibility, empirical, 'Whether the adaptation-priority pathway creates a structural trap via mitigation deferral feedback').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__adaptation_priority_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_response_adaptation_tr_t1992, climate_response_imperative__adaptation_priority_reading, theater_ratio, 1992, 0.15).
narrative_ontology:measurement(climate_response_adaptation_tr_t1997, climate_response_imperative__adaptation_priority_reading, theater_ratio, 1997, 0.22).
narrative_ontology:measurement(climate_response_adaptation_tr_t2001, climate_response_imperative__adaptation_priority_reading, theater_ratio, 2001, 0.28).
narrative_ontology:measurement(climate_response_adaptation_tr_t2009, climate_response_imperative__adaptation_priority_reading, theater_ratio, 2009, 0.33).
narrative_ontology:measurement(climate_response_adaptation_tr_t2015, climate_response_imperative__adaptation_priority_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(climate_response_adaptation_tr_t2021, climate_response_imperative__adaptation_priority_reading, theater_ratio, 2021, 0.4).
narrative_ontology:measurement(climate_response_adaptation_tr_t2024, climate_response_imperative__adaptation_priority_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(climate_response_adaptation_be_t1992, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 1992, 0.45).
narrative_ontology:measurement(climate_response_adaptation_be_t1997, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 1997, 0.52).
narrative_ontology:measurement(climate_response_adaptation_be_t2001, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 2001, 0.58).
narrative_ontology:measurement(climate_response_adaptation_be_t2009, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 2009, 0.62).
narrative_ontology:measurement(climate_response_adaptation_be_t2015, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 2015, 0.7).
narrative_ontology:measurement(climate_response_adaptation_be_t2021, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 2021, 0.74).
narrative_ontology:measurement(climate_response_adaptation_be_t2024, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(climate_response_adaptation_su_t1992, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 1992, 0.35).
narrative_ontology:measurement(climate_response_adaptation_su_t1997, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 1997, 0.42).
narrative_ontology:measurement(climate_response_adaptation_su_t2001, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 2001, 0.48).
narrative_ontology:measurement(climate_response_adaptation_su_t2009, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 2009, 0.55).
narrative_ontology:measurement(climate_response_adaptation_su_t2015, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(climate_response_adaptation_su_t2021, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 2021, 0.63).
narrative_ontology:measurement(climate_response_adaptation_su_t2024, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__adaptation_priority_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_imperative__adaptation_priority_reading, 0.15).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, climate_response_imperative__mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, climate_response_imperative__degrowth_reading).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, climate_finance_architecture).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, global_debt_architecture).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, unfccc_institutional_framework).

% DUAL FORMULATION NOTE:
% The climate_response_imperative kernel decomposes into three constraint stories: adaptation_priority_reading (this story), mitigation_priority_reading, and degrowth_reading. They have distinct ε values (this story: 0.78, mitigation_priority: ~0.65, degrowth: ~0.45), distinct victim sets, and distinct coordination functions. They are linked via affects_constraints because they compete for the same finance/political capital and because the adaptation_priority_reading's mitigation deferral structurally shapes the operating environment for the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_imperative__adaptation_priority_reading, institutional, 0.15).
constraint_indexing:directionality_override(climate_response_imperative__adaptation_priority_reading, powerful, 0.35).
constraint_indexing:directionality_override(climate_response_imperative__adaptation_priority_reading, moderate, 0.75).
constraint_indexing:directionality_override(climate_response_imperative__adaptation_priority_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
