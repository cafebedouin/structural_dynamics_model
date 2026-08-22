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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Endogenous Internalization Pathway for Imposed Practice Displacement
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   A state apparatus decrees displacement of locally-embedded practices
 *   (lunar calendar, traditional dress, clan-based governance) to achieve
 *   administrative modernization and unified identity. This constraint
 *   instantiates the ENDOGENOUS CLIMB READING: the state discovers that
 *   coercive decree alone fails to produce stable practice displacement;
 *   internalization requires bottom-up adoption pathways that grant
 *   communities agency in timing and depth. Communities preserve autonomy by
 *   maintaining old practices in private spaces while offering partial public
 *   compliance. The state faces a bind: surface compliance is observable but
 *   internalization is incomplete, requiring continuous suppression to
 *   maintain. From this reading's vantage, the constraint is a TANGLED ROPE —
 *   genuine coordination function (the state needs some form of unified
 *   temporal/administrative framework) layered with extraction (the state's
 *   preferred practice is imposed, not negotiated). The coordination problem
 *   is real; the solution's coercive form is extractive.
 *
 * KEY AGENTS:
 *   - state_modernization_apparatus: Institutional agenda-setter with generational time horizon; sets compliance targets and enforces via administrative machinery.
 *   - communities_preserving_autonomy: Organized payers with identity-locked exit; retain old practices in private space, benefit from negotiated adoption timing.
 *   - urban_merchants: Moderate power; adopt state practice in public, retain private observance; benefit from market integration, pay through bifurcated identity.
 *   - administrative_enforcement_apparatus: Institutional venue; discovers that coercion without internalization produces theater rather than belief change.
 *   - prior_practice_keepers: Identity-locked payers; lose status and livelihood as old practice is banned; maintain knowledge underground.
 *   - youth_in_transitional_communities: Powerless, constrained; navigate dual socialization; face identity confusion but gain expanded choice at adulthood.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.68).
domain_priors:suppression_score(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.72).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__endogenous_climb_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__endogenous_climb_reading, "Endogenous Internalization Pathway for Imposed Practice Displacement").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__endogenous_climb_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__endogenous_climb_reading, '9f9833f2-a2b5-4c6f-a2dc-810144ac2053').
narrative_ontology:cs_kernel_codification('9f9833f2-a2b5-4c6f-a2dc-810144ac2053', formalized).
narrative_ontology:cs_authority_grounding('9f9833f2-a2b5-4c6f-a2dc-810144ac2053', extraction).
narrative_ontology:cs_interpretation_layer_present('9f9833f2-a2b5-4c6f-a2dc-810144ac2053').
narrative_ontology:cs_reading_relation('9f9833f2-a2b5-4c6f-a2dc-810144ac2053', legitimacy_of_imposed_practice__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f9833f2-a2b5-4c6f-a2dc-810144ac2053', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('9f9833f2-a2b5-4c6f-a2dc-810144ac2053', foundational, internalization_requires_agency).
narrative_ontology:cs_axiom_status(internalization_requires_agency, holdable).
narrative_ontology:cs_axiom_grounding('9f9833f2-a2b5-4c6f-a2dc-810144ac2053', internalization_requires_agency, empirically_contingent).
narrative_ontology:cs_axiom('9f9833f2-a2b5-4c6f-a2dc-810144ac2053', foundational, coercion_produces_theater_not_belief).
narrative_ontology:cs_axiom_status(coercion_produces_theater_not_belief, holdable).
narrative_ontology:cs_axiom_grounding('9f9833f2-a2b5-4c6f-a2dc-810144ac2053', coercion_produces_theater_not_belief, empirically_contingent).
narrative_ontology:cs_reference_frame('9f9833f2-a2b5-4c6f-a2dc-810144ac2053', state_decree_sufficient_for_displacement).
narrative_ontology:cs_drift_state('9f9833f2-a2b5-4c6f-a2dc-810144ac2053', contemporary_resistance_plateau, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9f9833f2-a2b5-4c6f-a2dc-810144ac2053', '2026-08-03T14:22:00Z').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, communities_preserving_autonomy).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_timeline).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_merchants).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, youth_in_transitional_communities).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, communities_preserving_autonomy).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, youth_in_transitional_communities).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, prior_practice_keepers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decrees displacement of prior practice (lunar calendar, traditional dress, clan-based authority) and enforces via administrative machinery. Claims modernization is necessary for state coherence, bureaucratic efficiency, and national identity. Sets the compliance target and punishes non-compliance; monitors through census, school enrollment, marketplace regulation. Depends on internalization for stable compliance but initially believes decree alone will displace the old practice.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Retain lunar calendar observance, traditional dress, and clan structures despite state mandate. They experience the constraint as preservation of autonomy — continued internal practice alongside public compliance signals incomplete state capture. They pay through reduced state integration, restricted market access for traditional goods, and administrative harassment. They benefit by retaining decision-making authority over adoption timing and depth.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, communities_preserving_autonomy, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__endogenous_climb_reading, communities_preserving_autonomy, payer).

% Adopt state-mandated practice (calendar, dress code) to access urban markets and state licensing. They partially internalize — adopt in public settings, retain private practices in household and family settings. They benefit from market integration; they pay through bifurcated identity and reduced autonomy in personal space.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_merchants, beneficiary,
    moderate, biographical, constrained, local).

% Enforces compliance through inspection, punishment, incentive structures, and visibility in public space. They discover that coercion alone does not produce internalization — communities comply in observable contexts but retain old practices in private spaces. Enforcement requires continuous monitoring and escalating penalties to maintain surface compliance, with diminishing effect on actual belief change.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, administrative_enforcement_apparatus, agenda_setter,
    institutional, biographical, constrained, national).

% Produce arguments for and against displacement. Those supporting modernization argue that internalization will follow generation-cohort socialization; those documenting resistance point to persistent private practice as evidence that coercion without appeal-to-value fails. They generate the conceptual frameworks competing parties use.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, intellectuals_and_educators, observer,
    powerful, generational, arbitrage, national).

% Face bifurcated socialization: state schooling teaches new practice; family/community teaches old. They navigate dual commitment structures and often invent hybrid forms. They pay through identity confusion and conflicting loyalty signals; they benefit through expanded choice set and exit option when they reach adulthood.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, youth_in_transitional_communities, payer,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__endogenous_climb_reading, youth_in_transitional_communities, beneficiary).

% Specialists and custodians of displaced practice (calendar keepers, ritual practitioners, traditional authorities) who face loss of status and economic position as the practice is banned. They pay through loss of livelihood and authority; they persist by teaching underground, maintaining private adherence, and framing preservation as resistance to state tyranny.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, prior_practice_keepers, payer,
    moderate, generational, identity_locked, regional).

% Document the displacement attempt: the extent to which old practices persist, the mechanisms by which communities avoid detection, and the generational timeline for actual belief change. They provide comparative data (similar displacement attempts in other states) that inform whether internalization is achievable through time and socialization or inherently limited by identity fusion.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, external_observers_and_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_apparatus).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The state aims to achieve a unified temporal, sartorial, and administrative framework across heterogeneous communities by synchronizing observable practice. The coordination problem is: how to move a complex, identity-embedded practice from decentralized (community-specific) to standardized (state-mandated) implementation across a population that does not initially share the new practice's meaning.
% TRANSFER_FUNCTION: Moves social capital, autonomy, and legitimacy from communities (who controlled practice adoption timing and depth) to the state (which now controls public-space compliance and sets internalization targets). Communities trade immediate compliance observability for long-term autonomy erosion if enforcement succeeds; the state trades decree certainty for dependence on generational socialization and voluntary adoption for stable displacement.
% ABSENT_VOICES: Diaspora communities and external powers who would reject the displacement on grounds of cultural imperialism are structurally excluded from the negotiation. Alternative modernization pathways (gradual adoption, syncretism, federal autonomy) are framed as obstacles to progress and kept off the agenda. Internal dissenters who question whether modernization requires displacement (rather than coexistence) are marginalized as traditionalist obstructionists.
% DISAPPEARANCE_RATIONALE: If the enforcement disappeared, communities would immediately restore old practices in public space and accelerate private retention. The state would face a legitimacy crisis as its modernization claim proved merely coercive rather than genuinely internalized. Market structures built on state-mandated practice (urban merchants dependent on state licensing) would have to renegotiate with communities. Generational cohorts socialized entirely under the new practice would face identity fracture as the old practice resurfaces.
% FOUNDING_PROBLEM: State formation requires administrative coherence across diverse populations; heterogeneous temporal systems, dress codes, and authority structures impede bureaucratic efficiency, taxation collection, military mobilization, and national identity formation. The founding problem is: how to displace locally-embedded practices that are fused with identity and community governance when the state's legitimacy is not yet established enough to command voluntary adoption?
% FOUNDING_PROBLEM_CORROBORATION: State modernization theorists attest the founding problem is live and persistent; administrative historians document state-imposed displacement failures across multiple states where coercion was not followed by internalization. Communities documenting displacement attempts (ethnographers, historians embedded in communities) attest that the founding problem framing assumes state legitimacy is not the bottleneck — but their data shows voluntary adoption only accelerates when the state's practical utility to communities becomes undeniable (not decree). Competition authorities and post-colonial analysts attest the founding problem was real but the solution of forced displacement created new problems (resistance, identity fracture, administrative brittleness) that outweighed coordination gains.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__endogenous_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.42 to 0.68 over the interval, with a plateau after t=40, indicating the constraint reaches its stable extractive level when suppression intensity stabilizes. The state can extract autonomy from communities (by controlling public compliance) but extraction does not increase further after the enforcement machinery reaches saturation — suppression at 0.72 reflects the burden of monitoring private practice and punishing deviation despite apparent public compliance. Theater ratio rises from 0.28 to 0.58, indicating growing divergence between public compliance (theaters of conformity: urban dress, official calendar use) and private retention. The plateau in both metrics suggests the constraint has reached an equilibrium: the state's decree is enforced at cost, but internalization has not deepened beyond the urban merchant/youth cohorts. The measurement series tracks a cyclical pattern (tension → attempted enforcement → partial compliance → renewed tension) repeated at generational intervals as new cohorts enter the system. This reading holds that the plateau itself is diagnostic: extraction does not grow because internalization has not deepened, and internalization will not deepen without the state conceding negotiation space to communities.
 *
 * PERSPECTIVAL GAP:
 *   From the state apparatus seat: the constraint is a necessary coordination mechanism that will eventually succeed through generational socialization; theater is a temporary inefficiency, not a structural failure. From the community seat: the constraint is extractive coercion that masks state illegitimacy; theater is the proof that internalization is not happening and will not happen without voluntary adoption. From the enforcement apparatus seat: the constraint is a coordination failure — they achieve observable compliance but not the internalization they were told would follow, leaving them responsible for an unsustainable burden of continuous suppression. The engine computes per-seat types from these divergent structural relationships. The exogenous_override_reading assumes the state can mandate belief through decree and ignores the private retention evidence; the hybrid_scaffolding_reading assumes the state can generate quasi-endogenous pull through messaging. This reading (endogenous_climb) holds that neither top-down mechanism suffices without community agency in adoption.
 *
 * DIRECTIONALITY LOGIC:
 *   The state modernization apparatus is the structural beneficiary (sets the practice, controls compliance targets, increases administrative coherence) — d near 0.1 (full beneficiary). Communities are the primary targets (do not choose the practice, bear surveillance costs, lose autonomy over adoption timing) — d near 0.9 (full targets). Urban merchants sit near symmetric (d ~0.5): they adopt in public for market benefit, retain in private for identity safety, and experience net gain through market integration despite bifurcation cost. Youth are trapped payers (d near 0.85): they face enforcement from above and identity claims from below with no real exit until adulthood. Prior practice keepers are high-extraction targets (d ~0.92): they lose their primary economic and social role entirely. Enforcement apparatus agents are moderately constrained (d ~0.6): they are tasked with an impossible goal (internalization via coercion) and experience the mounting cost of theater-maintenance without success.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy: the state's mandate (displace traditional practice via decree) has outlived its functional justification (coordination via unifed temporal system) but persists due to institutional inertia and enforcement machinery lock-in. At t=0, the coordination function was live — truly heterogeneous practices impeded efficient administration. By t=40, the enforcement machinery has matured to the point of achieving surface compliance, but the measurement data show the mandate has not achieved internalization. The state faces a mandatrophy fork: (a) continue extracting via suppression without internalization (the plateau condition), or (b) concede negotiation space to communities, which would allow endogenous adoption but undermine the state's modernization legitimacy claim. This reading resolves the mandatrophy by holding that the state's preferred solution (coercive internalization) cannot work — the coordination function (unified administrative practice) can persist without internalization if the state accepts bifurcation (communities adopt officially in public, maintain privately in spaces outside state visibility). The constraint transitions from a functional tangled_rope to a piton-like state where enforcement continues theater-generation without producing the internalization that would justify suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalization_vs_observable_compliance,
    'Is private retention of old practice evidence that internalization has failed, or is private retention compatible with genuine adoption in contexts where the state permits private practice?',
    'Post-enforcement trajectory: if the state relaxes suppression and permits private practice openly, does the old practice expand back into public space (indicating no internalization) or remain private (indicating partial, context-dependent adoption)? Generational data: do children of adopters in later cohorts spontaneously restore old practice or maintain the new practice absent enforcement pressure?',
    'If private retention is evidence of failed internalization, the constraint is extractive and unsustainable (plateau at high suppression cost). If private retention is compatible with adoption-in-context, the constraint transitions to a stable hybrid arrangement. Classification consequence: Tangled Rope (coordination + extraction) vs. Rope (genuine coordination with bifurcated practice spaces).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_vs_observable_compliance, empirical, 'Whether the measurement of internalization is compatible with private practice or requires extinction of the old practice in all contexts.').

omega_variable(
    community_agency_vs_coercive_reach,
    'Can communities realistically negotiate adoption timing and depth with a state that depends on decree for legitimacy, or is the state structurally committed to non-negotiable displacement?',
    'Comparative historical evidence: states that permitted community-led adoption of new practices vs. states that imposed non-negotiable displacement. Do communities that negotiated timelines show higher internalization rates and lower subsequent resistance? Institutional analysis: are there feedback mechanisms that would allow the state to perceive community agency as legitimate governance rather than defiance?',
    'If communities cannot realistically negotiate, the constraint remains extractive even if suppression is reduced — the state''s power asymmetry is irreducible. If negotiation is possible, the constraint could transition to genuine Rope (coordination between state and community over adoption terms). Classification consequence: Snare (asymmetric extraction with no real alternative) vs. Tangled Rope (asymmetry but with negotiation potential).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(community_agency_vs_coercive_reach, conceptual, 'Whether the state''s institutional structure permits voluntary adoption or requires coercive displacement by definition.').

omega_variable(
    generational_internalization_timeline,
    'Will cohorts socialized entirely under the new practice (youth at t=0 reaching adulthood by t=60) spontaneously internalize the practice and abandon the old practice, or will the old practice resurface as identity-grounded knowledge even in cohorts with no direct experience of the old practice?',
    'Longitudinal cohort tracking: measure practice adherence and identity alignment in cohorts reaching adulthood post-implementation. Historical comparison: similar displacement attempts in prior states, tracking generational outcomes.',
    'If internalization deepens with generational socialization, the theater ratio and extraction should decline over time (they do not in the data, suggesting weak internalization even in youth cohorts). If the old practice resurfaces despite youth socialization, the constraint''s enforcement cannot be reduced. Classification consequence: Theater-dominant Piton (enforcement persists despite erosion of true function) vs. Tangled Rope continuing indefinitely (extraction remains functionally necessary).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_internalization_timeline, empirical, 'Whether time and generational socialization reduce suppression requirements or whether suppression must be maintained perpetually.').

omega_variable(
    reading_vs_sibling_relation_kernel_vs_constructed,
    'Does this kernel represent genuinely contested empirical questions about what works in practice displacement (different readings of an objective situation), or does it represent incommensurable normative commitments (the state''s right to impose vs. communities'' right to autonomy)?',
    'Genealogy of the dispute: did the readings emerge from different interpretations of shared evidence (empirical disagreement), or from incompatible value systems (normative disagreement)? Can evidence resolve which reading is right, or are the readings supported by different data selections?',
    'If the dispute is empirical, the three readings should converge toward one true reading as evidence accumulates. If the dispute is normative, all three readings may persist indefinitely as different value systems maintain different strategic choices. The termination condition changes: empirical reading converges toward one classification; normative reading remains multivalent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_vs_sibling_relation_kernel_vs_constructed, preference, 'Whether the kernel represents different readings of a shared situation or incommensurable value commitments.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) structural (external legal penalties, surveillance, market exclusion) or internalized (communities have incorporated state authority into their self-concept and fear violating it even absent external monitoring)?',
    'Post-enforcement trajectory: if external suppression is removed, does compliance collapse immediately (structural) or persist at some level (internalized)? Underground practice prevalence: do communities maintain old practices only in true privacy (structural suppression drives bifurcation) or even in spaces where state visibility is theoretically absent (internalized fear)? Identity surveys: do surveyed community members report shame or illegitimacy when practicing the old form (internalized), or do they frame non-compliance as dangerous rather than wrong (structural)?',
    'If internalized, the constraint is more stable (compliance persists without monitoring) but more extractive (communities have internalized state authority, reducing autonomy). If structural, the constraint is brittle (suppression costs remain high) but less deeply extractive (communities retain oppositional identity). Classification consequence: affects both the sustainability and the true cost of the extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression operates via external coercion or via incorporated state authority in community self-concept.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__endogenous_climb_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 30, 0.54).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 40, 0.58).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 50, 0.58).
narrative_ontology:measurement(legi_tr_t60, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 60, 0.58).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 30, 0.64).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(legi_be_t60, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(legi_su_t60, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 60, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__endogenous_climb_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.18).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'legitimacy_of_imposed_practice'. The kernel describes the standing arrangement a state established via decree: that traditional practices must be displaced and replaced with state-mandated alternatives. The ENDOGENOUS CLIMB READING holds that displacement requires internalization and internalization requires community agency. The EXOGENOUS OVERRIDE READING holds that decree is sufficient; compliance follows from authority regardless of internalization. The HYBRID SCAFFOLDING READING holds that messaging generates quasi-endogenous pull and accelerates partial displacement. All three readings share the same referent (the state decree and attempted displacement) but instantiate different constraints with different ε values, beneficiary/victim structures, and classifications. The endogenous_climb reading computes as Tangled Rope (coordination + extraction) because communities benefit from negotiated adoption timelines but pay through suppression costs while adoption remains incomplete. The exogenous_override reading computes as Snare (pure asymmetric extraction) because it denies the community agency channel and frames compliance as coerced. The hybrid_scaffolding reading computes as Scaffold (temporary coordination mechanism designed to transition to internalization). Each reading is linked via network.affects_constraints to its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_of_imposed_practice__endogenous_climb_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
