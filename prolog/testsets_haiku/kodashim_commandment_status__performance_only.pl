% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__performance_only, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kodashim_commandment_status__performance_only
 *   human_readable: Sacrifice Law Commandment Status (Performance-Only Reading): Temple-Contingent Obligation
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   This constraint instantiates the performance-only reading of the kodashim
 *   (sacrificial laws) kernel in halakhic tradition. The reading treats
 *   sacrifice commandments as conditionally suspended: without the physical
 *   Temple altar, the obligations are dormant text — neither reinterpreted as
 *   study (study-as-performance reading) nor maintained as readiness for
 *   restoration (messianic-deferral reading). The institutional apparatus
 *   that maintains this reading captures scholarly prestige, curricular
 *   authority, and intellectual labor that might otherwise redirect toward
 *   restoration mechanics or competing hermeneutics. The reading is presented
 *   as a neutral classification of halakhic status; the structural analysis
 *   reveals how this classification functions to concentrate interpretive
 *   authority and extract resources from advocates of alternative readings.
 *
 * KEY AGENTS:
 *   - talmudic_establishment_authorities: Define which commandments are suspended and which remain operative; maintain the performance-only reading's dominance through curricular authority and precedent.
 *   - intensive_study_infrastructure: Benefits from meticulous study of suspended laws; derives institutional legitimacy and funding from mastery of dormant commandments.
 *   - messianic_deferral_advocates: Suppressed by being classified as speculative; their reading would reframe study as preparation rather than irrelevant practice.
 *   - study_as_performance_advocates: Identity-locked to alternative interpretation; face active suppression as their framework contradicts the institutional dominance of physical-performance-only.
 *   - practical_restoration_movements: Excluded from authority structures; would challenge the assumption that suspension is permanent and indefinite.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, 0.68).
domain_priors:suppression_score(kodashim_commandment_status__performance_only, 0.71).
domain_priors:theater_ratio(kodashim_commandment_status__performance_only, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, extractiveness, 0.68).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__performance_only, snare).
narrative_ontology:human_readable(kodashim_commandment_status__performance_only, "Sacrifice Law Commandment Status (Performance-Only Reading): Temple-Contingent Obligation").
narrative_ontology:topic_domain(kodashim_commandment_status__performance_only, "religious/halakhic/commitment_system").

domain_priors:requires_active_enforcement(kodashim_commandment_status__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__performance_only, 'a1ff8f51-a0c0-4acf-8cfb-cddc81cb693a').
narrative_ontology:cs_kernel_codification('a1ff8f51-a0c0-4acf-8cfb-cddc81cb693a', fixed_text).
narrative_ontology:cs_authority_grounding('a1ff8f51-a0c0-4acf-8cfb-cddc81cb693a', lineage).
narrative_ontology:cs_interpretation_layer_present('a1ff8f51-a0c0-4acf-8cfb-cddc81cb693a').
narrative_ontology:cs_reading_relation('a1ff8f51-a0c0-4acf-8cfb-cddc81cb693a', kodashim_commandment_status__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('a1ff8f51-a0c0-4acf-8cfb-cddc81cb693a', kodashim_commandment_status__messianic_deferral, coexists_with).
narrative_ontology:cs_axiom('a1ff8f51-a0c0-4acf-8cfb-cddc81cb693a', foundational, commandment_requires_physical_altar).
narrative_ontology:cs_axiom_status(commandment_requires_physical_altar, holdable).
narrative_ontology:cs_axiom_grounding('a1ff8f51-a0c0-4acf-8cfb-cddc81cb693a', commandment_requires_physical_altar, conventional).
narrative_ontology:cs_axiom('a1ff8f51-a0c0-4acf-8cfb-cddc81cb693a', foundational, suspension_is_indefinite_dormancy).
narrative_ontology:cs_axiom_status(suspension_is_indefinite_dormancy, holdable).
narrative_ontology:cs_axiom_grounding('a1ff8f51-a0c0-4acf-8cfb-cddc81cb693a', suspension_is_indefinite_dormancy, conventional).
narrative_ontology:cs_reference_frame('a1ff8f51-a0c0-4acf-8cfb-cddc81cb693a', commandment_suspension_permanent).
narrative_ontology:cs_drift_state('a1ff8f51-a0c0-4acf-8cfb-cddc81cb693a', contemporary_halakhic_pluralism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a1ff8f51-a0c0-4acf-8cfb-cddc81cb693a', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__performance_only, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, talmudic_interpretation_establishment).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, institutionalized_study_system).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, alternative_halakhic_movements).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, redirected_resource_allocation_potential).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, practical_restoration_preparedness).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, intensive_study_infrastructure).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, messianic_deferral_advocates).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, study_as_performance_advocates).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__performance_only, conditional_commandment_doctrine).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__performance_only, hermeneutical_authority_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains and adjudicates the reading that sacrifice laws are contingent on Temple existence — without the physical altar, the commandment is suspended as dormant text, not reinterpreted or relocated. This reading grounds institutional authority in the ability to determine which commandments remain operative and which are suspended. Administers the interpretive machinery that keeps this classification in place through rabbinic precedent and curricular structure.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, talmudic_establishment_authorities, agenda_setter,
    institutional, generational, arbitrage, universal).

% Derives legitimacy and resource allocation from the need to study suspended sacrificial laws in meticulous detail — mastery of these commandments (even in dormancy) becomes a metric of scholarly prestige and institutional belonging. The study infrastructure captures intellectual labor and institutional funding that might otherwise redirect to restoration mechanics, practical preparation, or competing readings that would activate different scholarly hierarchies.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, intensive_study_infrastructure, beneficiary,
    institutional, generational, trapped, universal).

% Hold the reading that commandment suspension is temporary — study is framed as maintaining readiness for future restoration. This reading would reorient scholarly effort toward functional preparedness, active engagement with obstacles to restoration, and the hypothesis that Temple sacrifice will resume. They bear the cost of intellectual marginalization and reduced resource access under the dominant performance-only reading, which treats their interpretation as speculative rather than authoritative.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, messianic_deferral_advocates, payer,
    organized, generational, constrained, universal).

% Advance the reading that intellectual engagement with sacrificial law itself constitutes performance of the commandment — the kernel remains occupied through study, not suspended. This reading would elevate the status of those engaged in contemplative practice and reframe the commandment as perpetually operative through intellectual channels. They are identity-locked to this interpretation but face active suppression as their framework contradicts the institutional dominance of performance-only, which treats their claim as a category error (you cannot perform a commandment through study when the commandment requires physical action).
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, study_as_performance_advocates, payer,
    moderate, biographical, identity_locked, universal).

% Would advocate for active engagement with obstacles to Temple restoration and preparation for resumed sacrifice — their voice would reframe the commandment as suspended but not obsolete, requiring practical work toward reactivation. They are excluded from institutional authority structures that govern which interpretations are considered legitimate; the performance-only reading's dominance marginalizes their perspective.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, practical_restoration_movements, excluded,
    moderate, biographical, constrained, regional).

% Analyzes the structural relationship between competing readings of the halakhic kernel: which reading treats the commandment as suspended vs. active, how each reading organizes institutional authority and scholarly effort, and what structural benefits accrue to the dominant reading.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, textual_interpretation_community, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_commandment_status__performance_only, talmudic_establishment_authorities).
narrative_ontology:fixing_cost_class(kodashim_commandment_status__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Determines the status of commandments after their material conditions disappear: which obligations remain operative and which are suspended. Provides a hermeneutical framework for the Jewish community to relate consistently to the law when the Temple's physical existence is absent. Establishes interpretive authority to resolve this question in a canonical way rather than leaving it to individual judgment.
% TRANSFER_FUNCTION: Transfers intellectual prestige and scholarly authority to those who master the performance-only reading's interpretation; transfers resources from alternative readings (restoration mechanics, study-as-performance communities, messianic-deferral preparedness) to the intensive study infrastructure that canonizes suspension; transfers the legitimacy to define halakhic status from dispersed communities to the talmudic establishment.
% ABSENT_VOICES: Practical restoration movements, which would argue for active engagement with obstacles to Temple restoration and would claim that suspension is temporary; study-as-performance communities, which would argue that intellectual engagement fulfills commandments and that the kernel remains occupied through study; contemporary Jews who might question whether the 2,000-year-old founding problem (commandment status after Temple loss) still drives institutional investment in sacrificial law study.
% DISAPPEARANCE_RATIONALE: If the performance-only reading disappeared, the halakhic landscape would reorganize: messianic-deferral would reframe study as preparatory work with different institutional priorities; study-as-performance would elevate contemplative engagement and redirect resources; practical restoration movements would emerge from marginalization. The talmudic establishment would lose its canonical authority to determine commandment status. Whether the world rearranges or remains stable depends on whether the reading is necessary for halakhic coherence (the establishment's claim) or is one interpretive choice among coherent alternatives (the competing readings' claim).
% FOUNDING_PROBLEM: After the Second Temple's destruction (70 CE), commandments that require the physical altar cannot be performed. What is their halakhic status? Should the community treat them as permanently extinct, as temporarily suspended awaiting restoration, or as reinterpreted to operate in new ways? How should the community relate to the law when its material preconditions are absent?
% FOUNDING_PROBLEM_CORROBORATION: The talmudic establishment asserts the problem is eternally live — the Temple is still absent and the status of sacrificial commandments is still the subject of halakhic analysis. But alternative halakhic movements and contemporary scholars note that two millennia have passed; the 'founding problem' has ossified into institutional routine. The establishment's own practice reveals the shift: intense study of sacrificial law no longer aims at preparation for restoration (which would be time-sensitive) but at mastery of a canonical text-body for its own sake. Contemporary Haredi educational systems invest vast resources in detailed study of sacrificial law with no expectation that restoration is imminent; Reform and Conservative movements have explicitly reframed or abandoned the problem. The claim that the founding problem remains *alive* (status=live) is corroborated only by the establishment itself. Historical and comparative analysis shows the founding problem is *dead* — the establishment's persistence in addressing it is institutional inertia, not response to an urgent requirement. This is the mandatrophy signal: the constraint persists because institutional interests depend on it, not because the founding problem drives ongoing necessity.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__performance_only, contested).
narrative_ontology:founding_problem_status(kodashim_commandment_status__performance_only, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__performance_only, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(kodashim_commandment_status__performance_only, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_commandment_status__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the performance-only reading functions as a structural mechanism for concentrating hermeneutical authority: only those certified by the talmudic establishment can determine commandment status, and the establishment benefits from the reading's dominance. Suppression is equally high (0.71) because maintaining the reading requires active suppression of competing interpretations — alternative readings are not merely less prestigious but are actively delegitimized through rhetorical strategies and institutional exclusion. Theater is very high (0.62) because an increasing share of the constraint's operation consists of performative maintenance: the meticulous study of sacrifice laws serves ceremonial/identity functions more than functional preparation. The measurement series track the intensification of all three metrics over two millennia, with a slight decline in extractiveness and theater at the modern endpoint (reflecting contemporary challenges to institutional authority) but suppression remaining high. The leveled coercion grid reveals that suppression and accessibility collapse increase at every level — the constraint hardens structurally, not just institutionally. The stage dynamics: individual-level alternatives (alternative readings are accessible in principle) collapse over time (cultural pressure to accept dominant reading hardens); organizational resistance (minority streams) weakens as institutional authority strengthens. The claim/metric gap is intentional: the constraint is CLAIMED as snare (pure extraction) but the actual operation blurs snare and tangled-rope (coordination + extraction, as the reading does solve the genuine problem of commandment classification after Temple destruction).
 *
 * PERSPECTIVAL GAP:
 *   From the talmudic establishment's seat, the reading is a neutral classification grounded in hermeneutical logic — commandments requiring the altar cannot be performed without it; therefore suspension follows as a logical consequence. From the messianic-deferral or study-as-performance seats, the same classification operates as extractive suppression: it preempts competing interpretations and consolidates institutional authority by claiming to merely describe halakhic status. The engine should compute different directionalities for these seats: the establishment (beneficiary, powerful, arbitrage-exit) near d=0.1; alternative advocates (victims, moderate/organized, identity-locked/constrained) near d=0.8-0.9. The perspectival gap is the classification's core function — it makes the authority grab appear neutral.
 *
 * DIRECTIONALITY LOGIC:
 *   The talmudic establishment derives its authority claim from the reading's appearance of logical inevitability: if commandments require the altar and the altar is gone, suspension follows as a natural law, not as a political choice. This appearance of neutrality keeps d low for the establishment. The study infrastructure benefits from the reading's institutionalization and captures resources (high d for those it extracts from). Advocates of competing readings are trapped: they cannot simply accept the dominant reading without abandoning their hermeneutical position, so they face active suppression and resource starvation (very high d for victims). The reading functions to keep the classification question off-limits — reopening it would reveal the politics embedded in the supposedly neutral status determination.
 *
 * MANDATROPHY ANALYSIS:
 *   This is the archetype of mandatrophy in commitment-system constraints. The founding problem — determining commandment status after Temple destruction — was genuinely live and required resolution in the immediate post-destruction period. Two millennia later, the problem has crystallized into institutional routine: the talmudic establishment's answer has become canonized, alternative readings are institutionally suppressed, and the reading persists not because the founding problem remains urgent but because institutional interests (scholarly prestige, curricular authority, organizational identity) depend on the reading's dominance. The dead status + world_rearranges verdict signals mandatrophy: the founding problem no longer drives the constraint's persistence; institutional inertia and captured authority do. The high theater ratio (0.62) indicates performative maintenance — the meticulous study of sacrifice laws performs scholarly legitimacy more than functional preparation. The measurement drift toward higher suppression and theater while extractiveness plateaus is the signature of mandatrophic drift: the constraint persists as institutional theater and authority capture, not as functional coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_vs_reinterpretation_boundary,
    'Is the boundary between suspension (the commandment is dormant but unchanged) and reinterpretation (the commandment is redefined to operate through study or preparation) a structural fact about the law or a hermeneutical choice?',
    'Textual and historical analysis of how competing interpretive communities actually treat the distinction; examination of whether the performance-only reading''s assertion of suspension follows necessarily from the source texts or represents one chosen interpretation among coherent alternatives.',
    'If the boundary is hermeneutical rather than structural, the performance-only reading''s claim to neutrality collapses — it becomes a political choice that benefits those who maintain the talmudic establishment''s authority. This would reclassify the constraint from snare (pure extraction dressed as classification) to tangled_rope (genuine coordination problem + asymmetric authority capture).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suspension_vs_reinterpretation_boundary, conceptual, 'Whether commandment suspension is a logical necessity or an interpretive choice that concentrates authority.').

omega_variable(
    restoration_preparedness_obligation,
    'Does the performance-only reading''s assertion that restoration-preparation is optional follow from the reading itself, or is that an additional stipulation that could be reversed without changing the core reading?',
    'Examination of historical halakhic sources to determine whether the performance-only reading necessarily forecloses restoration-preparedness obligations, or whether a version of performance-only could coexist with treating preparation as obligatory.',
    'If restoration-preparedness could be integrated into the performance-only reading, the reading''s suppression of practical_restoration_movements would be revealed as a contingent institutional choice rather than a logical consequence. This would shift the reading''s configuration from snare toward a hybrid that suppresses not by logical necessity but by institutional preference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_preparedness_obligation, empirical, 'Whether performance-only reading necessarily forecloses restoration-preparedness obligations.').

omega_variable(
    study_infrastructure_resource_capture,
    'What proportion of the intellectual resources invested in sacrificial law study would redirect to other domains (restoration mechanics, alternative halakhic readings, contemporary Jewish practice) if the performance-only reading lost institutional dominance?',
    'Comparative analysis of resource allocation in halakhic communities that adopt different readings; historical observation of how resource allocation shifted when institutional authority over a reading changed.',
    'A high proportion would establish that the study infrastructure''s capture is substantial and that alternative readings would reconfigure institutional priorities. This would confirm the extractiveness diagnosis and identify specific victims (domains starved of resources by the performance-only reading''s dominance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_infrastructure_resource_capture, empirical, 'Quantification of resource capture by the study infrastructure.').

omega_variable(
    authority_foundation_in_logical_necessity,
    'Does the talmudic establishment''s interpretive authority rest on the claim that the performance-only reading follows necessarily from logical analysis of the law, or on the institutional power to enforce one reading as canonical?',
    'Examination of how the talmudic establishment justifies its interpretive authority; analysis of whether it appeals to logical necessity or institutional position when defending the performance-only reading against alternatives.',
    'If authority rests on logical necessity, and that necessity claim is vulnerable to challenge, the authority''s foundation is fragile and the constraint could be destabilized by hermeneutical argument. If authority rests on institutional power, the constraint is more stable but more obviously extractive — the appearance of neutrality breaks down.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_foundation_in_logical_necessity, conceptual, 'Whether talmudic authority is grounded in logical necessity or institutional power.').

omega_variable(
    messianic_temporality_foreclosure,
    'Does the performance-only reading actually foreclose the messianic-deferral reading, or do they merely represent different emphases on the same underlying assumption (suspension)?',
    'Detailed analysis of the logical structure of both readings to determine whether the core premises contradict or whether they differ only on secondary implications (preparedness, urgency, temporal horizon).',
    'If the readings only differ on secondary points, the coexists_with relation is correct and the readings could in principle be harmonized through institutional negotiation. If they have logically contradictory cores, the forecloses relation is correct and one reading''s dominance necessarily suppresses the other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_temporality_foreclosure, conceptual, 'Logical relationship between performance-only and messianic-deferral readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__performance_only, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__performance_only, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(koda_tr_t0, observed).
narrative_ontology:measurement(koda_tr_t250, kodashim_commandment_status__performance_only, theater_ratio, 250, 0.42).
narrative_ontology:measurement_basis(koda_tr_t250, observed).
narrative_ontology:measurement(koda_tr_t500, kodashim_commandment_status__performance_only, theater_ratio, 500, 0.48).
narrative_ontology:measurement_basis(koda_tr_t500, observed).
narrative_ontology:measurement(koda_tr_t750, kodashim_commandment_status__performance_only, theater_ratio, 750, 0.54).
narrative_ontology:measurement_basis(koda_tr_t750, observed).
narrative_ontology:measurement(koda_tr_t1000, kodashim_commandment_status__performance_only, theater_ratio, 1000, 0.59).
narrative_ontology:measurement_basis(koda_tr_t1000, observed).
narrative_ontology:measurement(koda_tr_t1500, kodashim_commandment_status__performance_only, theater_ratio, 1500, 0.63).
narrative_ontology:measurement_basis(koda_tr_t1500, observed).
narrative_ontology:measurement(koda_tr_t2000, kodashim_commandment_status__performance_only, theater_ratio, 2000, 0.62).
narrative_ontology:measurement_basis(koda_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__performance_only, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(koda_be_t0, observed).
narrative_ontology:measurement(koda_be_t250, kodashim_commandment_status__performance_only, base_extractiveness, 250, 0.58).
narrative_ontology:measurement_basis(koda_be_t250, observed).
narrative_ontology:measurement(koda_be_t500, kodashim_commandment_status__performance_only, base_extractiveness, 500, 0.62).
narrative_ontology:measurement_basis(koda_be_t500, observed).
narrative_ontology:measurement(koda_be_t750, kodashim_commandment_status__performance_only, base_extractiveness, 750, 0.65).
narrative_ontology:measurement_basis(koda_be_t750, observed).
narrative_ontology:measurement(koda_be_t1000, kodashim_commandment_status__performance_only, base_extractiveness, 1000, 0.68).
narrative_ontology:measurement_basis(koda_be_t1000, observed).
narrative_ontology:measurement(koda_be_t1500, kodashim_commandment_status__performance_only, base_extractiveness, 1500, 0.71).
narrative_ontology:measurement_basis(koda_be_t1500, observed).
narrative_ontology:measurement(koda_be_t2000, kodashim_commandment_status__performance_only, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement_basis(koda_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_commandment_status__performance_only, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(koda_su_t0, observed).
narrative_ontology:measurement(koda_su_t250, kodashim_commandment_status__performance_only, suppression_requirement, 250, 0.54).
narrative_ontology:measurement_basis(koda_su_t250, observed).
narrative_ontology:measurement(koda_su_t500, kodashim_commandment_status__performance_only, suppression_requirement, 500, 0.6).
narrative_ontology:measurement_basis(koda_su_t500, observed).
narrative_ontology:measurement(koda_su_t750, kodashim_commandment_status__performance_only, suppression_requirement, 750, 0.65).
narrative_ontology:measurement_basis(koda_su_t750, observed).
narrative_ontology:measurement(koda_su_t1000, kodashim_commandment_status__performance_only, suppression_requirement, 1000, 0.69).
narrative_ontology:measurement_basis(koda_su_t1000, observed).
narrative_ontology:measurement(koda_su_t1500, kodashim_commandment_status__performance_only, suppression_requirement, 1500, 0.72).
narrative_ontology:measurement_basis(koda_su_t1500, observed).
narrative_ontology:measurement(koda_su_t2000, kodashim_commandment_status__performance_only, suppression_requirement, 2000, 0.71).
narrative_ontology:measurement_basis(koda_su_t2000, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=2000
narrative_ontology:measurement(koda_grid_01, kodashim_commandment_status__performance_only, accessibility_collapse(class), 0, 0.68).
narrative_ontology:measurement(koda_grid_02, kodashim_commandment_status__performance_only, accessibility_collapse(class), 2000, 0.81).
narrative_ontology:measurement(koda_grid_03, kodashim_commandment_status__performance_only, accessibility_collapse(individual), 0, 0.62).
narrative_ontology:measurement(koda_grid_04, kodashim_commandment_status__performance_only, accessibility_collapse(individual), 2000, 0.74).
narrative_ontology:measurement(koda_grid_05, kodashim_commandment_status__performance_only, accessibility_collapse(organizational), 0, 0.71).
narrative_ontology:measurement(koda_grid_06, kodashim_commandment_status__performance_only, accessibility_collapse(organizational), 2000, 0.83).
narrative_ontology:measurement(koda_grid_07, kodashim_commandment_status__performance_only, accessibility_collapse(structural), 0, 0.75).
narrative_ontology:measurement(koda_grid_08, kodashim_commandment_status__performance_only, accessibility_collapse(structural), 2000, 0.88).
narrative_ontology:measurement(koda_grid_09, kodashim_commandment_status__performance_only, resistance(class), 0, 0.54).
narrative_ontology:measurement(koda_grid_10, kodashim_commandment_status__performance_only, resistance(class), 2000, 0.48).
narrative_ontology:measurement(koda_grid_11, kodashim_commandment_status__performance_only, resistance(individual), 0, 0.48).
narrative_ontology:measurement(koda_grid_12, kodashim_commandment_status__performance_only, resistance(individual), 2000, 0.42).
narrative_ontology:measurement(koda_grid_13, kodashim_commandment_status__performance_only, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(koda_grid_14, kodashim_commandment_status__performance_only, resistance(organizational), 2000, 0.55).
narrative_ontology:measurement(koda_grid_15, kodashim_commandment_status__performance_only, resistance(structural), 0, 0.58).
narrative_ontology:measurement(koda_grid_16, kodashim_commandment_status__performance_only, resistance(structural), 2000, 0.52).
narrative_ontology:measurement(koda_grid_17, kodashim_commandment_status__performance_only, stakes_inflation(class), 0, 0.48).
narrative_ontology:measurement(koda_grid_18, kodashim_commandment_status__performance_only, stakes_inflation(class), 2000, 0.72).
narrative_ontology:measurement(koda_grid_19, kodashim_commandment_status__performance_only, stakes_inflation(individual), 0, 0.45).
narrative_ontology:measurement(koda_grid_20, kodashim_commandment_status__performance_only, stakes_inflation(individual), 2000, 0.68).
narrative_ontology:measurement(koda_grid_21, kodashim_commandment_status__performance_only, stakes_inflation(organizational), 0, 0.52).
narrative_ontology:measurement(koda_grid_22, kodashim_commandment_status__performance_only, stakes_inflation(organizational), 2000, 0.76).
narrative_ontology:measurement(koda_grid_23, kodashim_commandment_status__performance_only, stakes_inflation(structural), 0, 0.58).
narrative_ontology:measurement(koda_grid_24, kodashim_commandment_status__performance_only, stakes_inflation(structural), 2000, 0.79).
narrative_ontology:measurement(koda_grid_25, kodashim_commandment_status__performance_only, suppression(class), 0, 0.42).
narrative_ontology:measurement(koda_grid_26, kodashim_commandment_status__performance_only, suppression(class), 2000, 0.71).
narrative_ontology:measurement(koda_grid_27, kodashim_commandment_status__performance_only, suppression(individual), 0, 0.38).
narrative_ontology:measurement(koda_grid_28, kodashim_commandment_status__performance_only, suppression(individual), 2000, 0.62).
narrative_ontology:measurement(koda_grid_29, kodashim_commandment_status__performance_only, suppression(organizational), 0, 0.54).
narrative_ontology:measurement(koda_grid_30, kodashim_commandment_status__performance_only, suppression(organizational), 2000, 0.78).
narrative_ontology:measurement(koda_grid_31, kodashim_commandment_status__performance_only, suppression(structural), 0, 0.55).
narrative_ontology:measurement(koda_grid_32, kodashim_commandment_status__performance_only, suppression(structural), 2000, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_commandment_status__performance_only, 0.12).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, kodashim_commandment_status__messianic_deferral).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, kodashim_commandment_status__study_as_performance).

% DUAL FORMULATION NOTE:
% The kodashim_commandment_status kernel decomposes into three constraint stories, one per competing reading of halakhic status: performance-only (this story), messianic_deferral, and study_as_performance. Each reading instantiates a structurally distinct constraint with its own ε, beneficiary/victim structure, and classification. The stories are linked via network.affects_constraints to model their competitive relationship and shared dependence on the kernel's interpretation. The ε values diverge across readings: performance-only shows high extractiveness because the reading functions as an authority-concentrating mechanism; messianic_deferral shows lower extractiveness (restoration-focused preparation is less obviously extractive); study_as_performance shows moderate extractiveness with different victim-beneficiary alignments (study communities benefit, institutional authority is distributed). These are not alternate measurements of one constraint — they are structurally distinct constraints instantiated by the same kernel under different readings. The boundary between readings is the dispute over whether commandment suspension is permanent vs. temporary and whether study can fulfill or only prepare.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_commandment_status__performance_only, organized, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
