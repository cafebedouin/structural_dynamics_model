% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
 *   human_readable: Dual-Practice Domain Partition: State/Traditional Authority Bifurcation
 *   domain: political_history/institutional_change
 *
 * SUMMARY:
 *   A state apparatus and traditional authorities negotiate a durable
 *   bifurcation: state governance applies to public/administrative domains
 *   (taxation, legal jurisdiction, official timekeeping), while traditional
 *   authority governs private/ritual domains (festivals, household norms,
 *   agricultural timing). This reading instantiates the
 *   dual-practice-equilibrium frame—the partition is presented as a
 *   legitimate permanent arrangement, not a transitional phase. The claim is
 *   tangled_rope because the partition both coordinates (solves the
 *   modernization dilemma without unilateral dominance) and extracts (from
 *   boundary-crossing practitioners who must internalize dual legitimacy, and
 *   from practitioners outside the partition who fit neither pole). The
 *   authored metrics (0.58 extractiveness, 0.62 suppression, 0.41 theater)
 *   reflect this: extraction rises early as the partition is established,
 *   then plateaus as it becomes institutionalized, suggesting the
 *   coordination function is genuine but persists partly through normalized
 *   enforcement.
 *
 * KEY AGENTS:
 *   - state_administrative_apparatus: sets public-domain authority, enforces fiscal standardization, legitimates through bureaucratic necessity
 *   - traditional_authority_holders: retain private-domain authority, cooperate with partition in exchange for preservation
 *   - boundary_crossing_practitioners: internalize dual legitimacy, bear cognitive/coordination costs, strategic compliance
 *   - cultural_minorities_outside_partition: subordinated to both systems, offered no legitimate alternative
 *   - unified_practice_movements: excluded by the partition's structure—any unified system would be illegitimate by definition
 *   - international_standards_bodies: external observers treating the partition as transitional anomaly
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.58).
domain_priors:suppression_score(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.62).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "Dual-Practice Domain Partition: State/Traditional Authority Bifurcation").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "political_history/institutional_change").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'ac6d6e90-28c6-4629-a38f-0d5825e7da17').
narrative_ontology:cs_kernel_codification('ac6d6e90-28c6-4629-a38f-0d5825e7da17', distributed).
narrative_ontology:cs_authority_grounding('ac6d6e90-28c6-4629-a38f-0d5825e7da17', extraction).
narrative_ontology:cs_interpretation_layer_present('ac6d6e90-28c6-4629-a38f-0d5825e7da17').
narrative_ontology:cs_reading_relation('ac6d6e90-28c6-4629-a38f-0d5825e7da17', legitimacy_of_practice_standardization__endogenous_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('ac6d6e90-28c6-4629-a38f-0d5825e7da17', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('ac6d6e90-28c6-4629-a38f-0d5825e7da17', foundational, practice_legitimacy_is_domain_partitioned).
narrative_ontology:cs_axiom_status(practice_legitimacy_is_domain_partitioned, holdable).
narrative_ontology:cs_axiom_grounding('ac6d6e90-28c6-4629-a38f-0d5825e7da17', practice_legitimacy_is_domain_partitioned, deontological).
narrative_ontology:cs_axiom('ac6d6e90-28c6-4629-a38f-0d5825e7da17', foundational, dual_authority_equilibrium_is_optimal_stable_state).
narrative_ontology:cs_axiom_status(dual_authority_equilibrium_is_optimal_stable_state, holdable).
narrative_ontology:cs_axiom_grounding('ac6d6e90-28c6-4629-a38f-0d5825e7da17', dual_authority_equilibrium_is_optimal_stable_state, instrumental).
narrative_ontology:cs_reference_frame('ac6d6e90-28c6-4629-a38f-0d5825e7da17', domain_partitioned_dual_authority).
narrative_ontology:cs_drift_state('ac6d6e90-28c6-4629-a38f-0d5825e7da17', contemporary_globalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ac6d6e90-28c6-4629-a38f-0d5825e7da17', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_administrative_apparatus).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_authority_holders).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, boundary_crossing_practitioners).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, cultural_minorities_outside_partition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_modernization_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decrees which practices fall under state governance (taxation calendar, legal proceedings, civil registration). Uses Gregorian calendar for fiscal administration, Western-derived dress codes for official contexts, standardized timekeeping. Enforces compliance through licensing, official recognition, and bureaucratic infrastructure. Justifies partition as enabling effective governance without requiring cultural conversion.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_administrative_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Retain legitimacy over private/ritual domains: festivals, family transitions, agricultural timing, household dress norms. Maintain lunar calendars for planting, traditional garments for ceremonies, customary dispute resolution. The partition legitimates their continued authority without requiring state sanction. Their power is constrained to non-state domains but is preserved within them.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_authority_holders, agenda_setter,
    powerful, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_authority_holders, beneficiary).

% Occupy roles spanning both domains: village headmen who must file taxes in state calendar while conducting rituals in lunar calendar; merchants who wear Western suits for licensing but traditional dress for trade ceremonies; professionals who navigate dual legitimacy systems. They internalize the partition's rules and bear the cognitive and coordination costs of code-switching. Their compliance is strategic: they learn when each authority applies.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, boundary_crossing_practitioners, payer,
    moderate, biographical, constrained, national).

% Groups whose practices do not align with either pole of the partition—neither state-standardized nor recognized traditional authority. Immigrant communities, syncretic movements, or practitioners whose calendar/dress/ritual belongs to a third tradition. They are subordinated to both systems: subject to state law but denied recognition in the traditional domain, pressured toward state conformity with no legitimate cultural alternative.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, cultural_minorities_outside_partition, payer,
    powerless, biographical, trapped, national).

% Intellectuals, reformers, officials who view the partition as the optimal compromise: modernity in public administration, tradition preserved in private life, no civilizational rupture. They benefit from social stability the partition provides—resistance is channeled into the traditional domain rather than opposing the state wholesale. Their advocacy reinforces the partition's legitimacy.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_modernization_advocates, beneficiary,
    organized, biographical, mobile, national).

% Activists, religious movements, or nationalist intellectuals advocating either full cultural restoration (all lunar calendar, all traditional dress) or complete modernization (single unified practice across all domains). The partition systematically excludes their vision by declaring it incoherent—you cannot govern by lunar calendar or conduct state business in kimono without state collapse. They would contest the partition's legitimacy but are structurally prevented from offering a viable alternative.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, unified_practice_movements, excluded,
    moderate, generational, trapped, national).

% Technical and diplomatic organizations (ISO, UN, trade bodies) that assume unified practice standards—one calendar, one timekeeping system, one measurement standard for commerce. They observe the dual partition and treat it as a temporary transition that will eventually resolve toward global standards. Their position provides external pressure toward convergence.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, international_standards_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, diffuse).
narrative_ontology:fixing_cost_class(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the modernization coordination problem without requiring cultural hegemony: enables state fiscal administration, legal jurisdiction, and bureaucratic efficiency to operate via Gregorian calendar and standardized timekeeping while preserving traditional authority over domains where cultural continuity is valued (agricultural timing, rituals, family law). Avoids both civilizational rupture and ungovernable heterogeneity.
% TRANSFER_FUNCTION: Moves legitimate authority from practitioners to one of two institutional seats depending on context. In public/administrative domains, authority transfers from traditional to state institutions, validated by the partition rule itself. In private/ritual domains, authority remains with traditional institutions under state sufferance. The partition extracts from practitioners the cognitive and compliance cost of maintaining dual legitimacy systems and code-switching between them.
% ABSENT_VOICES: Unified-practice movements (restoration or modernization) are excluded: they cannot coherently object within the partition framework because any unified system would be illegitimate by definition (either abandoning governance capacity or cultural practice). Practitioners from third traditions (immigrants, syncretic communities) lack a legitimate voice because they fit neither partition pole.
% DISAPPEARANCE_RATIONALE: The exogenous_override_reading predicts convergence toward unified state standards if the partition vanished; the endogenous_displacement_reading predicts voluntary re-emergence of dual practice if external enforcement ceased. The dual_practice_equilibrium_reading (this reading) holds that the partition would persist because it solves a genuine coordination problem—neither side can unilaterally impose unity without costs both find unacceptable, so the stable point is the boundary itself.
% FOUNDING_PROBLEM: Early modernization faced a dilemma: state taxation and administration required standardized calendars and timekeeping incompatible with agricultural and ritual life. Unified conversion displaced traditional practitioners and sparked resistance; unified preservation prevented effective fiscal governance. The partition legitimated both by declaring them separate legitimate domains with different authorities.
% FOUNDING_PROBLEM_CORROBORATION: State bureaucrats and traditional authorities both attest the founding problem remains live—without the partition, fiscal administration and cultural practice would conflict daily. Modernization advocates and unified-practice movements attest the problem is illusory or that the partition solves it at unacceptable cost. International standards bodies attest the dual-partition state is a transitional anomaly. No external corroboration affirms the partition as the optimal equilibrium; the corroboration exists only within the partition's own benefiting parties.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, contested).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures the degree to which the partition extracts from practitioners the cost of maintaining dual systems. It rises from 0.42 to 0.58 over the first 25 years as the partition becomes institutionalized and enforcement machinery develops, then plateaus—the asymptotic extraction level reflects the stable state where extraction is normalized. Theater rises modestly (0.28 to 0.41) because the partition's coordination function is real (it does enable both state governance and cultural preservation) but performs increasing performative work as time passes: boundary maintenance requires more conscious enforcement as boundary-crossing pressures accumulate. Suppression tracks theater—enforcement must actively maintain the boundary because neither side would naturally converge to it without enforcement. The measurement grid is aligned across all three metrics at every time point.
 *
 * PERSPECTIVAL GAP:
 *   The state apparatus views the partition as a legitimate framework enabling governance without cultural hegemony (beneficiary frame: d ≈ 0.2). Traditional authorities view it as preserving their legitimacy within a constrained domain (beneficiary frame: d ≈ 0.3). Boundary-crossing practitioners experience it as a constraint they must navigate strategically (target frame: d ≈ 0.75). Practitioners outside the partition experience it as double subordination (target frame: d ≈ 0.95). The engine computes these divergent directionalities from the structural data; this reading does not adjudicate which perspective is 'correct'—it documents the partition's structure under the reading's own terms.
 *
 * DIRECTIONALITY LOGIC:
 *   The state and traditional authorities both benefit from the partition (it legitimates both without requiring one to subjugate the other), giving them lower d values. Boundary-crossing practitioners are targets because they internalize the cognitive cost of code-switching and the constraint prevents them from operating under unified rules. Practitioners outside the partition are highest-target because they have no legitimate voice in either domain and are subject to pressure from both. This directionality structure makes the constraint tangled_rope: genuine coordination at the beneficiary level (state and traditional authority solve the modernization dilemma together) combined with asymmetric extraction at the target level (practitioners bear the boundary-maintenance costs).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading denies mandatrophy: it asserts the partition persists not because the founding problem is forgotten but because the founding problem is live and the partition remains the optimal solution. Under the exogenous_override reading, the partition would appear as mandatrophy—the state's overriding mandate to achieve complete modernization persists while the partition prevents its execution, suggesting zombie constraint. Under the endogenous_displacement reading, the partition would appear as voluntary custom that could dissolve if practitioners chose to converge. This reading (dual_practice_equilibrium) holds that neither characterization applies—the partition is a Schelling point: neither side can unilaterally change it without imposing costs both find prohibitive, so persistence reflects structural equilibrium, not mandate decay or voluntary adherence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_stability_vs_convergence_attractor,
    'Is the dual-practice partition a stable equilibrium that would persist without external enforcement, or is it an unstable metastable state that would converge toward either unified modernization or complete restoration if enforcement relaxed?',
    'Natural experiment: jurisdictions where enforcement relaxes (minority autonomy granted, international standards pressure removed) and the trajectory of practice convergence is observed. If practitioners voluntarily converge toward either pole without external mandate, the partition is unstable. If they maintain dual practice without enforcement, it is stable.',
    'If the partition is stable without enforcement, it is a legitimate equilibrium (tangled_rope remains accurate). If it requires continuous enforcement to prevent convergence, it is extractive inertia (reclassify toward snare). If it collapses toward unified modernization (supporting the exogenous_override_reading), the partition was artificial. If it collapses toward restoration (supporting the endogenous_displacement_reading), the partition suppressed organic preference.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(partition_stability_vs_convergence_attractor, empirical, 'Whether the dual-practice partition is structurally stable or relies on external enforcement to prevent convergence.').

omega_variable(
    reading_contest_over_legitimacy_grounding,
    'What makes a practice legitimately governed by one authority versus another—is it domain-based (public vs. private), tradition-based (which practices were traditionally under which authority), consent-based (which authority the practitioners prefer), or efficiency-based (which authority can govern it more effectively)?',
    'Close analysis of boundary disputes: when practice jurisdiction is contested (e.g., should marriage law be state or traditional? Should agricultural timing be state-managed or traditional?), what principle do the competing authorities appeal to? Their appeals reveal what legitimacy ground each is actually asserting.',
    'Different legitimacy grounds support different readings. Domain-based partitioning supports this reading (dual_practice_equilibrium). Tradition-based supports the endogenous_displacement_reading (we should follow traditional patterns). Efficiency-based supports the exogenous_override_reading (state authority is more efficient). Consent-based undermines all authority-centered readings and suggests a third reading not yet authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_over_legitimacy_grounding, conceptual, 'The contested question of what principle legitimates authority partition itself.').

omega_variable(
    kernel_reading_alternative_framing,
    'Should this constraint be read as one reading of the legitimacy_of_practice_standardization kernel, or as a distinct constraint about domain-partitioned authority structures (which might have multiple kernels)?',
    'Test whether the three declared readings (dual_practice_equilibrium, endogenous_displacement, exogenous_override) all engage the SAME contested question—what makes practice change legitimate—or whether this reading engages a different question (when is authority partition itself legitimate). If the former, the kernel framing holds. If the latter, this reading should decompose into a separate kernel.',
    'If kept within the kernel, the reading''s foundational axiom is about legitimacy_of_change. If decomposed, the reading''s axiom is about legitimacy_of_partition. The distinction affects which sibling readings are true siblings versus lateral readings of a different kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framing, conceptual, 'Ambiguity in the reading''s kernel assignment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(legi_tr_t5, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(legi_tr_t15, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(legi_tr_t25, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(legi_be_t5, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(legi_be_t15, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(legi_be_t25, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(legi_su_t5, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(legi_su_t15, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 15, 0.59).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(legi_su_t25, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.12).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization__endogenous_displacement_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the legitimacy_of_practice_standardization kernel. The three readings diverge on what makes practice change legitimate: dual_practice_equilibrium asserts permanent domain partition as optimal; endogenous_displacement asserts voluntary adoption as legitimate; exogenous_override asserts state mandate as legitimate. Each reading instantiates a different ε (extraction level), different beneficiary/victim structure, and different terminal state. They are linked here, not merged into one story, to preserve the ε-invariance principle: reading the kernel through different legitimacy frameworks produces different constraints with different structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
