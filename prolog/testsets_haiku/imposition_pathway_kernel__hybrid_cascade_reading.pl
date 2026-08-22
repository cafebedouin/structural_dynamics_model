% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__hybrid_cascade_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__hybrid_cascade_reading, []).

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
 *   constraint_id: imposition_pathway_kernel__hybrid_cascade_reading
 *   human_readable: State-Mandated Fringe Adoption Cascade (Meiji Hybrid Pattern)
 *   domain: social/institutional
 *
 * SUMMARY:
 *   The hybrid cascade reading asserts that commitment displacement often
 *   proceeds via a two-stage process: (1) the state issues a top-down mandate
 *   requiring adoption by a controlled cohort (military, bureaucracy, elite
 *   functionaries), creating an artificial fringe of visible adopters, and
 *   (2) this manufactured fringe then becomes the legitimacy carrier for
 *   organic climb among the broader population, who adopt the commitment not
 *   from direct coercion but from seeing it already normalized by
 *   power-proximate peers. The Meiji state's mandatory adoption of Western
 *   dress, hairstyle, and institutional forms by military and civil officials
 *   exemplifies this pattern. The constraint is CLAIMED as tangled_rope (it
 *   coordinates modernization while extracting from traditional
 *   practitioners) and the metrics reflect both the genuine coordination
 *   problem (moving a society-wide commitment requires some mechanism) and
 *   the asymmetric cost distribution (the conscripted and coerced cohort
 *   pays; the elite benefits from organic climb without mandate). The sibling
 *   readings—endogenous_climb_reading and exogenous_override_reading—offer
 *   competing framings of the same historical events.
 *
 * KEY AGENTS:
 *   - State administrative apparatus — sets the mandate, administers enforcement, benefits from legitimacy signal
 *   - Conscripted military — forced adopters, carries visible fringe, bears identity transformation cost
 *   - Coerced state employees — mandatory adopters, identity-locked professionals, legitimacy carriers
 *   - Traditional practitioners — prior-commitment holders, face economic displacement, excluded from decision seat
 *   - Educated merchant class — organic climbers, see the fringe and follow, benefit without mandate cost
 *   - Foreign powers — external audience, interpreting the fringe as legitimacy signal
 *   - Analytical observer — frames the hybrid mechanism as distinct from pure endogenous climb or pure exogenous override
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__hybrid_cascade_reading, 0.68).
domain_priors:suppression_score(imposition_pathway_kernel__hybrid_cascade_reading, 0.72).
domain_priors:theater_ratio(imposition_pathway_kernel__hybrid_cascade_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__hybrid_cascade_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__hybrid_cascade_reading, "State-Mandated Fringe Adoption Cascade (Meiji Hybrid Pattern)").
narrative_ontology:topic_domain(imposition_pathway_kernel__hybrid_cascade_reading, "social/institutional").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__hybrid_cascade_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__hybrid_cascade_reading, '03da0c6f-c924-49f6-a8f8-cdcde91015e0').
narrative_ontology:cs_kernel_codification('03da0c6f-c924-49f6-a8f8-cdcde91015e0', fixed_text).
narrative_ontology:cs_authority_grounding('03da0c6f-c924-49f6-a8f8-cdcde91015e0', extraction).
narrative_ontology:cs_interpretation_layer_present('03da0c6f-c924-49f6-a8f8-cdcde91015e0').
narrative_ontology:cs_reading_relation('03da0c6f-c924-49f6-a8f8-cdcde91015e0', imposition_pathway_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('03da0c6f-c924-49f6-a8f8-cdcde91015e0', imposition_pathway_kernel__exogenous_override_reading, influences).
narrative_ontology:cs_axiom('03da0c6f-c924-49f6-a8f8-cdcde91015e0', foundational, mandate_creates_legitimate_fringe).
narrative_ontology:cs_axiom_status(mandate_creates_legitimate_fringe, holdable).
narrative_ontology:cs_axiom_grounding('03da0c6f-c924-49f6-a8f8-cdcde91015e0', mandate_creates_legitimate_fringe, empirically_contingent).
narrative_ontology:cs_axiom('03da0c6f-c924-49f6-a8f8-cdcde91015e0', secondary, fringe_visibility_enables_organic_climb).
narrative_ontology:cs_axiom_status(fringe_visibility_enables_organic_climb, holdable).
narrative_ontology:cs_axiom_grounding('03da0c6f-c924-49f6-a8f8-cdcde91015e0', fringe_visibility_enables_organic_climb, empirically_contingent).
narrative_ontology:cs_reference_frame('03da0c6f-c924-49f6-a8f8-cdcde91015e0', unified_state_commitment_alignment).
narrative_ontology:cs_drift_state('03da0c6f-c924-49f6-a8f8-cdcde91015e0', post_initial_cascade_completion, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('03da0c6f-c924-49f6-a8f8-cdcde91015e0', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, state_administrative_apparatus).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, modernizing_elite).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, conscripted_military).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, coerced_state_employees).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, traditional_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, educated_merchant_class).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, educated_merchant_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the decree mandating commitment adoption by military and bureaucracy. Controls enforcement (administrative sanctions, salary loss, dismissal for non-compliance). Benefits from the legitimacy signal: foreign powers interpret the visible fringe as state modernization; internal populations see the fringe as proof the new commitment is viable and state-backed. Administers the interpretation layer that translates the mandate into normalization as organic climb proceeds.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, state_administrative_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Conscripted into state service under mandatory commitment adoption as a condition of duty. Cannot exit military service (desertion incurs severe penalty; lack of alternative livelihood). Bears the direct cost of abandoning traditional identity markers (hair, dress, ritual observance). Their mandated adoption is visible and public; they become the artificial fringe that legitimates organic climb in the broader population. The constraint extracts identity transformation in exchange for survival and subsistence.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, conscripted_military, payer,
    powerless, biographical, trapped, national).

% Civil administrators, teachers, tax collectors, and functionaries for whom commitment adoption is a condition of employment. Possess more formal exit capacity than military (can seek alternative work) but professional identity is fused with state service; exit means loss of status, income, and professional standing. Bear the cost of visible commitment transformation. Their adoption is less visible than military but more respectable; they serve as the intermediate legitimacy vector between the military fringe and the educated merchant climb.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, coerced_state_employees, payer,
    moderate, biographical, identity_locked, national).

% Monks, priests, hereditary craftspeople, traditional artists, and ritual specialists whose livelihood and identity depend on the prior commitment (e.g., Shinto priests maintaining traditional ritual, artisans producing traditional dress, performers of traditional theater). Face economic displacement as state patronage shifts to adopters of the new commitment. Their resistance is real (they argue against the mandate publicly, some refuse adoption, some emigrate) but constrained by lack of institutional power. The artificial fringe crowds out their audience and patronage.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, traditional_practitioners, payer,
    moderate, generational, constrained, national).

% Merchants, traders, and educated professionals who see the mandated military and bureaucratic fringe adopt the new commitment, then organically adopt it themselves without explicit decree. Possess significant choice: they could resist adoption and maintain traditional identity (exit option exists; no legal penalty), or adopt to signal alignment with state modernization and access state patronage and foreign trade networks. They benefit from the fringe legitimacy signal: 'if the military and bureaucracy adopt it, it is not alien, it is modern and viable.' They climb without bearing the mandate cost that the conscripted and coerced bear. Adopt to collect status and resources.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, educated_merchant_class, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__hybrid_cascade_reading, educated_merchant_class, payer).

% British, German, French, and other Western powers observe the state's commitment transformation (adoption of Western dress, hairstyle, institutional forms by military and bureaucracy). Interpret the visible fringe as legitimacy signal that the state is modernizing and adopting 'civilized' (Western-aligned) practices. The signal influences treaty negotiations, trade agreements, and diplomatic standing. They are the external audience the state's manufactured fringe addresses; they would not treat the commitment transformation as legitimate without the visible fringe proof. Not a decision-making seat in the domestic constraint; they are the external validator the state manages through the fringe strategy.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, foreign_powers, excluded,
    powerful, generational, arbitrage, global).

% Examines the historical record to determine whether commitment displacement occurred through pure organic climb (the endogenous_climb_reading), through state override without fringe mediation (the exogenous_override_reading), or through the hybrid pathway: state mandate creates artificial fringe, organic climb follows (the hybrid_cascade_reading, instantiated here). Compares rates of adoption, distribution of early adopters, timing of fringe visibility, and state enforcement intensity across different readings. Does not participate in the constraint; observes its mechanism.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_pathway_kernel__hybrid_cascade_reading, state_administrative_apparatus).
narrative_ontology:fixing_cost_class(imposition_pathway_kernel__hybrid_cascade_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of displacing one cultural commitment (dress, ritual, institutional form) with another across a population without fragmenting the state into resistant and compliant factions. The mandate creates a visible fringe that signals the new commitment's legitimacy and alignment with state power; organic climb follows because the signal has been established. Without the mandate, climb would be slower and more contested; without organic climb, the mandate would require perpetual suppression.
% TRANSFER_FUNCTION: Transfers cultural authority and economic patronage from traditional practitioners (who depend on the prior commitment) to those aligned with the new commitment (state-backed professionals, merchants, modernizing elite). Moves status and livelihood from hereditary keepers of the prior tradition to professionals and functionaries of the new institution. The conscripted and coerced cohort bears the direct cost of identity transformation; their forced adoption becomes the legitimacy carrier that the elite exploit for organic climb without bearing the mandate cost themselves.
% ABSENT_VOICES: Traditional practitioners whose livelihoods depend on the prior commitment and whose cultural authority is delegitimized by the state mandate. They would argue that commitment displacement should be organic, not state-mandated; that the artificial fringe undermines the legitimacy of the new commitment; and that cultural pluralism is preferable to state-enforced unification. These voices are structurally excluded from the decision seat because the state monopolizes the authority to issue the mandate. Some may be consulted (religious figures, cultural elders) but their dissent is not allowed to veto the override.
% DISAPPEARANCE_RATIONALE: If the state mandate disappeared—if military conscription were made voluntary, if bureaucratic adoption were optional—the constraint would halt at the cascade stage where organic climb remains incomplete. The visible fringe would contract, and the population would revert toward a contested plural equilibrium with both commitments present. The new commitment would not achieve the degree of unification that the mandate enabled. Foreign powers would interpret the reversion as loss of state modernization credibility. The state apparatus would lose the legitimacy mechanism the fringe provided.
% FOUNDING_PROBLEM: How to displace an embedded cultural commitment (one that constitutes identity, sustains livelihoods, and carries tradition) with another commitment across an entire population, when organic climb alone is too slow to deliver the external legitimacy signal the state requires for international standing and when outright coercion of the entire population is politically or economically unfeasible?
% FOUNDING_PROBLEM_CORROBORATION: Japanese historical scholarship (Gluck on 'Japan's Modern Myths'; Harootunian on 'Overcome by Modernity') documents the state's explicit strategy of mandating Western dress, hairstyle, and institutional forms among military and bureaucracy to create visible legitimacy for organic climb among merchants and educated classes. Imperial rescripts and Meiji bureaucratic records attest the founding problem directly. The constraint's mechanism (mandate → fringe → climb) is confirmed by contemporary foreign observers (diplomatic records from Britain, Germany, France) noting how the military and bureaucratic fringe became the legitimacy vector for broader adoption. The founding problem remains live: any state modernization or commitment displacement scenario where external legitimacy matters and organic climb is too slow must solve the same problem the Meiji state solved.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__hybrid_cascade_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__hybrid_cascade_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__hybrid_cascade_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_pathway_kernel__hybrid_cascade_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__hybrid_cascade_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__hybrid_cascade_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_pathway_kernel__hybrid_cascade_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_pathway_kernel__hybrid_cascade_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises steeply in the first half (0.42 to 0.63) as the mandate is enforced and the fringe becomes visible, then plateaus in the second half (0.63 to 0.68) as organic climb completes and the constraint transitions from active mandate-enforcement to normalization. Suppression requirement falls symmetrically: high suppression (0.85) is needed initially to hold the mandated cohort in compliance against resistance; it declines (0.82 to 0.72) as organic climb reduces the proportion of the population in open resistance and as the constraint's legitimacy stabilizes. Theater ratio rises and plateaus: the constraint begins with genuine enforcement (forcing adoption) but as organic climb proceeds, an increasing share of the visible adoption is self-directed compliance rather than coerced conformity—the performance of modernization becomes internalized. The measurements are on a single shared grid: every metric is authored at every time point from t=0 to t=40, enabling temporal coupling analysis. The constraint is claimed as tangled_rope, not snare, because the coordination function is genuine (society-wide commitment displacement does require some mechanism to avoid fragmentation), but the extraction is real (the burden falls asymmetrically on those conscripted and coerced, while the elite benefits from the climb without mandate).
 *
 * PERSPECTIVAL GAP:
 *   The state apparatus and modernizing elite experience the constraint as genuine coordination and necessary state capacity. The conscripted military and coerced state employees experience it as coercive mandate and identity dissolution. The educated merchant class—who adopt organically—experience it as legitimate climb toward alignment with modernization, not as coercion (they had choice; they selected adoption). The traditional practitioners experience it as economic displacement and cultural delegitimation. The engine computes directionality for each seat from power, exit_options, and the beneficiary/victim structure: state apparatus and elite sit near the beneficiary end (low d), conscripted and coerced sit near the target end (high d), merchants sit near symmetric (they chose to adopt, so coordination benefit exceeds cost), traditional practitioners sit at the payer end (high d, constrained exit, economic cost). These divergent d values produce seat-specific type computations: the state's seat may compute as rope (coordination-heavy), the conscripted seat as snare (extraction-heavy), the merchant seat as rope (genuine coordination benefit from moving to a unified commitment).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the state administrative apparatus (collects legitimacy benefit, controls the override machinery, presides over the cascade) and the modernizing elite (benefits from organic climb without mandate burden, gains status from adopting early). Victims are the conscripted military (trapped, mandatory adopters, identity-locked), coerced state employees (identity-locked professionals, cannot exit without status loss), and traditional practitioners (constrained, economically displaced by the shift in patronage). The educated merchant class, though they adopt, do not appear as victims because their exit options are mobile and their adoption is chosen, not mandated; they are coordinated by the artificial fringe, not extracted from. The directionality derivation chain runs: beneficiaries and victims declare the asymmetry; power and exit_options modulate d; the engine derives effective directionality for each seat. No overrides are necessary here: the structural data is sufficient.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to displace one commitment with another across a population when prior commitment is embedded in identity and tradition) remains live throughout the interval. The constraint does not resolve the founding problem; it manages it by using state power to front-load adoption into a visible fringe, then riding organic climb to complete the displacement. No mandatrophy is present: the constraint's persistence depends on the state's continued enforcement and on the legitimacy of the fringe strategy. The rising theater_ratio and falling suppression_requirement indicate that, as organic climb completes, the constraint transitions from active mandate to normalization—but this is not mandatrophy (atrophy of the original function). The original function (displacing the commitment while avoiding fragmentation) is achieved through the cascade; the constraint remains functional, not theatrical-without-function. If the original commitment re-emerged and threatened the unified state, the override machinery would reactivate, showing the constraint's functional readiness rather than atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fringe_artificiality_vs_organic_climb_continuum,
    'Is the state-mandated fringe (conscripted military, coerced bureaucrats) genuinely ''artificial'' (imposed without pre-existing demand) or does it amplify a pre-existing organic climb already underway in the broader population?',
    'Comparative historical analysis: examine equivalent commitment transitions in contexts where the state had no mandate power, and measure the climb rate and fringe visibility in the absence of top-down machinery. If climb rates are substantially lower and fringe visibility is diffuse, the state mandate artificially accelerates and concentrates the climb. If climb rates and patterns are similar, the mandate amplifies existing organic momentum rather than creating artificiality.',
    'If the fringe is genuinely artificial (created by mandate, not by organic demand), the constraint is a state-manufactured legitimacy vector: the state manufactures the fringe to justify the climb. If the fringe amplifies organic climb, the state is accelerating an endogenous process, and the constraint dissolves partway toward the endogenous_climb_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fringe_artificiality_vs_organic_climb_continuum, empirical, 'Whether the state-mandated fringe is structurally distinct from organic climb or an amplification of existing organic momentum.').

omega_variable(
    sibling_reading_foreclosure_ambiguity,
    'Does the hybrid_cascade_reading foreclose the endogenous_climb_reading (all displacement is organic climb), or do both readings describe the same historical process viewed from different analytical heights?',
    'Philosophical analysis of the competing readings'' core claims. If endogenous_climb_reading asserts ''mandates are always compressed climbs,'' while hybrid_cascade_reading asserts ''mandates create artificial fringes that legitimize organic climb,'' the readings may be foreclosing (mutually exclusive premises) or coexisting (different emphasis, same underlying process viewed from different levels).',
    'If the readings foreclose each other, one is structurally correct and the other is misattribution of mechanism. If they coexist (endogenous climb is the underlying universal mechanism, but the state''s mandate creates an artificial fringe that accelerates visible climb), then they are describing the same process at different granularities, and the hybrid reading is a refinement, not a replacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_ambiguity, conceptual, 'Whether the hybrid_cascade_reading forecloses the endogenous_climb_reading or describes a coexisting mechanism at higher analytical resolution.').

omega_variable(
    elite_class_directionality_ambiguity,
    'The educated merchant class adopts the commitment organically after seeing the mandated fringe. Are they beneficiaries of the constraint (the fringe legitimacy signal enables their climb), or are they coordinated participants (they solve a coordination problem by aligning with the state-backed commitment)?',
    'Examine the merchants'' counterfactual exit options: if they had not adopted the new commitment, would they have faced coercion (making them victims), or would adoption have been purely instrumental choice (making them beneficiaries)? If adoption was a means to access state patronage or foreign trade advantages, they are beneficiaries (the constraint enables them to collect status/resources). If adoption was necessary for membership in the modernized merchant class, they are coordinated (the constraint solves their collective-action problem).',
    'If merchants are beneficiaries, the constraint''s extraction is concentrated on the conscripted/coerced seats. If merchants are coordinated participants, the extraction is more diffuse and the constraint tilts toward rope rather than tangled_rope. The directionality computation already assigns merchants d ≈ 0.4-0.5 (symmetric, mobile exit); this omega clarifies whether they should sit higher (beneficiary end, collecting from the constraint) or stay symmetric (coordinated with the constraint).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_class_directionality_ambiguity, conceptual, 'Whether the organic-climbing merchant class are beneficiaries of the constraint or coordinated participants solving a collective-action problem.').

omega_variable(
    reading_distribution_across_kernels,
    'Is the hybrid_cascade_reading unique to commitment-displacement contexts, or does it generalize to any institutional transformation where a state enforces adoption by a cohort, creating visible legitimacy for broader organic change?',
    'Comparative institutional analysis across domains (dress codes, language policy, religious conversion, professional licensing, legal system adoption) examining whether the pattern ''mandate creates visible fringe → organic climb follows'' is a stable mechanism or specific to cultural commitments.',
    'If the mechanism is domain-general, the constraint is a generalizable institutional form with application to understanding state modernization, decolonization, and institutional reform. If it is specific to cultural commitments, the constraint''s scope is narrower and the sibling readings may not generalize to other transformation contexts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_distribution_across_kernels, conceptual, 'Whether the hybrid_cascade mechanism is specific to commitment displacement or generalizes to institutional transformation more broadly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__hybrid_cascade_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(impo_tr_t5, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement(impo_tr_t10, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 10, 0.29).
narrative_ontology:measurement(impo_tr_t15, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement(impo_tr_t20, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement(impo_tr_t25, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(impo_tr_t30, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(impo_tr_t40, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(impo_be_t5, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(impo_be_t10, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(impo_be_t15, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement(impo_be_t20, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(impo_be_t25, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(impo_be_t30, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement(impo_be_t40, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(impo_su_t5, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 5, 0.82).
narrative_ontology:measurement(impo_su_t10, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(impo_su_t15, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(impo_su_t20, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 20, 0.73).
narrative_ontology:measurement(impo_su_t25, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(impo_su_t30, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(impo_su_t40, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__hybrid_cascade_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(imposition_pathway_kernel__hybrid_cascade_reading, 0.1).
narrative_ontology:affects_constraint(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% The imposition_pathway_kernel has three structurally distinct readings: endogenous_climb_reading (all displacement is organic fringe adoption; mandate is narrative artifact), exogenous_override_reading (state directly displaces commitment via override without fringe mediation), and hybrid_cascade_reading (this constraint—state mandate creates artificial fringe, which legitimates organic climb). The readings are linked by coexistence in the kernel; all three represent live positions in the historical and sociological literature on state formation and cultural displacement. Each reading has its own ε value, its own beneficiary/victim structure, and its own computed type. The M-set classification framework assigns each reading to the cell that captures its core mechanism: hybrid_cascade is tangled_rope (both coordination function and asymmetric extraction); endogenous_climb would be rope (pure coordination); exogenous_override would be snare or tangled_rope depending on whether the override carries genuine coordination benefit or is pure extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
