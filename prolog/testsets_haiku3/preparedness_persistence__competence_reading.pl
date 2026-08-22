% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__competence_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: preparedness_persistence__competence_reading
 *   human_readable: Preparedness as Live Exercised Competence
 *   domain: institutional/organizational
 *
 * SUMMARY:
 *   This is the competence_reading of the preparedness_persistence kernel:
 *   drills and inspections are live exercised knowledge that maintains
 *   genuine operational readiness. The constraint's structure is a Rope
 *   (coordination function with no extraction asymmetry). Response agencies
 *   coordinate collective action under extreme time pressure through
 *   rehearsal. Protected populations and personnel benefit from the
 *   coordination directly; there is no seat that captures extraction at the
 *   expense of others. The founding problem (skill decay without practice) is
 *   live and corroborated by independent organizational research. This
 *   reading contrasts with the husk_reading (drills are memorial performance
 *   with atrophied function) and the hybrid_reading (stratified competence
 *   where some components remain live while others ritualize). The
 *   competence_reading author asserts that, under conditions of sustained
 *   funding and genuine participation, drills maintain measurable competence
 *   across the preparedness system.
 *
 * KEY AGENTS:
 *   - Emergency response agencies — institutional agenda-setters, design and execute drills, invest operational time, maintain the coordination structure
 *   - Protected populations — powerless beneficiaries, depend on actual readiness, cannot directly shape drill frequency or quality
 *   - Political administrators — powerful payers who fund the system but may defer budget, face competing claims on resources
 *   - Response personnel — organized beneficiaries who gain competence, constrained from unilateral exit, carry institutional duty
 *   - Post-hoc evaluators — analytical observers who measure whether drill frequency predicts real-event performance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__competence_reading, 0.15).
domain_priors:suppression_score(preparedness_persistence__competence_reading, 0.08).
domain_priors:theater_ratio(preparedness_persistence__competence_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__competence_reading, rope).
narrative_ontology:human_readable(preparedness_persistence__competence_reading, "Preparedness as Live Exercised Competence").
narrative_ontology:topic_domain(preparedness_persistence__competence_reading, "institutional/organizational").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__competence_reading, '972fdefd-5de0-4f3d-ac6f-026fe8573ba1').
narrative_ontology:cs_kernel_codification('972fdefd-5de0-4f3d-ac6f-026fe8573ba1', distributed).
narrative_ontology:cs_authority_grounding('972fdefd-5de0-4f3d-ac6f-026fe8573ba1', expertise).
narrative_ontology:cs_interpretation_layer_present('972fdefd-5de0-4f3d-ac6f-026fe8573ba1').
narrative_ontology:cs_reading_relation('972fdefd-5de0-4f3d-ac6f-026fe8573ba1', preparedness_persistence__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('972fdefd-5de0-4f3d-ac6f-026fe8573ba1', preparedness_persistence__hybrid_reading, influences).
narrative_ontology:cs_axiom('972fdefd-5de0-4f3d-ac6f-026fe8573ba1', foundational, drills_maintain_operant_competence).
narrative_ontology:cs_axiom_status(drills_maintain_operant_competence, holdable).
narrative_ontology:cs_axiom_grounding('972fdefd-5de0-4f3d-ac6f-026fe8573ba1', drills_maintain_operant_competence, empirically_contingent).
narrative_ontology:cs_axiom('972fdefd-5de0-4f3d-ac6f-026fe8573ba1', foundational, skill_decay_without_practice_is_real).
narrative_ontology:cs_axiom_status(skill_decay_without_practice_is_real, holdable).
narrative_ontology:cs_axiom_grounding('972fdefd-5de0-4f3d-ac6f-026fe8573ba1', skill_decay_without_practice_is_real, empirically_contingent).
narrative_ontology:cs_reference_frame('972fdefd-5de0-4f3d-ac6f-026fe8573ba1', competence_sustaining_practice_cycle).
narrative_ontology:cs_drift_state('972fdefd-5de0-4f3d-ac6f-026fe8573ba1', contemporary_budget_pressure_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('972fdefd-5de0-4f3d-ac6f-026fe8573ba1', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__competence_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, emergency_response_agencies).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, protected_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, political_administrators).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, response_personnel).
narrative_ontology:constraint_victim(preparedness_persistence__competence_reading, political_administrators).
narrative_ontology:constraint_vindicates(preparedness_persistence__competence_reading, organizational_learning_through_practice).
narrative_ontology:constraint_vindicates(preparedness_persistence__competence_reading, skill_degradation_under_non_use).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, schedule, and execute preparedness drills and inspections. Invest personnel time and budget in live exercise cycles. Justify drills as essential competence maintenance: skills atrophy without practice, equipment fails without testing, team coordination requires rehearsal. Bear the operational cost of taking personnel offline for training.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, emergency_response_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Depend on the response agencies' actual readiness when disasters occur. Benefit from competence that drills maintain: trained responders, tested equipment, practiced protocols reduce mortality and injury. Have no direct control over drill frequency or quality; trust is the only available option.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, protected_populations, beneficiary,
    powerless, biographical, trapped, national).

% Fund preparedness operations but also face pressure to reallocate budget to visible services (hospitals, schools). Benefit from successful disaster response (political credit) but may reduce drill frequency during budget cycles. Can choose to sustain or defer funding without operational barrier.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, political_administrators, payer,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__competence_reading, political_administrators, beneficiary).

% Gain competence and confidence from realistic drills; reduced on-the-job improvisation lowers risk when real events occur. Invest time in training but retain skill, institutional reputation, and professional identity from validated readiness. Constrained exit: cannot unilaterally withdraw from institutional duty cycles.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, response_personnel, beneficiary,
    organized, biographical, constrained, national).

% Conduct after-action reviews and empirical studies of disaster response outcomes. Measure whether agencies that maintained higher drill frequency sustained better real-event performance. Do not participate in drill design but provide external accountability evidence.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, post_hoc_evaluators, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_persistence__competence_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_persistence__competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a shared competence baseline across a distributed emergency response system: drills synchronize understanding of protocols, test equipment interoperability, and keep personnel ready to execute coordinated action under extreme time pressure. Without rehearsal, each agency would improvise independently when disaster occurs, losing critical-path efficiency.
% TRANSFER_FUNCTION: Moves personnel time and administrative budget from routine operations into rehearsal cycles. The constraint transfers from protected populations and political administrators (who fund the system) into response agencies and personnel (who invest the time in preparation). The transfer is not asymmetric extraction — it funds the competence that benefits everyone.
% ABSENT_VOICES: Future disaster victims — those who will face actual events years hence — cannot testify about whether today's drills improve outcomes for them. The reading assumes preparedness accrues compound benefit over long time horizons; absent future testimony leaves this assumption uncontested but not corroborated from that seat.
% DISAPPEARANCE_RATIONALE: If the drill and inspection regime vanished, response agencies would lose operational readiness within months (skills decay, equipment condition unknown, team coordination untested). Real disaster response would become reactive improvisation; mortality and injury would rise substantially. The system depends structurally on continuous rehearsal.
% FOUNDING_PROBLEM: After major disasters revealed coordination failures and equipment failures in response operations, agencies concluded that competence requires practice: skills atrophy under non-use, equipment degrades unpredictably, and teams cannot coordinate effectively if they have never rehearsed together under realistic conditions.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by independent research in organizational learning, emergency response science, and post-disaster after-action reports from outside the response agencies themselves. Historical analysis of disasters shows measurable improvement in outcomes when responding organizations had maintained higher drill frequencies in preceding years. Professional standards from international emergency management bodies endorse practice-based competence maintenance as foundational.
narrative_ontology:disappearance_verdict(preparedness_persistence__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_persistence__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__competence_reading, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__competence_reading_tests).
:- end_tests(preparedness_persistence__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the constraint moves resources from administrators and populations into preparedness, and everyone measurably benefits: response agencies gain competence capital, personnel gain professional validation, administrators gain political credit from better disaster response, populations gain reduced mortality risk. There is no identifiable capturer or victim. Suppression is near-zero (0.08) because participation is mandated by professional duty, not coerced against preference — the agencies want their personnel trained. Theater is minimal (0.12) because drills are operationally targeted at real-world scenarios; some ceremonial elements exist (public reassurance drills) but the functional core is substantial. Accessibility_collapse is high (0.72) because alternatives to coordinated rehearsal (improvisation, reduced redundancy, no interoperability testing) are genuinely inferior under disasters — the constraint's logic is self-evident once the founding problem is accepted. Resistance is low (0.18) because the professional consensus supports drills; resistance arises mainly from budget pressure, not from denial of the competence principle itself. The measurement trajectory is stable because the competence reading assumes no dramatic shifts in the system's operational model over the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the response agencies' seat, drills are essential operational practice. From the political administrators' seat, drills are a budget item that competes with other visible services — this divergence is real but does not instantiate extraction, only difference in time horizon and cost-sensitivity. From the protected populations' seat, drills are invisible unless a disaster occurs; the benefit is deferred and probabilistic. The engine's per-seat computation should find no structural asymmetry: all seats benefit from competence, though at different time scales and visibility levels.
 *
 * DIRECTIONALITY LOGIC:
 *   Response agencies are structural beneficiaries (they build and control the competence regime, gain institutional reputation from readiness) — d near 0.2. Protected populations are beneficiaries (reduced disaster mortality) but powerless and trapped — d near 0.25, offset by their powerlessness and no exit. Political administrators are symmetric or slightly toward target (they pay but gain political credit) — d near 0.5. Response personnel are beneficiaries (skill, confidence, professional identity) but identity-locked (cannot exit without career cost) — d near 0.3, acknowledging the constraint on exit. The average directionality is symmetric or slightly beneficiary-skewed, consistent with a Rope claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (skill atrophy without practice) remains live and empirically demonstrated. Response agencies continue to endorse the competence principle. Political administrators continue to fund the regime, albeit sometimes inconsistently. There is no evidence that the constraint persists as ritual without function — it remains operationally motivated. However, a latent risk exists: if administrators systematically defer drill funding without corresponding reduction in operational expectations, the regime could degrade into the husk_reading (performance without competence) over 10–15 years. This story authors the current state under the competence_reading; the husk_reading is a separate constraint story modeling the degraded attractor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence_horizon,
    'Over what time scale does skill atrophy and equipment degradation become operationally critical if drills cease?',
    'Historical data from agencies that discontinued drills: time-to-measurable-performance-loss in post-discontinuation deployments. Organizational learning research on skill decay in high-reliability professions.',
    'If atrophy is rapid (months), the founding problem remains acutely live and drill frequency should be high; if slow (years), the constraint''s urgency is lower and lower-frequency cycles may suffice. Affects scheduling economics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence_horizon, empirical, 'How quickly operational competence decays without practice in emergency response systems.').

omega_variable(
    competence_measurability_and_visibility,
    'Is genuine operational competence empirically distinguishable from the appearance of competence in a well-executed drill?',
    'Comparative analysis: measure real-disaster response outcomes for agencies stratified by prior drill frequency and quality. Distinguish outcomes that predict from drill metrics versus outcomes that arise from structural factors (equipment, staffing, funding) independent of practice.',
    'If real-event performance is strongly coupled to drill frequency, the competence_reading is empirically supported; if decoupled, drills may provide reassurance (theater) without meaningfully improving outcomes, supporting the husk_reading. This determines whether the constraint is genuinely coordinative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competence_measurability_and_visibility, empirical, 'Whether drills produce measurable competence improvements or primarily provide reassurance.').

omega_variable(
    funding_sustainability_and_political_cycle,
    'Will political administrators maintain consistent drill funding across budget cycles, or will drills become first to defer in fiscal pressure?',
    'Historical budget data; interviews with administrators about prioritization logic; tracking of actual drill frequency over 10+ year intervals through policy cycles.',
    'Consistent funding sustains the competence_reading; intermittent defer-resume cycles risk drift toward husk_reading (personnel skills atrophy in low-activity years, making the high-activity drills increasingly theatrical). The reading''s viability depends on political commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_sustainability_and_political_cycle, preference, 'Whether political will sustains preparedness funding against competing demands.').

omega_variable(
    competence_vs_husk_reading_boundary,
    'This story claims the preparedness_persistence kernel instantiates as competence_reading (live exercised knowledge). The husk_reading claims the same kernel instantiates as memorial performance (atrophied competence). What observable facts would disambiguate them?',
    'The kernel is contested: both readings claim the same arrangement (drills and inspections) but differ on whether competence is maintained. The ambiguity cannot be resolved within a single reading''s framework — it is a property of the kernel itself. Resolution requires real-world disaster events that reveal whether response agencies operate with trained readiness (competence_reading) or improvise from degraded baselines (husk_reading). Alternatively, controlled comparison of agencies with sustained vs. deferred drill regimes in the same disaster context.',
    'If competence is confirmed live, this reading (Rope, low extraction) holds. If competence is confirmed atrophied, the husk_reading (Piton, inertial) holds. If competence is stratified, the hybrid_reading holds. The three readings are not empirical alternatives within a single framework — they are the alternative frameworks themselves. The kernel''s ambiguity is irreducible until external validation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competence_vs_husk_reading_boundary, conceptual, 'Irreducible ambiguity of the kernel: whether drills maintain competence (this reading) or persist as memorial performance (husk reading).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__competence_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__competence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t4, preparedness_persistence__competence_reading, theater_ratio, 4, 0.09).
narrative_ontology:measurement_basis(prep_tr_t4, observed).
narrative_ontology:measurement(prep_tr_t8, preparedness_persistence__competence_reading, theater_ratio, 8, 0.1).
narrative_ontology:measurement_basis(prep_tr_t8, observed).
narrative_ontology:measurement(prep_tr_t12, preparedness_persistence__competence_reading, theater_ratio, 12, 0.11).
narrative_ontology:measurement_basis(prep_tr_t12, observed).
narrative_ontology:measurement(prep_tr_t16, preparedness_persistence__competence_reading, theater_ratio, 16, 0.12).
narrative_ontology:measurement_basis(prep_tr_t16, observed).
narrative_ontology:measurement(prep_tr_t20, preparedness_persistence__competence_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(prep_tr_t20, observed).
narrative_ontology:measurement(prep_tr_t24, preparedness_persistence__competence_reading, theater_ratio, 24, 0.12).
narrative_ontology:measurement_basis(prep_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__competence_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t4, preparedness_persistence__competence_reading, base_extractiveness, 4, 0.13).
narrative_ontology:measurement_basis(prep_be_t4, observed).
narrative_ontology:measurement(prep_be_t8, preparedness_persistence__competence_reading, base_extractiveness, 8, 0.14).
narrative_ontology:measurement_basis(prep_be_t8, observed).
narrative_ontology:measurement(prep_be_t12, preparedness_persistence__competence_reading, base_extractiveness, 12, 0.15).
narrative_ontology:measurement_basis(prep_be_t12, observed).
narrative_ontology:measurement(prep_be_t16, preparedness_persistence__competence_reading, base_extractiveness, 16, 0.16).
narrative_ontology:measurement_basis(prep_be_t16, observed).
narrative_ontology:measurement(prep_be_t20, preparedness_persistence__competence_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement_basis(prep_be_t20, observed).
narrative_ontology:measurement(prep_be_t24, preparedness_persistence__competence_reading, base_extractiveness, 24, 0.15).
narrative_ontology:measurement_basis(prep_be_t24, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_persistence__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_persistence__competence_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, preparedness_persistence__husk_reading).
narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, preparedness_persistence__hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_persistence kernel decomposes into three distinct constraint stories modeling three readings of the practice-requirement claim. competence_reading (this file) asserts drills maintain operational readiness — Rope, low extraction, live founding problem. husk_reading asserts drills persist as memorial performance while competence atrophies — Piton, inertial structure. hybrid_reading asserts competence is stratified (some components live, others ritualized). Each reading has its own ε, beneficiary/victim structure, and type. The three are linked via this network array because they share a kernel and offer competing interpretations of the same institutional arrangement. The competence_reading influences the other two by establishing the coordinating logic that the husk and hybrid readings either degrade or stratify.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
