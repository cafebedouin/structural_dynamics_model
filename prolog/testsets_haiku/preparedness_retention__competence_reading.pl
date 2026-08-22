% ============================================================================
% CONSTRAINT STORY: preparedness_retention__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__competence_reading, []).

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
 *   constraint_id: preparedness_retention__competence_reading
 *   human_readable: Preparedness as Live Competence Maintenance
 *   domain: disaster_preparedness/institutional_memory
 *
 * SUMMARY:
 *   In the competence reading of the preparedness-retention kernel, drills
 *   and inspections are live practices that maintain embodied, adaptive
 *   knowledge among emergency responders and institutional memory keepers.
 *   The constraint is claimed as Rope—genuine coordination solving a real
 *   collective-action problem (the gap between theoretical knowledge and
 *   practical competence under stress). The authored metrics reflect low but
 *   nonzero extraction (fiscal cost, responder time) and moderate
 *   ceremony-to-competence ratio (some drills have training value, some are
 *   theater). The reading asserts that preparation is only effective when it
 *   is actively practiced, repeatedly tested against friction, and
 *   transmitted across generations through shared enactment—not archive
 *   alone.
 *
 * KEY AGENTS:
 *   - emergency_responders: Organize and execute drills; gain directly from competence maintenance; constrained by professional mandate.
 *   - population_at_risk: Depend on live responder competence; powerless, trapped; benefit only when preparation is real.
 *   - institutional_memory_keepers: Maintain disaster-response knowledge across generations; organized; face budget/political pressure to reduce drills.
 *   - fiscal_efficiency_advocates: Budget controllers arguing for archive substitution; mobile exit option; experience preparedness as cost without visible benefit.
 *   - disaster_survivors: Validate preparation quality in real time (after failure); excluded from preparation conversation; powerless, trapped.
 *   - risk_modelers: Observe mismatch between modeled and actual competence; neutral analytical seat.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__competence_reading, 0.18).
domain_priors:suppression_score(preparedness_retention__competence_reading, 0.12).
domain_priors:theater_ratio(preparedness_retention__competence_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__competence_reading, rope).
narrative_ontology:human_readable(preparedness_retention__competence_reading, "Preparedness as Live Competence Maintenance").
narrative_ontology:topic_domain(preparedness_retention__competence_reading, "disaster_preparedness/institutional_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__competence_reading, '14eea61b-974f-4a05-a617-1f13c568b498').
narrative_ontology:cs_kernel_codification('14eea61b-974f-4a05-a617-1f13c568b498', distributed).
narrative_ontology:cs_authority_grounding('14eea61b-974f-4a05-a617-1f13c568b498', practice).
narrative_ontology:cs_interpretation_layer_present('14eea61b-974f-4a05-a617-1f13c568b498').
narrative_ontology:cs_reading_relation('14eea61b-974f-4a05-a617-1f13c568b498', preparedness_retention__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('14eea61b-974f-4a05-a617-1f13c568b498', preparedness_retention__hybrid_reading, influences).
narrative_ontology:cs_axiom('14eea61b-974f-4a05-a617-1f13c568b498', foundational, procedural_knowledge_irreplaceable).
narrative_ontology:cs_axiom_status(procedural_knowledge_irreplaceable, holdable).
narrative_ontology:cs_axiom_grounding('14eea61b-974f-4a05-a617-1f13c568b498', procedural_knowledge_irreplaceable, empirically_contingent).
narrative_ontology:cs_axiom('14eea61b-974f-4a05-a617-1f13c568b498', foundational, competence_maintained_through_enactment).
narrative_ontology:cs_axiom_status(competence_maintained_through_enactment, holdable).
narrative_ontology:cs_axiom_grounding('14eea61b-974f-4a05-a617-1f13c568b498', competence_maintained_through_enactment, empirically_contingent).
narrative_ontology:cs_reference_frame('14eea61b-974f-4a05-a617-1f13c568b498', embodied_competence_model).
narrative_ontology:cs_drift_state('14eea61b-974f-4a05-a617-1f13c568b498', contemporary_budget_pressure_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('14eea61b-974f-4a05-a617-1f13c568b498', '').
narrative_ontology:cs_kernel_id(preparedness_retention__competence_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, population_at_risk).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, emergency_responders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_retention__competence_reading, fiscal_efficiency_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Conduct and design drills and inspections to maintain live operational knowledge. They carry both responsibility and direct benefit: muscle memory, procedural fluency, and adaptive capacity are prerequisites for effective response. Professional identity is fused with preparedness mandate; exit would require abandoning career identity.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, emergency_responders, agenda_setter,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__competence_reading, emergency_responders, beneficiary).

% Depends on responders' live competence during actual disasters. Gains directly from real preparedness: faster response, better triage, adaptive decision-making under chaos. Cannot exit the exposure to hazard; must trust preparation is genuine, not ceremonial.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, population_at_risk, beneficiary,
    powerless, biographical, trapped, regional).

% Maintain and transmit disaster-response knowledge across generations. Competence is grounded in repeated practice with historical cases, feedback loops from past failures, sustained technical training. Constrained by budget pressure to reduce 'redundant' drills and political pressure to reduce 'false alarms'.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, institutional_memory_keepers, agenda_setter,
    organized, generational, constrained, national).

% Budget controllers and efficiency-focused administrators who argue that frequent drills are costly, that recorded knowledge could substitute for live practice, and that resources should be reallocated. They experience preparedness as a cost center without visible direct benefit until failure occurs.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, fiscal_efficiency_advocates, payer,
    institutional, biographical, mobile, national).

% Produce hazard forecasts and risk assessments that inform preparedness budgets and drill frequency. They observe whether actual response competence matches modeled expectations and can report divergence between preparation theory and observed performance.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, risk_modelers_and_forecasters, observer,
    moderate, generational, analytical, global).

% Experience the consequences of preparation quality in real time during disasters but are not in the preparation conversation. They have no voice in deciding drill frequency, responder training depth, or institutional memory transmission. Their testimony comes only after failure, as after-action evidence.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, disaster_survivors, excluded,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__competence_reading, population_at_risk).
narrative_ontology:fixing_cost_class(preparedness_retention__competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sustains a shared reality of operational hazard and procedural fluency across responder teams, generations, and institutional memory holders. Each drill creates intersubjective confidence—'we have done this, we can do it again'—and tests procedures against real-world friction. Institutional memory is preserved through repeated enactment, not archive alone.
% TRANSFER_FUNCTION: Moves time, resources, and cognitive load from responders and institutions to maintain competence; in return, moves probability of effective crisis response to the population at risk. Fiscal efficiency advocates experience this as a transfer of money away from other priorities.
% ABSENT_VOICES: Disaster survivors and at-risk populations who would validate (or refute) whether competence is real; citizens in low-hazard regions who bear tax cost for preparedness they may never use; competitors for the same budget (healthcare, education, social services) who argue preparedness is over-invested.
% DISAPPEARANCE_RATIONALE: If live drills and inspections ceased and were replaced by archival study alone, responder competence would degrade within 3–5 years (documented in military training research and emergency management literature). When the next disaster strikes, response times would lengthen, decision quality would drop, and casualty patterns would shift. The at-risk population would face different hazard exposure.
% FOUNDING_PROBLEM: Disaster response requires adaptive, real-time decision-making under incomplete information and chaotic conditions. Knowledge that sits in documents without live practice atrophies; responders trained in simulation alone fail under actual stress. The founding problem is the gap between theoretical knowledge and embodied competence.
% FOUNDING_PROBLEM_CORROBORATION: After-action reviews from Hurricane Katrina, the 2004 Indian Ocean tsunami, the 2011 Japan earthquake, and Dutch flood-response audits all document that institutions with sustained drilling recovered faster and made better decisions than those with documentation-only preparation. Cognitive science and motor-learning research establish that procedural memory requires repeated enactment. Testimony comes from responders, disaster researchers, and survivors—sources outside the preparedness establishment.
narrative_ontology:disappearance_verdict(preparedness_retention__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_retention__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__competence_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__competence_reading_tests).
:- end_tests(preparedness_retention__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint's coordination benefit (adaptive response competence) outweighs the resource transfer. The beneficiary structure is clear: responders and at-risk population both gain from real competence; fiscal efficiency advocates pay a cost they experience as unnecessary overhead. Suppression is minimal (0.12) because enforcement of drills is weak—they persist through professional culture and legal mandate, not coercion. Theater ratio is low-moderate (0.22): some drills are rote procedure, some contain genuine learning; the measurement series captures slight upward drift mid-interval (budget pressure rising, more ceremonies masking competence erosion) then stabilization as adaptive response by institutions. The measurement grid is shared across all three metrics at every time point (OQ-105 alignment rule).
 *
 * PERSPECTIVAL GAP:
 *   From the responder and at-risk population seats, this is genuine rope—coordination that solves an irreducible problem. From the fiscal efficiency advocate seat, the same constraint looks more extractive—resources flowing to overhead without justified return. The engine computes per-seat classifications; this perspectival gap is the structural divergence the story describes. The responder seat computes Rope; the efficiency seat computes Scaffold or Tangled Rope depending on whether they frame it as transitional or extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   Responders and memory keepers benefit directly from competence (d near 0.0); at-risk populations benefit indirectly but substantially (d near 0.1); fiscal efficiency advocates pay cost and question benefit (d near 0.6). The directionality divergence is structural: responders have identity-locked exit (professional identity fused with preparedness mandate) while efficiency advocates have mobile exit (can reallocate budgets). This drives different d values from the same constraint. No override needed; structural derivation captures it.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading explicitly rejects the mandatrophy hypothesis. The founding problem (gap between theory and embodied competence) is live, the constraint's function is real, and responders and at-risk populations both benefit. The alternative reading (husk_reading) would claim competence is ceremonial and mandatrophy has set in; this reading asserts that claim is empirically false. The measurement series shows minor theater drift (festivals, ceremonies multiplying) but the core drill function remains high-fidelity. If theater ratio rose above 0.6 and at-action-review reports documented competence loss, the verdict would shift toward husk_reading and mandatrophy would become credible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_measurement_ambiguity,
    'Can responder competence be reliably measured and validated independently of actual disasters?',
    'Longitudinal study tracking after-action review scores, simulation performance metrics, and actual response times across jurisdictions with different drill frequencies and intensities. Compare outcomes against disaster typologies (flash floods vs. slow-onset, familiar vs. novel hazards) to isolate drill contribution.',
    'If competence correlates tightly with drill frequency and actual response is measurably better in high-drill cohorts, the competence reading is supported. If correlation is weak, the husk reading gains credibility. If correlation is strong for specialized teams (water boards) but weak for general responders, the hybrid reading is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_measurement_ambiguity, empirical, 'Whether competence maintenance through drilling is demonstrably real or ceremonial.').

omega_variable(
    embodied_knowledge_irreplaceability,
    'Can procedural knowledge acquired through archive study (videos, documentation, simulation software) substitute for live drill experience in maintaining competence under actual stress?',
    'Randomized comparison: train two responder cohorts on the same hazard scenario—one via traditional drills, one via archive and simulation only. Test competence via high-fidelity disaster simulation with novel constraints (communication breakdown, resource scarcity, contradictory orders). Measure decision speed, error rate, adaptation quality.',
    'If archive+simulation cohorts match drill cohorts in performance, the two modes are functionally equivalent and the constraint''s extractiveness drops (archive substitution is cheaper). If drill cohorts outperform significantly, embodied knowledge is irreplaceable and the competence reading''s core claim is validated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(embodied_knowledge_irreplaceability, empirical, 'Whether live drilling is structurally necessary for competence maintenance or is one possible input among substitutable alternatives.').

omega_variable(
    sibling_reading_discrimination,
    'How would we distinguish empirically between the competence_reading and the husk_reading?',
    'Post-disaster comparison: after a significant event, audit responder decisions, error patterns, and adaptation quality against pre-disaster training records. If decisions reflect drilled procedures and error patterns are lower where drills were frequent, competence_reading is supported. If decisions are chaotic or reflect non-drilled improvisation even in high-drill jurisdictions, husk_reading is supported.',
    'The readings coexist until evidence resolves them. A major disaster with detailed post-action analysis (like the 2011 Japan earthquake or 2017 Atlantic hurricane season) can move confidence toward one reading or the other.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_discrimination, empirical, 'Empirical discrimination between the competence_reading (drills preserve real competence) and husk_reading (drills are theater, competence is mythical).').

omega_variable(
    budget_pressure_degradation,
    'Is the measured upward drift in theater_ratio (t=16 to t=24) a harbinger of competence degradation, or a natural oscillation within a stable system?',
    'Track drill frequency, responder training hours, and institutional memory transmission explicitly in the time series. If theater rise correlates with budget cuts and drill reductions, it is early-stage competence erosion. If theater rises while core competence metrics stay flat, the increase is performative but not yet damaging.',
    'If degradation is real, the constraint''s function is eroding and the hybrid_reading (stratified competence—specialized institutions retaining real knowledge while general preparedness becomes ceremonial) will eventually apply. If theater rise is decoupled from competence loss, the competence_reading remains robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(budget_pressure_degradation, empirical, 'Whether theater-ratio drift signals early-stage competence degradation or is noise around a stable equilibrium.').

omega_variable(
    identity_locked_responder_exit,
    'How much responder exit from the constraint is identity-locked (professional identity fused with preparedness mandate) versus structurally constrained (legal/employment barriers)?',
    'Survey responders on perceived exit options: can they imagine leaving the field without identity loss? Can they transfer to non-preparedness roles? Are there jurisdictions where responders have actually exited preparedness and what patterns of replacement and knowledge loss follow?',
    'If exit is primarily identity-locked, responders are bound to the constraint by their self-concept, not external force. This makes the constraint more robust to shocks (they persist from internal motivation) but more vulnerable to identity-changing events (burnout, moral injury, generational shift). If exit is primarily structural, the constraint depends on enforcement and is vulnerable to policy change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_responder_exit, empirical, 'Mechanism of responder commitment: identity fusion versus structural constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__competence_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(prep_tr_t8, preparedness_retention__competence_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(prep_tr_t16, preparedness_retention__competence_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(prep_tr_t24, preparedness_retention__competence_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(prep_tr_t32, preparedness_retention__competence_reading, theater_ratio, 32, 0.23).
narrative_ontology:measurement(prep_tr_t40, preparedness_retention__competence_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__competence_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(prep_be_t8, preparedness_retention__competence_reading, base_extractiveness, 8, 0.15).
narrative_ontology:measurement(prep_be_t16, preparedness_retention__competence_reading, base_extractiveness, 16, 0.17).
narrative_ontology:measurement(prep_be_t24, preparedness_retention__competence_reading, base_extractiveness, 24, 0.19).
narrative_ontology:measurement(prep_be_t32, preparedness_retention__competence_reading, base_extractiveness, 32, 0.18).
narrative_ontology:measurement(prep_be_t40, preparedness_retention__competence_reading, base_extractiveness, 40, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_retention__competence_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(prep_su_t8, preparedness_retention__competence_reading, suppression_requirement, 8, 0.09).
narrative_ontology:measurement(prep_su_t16, preparedness_retention__competence_reading, suppression_requirement, 16, 0.11).
narrative_ontology:measurement(prep_su_t24, preparedness_retention__competence_reading, suppression_requirement, 24, 0.13).
narrative_ontology:measurement(prep_su_t32, preparedness_retention__competence_reading, suppression_requirement, 32, 0.12).
narrative_ontology:measurement(prep_su_t40, preparedness_retention__competence_reading, suppression_requirement, 40, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_retention__competence_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__husk_reading).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_retention kernel decomposes into three distinct constraint readings. competence_reading asserts live competence is maintained through active drilling (low ε, Rope). husk_reading asserts drills are memorial theater and competence is mythical (high ε, Piton or Snare). hybrid_reading asserts competence is stratified—specialized institutions retain real knowledge while general preparedness becomes ceremonial (medium ε, Tangled Rope). Each reading has different ε, different beneficiary/victim structures, and different stakeholder power dynamics. The readings coexist held by different parties; empirical evidence from major disasters discriminates between them slowly. All three stories link via network.affects_constraints to model the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
