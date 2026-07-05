% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__near_miss_as_bridge
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__near_miss_as_bridge, []).

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
 *   constraint_id: competence_retention_exercise__near_miss_as_bridge
 *   human_readable: Near-Miss Investigation as the Bridge Between Simulation and Catastrophe
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This story instantiates the near_miss_as_bridge reading of the
 *   competence_retention_exercise kernel: the claim that near-miss incidents
 *   and minor failures, actively investigated and fed into simulator updates,
 *   are sufficient to maintain and validate operational competence without
 *   requiring either full catastrophes (the catastrophe_as_necessary sibling)
 *   or simulation alone (the simulation_as_sufficient sibling). The reading
 *   is a hybrid position — it treats simulators as necessary for routine
 *   skill preservation but insists they must be continuously informed by
 *   real-world near-miss data to avoid drifting away from actual risk. This
 *   is authored as its own constraint with its own ε, distinct from and not
 *   averaged with the sibling readings, which are separate stories.
 *
 * KEY AGENTS:
 *   - safety_engineering_departments: agenda_setter (institutional/constrained) — designs and defends the hybrid pipeline
 *   - incident_reporting_workers: payer (powerless/trapped) — bear personal risk disclosing their own errors
 *   - frontline_operators: beneficiary/payer (moderate/constrained) — trained on updated simulators, also must self-report
 *   - regulatory_bodies: beneficiary/observer (institutional/analytical) — use reporting rates as evidence of self-correction
 *   - the_traveling_public: beneficiary (powerless/trapped) — benefits invisibly, has no seat at the table
 *   - simulator_vendors: excluded (organized/mobile) — commercial interest in the rival simulation_as_sufficient framing
 *   - safety_researchers: observer (analytical/global) — assess whether the pipeline tracks real catastrophic causal structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__near_miss_as_bridge, 0.28).
domain_priors:suppression_score(competence_retention_exercise__near_miss_as_bridge, 0.22).
domain_priors:theater_ratio(competence_retention_exercise__near_miss_as_bridge, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, extractiveness, 0.28).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__near_miss_as_bridge, rope).
narrative_ontology:human_readable(competence_retention_exercise__near_miss_as_bridge, "Near-Miss Investigation as the Bridge Between Simulation and Catastrophe").
narrative_ontology:topic_domain(competence_retention_exercise__near_miss_as_bridge, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_retention_exercise__near_miss_as_bridge).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__near_miss_as_bridge, '0a3a009e-ca46-4de0-a174-06bcd940394c').
narrative_ontology:cs_kernel_codification('0a3a009e-ca46-4de0-a174-06bcd940394c', distributed).
narrative_ontology:cs_authority_grounding('0a3a009e-ca46-4de0-a174-06bcd940394c', practice).
narrative_ontology:cs_interpretation_layer_present('0a3a009e-ca46-4de0-a174-06bcd940394c').
narrative_ontology:cs_reading_relation('0a3a009e-ca46-4de0-a174-06bcd940394c', competence_retention_exercise__simulation_as_sufficient, influences).
narrative_ontology:cs_reading_relation('0a3a009e-ca46-4de0-a174-06bcd940394c', competence_retention_exercise__catastrophe_as_necessary, influences).
narrative_ontology:cs_axiom('0a3a009e-ca46-4de0-a174-06bcd940394c', foundational, near_miss_data_is_causally_representative).
narrative_ontology:cs_axiom_status(near_miss_data_is_causally_representative, holdable).
narrative_ontology:cs_axiom_grounding('0a3a009e-ca46-4de0-a174-06bcd940394c', near_miss_data_is_causally_representative, empirically_contingent).
narrative_ontology:cs_axiom('0a3a009e-ca46-4de0-a174-06bcd940394c', foundational, simulation_requires_continuous_real_world_recalibration).
narrative_ontology:cs_axiom_status(simulation_requires_continuous_real_world_recalibration, holdable).
narrative_ontology:cs_axiom_grounding('0a3a009e-ca46-4de0-a174-06bcd940394c', simulation_requires_continuous_real_world_recalibration, instrumental).
narrative_ontology:cs_reference_frame('0a3a009e-ca46-4de0-a174-06bcd940394c', hybrid_feedback_competence_model).
narrative_ontology:cs_drift_state('0a3a009e-ca46-4de0-a174-06bcd940394c', contemporary_safety_culture_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('0a3a009e-ca46-4de0-a174-06bcd940394c', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, frontline_operators).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, safety_engineering_departments).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, regulatory_bodies).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, the_traveling_public).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, incident_reporting_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, frontline_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers the near-miss reporting and investigation pipeline, decides which incidents feed simulator scenario updates, and defends the hybrid model against both pure-simulation budget cuts and pressure to wait for 'real data' from actual disasters. Their institutional legitimacy depends on demonstrating the pipeline actually improves outcomes.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, safety_engineering_departments, agenda_setter,
    institutional, generational, constrained, national).

% Train on simulators whose scenarios are continuously updated from near-miss data, giving them exposure to realistic failure modes without personally living through a catastrophe. They also bear the cost of reporting their own errors and close calls, which requires trusting a non-punitive reporting culture that isn't always reliably delivered.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, frontline_operators, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__near_miss_as_bridge, frontline_operators, payer).

% The individuals who actually experienced or caused a near-miss and must disclose it, often against organizational or peer incentives to minimize, hide, or reframe the event. They carry personal, reputational, and sometimes legal risk for supplying the raw material the entire system depends on, with uneven protection from retaliation depending on the jurisdiction and employer.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, incident_reporting_workers, payer,
    powerless, immediate, trapped, local).

% Rely on near-miss reporting rates and simulator-update cycles as evidence that an industry is self-correcting without needing to wait for fatal accidents to mandate reform. Their credibility depends on the pipeline being real rather than a paperwork exercise.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, regulatory_bodies, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__near_miss_as_bridge, regulatory_bodies, observer).

% Never sees the pipeline directly but is the ultimate beneficiary of competence maintained through near-miss-informed training rather than through learning exclusively from disasters that would otherwise have to happen to them first. Has no seat in how the reporting culture is designed or funded.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, the_traveling_public, beneficiary,
    powerless, immediate, trapped, national).

% Build and sell the simulator platforms and have a commercial interest in simulation being framed as self-sufficient (reducing their obligation to continuously integrate costly incident-derived updates). They are not formally part of the near-miss investigation process and can lobby for the simulation_as_sufficient framing without being accountable to the reporting culture they'd rather not depend on.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, simulator_vendors, excluded,
    organized, biographical, mobile, national).

% Study whether near-miss-informed simulator updates actually track the causal structure of eventual catastrophes, or whether organizations cherry-pick comfortable near-misses while systemic risks accumulate unreported. They publish comparative safety-record analyses across industries with different reporting cultures.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, safety_researchers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a continuous feedback loop where minor failures and near-misses are investigated, generalized, and fed into simulator scenario design, so that competence in catastrophe-avoidance is maintained and updated without requiring an actual catastrophe to occur first.
% TRANSFER_FUNCTION: Moves the burden of producing organizational learning material from the abstract 'the next disaster' onto the specific workers who must disclose their own errors and close calls, in exchange for distributing the resulting safety improvement broadly across operators, the public, and the institution's regulatory legitimacy.
% ABSENT_VOICES: Individual incident reporters who fear retaliation or reputational damage are structurally underrepresented in shaping how the reporting culture is designed — they supply the raw material but rarely sit on the committees that decide how their disclosures will be used, credited, or protected. Simulator vendors are also excluded from the investigation loop despite having a stake in whether it is judged necessary.
% DISAPPEARANCE_RATIONALE: If near-miss investigation and integration vanished overnight, simulator scenarios would stagnate on whatever failure modes were known at the time of last major update, regulatory bodies would lose their principal non-catastrophic evidence of industry self-correction, and organizations would face pressure to either wait for actual disasters to justify training updates or fabricate confidence in simulation alone — both worse equilibria than the hybrid.
% FOUNDING_PROBLEM: High-reliability industries needed a way to keep operator competence current against evolving real-world failure modes without waiting for catastrophic events to supply the lessons, and without over-trusting simulators that might drift away from actual operational risk.
% FOUNDING_PROBLEM_CORROBORATION: Independent safety researchers and comparative-industry accident investigators (outside the safety engineering departments that administer the pipeline) corroborate that industries with active near-miss reporting cultures show measurably better long-run safety records than those relying on simulation alone or on catastrophe-driven reform, though they also note reporting completeness varies widely and is not independently audited in most jurisdictions.
narrative_ontology:disappearance_verdict(competence_retention_exercise__near_miss_as_bridge, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__near_miss_as_bridge, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__near_miss_as_bridge, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_retention_exercise__near_miss_as_bridge, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__near_miss_as_bridge, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__near_miss_as_bridge_tests).
:- end_tests(competence_retention_exercise__near_miss_as_bridge_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is modest (0.28 at interval end) because the core function — using minor failures to update training — genuinely improves outcomes for nearly everyone downstream; the extraction that exists falls narrowly on incident reporters who bear disclosure risk without proportionate protection or credit. Suppression is low (0.22) because reporting is generally voluntary and protected by non-punitive culture policies where those policies are honored, though enforcement of those protections is uneven. Theater ratio (0.30) reflects a real but imperfect risk: some near-miss reporting programs become box-checking exercises where reports are logged but not meaningfully integrated into simulator redesign, and this proportion has crept up over the measured interval as reporting volume outpaces investigative capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   Safety engineering departments and regulatory bodies sit near the beneficiary end: they administer and derive legitimacy from the pipeline. Frontline operators and the traveling public are largely beneficiaries with diffuse, structural exposure. Incident reporting workers are the clearest target: they supply the raw material (their own errors and close calls) and carry concentrated personal risk, while the benefit of better-trained colleagues and safer systems is spread across the whole population — a classic asymmetry between who bears the disclosure cost and who captures the systemic benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading resists two mandatrophic failure modes at once: (1) treating the constraint as pure Rope with no cost (ignoring that reporting workers bear real, under-compensated risk) and (2) treating any friction in the reporting pipeline as proof the whole approach is a Snare or theater. The classification should register genuine coordination function (better competence, avoided catastrophes) coexisting with a real, identifiable payer class whose burden is not symmetric with the beneficiary class — but the enforcement is not so coercive, nor the alternatives so collapsed, that this rises to tangled_rope; incident reporters, while bearing risk, are not trapped in the sense of having no recourse (whistleblower protections and union grievance channels exist in most of the mature safety-culture jurisdictions this reading describes, even if imperfectly enforced).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    near_miss_representativeness,
    'Do near-miss incidents actually sample the same causal space as full catastrophes, or do organizations selectively investigate the near-misses that are comfortable to investigate while systemic risks that would only surface in genuine catastrophe remain invisible to the pipeline?',
    'Retrospective analysis comparing pre-catastrophe near-miss records (where catastrophes did eventually occur despite an active near-miss program) against the causal chain of the catastrophe itself, across multiple industries and jurisdictions.',
    'If near-misses systematically fail to sample the rarer, more severe causal pathways, the near_miss_as_bridge reading overstates sufficiency and the catastrophe_as_necessary reading gains support for at least a residual class of risk; if near-misses do sample representatively, the hybrid reading is vindicated as sufficient without catastrophe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(near_miss_representativeness, empirical, 'Whether near-miss data structurally samples the same failure space as catastrophic events.').

omega_variable(
    reporting_culture_authenticity,
    'Is the near-miss reporting culture genuinely non-punitive in practice, or does the theater_ratio increase over time reflect reporting becoming a compliance ritual disconnected from actual simulator scenario redesign?',
    'Audit trail linking specific near-miss reports to specific simulator scenario changes, plus anonymous worker surveys on perceived retaliation risk, tracked longitudinally.',
    'If reporting has decoupled from actual scenario updates, the constraint drifts from rope toward piton (a formerly functional pipeline maintained mostly as institutional performance) even though the claimed_type remains rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reporting_culture_authenticity, empirical, 'Whether the reporting-to-simulator-update pipeline remains functionally connected or has become largely performative.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the near_miss_as_bridge reading''s claim of sufficiency break down — is there an identifiable class of catastrophic failure modes for which no amount of near-miss aggregation would have provided adequate warning, effectively requiring the catastrophe_as_necessary reading for that subset while near_miss_as_bridge holds for the rest?',
    'Cross-industry taxonomy of catastrophe causal chains, coded for whether precursor near-misses existed and were reportable in principle versus catastrophes with no plausible near-miss precursor (e.g., novel failure modes with no prior partial manifestation).',
    'If a nontrivial class of catastrophes has no near-miss precursor structure, this reading''s sufficiency claim is falsified for that subclass, sharpening the boundary with catastrophe_as_necessary rather than eliminating either reading; this is the specific structural disagreement the kernel readings are meant to locate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'The located disagreement between this reading and catastrophe_as_necessary: whether all catastrophic causal structure has near-miss precursors.').

omega_variable(
    simulator_fidelity_vs_incident_integration_tradeoff,
    'Is there a resource-allocation tradeoff between investing in higher-fidelity simulation (simulation_as_sufficient''s preferred investment) and investing in near-miss investigation capacity, such that organizations adopting this hybrid reading are structurally under-resourcing one or the other?',
    'Budget allocation studies across organizations claiming the hybrid approach, correlated with safety-outcome measures, to detect whether hybrid-claiming organizations actually under-invest in both relative to organizations that commit fully to one sibling reading.',
    'If hybrid organizations systematically under-resource both components relative to committed single-reading organizations, the near_miss_as_bridge reading may function as a rationalization for under-investment rather than a genuinely superior structural design.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulator_fidelity_vs_incident_integration_tradeoff, preference, 'Whether claiming the hybrid reading provides cover for under-investing in either simulation fidelity or incident investigation capacity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__near_miss_as_bridge, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0, 0.18).
narrative_ontology:measurement(comp_tr_t4, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 4, 0.2).
narrative_ontology:measurement(comp_tr_t8, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 8, 0.22).
narrative_ontology:measurement(comp_tr_t12, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 12, 0.24).
narrative_ontology:measurement(comp_tr_t16, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 16, 0.26).
narrative_ontology:measurement(comp_tr_t20, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 20, 0.28).
narrative_ontology:measurement(comp_tr_t24, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 24, 0.3).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(comp_be_t4, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 4, 0.22).
narrative_ontology:measurement(comp_be_t8, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 8, 0.24).
narrative_ontology:measurement(comp_be_t12, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 12, 0.25).
narrative_ontology:measurement(comp_be_t16, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 16, 0.26).
narrative_ontology:measurement(comp_be_t20, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 20, 0.27).
narrative_ontology:measurement(comp_be_t24, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 24, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(comp_su_t4, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 4, 0.16).
narrative_ontology:measurement(comp_su_t8, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 8, 0.18).
narrative_ontology:measurement(comp_su_t12, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 12, 0.19).
narrative_ontology:measurement(comp_su_t16, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 16, 0.2).
narrative_ontology:measurement(comp_su_t20, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 20, 0.21).
narrative_ontology:measurement(comp_su_t24, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 24, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__near_miss_as_bridge, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_retention_exercise__near_miss_as_bridge, 0.12).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, simulation_as_sufficient).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, catastrophe_as_necessary).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the competence_retention_exercise kernel. simulation_as_sufficient claims high-fidelity simulation alone is structurally equivalent to real events; catastrophe_as_necessary claims only actual catastrophic events provide adequate learning and stakes. This reading (near_miss_as_bridge) takes an intermediate position: simulators are necessary but insufficient alone, and must be continuously validated and updated using near-miss and minor-failure data, without requiring catastrophe. Each reading carries its own ε, its own beneficiary/victim structure, and its own claimed_type — they are not to be averaged or merged. The near_miss_as_bridge reading has the lowest extractiveness of the three because it distributes both the burden (routine reporting risk) and the benefit (continuously updated training) most broadly, whereas simulation_as_sufficient concentrates cost-avoidance benefit on vendors/institutions and catastrophe_as_necessary concentrates cost on populations who must experience the catastrophe.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
