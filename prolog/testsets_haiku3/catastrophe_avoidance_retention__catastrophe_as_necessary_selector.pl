% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__catastrophe_as_necessary_selector, []).

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
 *   constraint_id: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
 *   human_readable: Catastrophe as Necessary Selector for Competence Maintenance
 *   domain: safety/organizational_learning/high_reliability
 *
 * SUMMARY:
 *   This reading asserts that in high-reliability systems, actual
 *   catastrophic events are structurally necessary to maintain competence and
 *   organizational readiness. During peacetime — long periods without
 *   incidents — personnel expertise erodes, training becomes ritualistic, and
 *   regulatory enforcement relaxes. Only when catastrophes occur does
 *   mortality salience, investigative pressure, and organizational trauma
 *   generate the selection pressure needed to restore real competence.
 *   Simulation creates the illusion of readiness without delivering genuine
 *   preparedness; the constraint extracts the cost of this illusion from both
 *   peacetime personnel (who must engage in theater) and from
 *   simulation-investing communities (whose infrastructure is rendered
 *   structurally obsolete by the constraint's logic). This is ONE reading of
 *   the contested 'catastrophe_avoidance_retention' kernel — a kernel about
 *   how organizations learn from and prepare for rare, catastrophic events.
 *   Sibling readings claim simulation and hybrid near-miss learning can
 *   substitute for actual catastrophes, or that neither catastrophe nor
 *   simulation alone is sufficient. This reading forecloses those
 *   alternatives by asserting catastrophe is uniquely necessary.
 *
 * KEY AGENTS:
 *   - peacetime_organizational_personnel: Front-line operators constrained to carry competence maintenance burden during stable periods; exit is career-terminal
 *   - catastrophe_survivors: Retroactively validated by survival; become credential-holders for next learning cycle
 *   - regulatory_authorities_post_incident: Set competence standards via post-incident investigation; their authority depends on incidents occurring
 *   - simulation_investment_communities: Corporate infrastructure providers whose business model is invalidated by the constraint's core claim
 *   - safety_engineering_discipline: Benefits from catastrophes as case studies and validation of methods
 *   - organizational_leadership: Excluded from the constraint's logic — their incentive is prevention, not readiness-via-selection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.67).
domain_priors:suppression_score(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.72).
domain_priors:theater_ratio(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, extractiveness, 0.67).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "Catastrophe as Necessary Selector for Competence Maintenance").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "safety/organizational_learning/high_reliability").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 'b08faea7-4d6c-42c5-8eef-73f731744b0f').
narrative_ontology:cs_kernel_codification('b08faea7-4d6c-42c5-8eef-73f731744b0f', distributed).
narrative_ontology:cs_authority_grounding('b08faea7-4d6c-42c5-8eef-73f731744b0f', practice).
narrative_ontology:cs_interpretation_layer_present('b08faea7-4d6c-42c5-8eef-73f731744b0f').
narrative_ontology:cs_reading_relation('b08faea7-4d6c-42c5-8eef-73f731744b0f', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, forecloses).
narrative_ontology:cs_reading_relation('b08faea7-4d6c-42c5-8eef-73f731744b0f', catastrophe_avoidance_retention__hybrid_near_miss_learning, coexists_with).
narrative_ontology:cs_axiom('b08faea7-4d6c-42c5-8eef-73f731744b0f', foundational, catastrophe_is_necessary_selector).
narrative_ontology:cs_axiom_status(catastrophe_is_necessary_selector, holdable).
narrative_ontology:cs_axiom_grounding('b08faea7-4d6c-42c5-8eef-73f731744b0f', catastrophe_is_necessary_selector, empirically_contingent).
narrative_ontology:cs_axiom('b08faea7-4d6c-42c5-8eef-73f731744b0f', foundational, simulation_creates_false_confidence).
narrative_ontology:cs_axiom_status(simulation_creates_false_confidence, holdable).
narrative_ontology:cs_axiom_grounding('b08faea7-4d6c-42c5-8eef-73f731744b0f', simulation_creates_false_confidence, empirically_contingent).
narrative_ontology:cs_reference_frame('b08faea7-4d6c-42c5-8eef-73f731744b0f', catastrophe_driven_competence_maintenance).
narrative_ontology:cs_drift_state('b08faea7-4d6c-42c5-8eef-73f731744b0f', simulation_investment_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b08faea7-4d6c-42c5-8eef-73f731744b0f', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_survivors).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, regulatory_authorities_post_incident).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, safety_engineering_discipline).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, peacetime_organizational_personnel).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_investment_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Front-line operators, maintenance crews, and staff in high-reliability systems (aviation, nuclear, maritime) who work under the constraint that their competence will erode during stable periods. They must repeatedly train on catastrophic scenarios they never encounter, and their expertise is continuously devalued by peacetime normalization. Exit means leaving the industry entirely; retraining to other sectors is career-terminal for domain specialists.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, peacetime_organizational_personnel, payer,
    moderate, biographical, constrained, regional).

% Organizations and personnel who survive incidents validate their competence retroactively through survival itself. Survivors become the credential-holders for the next cycle of learning; their experience is weaponized as institutional memory and justification for training regimes. The constraint affirms their readiness retroactively.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_survivors, beneficiary,
    organized, biographical, mobile, global).

% After catastrophes, regulatory bodies enforce intensive retraining, investigation, and protocol hardening. They set the standard for what constitutes 'competence' by the post-incident investigation findings. Between incidents, they relax enforcement and permit competence drift. Their power derives from their ability to investigate and mandate after failure, making catastrophe the mechanism that justifies their authority.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, regulatory_authorities_post_incident, agenda_setter,
    institutional, generational, analytical, global).

% High-fidelity simulation providers, training vendors, and drill-infrastructure companies have invested capital in the claim that simulation can maintain competence. Under this constraint's frame, their entire infrastructure is performative theater — it creates the illusion of readiness while actual competence depends on incidents they are contractually designed to prevent. They pay by bearing the cost of their invalidated business model.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_investment_communities, payer,
    powerful, biographical, arbitrage, global).

% The safety engineering field and its knowledge base benefit from catastrophes because catastrophes vindicate the discipline's methods and provide real-world validation cases. Each incident becomes a case study, a publication opportunity, a training module. The discipline's authority and funding depend on demonstrating that catastrophes are legible, preventable, and improvable — which requires catastrophes to have happened.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, safety_engineering_discipline, beneficiary,
    organized, generational, analytical, global).

% Senior leadership in organizations operating under this constraint face a direct incentive conflict: they are structurally excluded from the constraint's logic because their career success depends on preventing the catastrophes the constraint says are necessary for competence maintenance. They would argue, from outside the constraint, for prevention as the actual goal, not competence maintenance through selection.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, organizational_leadership, excluded,
    powerful, biographical, mobile, global).

% Academic and theoretical observers who study rare-event dynamics and organizational preparedness. They measure whether the constraint's claim — that peacetime necessarily produces competence decay — holds empirically across domains, and whether this decay predictably re-exposes vulnerabilities to novel catastrophes.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, black_swan_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, regulatory_authorities_post_incident).
narrative_ontology:fixing_cost_class(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains collective readiness in high-reliability domains by anchoring competence to actual survival outcomes rather than proxy measures. The constraint coordinates distributed learning: when catastrophes occur, they generate mortality salience, investigative pressure, and organizational trauma that forces coherent retraining across entire industries and regulatory domains. Simulation and routine training cannot generate this universal signal.
% TRANSFER_FUNCTION: Transfers the burden of competence maintenance from peacetime continuous learning to post-incident crisis response. During stable periods, organizations and regulatory bodies economize on training enforcement; competence degrades. When catastrophes occur, the regulatory response transfers intensive retraining requirements and protocol changes to personnel and organizations, concentrating the cost at the moment of failure.
% ABSENT_VOICES: Organizational leadership invested in prevention-as-primary-goal would argue that competence maintenance and catastrophe avoidance are not symmetric trade-offs — that prevention is always preferable to managing readiness via selection pressure. They are structurally excluded from the constraint's framing because the constraint asserts catastrophe is necessary; leadership argues it should be avoided entirely. International organizations focused on supply-chain resilience and low-incident operations would argue that peacetime stability is achievable and that the constraint's claim about inevitable decay is false.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared — if industries stopped accepting catastrophe as a necessary selector and instead committed fully to simulation-based competence maintenance or hybrid near-miss learning — the landscape would reorganize: regulatory authority would shift toward continuous validation rather than post-incident enforcement; organizations would invest in preventive infrastructure rather than post-incident response; the entire framing of 'readiness' would shift from survival-retrospective to forward-predictive. The disappearance would not change the underlying organizational dynamics, but it would change who sets the standard for competence and how it is measured.
% FOUNDING_PROBLEM: High-reliability systems (aviation, nuclear power, maritime) require personnel to maintain readiness for catastrophic scenarios that have not occurred in their lifetime. How do you keep a large, distributed workforce sharp on rare-event handling when rare events are exactly what prevention systems are designed to prevent? How do you maintain institutional memory of catastrophe response when the point is to prevent catastrophes?
% FOUNDING_PROBLEM_CORROBORATION: Catastrophe survivors and post-incident investigations consistently report that competence decayed during peacetime and was restored through incident response and subsequent regulatory enforcement. This is documented in NTSB reports, nuclear incident analyses, and maritime safety reviews. However, simulation industry representatives and hybrid-learning proponents argue the founding problem is resolvable through better simulation fidelity and distributed near-miss learning networks, making the constraint's claim about necessity false. Academic researchers in organizational learning (Weick, Schulman) attest the founding problem is real but contested on whether catastrophe is the ONLY solution.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.67, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply at time-point 20 (0.35 → 0.65) because this marks a major incident in the historical case. Before the incident, the constraint operates as peacetime theater: competence measures are low-cost compliance performances, training is ritualistic, extractiveness is moderate because the demand for actual readiness is suppressed. The incident triggers regulatory authority (suppression rises to 0.78) and the constraint becomes actively enforced: retraining is mandatory, protocols are hardened, performance standards are elevated. Extractiveness plateaus post-incident because survivors and regulators have now locked in a high standard of readiness — but only until the next long peacetime period causes decay. Theater ratio drops at incident (0.72 → 0.42) because the constraint's function shifts from performance-compliance to survival-validation: actual lives depend on actual competence, not on appearing competent. Post-incident, theater rises again (0.58) as the organizational response becomes institutionalized and bureaucratized — procedures replace judgment. The measurement series captures one full cycle: peacetime decay, incident, post-incident response, normalization. The shared time grid ensures every metric is authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter regulatory seat and the payer peacetime personnel seat compute dramatically differently. From the regulatory perspective, the constraint is a genuine coordination function — it defines what 'readiness' means and enforces standards that protect public safety. From the personnel perspective, the constraint is extractive theater during peacetime and selective enforcement after incidents. The engine computes these divergent types from the structural data: regulatory seats see low extraction (they set and enforce the standard, they profit from its vindication); personnel seats see high extraction (they bear the cost, their expertise is devalued, their exit is constrained). This divergence is the measurement the constraint story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Peacetime personnel face high directionality toward the target end (d ≈ 0.75): they bear the cost of continuous readiness that may never be deployed, and their competence is continuously devalued by peacetime stability. Their exit is constrained — leaving the industry means terminal career loss. Catastrophe survivors sit at the beneficiary end (d ≈ 0.1): the constraint retroactively validates their competence and elevates their status. Regulatory authorities occupy the agenda-setter seat (d ≈ 0.2): they enforce the constraint and set standards, but their power is structural, not extractive — they enforce what the constraint logically demands. Simulation communities are targets (d ≈ 0.8): their entire infrastructure is invalidated by the constraint's core claim, and they pay the cost of holding a business model the constraint makes obsolete. From a simulation provider's perspective, the constraint extracts their investment and competitive advantage. From peacetime personnel's perspective, the constraint extracts their career time and psychological costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids the mandatrophy trap by maintaining a real coordination function even as its mechanism is extractive. The founding problem — how to maintain readiness for rare events — is genuinely alive (contested status confirms this). The coordination function is real: catastrophes do generate organization-wide learning signals that routine training cannot replicate. The extraction is also real: the cost of maintaining readiness-via-selection is borne by peacetime personnel who must engage in theater, and by simulation investors whose infrastructure is rendered structurally obsolete. A Tangled Rope classification holds because: (1) beneficiaries exist (catastrophe survivors, regulatory authorities, safety discipline), (2) victims exist (peacetime personnel, simulation communities), (3) active enforcement is required (regulatory authority must continuously set and reinforce readiness standards, and must resist alternative learning models like simulation). Without the enforcement machinery, personnel could abandon training, organizations could rely entirely on simulation, and the regulatory standard could shift. The constraint persists because the catastrophe-survivors and regulatory authorities have institutional power to define 'competence' and the catastrophe-cycle regenerates their authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    peacetime_competence_decay_mechanism,
    'Is competence decay during peacetime an inherent organizational property, or is it primarily a result of rational economizing on training investment when risks appear distant?',
    'Controlled organizational studies comparing training intensity and competence retention across organizations with different peacetime durations, controlling for training investment levels. Also: do organizations that maintain high training investment during peacetime show slower decay?',
    'If decay is primarily organizational inertia/economizing, then simulation with sufficient investment could substitute for catastrophes. If decay is inherent to human skill degradation under low-frequency deployment, then the constraint''s claim is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peacetime_competence_decay_mechanism, empirical, 'Whether peacetime competence decay is structural or an artifact of economizing behavior.').

omega_variable(
    simulation_fidelity_sufficiency_frontier,
    'Is there a fidelity threshold above which simulation produces genuine competence equivalent to catastrophe-trained competence, or is the gap unbridgeable?',
    'Post-incident performance analysis: do organizations with high-fidelity simulation training show significantly different incident-response performance than organizations trained via previous real incidents? What about domains with no recent incidents but high simulation investment (e.g., some aviation sectors with decades without major incidents)?',
    'A bridgeable gap would vindicate the ''simulation_as_proxy_catastrophe'' sibling reading and foreclose this reading. An unbridgeable gap would strengthen this reading''s logical foreclosure of the simulation alternative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fidelity_sufficiency_frontier, empirical, 'Whether simulation can ever achieve functional equivalence to catastrophe training.').

omega_variable(
    regulatory_authority_cycle_dependence,
    'Does regulatory authority in high-reliability systems structurally depend on catastrophes occurring to justify enforcement and justify their institutional existence?',
    'Institutional analysis: do regulatory bodies with longer incident-free periods show declining enforcement budgets, relaxed standards, or reduced authority relative to incident-recent periods? Are there documented cases of regulators actively suppressing alternative learning models (like simulation) to preserve their authority?',
    'If regulatory authority is cycle-dependent, the constraint includes a structural perverse incentive: regulators benefit from catastrophes. This would strengthen the classification as extractive and potentially shift it toward snare (pure extraction riding false coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_authority_cycle_dependence, conceptual, 'Whether the constraint''s enforcement machinery structurally depends on catastrophes to justify its authority.').

omega_variable(
    reading_identity_foreclosure_test,
    'Does the core premise of this reading — ''catastrophes are necessary for competence maintenance'' — logically foreclose the sibling readings, or are they compatible claims about different organizational domains?',
    'Formal logical analysis: if catastrophe is necessary, can simulation be functionally equivalent (simulation reading)? Can hybrid near-miss learning substitute (hybrid reading)? Or does the necessity claim strictly imply the alternatives are insufficient?',
    'If the logical foreclosure is strict, this reading forecloses its siblings (rare relation, but justified here). If the necessity claim is weaker (catastrophe is necessary but can be supplemented), then the readings coexist rather than foreclose.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_foreclosure_test, conceptual, 'Whether this reading''s logical structure forecloses the sibling readings or merely competes with them.').

omega_variable(
    catastrophe_as_sufficient_or_necessary,
    'The constraint claims catastrophe is NECESSARY for competence maintenance. But is it also SUFFICIENT? Can competence maintenance via catastrophe alone produce better outcomes than catastrophe + simulation, or is catastrophe alone inefficient and should be supplemented?',
    'Post-incident organizational analysis: do organizations that rely only on catastrophe-driven learning (rare cases) show different long-term competence trajectories than organizations that combine catastrophe with continuous simulation training?',
    'If catastrophe is necessary but not sufficient (competence is better with both), then a hybrid model becomes the optimal arrangement, potentially shifting the constraint''s claim from ''catastrophe is necessary'' to ''catastrophe-alone is insufficient but catastrophe-plus-simulation is optimal.'' This would support the hybrid reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_as_sufficient_or_necessary, empirical, 'Whether catastrophe is necessary and sufficient, or necessary but insufficient, for optimal competence maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0, 0.72).
narrative_ontology:measurement(cata_tr_t5, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 5, 0.68).
narrative_ontology:measurement(cata_tr_t10, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 10, 0.65).
narrative_ontology:measurement(cata_tr_t15, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 15, 0.62).
narrative_ontology:measurement(cata_tr_t20, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 20, 0.42).
narrative_ontology:measurement(cata_tr_t25, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 25, 0.58).
narrative_ontology:measurement(cata_tr_t30, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 30, 0.58).
narrative_ontology:measurement(cata_tr_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 40, 0.65).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cata_be_t5, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(cata_be_t10, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(cata_be_t15, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(cata_be_t20, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(cata_be_t25, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(cata_be_t30, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 30, 0.67).
narrative_ontology:measurement(cata_be_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 40, 0.67).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(cata_su_t5, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(cata_su_t10, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(cata_su_t15, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(cata_su_t20, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(cata_su_t25, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 25, 0.76).
narrative_ontology:measurement(cata_su_t30, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(cata_su_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, global_infrastructure).
narrative_ontology:boltzmann_floor_override(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.18).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_as_proxy_catastrophe).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, hybrid_near_miss_learning).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, organizational_knowledge_decay).

% DUAL FORMULATION NOTE:
% This story is one reading of the 'catastrophe_avoidance_retention' kernel. The other two readings ('simulation_as_proxy_catastrophe' and 'hybrid_near_miss_learning') are separate constraint stories with different ε values, different beneficiary/victim sets, and different classifications. This reading asserts catastrophe is structurally necessary; the siblings claim it is not. The three stories form a constraint family linked by this network block. The epsilon-invariance principle applies: each reading has a fixed, single ε (the standing arrangement it is about — catastrophe-dependence, simulation-sufficiency, or hybrid-optimality) and a fixed beneficiary/victim structure. The readings do not average or hedge across alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
