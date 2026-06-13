% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__hybrid_degradation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__hybrid_degradation_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_proxy_sufficiency__hybrid_degradation_reading
 *   human_readable: Simulation-Based Certification Maintains Procedural Competence While Degrading Tacit Knowledge
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   High-consequence domains (aviation, nuclear power, medicine, emergency
 *   response) have transitioned to simulation-based training as the primary
 *   mechanism for building and certifying operator competence. This reading
 *   asserts that simulation MAINTAINS procedural competence (checklists,
 *   standard responses, system operation) but DEGRADES tacit knowledge
 *   (pattern recognition under uncertainty, intuitive stress response,
 *   improvisation when procedures fail) over generational timescales. The
 *   degradation is silent — no single catastrophic failure appears to trigger
 *   revision — because simulation masks the erosion through passing
 *   certification scores. The beneficiary is the certification industry
 *   (ongoing licensing/scenario revenue) and regulators (liability
 *   avoidance), who have structural incentive to assert simulation
 *   sufficiency. The victim is the accumulation of generational knowledge
 *   loss and elevated future incident risk. This reading is ONE
 *   INTERPRETATION of a contested kernel: whether simulation alone suffices
 *   for competence maintenance. Sibling readings take opposite positions
 *   (simulation is fully sufficient, only real catastrophe maintains
 *   competence, fidelity thresholds matter more than categorical claims).
 *
 * KEY AGENTS:
 *   - Certification Training Industry: agenda-setter, designs curricula, collects revenue, asserts simulation sufficiency
 *   - Regulatory Agencies: beneficiary-payer dual position, mandate simulation to avoid liability while avoiding explicit responsibility for training quality
 *   - Operating Organizations: payers, trapped by regulation, accumulate degraded tacit knowledge across cohorts
 *   - Operating Personnel: identity-locked payers, cannot reject certification system without leaving profession
 *   - Future Incident Victims: excluded payers, experience elevated risk from degraded operator capacity
 *   - Generational Knowledge Carriers (Senior Operators): excluded, knowledge lost at retirement without transfer mechanism
 *   - Safety Research Community: observers, can measure degradation but structurally deprioritized by industry and regulators
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.68).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.72).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, resistance, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__hybrid_degradation_reading, "Simulation-Based Certification Maintains Procedural Competence While Degrading Tacit Knowledge").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__hybrid_degradation_reading, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_proxy_sufficiency__hybrid_degradation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__hybrid_degradation_reading, '741f7444-06fe-4a57-918f-444854f9ea46').
narrative_ontology:cs_kernel_codification('741f7444-06fe-4a57-918f-444854f9ea46', distributed).
narrative_ontology:cs_authority_grounding('741f7444-06fe-4a57-918f-444854f9ea46', extraction).
narrative_ontology:cs_interpretation_layer_present('741f7444-06fe-4a57-918f-444854f9ea46').
narrative_ontology:cs_reading_relation('741f7444-06fe-4a57-918f-444854f9ea46', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('741f7444-06fe-4a57-918f-444854f9ea46', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, coexists_with).
narrative_ontology:cs_reading_relation('741f7444-06fe-4a57-918f-444854f9ea46', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('741f7444-06fe-4a57-918f-444854f9ea46', foundational, tacit_knowledge_irreducible).
narrative_ontology:cs_axiom_status(tacit_knowledge_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('741f7444-06fe-4a57-918f-444854f9ea46', tacit_knowledge_irreducible, empirically_contingent).
narrative_ontology:cs_axiom('741f7444-06fe-4a57-918f-444854f9ea46', foundational, generational_transfer_mechanism_severable).
narrative_ontology:cs_axiom_status(generational_transfer_mechanism_severable, holdable).
narrative_ontology:cs_axiom_grounding('741f7444-06fe-4a57-918f-444854f9ea46', generational_transfer_mechanism_severable, empirically_contingent).
narrative_ontology:cs_reference_frame('741f7444-06fe-4a57-918f-444854f9ea46', simulation_sufficient_assumption).
narrative_ontology:cs_drift_state('741f7444-06fe-4a57-918f-444854f9ea46', contemporary_incident_analysis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('741f7444-06fe-4a57-918f-444854f9ea46', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_training_industry).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, regulatory_agencies_avoiding_fault).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, operating_organizations).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, future_cohorts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, operating_personnel).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, operating_personnel).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, future_incident_victims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, delivers, and certifies simulation-based training curricula for high-consequence domains (aviation, nuclear, medicine, emergency response). Collects revenue from licensing, scenario development, instructor certification, and recurring training mandates. Justifies simulation as the gold standard for safety because it eliminates catastrophe risk while building competence. Has structural incentive to maintain simulation's perceived sufficiency — if real-world experience were necessary, simulation demand would collapse and alternative training pathways would emerge.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_training_industry, agenda_setter,
    organized, generational, arbitrage, global).

% Mandate simulation-based training and certification because it appears to eliminate their liability for incidents — they can claim they enforced best-practice training. If they allowed real-world catastrophes as training, they would be legally liable for injuries/deaths resulting from the training itself. Simulation permits them to demonstrate due diligence without bearing accountability for training-induced harm.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, regulatory_agencies_avoiding_fault, beneficiary,
    institutional, generational, constrained, national).

% Bear the cost of recurring simulation-based certification (instructor time, scenario licensing, infrastructure) but cannot exit without losing regulatory approval. They cannot demand real-world experience-based training as an alternative because regulation prohibits it. Over generational timescales, they accumulate degraded tacit knowledge and stress-response capacity in their cohorts while maintaining the fiction of certification sufficiency through passing simulation scores.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, operating_organizations, payer,
    organized, generational, trapped, global).

% Undergo mandatory simulation-based certification cycles to maintain licensure. They experience the procedural training (how to execute checklist steps in simulation) but accumulate compressed, shallow tacit knowledge compared to cohorts trained in prior eras when real operational experience and near-miss events were part of the training trajectory. Their professional identity is constituted through certification; rejecting the certification system means leaving the profession. They cannot demand real-world experience-based training because the regulatory system prohibits it.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, operating_personnel, payer,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__hybrid_degradation_reading, operating_personnel, beneficiary).

% Are not present during certification design but will experience elevated incident risk when operating personnel face real-world stressors (cascading failures, novel combinations of failures, extreme uncertainty) that simulation never exposed them to. The degraded tacit knowledge and stress-response capacity of their operating personnel increases the probability and severity of incidents they would suffer. They cannot exit the system because they are affected without consent.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, future_incident_victims, payer,
    powerless, civilizational, trapped, global).

% Senior operators with deep tacit knowledge from era when real operational experience and near-miss learning were part of the training pathway retire and are replaced by cohorts trained entirely in simulation. Their situated knowledge (how to read subtle equipment behavior, how to maintain composure under genuine uncertainty, how to improvise when procedures fail) is not transferable through simulation curricula and is lost when they leave. They are structurally excluded from training design because curriculum authority has passed to the certification industry.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, generational_knowledge_carriers, excluded,
    moderate, generational, constrained, global).

% Conducts longitudinal research on competence retention, stress-response patterns, and incident rates across cohorts trained under different regimes. Can measure the degradation of tacit knowledge over generational timescales and correlate it with incident risk. Positioned outside the certification system; their findings can either support or undermine the sufficiency claim, but the industry and regulators have structural incentive to ignore or fund research that supports the sufficiency narrative.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, safety_research_community, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Simulation-based certification solves the coordination problem of how to train for high-consequence domains without making trainees' errors fatal: it compresses procedural skill development, reduces real-world infrastructure risk, and permits standardized curriculum delivery across geographies. It coordinates regulatory liability avoidance with training delivery — regulators can mandate training without bearing liability for training-induced harm.
% TRANSFER_FUNCTION: Moves training costs and generational knowledge degradation from the certification industry and regulators to operating organizations and future incident victims. The constraint transfers revenue from organizations to the certification industry and transfers tacit knowledge and stress-response capacity from senior operators (who retire) to a knowledge sink (uncaptured, not transferred to successors).
% ABSENT_VOICES: Senior operators whose knowledge is being lost, and future incident victims whose risk is being elevated, are structurally excluded from training curriculum design. Their objections — that simulation cannot substitute for real operational uncertainty, that generational knowledge transfer is being severed, that future incidents will expose the degradation — are not voiced in the certification system because they have no seat at the design table. Safety research community findings are present but structurally deprioritized.
% DISAPPEARANCE_RATIONALE: If the simulation sufficiency constraint vanished and regulators permitted real-world experience-based training (near-miss learning, structured observation during operational stress), the certification industry would contract, operating organizations would shift training costs to real operations, and tacit knowledge transfer would resume through generational proximity. Future incident rates would likely decline as stress-response capacity was restored. The organizational and financial landscape of training would reorganize.
% FOUNDING_PROBLEM: High-consequence domains need competent personnel without trainees' errors being fatal. Prior regimes incurred training fatalities (aviation cadets dying in training, medical residents harming patients during learning, nuclear operators causing incidents during qualification). Simulation emerged as solution to eliminate training-induced harm while maintaining competence.
% FOUNDING_PROBLEM_CORROBORATION: Training-induced fatalities in aviation, nuclear, and medicine have declined to near-zero — this is factual and attested by incident data across all domains. The founding problem (prevent trainees from dying while learning) is solved. The constraint now persists justified by a different problem (maintain competence through simulation) which is contestable. Safety research community has documented the shift: early simulation adoption was justified by specific fatality prevention; contemporary simulation justification is competence maintenance via assertion of fidelity equivalence.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__hybrid_degradation_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__hybrid_degradation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__hybrid_degradation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__hybrid_degradation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__hybrid_degradation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_proxy_sufficiency__hybrid_degradation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction begins low (0.42 at t=0) when simulation was genuinely innovative and addressed real training fatality problem. As decades pass without catastrophic incidents to test the knowledge base, operating organizations accumulate confidence in simulation sufficiency while their personnel's tacit knowledge actually erodes. Extractiveness rises to 0.68 as the mismatch between procedural certification and actual stress-response capacity widens — the system extracts generational knowledge without providing it. Theater ratio rises sharply (0.28 → 0.62 over 40 years) because more of the enforcement activity defends the fiction of sufficiency (research suppression, incident under-reporting, curriculum rhetoric about fidelity) than builds actual competence. Suppression is steady-high (0.52 → 0.74) because the regulatory system actively prevents alternative training regimes (real-world experience, near-miss learning) that would compete with simulation. Accessibility collapse is low-moderate (0.42) because alternatives conceptually exist (real operational learning) even though institutionally blocked — an operator can imagine how to train differently but cannot practice it under current regulation. Resistance is moderate (0.51) because senior operators and some safety researchers push back, but the certification industry's organizational power and regulatory capture suppress that resistance. The shared time grid shows coordinated measurement at seven time points (t=0,5,10,15,25,30,40), enabling lifecycle analysis of constraint drift.
 *
 * PERSPECTIVAL GAP:
 *   From the certification industry's seat: simulation solved a real problem (training fatalities) and continues to deliver safe operators (measured via certification scores and low incident attribution to training). From the operating organization's seat: we pay for simulation but sense our personnel are under-equipped for real uncertainty — we cannot exit but bear the cost. From senior operators' seat: we taught through apprenticeship and real operational exposure; younger cohorts trained in simulation know procedures but not intuition. From regulators' seat: we mandated simulation to avoid liability and can claim due diligence; whether it works is legally someone else's problem. From future incident victims' seat: we are not at the table; we will experience the risk when incidents spike from the degraded capacity of our operators. The engine should compute different constraint types from each seat — the agenda-setter (industry) sees coordination; the payers see extraction; the excluded see hidden cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Certification industry: d ~0.1 (beneficiary, collects revenue, controls rules, arbitrage exit to other markets). Regulators: d ~0.2 (beneficiary via liability avoidance, but constrained by liability exposure — if incidents spike they bear reputational cost, so modest target status). Operating organizations: d ~0.75 (payer, trapped exit, carry recurring costs, accumulate degraded knowledge). Operating personnel: d ~0.8 (payer identity-locked, cannot exit profession, undergo training they suspect inadequate, carry identity-fused suppression post-training). Future incident victims: d ~1.0 (external targets, non-consenting, elevated risk from degraded operator capacity). Generational knowledge carriers: d ~0.7 (payer, excluded from curriculum design, knowledge lost at retirement). Safety researchers: d ~0.5 (symmetric observer, produce credible findings that threaten industry's legitimacy, constrained by funding captured by industry). No directionality overrides needed; the power and exit derivations capture the structure. The asymmetry between beneficiary (organized, arbitrage exit, institutional power) and payer (trapped, identity-locked, powerless) confirms tangled rope structure with active enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is specifically designed to resolve a mandatrophy question: was simulation-based certification built to solve a real competence problem (training fatality elimination) that is now dead (training fatalities no longer occur) but the constraint persists (simulation remains mandatory and expanding)? This reading says YES, with a crucial amendment: the founding mandate (eliminate training-induced harm) is satisfied, but the hidden cost (generational knowledge degradation) only becomes visible over decades. The constraint is not a pure mandatrophic vestige (like a regulation whose justification has evaporated); it is a Tangled Rope where coordination (safe training) and extraction (generational knowledge transfer interrupted) coexist. The founding problem (training fatalities) is DEAD — this is fact, not opinion. But the problem it was solving cannot be recovered to justify its persistence; instead, the certification industry and regulators have shifted to asserting ongoing necessity of simulation for competence (a different problem). This shift from 'eliminate training harm' to 'competence requires simulation' is the move that requires empirical resolution — it is contestable in a way the original mandate was not. The omega variable 'kernel_sufficiency_contested' captures this directly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_sufficiency_contested,
    'Is simulation-based training SUFFICIENT to maintain competence in high-consequence domains indefinitely, or does generational knowledge transfer and real-world stress exposure remain necessary?',
    'Longitudinal cohort comparison: compare incident rates, stress-response time, and tacit knowledge depth in cohorts trained under pure-simulation regime vs. cohorts trained under mixed regime (simulation + structured real-world experience). Multi-decade comparison across aviation, nuclear, medicine, emergency response.',
    'If simulation sufficiency is confirmed, the constraint is genuine coordination with manageable costs. If insufficiency is confirmed, the constraint is extractive rent-seeking riding a cover story; this reading foreclosed by evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_sufficiency_contested, empirical, 'Whether simulation alone maintains competence or hidden degradation occurs over generational timescales.').

omega_variable(
    tacit_knowledge_transfer_mechanism,
    'What mechanisms transfer tacit knowledge across generations in high-consequence domains, and does simulation-only training sever those mechanisms?',
    'Ethnographic and cognitive apprenticeship studies: observe how senior operators train junior operators in real operational settings vs. simulation-only curricula. Measure knowledge transfer efficiency and retained expertise in stress conditions.',
    'If mechanisms are severed, this reading''s core claim (degradation without catastrophes) is validated; the constraint extracts knowledge-bearing capacity from future cohorts. If mechanisms persist within simulation, the claim is weakened; simulation may incorporate more authentic knowledge transfer than this reading assumes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tacit_knowledge_transfer_mechanism, empirical, 'Whether simulation-based curricula preserve or degrade generational knowledge transfer.').

omega_variable(
    stress_fidelity_threshold_ambiguity,
    'Does stress-response capacity depend on crossing a fidelity threshold where simulation matches real-world uncertainty exactly, or does degradation occur smoothly as a function of fidelity?',
    'Psychophysiological measurement: compare stress physiology (cortisol, heart rate variability, decision latency under uncertainty) in simulation cohorts vs. operators with real operational experience. Identify whether fidelity improvements show step-function improvement in capacity or continuous improvement.',
    'This reading assumes continuous degradation. If evidence shows step-function threshold, the sibling reading ''simulation_fidelity_threshold'' is more precise. If degradation is truly continuous, this reading''s framing is more accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stress_fidelity_threshold_ambiguity, empirical, 'Whether competence degradation is continuous or threshold-dependent on simulation fidelity.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of alternative training regimes (real-world experience, near-miss learning) structural (regulatory barriers, liability architecture) or internalized (operating organizations and personnel have accepted simulation sufficiency)?',
    'Post-regulatory-change trajectory: if a jurisdiction permits mixed simulation/real-world training, does adoption surge immediately (structural suppression) or remain low (internalized belief in simulation sufficiency)? Observe demand elasticity.',
    'If suppression is purely structural, removing regulation would shift training immediately. If internalized, even regulatory opening would not undo the belief in simulation sufficiency, indicating deeper capture of the knowledge commons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of alternative training is structural or internalized.').

omega_variable(
    beneficiary_alignment_regulators_vs_industry,
    'Do regulatory agencies genuinely benefit from certification industry''s sufficiency claim, or do they have independent incentive to demand real-world competence that they are suppressing?',
    'Analysis of regulatory incentive structure: do incident rates within regulated domains provide feedback that would trigger regulatory demand for higher fidelity? Are regulators aware of incident data showing correlation with simulation-only training cohorts?',
    'If regulators are aware and suppressing demand for higher fidelity, the constraint is purely extractive (tangled rope confirmed). If regulators genuinely believe simulation sufficiency or lack incident data, they are more co-opted than beneficiary — this reading''s structure changes to identify them as victims rather than beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_alignment_regulators_vs_industry, empirical, 'Whether regulators benefit from or are captured by the simulation sufficiency claim.').

omega_variable(
    kernel_reading_contest_structural_location,
    'Where is the irreducible contest between the four readings located: in empirical claims about simulation fidelity, in normative claims about acceptable risk, in institutional incentive structures, or in epistemological commitments about knowledge transfer?',
    'Cross-reading analysis: map which readings agree/disagree on fidelity (empirical), on acceptable incident risk (normative), on institutional capture (structural), and on how knowledge persists (epistemological). Identify which disagreements are empirically resolvable and which are constitutive.',
    'This informs whether one reading can be definitively falsified by evidence (if empirically-located) or whether contest is irresolvable within current institutions (if structural or epistemological). Shapes the omega_c typology for the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_structural_location, conceptual, 'The deep location of the kernel reading contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t5, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement_basis(cata_tr_t5, observed).
narrative_ontology:measurement(cata_tr_t10, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement_basis(cata_tr_t10, observed).
narrative_ontology:measurement(cata_tr_t15, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 15, 0.49).
narrative_ontology:measurement_basis(cata_tr_t15, observed).
narrative_ontology:measurement(cata_tr_t25, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 25, 0.56).
narrative_ontology:measurement_basis(cata_tr_t25, observed).
narrative_ontology:measurement(cata_tr_t30, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement_basis(cata_tr_t30, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 40, 0.62).
narrative_ontology:measurement_basis(cata_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t5, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(cata_be_t5, observed).
narrative_ontology:measurement(cata_be_t10, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(cata_be_t10, observed).
narrative_ontology:measurement(cata_be_t15, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(cata_be_t15, observed).
narrative_ontology:measurement(cata_be_t25, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 25, 0.65).
narrative_ontology:measurement_basis(cata_be_t25, observed).
narrative_ontology:measurement(cata_be_t30, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(cata_be_t30, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(cata_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t5, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(cata_su_t5, observed).
narrative_ontology:measurement(cata_su_t10, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(cata_su_t10, observed).
narrative_ontology:measurement(cata_su_t15, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(cata_su_t15, observed).
narrative_ontology:measurement(cata_su_t25, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(cata_su_t25, observed).
narrative_ontology:measurement(cata_su_t30, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(cata_su_t30, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement_basis(cata_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__hybrid_degradation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.18).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel catastrophe_proxy_sufficiency. The kernel concerns whether simulation-based training is sufficient to maintain competence in high-consequence domains. This reading (hybrid_degradation) asserts that simulation maintains procedural competence while degrading tacit knowledge over generational timescales. Sibling readings take contradictory positions on the same kernel. All readings share the same constraint domain but differ in ε-values, beneficiary/victim structures, and predictions about future incident risk. Decomposition follows the ε-invariance principle: if measuring 'simulation sufficiency' via procedural competence yields a different classification than measuring it via tacit knowledge retention, the observer is looking at two constraints. This reading measures the latter; the simulation_as_proxy_catastrophe_reading measures the former. They affect each other because evidence that procedural competence persists (supporting the proxy reading) does not settle evidence that tacit knowledge degrades (supporting this reading). All four sibling readings must be evaluated together to understand the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_proxy_sufficiency__hybrid_degradation_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
