% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__catastrophe_as_necessary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__catastrophe_as_necessary, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: competence_retention_exercise__catastrophe_as_necessary
 *   human_readable: Catastrophe-as-Necessary Competence Maintenance Doctrine
 *   domain: safety/organizational/epistemological
 *
 * SUMMARY:
 *   In high-reliability organizations, a doctrine holds that organizational
 *   competence in catastrophe-avoidance requires actual catastrophic events
 *   to maintain—that simulation and near-miss learning constitute 'rehearsal'
 *   but not genuine competence exercise. This constraint structures the
 *   epistemology of what counts as real learning, designating catastrophe as
 *   the sole legitimate validator and suppressing competing learning regimes.
 *   The constraint's extractive character emerges from its function: it
 *   reserves learning authority for post-catastrophe investigation
 *   hierarchies, devalues continuous-improvement practitioners, and
 *   suppresses organizational cultures built on near-miss reporting and
 *   high-fidelity simulation. It is a snare because persistence depends on
 *   active suppression of the alternative learning narratives and because
 *   those suppressed narratives (that simulation can constitute genuine
 *   competence validation, that near-misses provide sufficient learning
 *   signals) represent real organizational knowledge that the constraint
 *   keeps subordinate.
 *
 * KEY AGENTS:
 *   - catastrophe_narrative_authorities: Institutional seats (formal investigation boards, senior leadership) that control the framing of what counts as legitimate learning; benefit by maintaining the doctrine because it justifies their epistemic authority
 *   - safety_improvement_communities: Organized practitioners (engineers, safety specialists) advocating for continuous improvement; bear the cost because their evidence is systematically devalued
 *   - simulated_learning_practitioners: Moderate-power specialists whose professional identity depends on simulation as genuine competence exercise; trapped by identity-lock because accepting the doctrine requires abandoning their epistemic foundation
 *   - near_miss_reporting_cultures: Frontline safety cultures suppressed by the doctrine; the constraint directly undermines psychological safety and honest reporting
 *   - incident_investigation_hierarchies: Beneficiary institutional seats whose authority and resource allocation depend on catastrophe as the primary knowledge generator
 *   - catastrophe_survivors_and_victims: Excluded until after the catastrophe occurs; most acutely affected but structurally barred from the learning decision-making
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__catastrophe_as_necessary, 0.68).
domain_priors:suppression_score(competence_retention_exercise__catastrophe_as_necessary, 0.71).
domain_priors:theater_ratio(competence_retention_exercise__catastrophe_as_necessary, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__catastrophe_as_necessary, snare).
narrative_ontology:human_readable(competence_retention_exercise__catastrophe_as_necessary, "Catastrophe-as-Necessary Competence Maintenance Doctrine").
narrative_ontology:topic_domain(competence_retention_exercise__catastrophe_as_necessary, "safety/organizational/epistemological").

domain_priors:requires_active_enforcement(competence_retention_exercise__catastrophe_as_necessary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__catastrophe_as_necessary, '3d36e0d2-8615-4704-b19f-c4f7bbb19df5').
narrative_ontology:cs_kernel_codification('3d36e0d2-8615-4704-b19f-c4f7bbb19df5', distributed).
narrative_ontology:cs_authority_grounding('3d36e0d2-8615-4704-b19f-c4f7bbb19df5', extraction).
narrative_ontology:cs_interpretation_layer_present('3d36e0d2-8615-4704-b19f-c4f7bbb19df5').
narrative_ontology:cs_reading_relation('3d36e0d2-8615-4704-b19f-c4f7bbb19df5', competence_retention_exercise__simulation_as_sufficient, forecloses).
narrative_ontology:cs_reading_relation('3d36e0d2-8615-4704-b19f-c4f7bbb19df5', competence_retention_exercise__near_miss_as_bridge, coexists_with).
narrative_ontology:cs_axiom('3d36e0d2-8615-4704-b19f-c4f7bbb19df5', foundational, real_catastrophe_epistemic_necessity).
narrative_ontology:cs_axiom_status(real_catastrophe_epistemic_necessity, holdable).
narrative_ontology:cs_axiom_grounding('3d36e0d2-8615-4704-b19f-c4f7bbb19df5', real_catastrophe_epistemic_necessity, empirically_contingent).
narrative_ontology:cs_axiom('3d36e0d2-8615-4704-b19f-c4f7bbb19df5', foundational, simulation_inherent_fidelity_insufficiency).
narrative_ontology:cs_axiom_status(simulation_inherent_fidelity_insufficiency, holdable).
narrative_ontology:cs_axiom_grounding('3d36e0d2-8615-4704-b19f-c4f7bbb19df5', simulation_inherent_fidelity_insufficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('3d36e0d2-8615-4704-b19f-c4f7bbb19df5', catastrophe_validation_epistemology).
narrative_ontology:cs_drift_state('3d36e0d2-8615-4704-b19f-c4f7bbb19df5', contemporary_simulation_and_near_miss_research_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3d36e0d2-8615-4704-b19f-c4f7bbb19df5', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, catastrophe_narrative_authorities).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, incident_investigation_hierarchies).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, incident_prevention_advocates).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, safety_improvement_communities).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, simulated_learning_practitioners).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, near_miss_reporting_cultures).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, organizational_leadership).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, organizational_leadership).
narrative_ontology:constraint_vindicates(competence_retention_exercise__catastrophe_as_necessary, visceral_learning_irreplaceability).
narrative_ontology:constraint_vindicates(competence_retention_exercise__catastrophe_as_necessary, simulation_fidelity_incompleteness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the doctrine that 'only real catastrophes teach genuine competence.' Controls which incidents are formally investigated, frames the learning extracted, and determines which evidence (simulation data, near-miss reports, continuous-improvement metrics) receives institutional legitimacy. Benefits directly from the doctrine because it justifies their role as the primary translators of safety knowledge and concentrates learning authority in their hands. Could shift to a prevention-focused learning regime but maintains the current doctrine because it preserves their institutional power.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, catastrophe_narrative_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Formal structures (federal agencies, corporate incident response boards, industry review bodies) whose existence, funding, and authority depend on catastrophe as the primary organizational learning event. Catastrophes justify their operations, scale their budgets, and position them as the seats of legitimate knowledge-generation. Would face reorganization and reduced scope if organizations shifted to prevention-centered learning regimes. Actively maintain the doctrine through their authority to frame incident investigations and evaluate competing learning claims.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, incident_investigation_hierarchies, beneficiary,
    institutional, generational, arbitrage, global).

% Engineers, safety specialists, continuous-improvement practitioners, and quality advocates working on prevention-based learning. Bear the cost of the doctrine because their evidence (simulation effectiveness data, near-miss insights, prevented-failure analyses) is systematically devalued as 'rehearsal' unless catastrophe retroactively validates it. Are trapped because challenging the doctrine requires questioning whether they are wasting organizational resources; their entire profession is positioned as methodologically secondary. Maintain their commitment to prevention even though the constraint subordinates it.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, safety_improvement_communities, payer,
    organized, biographical, constrained, global).

% Training designers, scenario developers, high-fidelity simulation specialists, and competency assessment experts whose professional identity is built on the epistemological claim that simulation constitutes genuine competence exercise. The doctrine that 'only real catastrophes teach' renders their discipline methodologically secondary—they can produce extensive evidence of learning transfer and skill acquisition, but the constraint pre-judges that evidence as noise until catastrophe validates it. Exit is identity-locked because accepting the doctrine means abandoning the epistemic foundation of their entire field; they would have to believe that their life's work (designing high-fidelity scenarios) cannot constitute real learning.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, simulated_learning_practitioners, payer,
    moderate, biographical, identity_locked, national).

% Frontline safety cultures, incident reporting systems, and near-miss learning programs that accumulate detailed data from minor failures and near-catastrophes. Bear the cost of the doctrine because it suppresses the psychological safety required to report and learn from near-misses honestly. Workers rationally retreat from honest reporting when organizational signals indicate that near-misses 'don't count as real learning'—the constraint directly undermines the behavioral conditions for near-miss culture to function. Are constrained because they cannot exit the organization without losing access to the accumulating near-miss knowledge they have built.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, near_miss_reporting_cultures, payer,
    moderate, biographical, constrained, regional).

% Executive decision-makers in high-risk industries who must choose budget allocations between continuous-improvement funding and incident-response preparation. Bear the cost of the doctrine (deferred learning, invisibly accumulating incompetence during incident-free periods, eventual catastrophic failure that materializes all the deferred costs at once). Also benefit from it in the short term (if incident prevention succeeds, the doctrine's falsity remains hidden; catastrophes are rare enough that the cost never fully materializes during any individual leader's tenure). The constraint lets leadership defer expensive prevention investments while appearing to be serious about safety. Exit is mobile in principle but tenure-constrained in practice—no leader wants the catastrophe that would reveal the doctrine's cost to occur on their watch.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, organizational_leadership, payer,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__catastrophe_as_necessary, organizational_leadership, beneficiary).

% Individuals harmed or killed in actual catastrophic failures of systems that were allowed to degrade competence between incidents. Have the strongest stake in the learning conversation—their lives are the cost of the doctrine—but are structurally excluded from its adjudication. Their testimony and stake enter only AFTER the catastrophe, at which point the constraint has already done its destructive work. They cannot participate in choosing whether simulation or near-miss learning is sufficient because they are powerless and have no seat at the decision table until the catastrophe retroactively includes them as evidence for future learning.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, catastrophe_survivors_and_victims, excluded,
    powerless, immediate, trapped, local).

% Academic and industry researchers studying the relationship between simulation fidelity, procedural learning, transfer effects, and real-world performance outcomes. Produce empirical evidence on whether high-fidelity simulation produces competence equivalent to real incidents, and evidence that learning transfer from simulation is measurable and robust. Their findings are entered into the constraint's adjudication as 'only applicable to routine scenarios, not novel or catastrophic failures'—a methodological gate that makes simulation efficacy research structurally unable to falsify the catastrophe-as-necessary claim. Seated as observers but their evidence is pre-discounted.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, simulation_efficacy_researchers, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__catastrophe_as_necessary, catastrophe_narrative_authorities).
narrative_ontology:fixing_cost_class(competence_retention_exercise__catastrophe_as_necessary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is no genuine coordination function. The constraint is presented as ensuring that competence is maintained through visceral stakes, but the real function is to concentrate learning authority in catastrophe-aftermath investigation hierarchies. If there were a coordination problem being solved, it would be: 'how do we ensure that organizations do not become complacent and degrade competence during long incident-free periods?' But the constraint does not solve this problem—it instead accepts inevitable competence decay and positions catastrophe as the necessary reset mechanism. Prevention-centered learning regimes (continuous improvement, high-fidelity simulation, near-miss analysis) would solve the actual founding problem; the constraint's function is to suppress those regimes.
% TRANSFER_FUNCTION: Transfers epistemic authority and legitimacy from safety-improvement practitioners (engineers, simulation specialists, near-miss analysts) to catastrophe-aftermath authorities (formal investigation hierarchies, incident response leadership). Also transfers organizational resources: funding flows toward post-incident investigation and incident response rather than toward continuous-improvement infrastructure. The constraint transfers the framing power to interpret what counts as real learning—only knowledge derived from actual catastrophes receives institutional legitimacy; simulation fidelity research, near-miss insights, and prevention successes are devalued as 'rehearsal' until catastrophe retroactively validates them.
% ABSENT_VOICES: Catastrophe survivors and victims are excluded until after the catastrophe occurs—by which point the learning decision has already been made. Prevention-first voices (advocates who prioritize incident prevention as an end in itself rather than as a side effect) are excluded or marginalized because the constraint requires accepting that catastrophe is organizationally functional. Safety-improvement practitioners are seated but with subordinate authority: their evidence enters the conversation only as candidate for post-incident validation. Simulation researchers and near-miss culture builders are seated but their findings are pre-discounted by the methodological gate that 'routine scenarios don't teach catastrophe-avoidance'—they are heard but their evidence is treated as insufficient by structural design.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared overnight, organizational learning regimes would immediately shift toward continuous improvement, high-fidelity simulation, and near-miss learning cultures as the primary competence-maintenance mechanisms. Formal investigation hierarchies would shrink and reorganize around prevention support rather than post-catastrophe adjudication. Funding would flow to prevention infrastructure instead of incident response. The epistemic gate that devalues simulation and near-miss research would open, and these alternative learning regimes would be treated as legitimate validators of competence rather than as secondary 'rehearsal.' Within years, organizations operating under the constraint's absence would show demonstrably different safety outcomes and competence trajectories. The constraint's disappearance would rearrange the entire learning infrastructure of high-reliability organizations.
% FOUNDING_PROBLEM: In high-reliability organizations (nuclear plants, aircraft carriers, surgical teams, chemical processing facilities), competence in catastrophe-avoidance requires continuous cognitive maintenance and procedural exercising. During long incident-free periods (which can last decades), organizations face a genuine organizational danger: that complacency, skill decay, and loss of institutional memory of why particular procedures exist will outpace the vigilance required to prevent catastrophes. The founding problem was: how can organizations maintain genuine competence and vigilance during incident-free periods when there is no immediate evidence that competence is degrading and when the absence of incidents creates an illusion of safety?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by incident investigation reports into major catastrophes (Three Mile Island, Space Shuttle Challenger, Fukushima Daiichi, Tenerife airport collision, hospital infections, aviation accidents, chemical plant explosions). These investigations document that long incident-free periods preceded the catastrophe and that competence degradation during those periods contributed to the failure. However, this attestation does NOT logically support the constraint's solution (that catastrophe is necessary to solve the problem). The attestation only establishes that catastrophe *reveals* competence failures after the fact. Researchers in organizational learning, simulation efficacy, and near-miss learning cultures (attesting from outside the benefiting parties) show that the founding problem can be directly addressed through continuous improvement without requiring catastrophe as the learning mechanism. High-reliability organizations with strong near-miss reporting systems, high-fidelity simulation programs, and continuous-improvement cultures (anesthesia teams in hospital systems, some nuclear power plants, surgical error prevention programs) demonstrate sustained competence maintenance during incident-free periods without relying on catastrophe as a learning tool. The causality claimed by the constraint—that competence maintenance requires catastrophe—is not corroborated by these organizations' success at maintaining competence through alternative mechanisms.
narrative_ontology:disappearance_verdict(competence_retention_exercise__catastrophe_as_necessary, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__catastrophe_as_necessary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__catastrophe_as_necessary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(competence_retention_exercise__catastrophe_as_necessary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__catastrophe_as_necessary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_retention_exercise__catastrophe_as_necessary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_retention_exercise__catastrophe_as_necessary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures 0.68 at interval end, representing the degree to which the constraint transfers epistemic and organizational authority from safety-improvement practitioners to catastrophe-aftermath hierarchies. Suppression measures 0.71, reflecting active devaluation of simulation efficacy research and near-miss data, and the psychological suppression of reporting cultures. Theater ratio measures 0.52 and rises toward that level by t=20, indicating that a growing share of the constraint's enforcement activity is performative maintenance of the doctrine (post-incident rhetoric, simulation 'training' labeled as insufficient, near-miss reports treated as data-collection without learning consequences) rather than functional catastrophe-avoidance. The measurements show extractiveness rising steeply through t=15 (doctrine hardening as incident-free periods accumulate), then plateauing around t=25–40, suggesting that once the doctrine becomes institutionalized (investigation hierarchies organized around it, simulation training funded but explicitly labeled as secondary), further growth in extractiveness flattens—the constraint reaches a stable state of suppressed alternatives. The theater ratio's asymptotic approach to 0.52 reflects the doctrine's stabilization: the enforcement machinery persists, but the gap between stated rationale (competence maintenance through catastrophe-validation) and actual function (gatekeeping learning authority) becomes the constraint's primary operation.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (catastrophe_narrative_authorities and incident_investigation_hierarchies), the constraint is experienced as necessary epistemic rigor—real learning requires real stakes. From the payer seats (safety_improvement_communities, simulated_learning_practitioners, near_miss_reporting_cultures), the same constraint is experienced as an extractive gatekeeping regime that devalues legitimate evidence and suppresses prevention-centered learning cultures. The engine should compute substantially different directionality values: agenda-setters sit at low d (beneficiary, control, arbitrage options); payers sit at high d (victims, constrained/identity-locked exit, bearing the cost of epistemic suppression). The identity_locked exit of simulation practitioners is particularly acute: they cannot exit the field (their professional identity is fused with the belief that simulation constitutes genuine competence exercise) but the constraint tells them their work is methodologically secondary. The constraint's persistence depends on maintaining this asymmetry—if simulation practitioners or near-miss advocates gained the power to reframe the epistemic foundations, the constraint would collapse.
 *
 * DIRECTIONALITY LOGIC:
 *   Catastrophe_narrative_authorities and incident_investigation_hierarchies are full beneficiaries (d~0.1–0.2): they control the learning agenda, accumulate institutional authority, and have arbitrage options (they can shift their authority base if needed but benefit from the current arrangement). Safety_improvement_communities sit at high d (~0.75–0.85): their evidence is devalued, their professional standing is subordinate, and their exit is constrained (they remain in organizations that officially designate their work as secondary). Simulated_learning_practitioners sit at even higher d (~0.85–0.95): their entire professional identity depends on contradicting the constraint, so exit is identity-locked; they are forced to operate in a regime that pre-judges their work as insufficient. Organizational_leadership sits near d~0.5 (symmetric)—they bear the deferred cost (competence decay during incident-free periods, eventual catastrophe risk) but also benefit from the constraint (if prevention succeeds, the doctrine's falsity remains hidden; catastrophes are rare enough that leadership can operate as if the problem is solved). Catastrophe_survivors sit at d~1.0 (full target) but are excluded from the calculation because they are powerless and trapped—their directionality is computed but never entered into organizational decision-making until the catastrophe has already occurred.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how do we ensure competence does not decay during long incident-free periods?) is LIVE and URGENT. However, the constraint's claimed solution (catastrophe as necessary learning) does not actually solve the founding problem—it merely accepts the failure state as inevitable. The mandatrophy here is one of FUNCTION SUBSTITUTION: the stated mandate (maintain competence) has been replaced with an operationalized mandate (maintain institutional authority for catastrophe investigation) that is orthogonal or opposed to the original. A competence-maintenance regime centered on continuous improvement, high-fidelity simulation, and near-miss learning would directly address the founding problem. The constraint that 'only catastrophe teaches' achieves the opposite—it permits competence to decay invisibly, justifies deferring investment in prevention, and positions catastrophe as organizationally functional. The mandatrophy is not yet resolved because the constraint persists despite being functionally obsolete (alternatives exist, are demonstrably effective in some high-reliability contexts, and would directly solve the founding problem). The theater ratio's asymptotic rise reflects the mandatrophy: the constraint is increasingly maintained through rhetoric and post-incident analysis (theater) rather than through genuine functional necessity. The doctrine has become a cover story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_decay_causality,
    'Does organizational competence in catastrophe-avoidance decay invisibly during incident-free periods because catastrophe is structurally required to maintain it, or because organizational complacency and lack of continuous exercise produce decay that catastrophe *reveals* but does not *cure*?',
    'Longitudinal competence measurement (skills testing, procedural audits, simulation performance) on organizations with different learning regimes: those relying on catastrophe validation versus those maintaining continuous improvement, near-miss learning, and high-fidelity simulation. Measure competence trajectory before and after incident-free periods in both regime types.',
    'If decay is intrinsic to long incident-free periods regardless of learning regime, the constraint''s framing is accurate. If decay can be prevented through continuous improvement while incident-free periods persist, the constraint is a false necessity—competence can be maintained without catastrophe, and the doctrine functions as suppression of competing learning regimes. High-fidelity measurement of competence during incident-free periods is the critical evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_decay_causality, empirical, 'Whether competence decay during incident-free periods is intrinsic to the organizational condition or a failure of learning regime choice.').

omega_variable(
    simulation_fidelity_threshold,
    'Is there a threshold of simulation fidelity (cognitive load, time pressure, uncertainty, irreversibility of consequences, social/institutional stakes) above which simulation constitutes genuine competence exercise rather than rehearsal? If so, are modern high-fidelity simulators reaching that threshold?',
    'Transfer learning research comparing competence developed in high-fidelity simulation to competence demonstrated in real catastrophe scenarios. Include measurement of cognitive demand, stress response, procedural automaticity, and error recovery. Compare learning transfer rates across fidelity levels.',
    'If a competence-equivalence threshold exists and is being reached by current simulators, then ''simulation_as_sufficient'' becomes empirically defensible and the constraint''s core claim (that only real catastrophe teaches) is falsified. If the threshold remains beyond current simulation capability, the constraint''s framing survives but must narrow its claim to acknowledge that future simulation improvements could change the conclusion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fidelity_threshold, empirical, 'Whether high-fidelity simulation can achieve competence-equivalence to real catastrophe through sufficient replication of cognitive and emotional demand.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of near-miss learning cultures and simulation-based evidence structural (the doctrine officially devalues these sources, creating external barriers to their adoption) or internalized (practitioners themselves believe that simulation is inherently insufficient, creating self-imposed epistemic subordination)?',
    'Post-suppression trajectory analysis: organizations that explicitly reject the catastrophe-as-necessary doctrine and invest in near-miss and simulation learning cultures. Measure whether competence maintenance improves, and whether practitioners formerly operating under suppression continue to experience subordination or recover epistemic confidence when the doctrine is removed.',
    'If suppression is purely structural, removing the doctrine should rapidly shift learning culture and outcomes. If partially internalized, even doctrine-rejection may leave residual epistemic distrust in simulation and near-miss evidence; recovery would require active retraining of evidentiary standards. High internalization indicates the constraint has produced deep identity-lock effects beyond the formal doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether the suppression of alternative learning regimes operates primarily through institutional gatekeeping or through internalized epistemic beliefs.').

omega_variable(
    kernel_reading_committer_stakes,
    'This constraint is one reading of a contested kernel (competence_retention_exercise). If organizations shift to ''near_miss_as_bridge'' or ''simulation_as_sufficient'' readings, how would institutional power structures and resource allocation change? Which organizational seats have stakes in preserving the ''catastrophe_as_necessary'' reading specifically?',
    'Analysis of which stakeholders would lose authority, resources, or institutional standing if the reading shifted. Cross-organizational comparison of investigation-hierarchy size and funding in organizations adopting different readings of the kernel.',
    'If shifting readings would substantially reduce the power and resource allocation of incident investigation hierarchies, that institutional structure has a vested interest in suppressing alternative readings. This would indicate that the kernel contest is not purely epistemological but is being sustained by institutional capture—the constraint is snare-structured because its persistence depends on suppressing competing readings that would redistributed authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_stakes, conceptual, 'Committer-frame analysis: which institutional seats benefit from this specific reading of the kernel, and would their power change if alternative readings were adopted?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__catastrophe_as_necessary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(comp_tr_t0, observed).
narrative_ontology:measurement(comp_tr_t5, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 5, 0.42).
narrative_ontology:measurement_basis(comp_tr_t5, observed).
narrative_ontology:measurement(comp_tr_t10, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 10, 0.46).
narrative_ontology:measurement_basis(comp_tr_t10, observed).
narrative_ontology:measurement(comp_tr_t15, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 15, 0.49).
narrative_ontology:measurement_basis(comp_tr_t15, observed).
narrative_ontology:measurement(comp_tr_t20, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 20, 0.51).
narrative_ontology:measurement_basis(comp_tr_t20, observed).
narrative_ontology:measurement(comp_tr_t25, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 25, 0.52).
narrative_ontology:measurement_basis(comp_tr_t25, observed).
narrative_ontology:measurement(comp_tr_t30, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 30, 0.52).
narrative_ontology:measurement_basis(comp_tr_t30, observed).
narrative_ontology:measurement(comp_tr_t35, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 35, 0.52).
narrative_ontology:measurement_basis(comp_tr_t35, projected).
narrative_ontology:measurement(comp_tr_t40, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 40, 0.52).
narrative_ontology:measurement_basis(comp_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(comp_be_t0, observed).
narrative_ontology:measurement(comp_be_t5, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(comp_be_t5, observed).
narrative_ontology:measurement(comp_be_t10, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(comp_be_t10, observed).
narrative_ontology:measurement(comp_be_t15, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(comp_be_t15, observed).
narrative_ontology:measurement(comp_be_t20, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(comp_be_t20, observed).
narrative_ontology:measurement(comp_be_t25, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(comp_be_t25, observed).
narrative_ontology:measurement(comp_be_t30, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 30, 0.67).
narrative_ontology:measurement_basis(comp_be_t30, observed).
narrative_ontology:measurement(comp_be_t35, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(comp_be_t35, projected).
narrative_ontology:measurement(comp_be_t40, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(comp_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(comp_su_t0, observed).
narrative_ontology:measurement(comp_su_t5, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(comp_su_t5, observed).
narrative_ontology:measurement(comp_su_t10, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(comp_su_t10, observed).
narrative_ontology:measurement(comp_su_t15, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(comp_su_t15, observed).
narrative_ontology:measurement(comp_su_t20, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(comp_su_t20, observed).
narrative_ontology:measurement(comp_su_t25, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(comp_su_t25, observed).
narrative_ontology:measurement(comp_su_t30, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(comp_su_t30, observed).
narrative_ontology:measurement(comp_su_t35, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(comp_su_t35, projected).
narrative_ontology:measurement(comp_su_t40, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(comp_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__catastrophe_as_necessary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_retention_exercise__catastrophe_as_necessary, 0.12).
narrative_ontology:affects_constraint(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise__near_miss_as_bridge).
narrative_ontology:affects_constraint(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise__simulation_as_sufficient).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'competence_retention_exercise.' Sibling readings 'near_miss_as_bridge' and 'simulation_as_sufficient' are separate constraint stories in the same family. All three stories share a common founding problem (how to maintain competence during incident-free periods) but propose fundamentally different solutions with different beneficiary structures and different epistemic gatekeeping mechanisms. The 'catastrophe_as_necessary' reading (this story) forecloses 'simulation_as_sufficient' logically and coexists with 'near_miss_as_bridge' across different organizational positions. Decomposition by ε-invariance principle: the three readings have different ε values because they instantiate different constraint types—this one (snare, ε~0.68) structures extraction of epistemic authority through doctrine; near-miss-as-bridge would instantiate a rope-like coordination (genuine prevention without extraction); simulation-as-sufficient would instantiate a mountain-like claim if simulation fidelity permits genuine equivalence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_retention_exercise__catastrophe_as_necessary, moderate, 0.88).
constraint_indexing:directionality_override(competence_retention_exercise__catastrophe_as_necessary, moderate, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
