% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__near_miss_as_bridge
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: competence_retention_exercise__near_miss_as_bridge
 *   human_readable: Near-Miss Integration as Competence Validation Bridge
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   High-reliability organizations maintain safety competence in domains
 *   where actual catastrophes are rare and unacceptable — commercial
 *   aviation, nuclear power, intensive care medicine, deep-sea operations.
 *   The competence-maintenance problem: simulator training alone cannot keep
 *   practitioners' mental models current with evolving hazards, yet waiting
 *   for catastrophes to teach is ethically and operationally unacceptable.
 *   This reading claims that systematized near-miss investigation — active
 *   collection of incidents that did not cascade to failure, root-cause
 *   analysis, and integration into simulator curricula — provides sufficient
 *   real-world feedback to bridge the gap. Near-miss incidents carry high
 *   information density (they reveal latent failure modes) without the
 *   catastrophic consequence that makes learning organizationally disruptive.
 *   This constraint describes the institutional architecture of
 *   near-miss-driven competence validation: investigation infrastructure,
 *   reporting mandates, training integration feedback loops, and regulatory
 *   enforcement. The reading is contested by two siblings: one asserts only
 *   catastrophes generate the visceral learning stakes required for genuine
 *   competence; another asserts high-fidelity simulation alone suffices
 *   without real-world validation.
 *
 * KEY AGENTS:
 *   - Regulatory bodies (FAA, NRC, ICAO, medical boards) — institutional agenda-setters for near-miss mandates
 *   - Safety culture researchers and human factors experts — beneficiaries whose expertise is legitimated by the arrangement
 *   - Operational organizations (airlines, hospitals, nuclear plants) — institutional payers bearing investigation and integration costs
 *   - Frontline practitioners (pilots, physicians, operators) — beneficiaries whose training is grounded in organizational reality; identity-locked exit
 *   - Incident investigation and training software vendors — organized payers constrained by mandate
 *   - Catastrophe-prevention framers (excluded) — would argue learning requires actual crisis stakes
 *   - Simulation-sufficiency framers (excluded) — would argue real-world feedback is unnecessary if simulation is high-fidelity enough
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__near_miss_as_bridge, 0.38).
domain_priors:suppression_score(competence_retention_exercise__near_miss_as_bridge, 0.22).
domain_priors:theater_ratio(competence_retention_exercise__near_miss_as_bridge, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, extractiveness, 0.38).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__near_miss_as_bridge, rope).
narrative_ontology:human_readable(competence_retention_exercise__near_miss_as_bridge, "Near-Miss Integration as Competence Validation Bridge").
narrative_ontology:topic_domain(competence_retention_exercise__near_miss_as_bridge, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_retention_exercise__near_miss_as_bridge).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__near_miss_as_bridge, 'e9a0b78f-ed54-44d9-a136-acb47f3fa339').
narrative_ontology:cs_kernel_codification('e9a0b78f-ed54-44d9-a136-acb47f3fa339', formalized).
narrative_ontology:cs_authority_grounding('e9a0b78f-ed54-44d9-a136-acb47f3fa339', extraction).
narrative_ontology:cs_interpretation_layer_present('e9a0b78f-ed54-44d9-a136-acb47f3fa339').
narrative_ontology:cs_reading_relation('e9a0b78f-ed54-44d9-a136-acb47f3fa339', competence_retention_exercise__catastrophe_as_necessary, forecloses).
narrative_ontology:cs_reading_relation('e9a0b78f-ed54-44d9-a136-acb47f3fa339', competence_retention_exercise__simulation_as_sufficient, influences).
narrative_ontology:cs_axiom('e9a0b78f-ed54-44d9-a136-acb47f3fa339', foundational, near_miss_data_suffices_for_competence_validation).
narrative_ontology:cs_axiom_status(near_miss_data_suffices_for_competence_validation, holdable).
narrative_ontology:cs_axiom_grounding('e9a0b78f-ed54-44d9-a136-acb47f3fa339', near_miss_data_suffices_for_competence_validation, empirically_contingent).
narrative_ontology:cs_axiom('e9a0b78f-ed54-44d9-a136-acb47f3fa339', foundational, catastrophe_not_necessary_for_learning).
narrative_ontology:cs_axiom_status(catastrophe_not_necessary_for_learning, holdable).
narrative_ontology:cs_axiom_grounding('e9a0b78f-ed54-44d9-a136-acb47f3fa339', catastrophe_not_necessary_for_learning, deontological).
narrative_ontology:cs_reference_frame('e9a0b78f-ed54-44d9-a136-acb47f3fa339', hybrid_learning_system_necessity).
narrative_ontology:cs_drift_state('e9a0b78f-ed54-44d9-a136-acb47f3fa339', contemporary_regulatory_implementation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e9a0b78f-ed54-44d9-a136-acb47f3fa339', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, safety_culture_advocates).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, regulatory_compliance_bodies).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, operational_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, frontline_practitioners).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, operational_organizations).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, simulation_training_vendors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Professional communities (human factors researchers, safety engineers, high-reliability organization theorists) who advocate for systematized near-miss investigation and integration into training. They benefit from institutional acceptance of the hybrid approach — it legitimates their expertise in incident investigation and validates their theoretical frameworks. Their exit is mobility across organizations and publication venues.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, safety_culture_advocates, beneficiary,
    organized, generational, mobile, global).

% Aviation, nuclear, maritime, and medical safety regulators who mandate near-miss reporting systems and require evidence of integration into training. They set the standards, enforce compliance through audit, and benefit from institutional legitimacy when organizations can demonstrate learning loops closing. Their exit is only structural change to regulatory authority.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, regulatory_compliance_bodies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__near_miss_as_bridge, regulatory_compliance_bodies, beneficiary).

% Airlines, hospitals, nuclear plants, maritime operators who must maintain competence in their workforce. They bear the cost of near-miss investigation programs (personnel time, systemization overhead, integration into training pipelines), but benefit from improved safety culture and regulatory compliance. Exiting means regulatory sanctions or loss of operating license.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, operational_organizations, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__near_miss_as_bridge, operational_organizations, beneficiary).

% Pilots, physicians, nuclear operators, ship captains whose competence is maintained through simulator training updated by near-miss investigation. They benefit from realistic, organizationally-grounded training that reflects actual hazards in their operational context. Exit means career abandonment — professional identity is constituted through the role.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, frontline_practitioners, beneficiary,
    moderate, biographical, identity_locked, global).

% Specialized investigative teams, incident analysis software vendors, safety management consultancies that conduct and monetize near-miss investigation. They benefit from mandated reporting and integration requirements. Their exit is redeployment to other safety domains or adjacent markets.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, incident_investigation_infrastructure, agenda_setter,
    organized, generational, arbitrage, global).

% Simulator manufacturers and flight/medical training centers whose training content must be validated against and updated by real-world incident data. They bear the cost of integration infrastructure and curriculum updates. Exiting means losing contracts with regulatory-compliant organizations.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, simulation_training_vendors, payer,
    organized, biographical, constrained, global).

% The doctrine/framework that only catastrophic events generate the organizational and cognitive stakes necessary for genuine learning — this reading excludes and contradicts that framing by asserting that near-miss investigation can bridge the gap without catastrophe. Its proponents would argue for catastrophe-based learning but are structurally excluded by the near-miss mandate.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, catastrophe_prevention_framing, excluded,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(competence_retention_exercise__near_miss_as_bridge, catastrophe_prevention_framing).

% The doctrine that high-fidelity simulation alone provides sufficient validation of competence without real-world feedback loops — this reading creates pressure against that framing by insisting near-miss integration is necessary to keep simulators grounded in operational reality.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, simulation_sufficiency_framing, excluded,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(competence_retention_exercise__near_miss_as_bridge, simulation_sufficiency_framing).

% Empirical researchers measuring whether near-miss integration actually closes the gap between simulator-trained competence and real-world decision-making under stress, or whether catastrophe remains the primary teacher.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, observer_seat, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__near_miss_as_bridge, regulatory_compliance_bodies).
narrative_ontology:fixing_cost_class(competence_retention_exercise__near_miss_as_bridge, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains competence in safety-critical domains where catastrophes are rare, practice is infrequent, and pure simulation cannot fully ground training in operational hazard evolution. By systematizing near-miss investigation and feeding results into simulator design, the arrangement solves the competence-maintenance problem without requiring catastrophic events to teach.
% TRANSFER_FUNCTION: Moves investigative labor, training development effort, and reporting compliance burden from ad-hoc crisis response into institutionalized near-miss investigation and simulator update pipelines. Transfers analytical authority from catastrophe-response expertise into incident-investigation expertise.
% ABSENT_VOICES: Practitioners in low-formality or non-unionized organizations without mature incident investigation infrastructure are structurally excluded from both the benefits (grounded training) and the formal investigation system. Catastrophe-as-necessary advocates (researchers, practitioners, organizational theorists who believe real crises are irreplaceable teachers) are also excluded — their position would argue for different competence-maintenance strategies but is structurally overridden by regulatory mandate.
% DISAPPEARANCE_RATIONALE: If near-miss investigation mandates and simulator integration requirements vanished, competence maintenance would revert to simulation-only training (increasingly decoupled from real-world hazard evolution) or reliance on catastrophic failures (rare, ethically unacceptable) to drive learning. Organizations would lose regulatory pressure to close the feedback loop between real incidents and training design. Competence drift and incident rates would likely increase in the medium to long term.
% FOUNDING_PROBLEM: High-reliability organizations must maintain practitioner competence in domains where actual catastrophes are rare (unacceptable as learning opportunities) and frequent practice is impossible. Simulators provide routine skill preservation but cannot fully ground practitioners' mental models in the evolving hazard landscape of real operations. Traditional learning from catastrophes is too rare and ethically unacceptable. The founding solution: systematize collection of near-miss incidents, conduct root-cause investigation, and feed findings into simulator scenario design to provide real-world feedback that keeps training current without waiting for catastrophes.
% FOUNDING_PROBLEM_CORROBORATION: Aviation safety authorities (FAA, ICAO, pilot associations) and independent HRO researchers (Weick, Reason, Dekker, Leveson) attest that competence-maintenance in low-incident-rate domains remains unsolved and is a persistent challenge in safety culture. Regulatory agencies mandate near-miss reporting and integration, treating the founding problem as live. However, some researchers (catastrophe-as-necessary advocates) dispute whether near-miss integration is actually sufficient — they contend the founding problem is unsolvable without real catastrophic learning events. The founding problem is live and contested; the solution (near-miss integration) is a proposed bridge whose sufficiency remains empirically unresolved.
narrative_ontology:disappearance_verdict(competence_retention_exercise__near_miss_as_bridge, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__near_miss_as_bridge, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__near_miss_as_bridge, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_retention_exercise__near_miss_as_bridge, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__near_miss_as_bridge, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.38 at plateau) and initially climbing (0.22 to 0.38 over the first 25 time units), then stable. The climb reflects adoption cost and investigative infrastructure overhead — organizations must build reporting systems, hire investigators, and redesign training pipelines. The plateau indicates that at mature integration, the per-incident investigative cost stabilizes as a routine organizational function. Extractiveness does not reach high (>0.6) because the arrangement solves a genuine coordination problem (competence maintenance without catastrophe), and beneficiaries (practitioners, regulators) genuinely value the outcome. Suppression is low (0.22) and declining, indicating that the constraint is not sustained by coercion or exit-blocking — organizations adopt near-miss integration because it improves safety outcomes, and practitioners voluntarily feed incidents into the system. Theater ratio is initially high (0.35 at t=0) and declines to low (0.18) as practices mature, suggesting early-stage performative compliance (check-box reporting) that genuinely converts to functional incident-to-training feedback loops over time. This decline is positive signal: theater decreases as the coordination function becomes real. Accessibility collapse (0.42) is moderate: near-miss integration is not inevitable (alternative framings assert catastrophe or simulation suffice), but once the institutional framework is established, exiting it requires regulatory violation or losing operating license. Resistance (0.55) is substantial: practitioners and some safety researchers actively resist the premise by arguing catastrophes alone teach genuine competence, or that simulation alone should suffice. The measurement series tracks the interval from early adoption (t=0) through institutional maturation (t=40), with all metrics sampled on one shared grid.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (regulators) experiences the constraint as a legitimate mandate enforcing safety standards. The organizational payer seats (airlines, hospitals) experience it as routine compliance cost that improves safety metrics and reduces regulatory friction — a genuine benefit-cost ratio, not extraction. The frontline practitioner seat (pilots, physicians) experiences it as grounding their training in real hazards, improving competence confidence — a benefit. The vendor seat (training and investigation firms) experiences it as regulatory demand creating market opportunity, offset by integration and content-update costs — near-symmetric. Only the excluded framers (catastrophe-necessity, simulation-sufficiency advocates) experience it as delegitimizing and constraining their alternative theories. The engine should compute the constraint differently from each seat: from the agenda-setter and organizational seats, it is coordination (moderate extracted cost supporting genuine safety function); from the excluded framers, it is suppression (their alternative theories are ruled out by mandate, not by evidence).
 *
 * DIRECTIONALITY LOGIC:
 *   No victims are declared — the arrangement genuinely benefits all seated parties: regulators get compliance infrastructure, organizations get improved safety culture and regulatory legitimacy, practitioners get grounded training, vendors get market demand. The beneficiary seats (safety advocates, regulators, organizations, practitioners) have low directionality (d toward beneficiary end) because they collect or benefit without bearing disproportionate costs. The constrained and identity-locked practitioners sit near symmetric (costs of participation ≈ benefits of grounded training). The excluded framers are not seated — they are doctrinal positions overridden by mandate, not agents with negotiable exits. No directionality override is needed because the derivation chain (beneficiaries + moderate/institutional power + constrained/identity-locked exit) naturally produces the right relationship: this is coordination with selective beneficiary capture, not pure extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is 'maintain safety competence in low-incident-rate systems without relying on catastrophes to train.' The near-miss-as-bridge reading asserts that active near-miss investigation and integration into simulator curricula solves this problem without requiring catastrophes. The constraint's classification (rope: genuine coordination) depends on whether the founding problem remains live and whether near-miss integration is actually sufficient to solve it. If near-miss integration proves insufficient — if incidents keep occurring despite updated training, or if practitioners still exhibit competence gaps not caught by near-miss feedback — then the reading's founding problem persists unsolved and the constraint becomes zombie or piton (persists despite failure to solve its founding problem). Alternatively, if catastrophes actually decline measurably while near-miss integration is in place, the founding problem is solved and the constraint retains rope classification. The measurement series tracks theater decline (0.35 to 0.18) as a positive signal: early performative adoption (just-filing-reports) converts to functional integration (real training updates and improved hazard awareness). If theater were to rise back above 0.5, that would signal zombie piton — the constraint persists as regulatory requirement but no longer solves the competence problem. The mandatrophy question: Is near-miss integration genuinely sufficient as a bridge, or is the founding problem fundamentally unsolvable without catastrophic learning events? The empirical answer (live HRO data on incident rates, competence assessments, training effectiveness) will resolve this. Until resolved: the constraint is claimed as rope, but the sufficiency of near-miss integration remains omega (see omega_near_miss_sufficiency below).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    near_miss_sufficiency,
    'Is the information density and emotional/cognitive weight of near-miss investigation sufficient to maintain practitioner competence mental models, or do actual catastrophic failures provide irreducible learning that near-misses cannot replace?',
    'Longitudinal empirical study comparing competence maintenance outcomes (incident rates, practitioner decision-making quality, hazard recognition) across HROs with mature near-miss integration, HROs relying primarily on simulation, and HROs with historical catastrophe-based learning. Post-catastrophe competence rebounds are measured against pre-catastrophe baselines; near-miss integration groups are measured against control groups without systematic integration.',
    'If near-miss integration maintains competence and incident rates improve, the rope classification holds and the founding problem is solved. If incidents continue despite near-miss integration, or if competence gaps emerge despite updated training, the constraint becomes zombie piton (persists as regulatory mandate but fails to solve the problem). If practice diverges sharply between simulation-only and near-miss-integrated groups, near-miss integration moves from rope toward tangled-rope (genuine function but with higher cost/extraction than initially measured).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(near_miss_sufficiency, empirical, 'Whether near-miss data suffices for competence maintenance or whether catastrophe provides irreducible learning.').

omega_variable(
    catastrophe_suppression_by_mandate,
    'Does regulatory mandating of near-miss integration suppress the catastrophe-as-necessary learning theory, or are both frameworks compatible within the same organizational system?',
    'Organizational ethnography and interview study: ask practitioners and investigators whether the near-miss mandate enables catastrophe-based learning (by creating investigation infrastructure and organizational readiness) or blocks it (by directing resources away from catastrophe-response preparation). Examine whether organizations with mature near-miss integration handle actual catastrophes better or worse than those without.',
    'If the two frameworks are compatible (near-miss integration improves catastrophe response capability), the constraint is rope for both readings. If they compete (near-miss integration diverts resources from catastrophe preparation), the constraint forecloses catastrophe-as-necessary and becomes snare from that framing''s perspective. This determines whether the reading_relations value is ''coexists_with'' or ''forecloses''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_suppression_by_mandate, conceptual, 'Whether near-miss integration and catastrophe-based learning are compatible frameworks or structural alternatives.').

omega_variable(
    simulation_fidelity_threshold,
    'At what level of simulator fidelity and scenario realism does simulation alone (without near-miss integration) produce competence maintenance equivalent to near-miss-integrated systems?',
    'Experimental design: deploy three matched groups in high-reliability domains (aviation, medical simulation) — pure simulation training with maximal fidelity, pure near-miss integration without new simulator content, and hybrid near-miss+simulation. Measure competence retention, hazard recognition, and decision-making quality at equivalent time points. If pure simulation produces equivalent outcomes, the threshold is met; if hybrid outperforms, simulation alone is insufficient.',
    'If simulation-alone suffices at high fidelity, this reading''s distinctive claim (that near-miss integration is necessary) is weakened — the constraint moves from rope to piton (both methods work, but near-miss integration persists as regulatory mandate despite diminishing marginal return). If hybrid consistently outperforms simulation-alone, near-miss integration is not dispensable and the rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fidelity_threshold, empirical, 'The threshold simulator fidelity at which real-world feedback becomes redundant vs. necessary.').

omega_variable(
    extraction_from_investigation_asymmetry,
    'Do investigative teams, regulators, or training vendors extract disproportionate benefit (status, funding, market capture) from near-miss mandates relative to the operational organizations that fund the investigation infrastructure?',
    'Cost-benefit audit: track investigative labor, regulatory overhead, vendor revenue, and training content refresh costs across a cohort of HROs. Compare benefits accrued (incident reduction, regulatory compliance, training improvement) to costs borne by operational organizations. Measure whether investigation infrastructure growth is proportional to actual incident prevention benefit or driven by career incentives of investigators and vendor capture.',
    'If investigation infrastructure extracts disproportionate benefit, the constraint shifts from rope toward tangled-rope (genuine coordination function, but with asymmetric benefit distribution). If costs are proportional to benefits, the rope classification holds. High extraction asymmetry would surface a hidden victim set: operational organizations paying for infrastructure that primarily benefits professional investigators and vendors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_from_investigation_asymmetry, empirical, 'Whether near-miss investigation infrastructure extracts value or proportionally funds genuine safety improvement.').

omega_variable(
    identity_lock_practitioner_voice,
    'Is the low measured resistance (0.55) and high beneficiary status of frontline practitioners genuine, or does identity-lock to professional role prevent voiced objection to investigation burden and training overhead?',
    'Post-exit practitioner interviews: survey practitioners who have left the profession (early retirement, career change) and ask whether investigation participation, reporting burden, and continuous simulator retraining were experienced as beneficial (as the on-role measurement assumes) or as extractive overhead. Compare on-role and post-exit sentiment. If exit enables criticism, the exit options classification (identity_locked) is correct and the beneficiary status is partly artifact of identity fusion, not genuine preference.',
    'If exit practitioners consistently report investigation burden as extractive, the suppression metric may be higher than measured (0.22), and the constraint may be partially snare from the practitioner seat (suppression internalized through professional identity). The rope classification would hold from agenda-setter and organizational seats, but practitioners might compute tangled-rope or snare. This affects seat divergence analysis and may reveal identity-lock as a suppression mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_practitioner_voice, empirical, 'Whether practitioner identity-lock masks suppression or reflects genuine beneficial coordination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__near_miss_as_bridge, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0, 0.35).
narrative_ontology:measurement(comp_tr_t5, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 5, 0.28).
narrative_ontology:measurement(comp_tr_t10, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 10, 0.23).
narrative_ontology:measurement(comp_tr_t15, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 15, 0.2).
narrative_ontology:measurement(comp_tr_t20, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 20, 0.19).
narrative_ontology:measurement(comp_tr_t25, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 25, 0.18).
narrative_ontology:measurement(comp_tr_t30, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 30, 0.18).
narrative_ontology:measurement(comp_tr_t40, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 40, 0.18).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(comp_be_t5, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 5, 0.26).
narrative_ontology:measurement(comp_be_t10, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 10, 0.31).
narrative_ontology:measurement(comp_be_t15, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(comp_be_t20, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(comp_be_t25, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(comp_be_t30, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(comp_be_t40, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(comp_su_t5, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 5, 0.25).
narrative_ontology:measurement(comp_su_t10, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 10, 0.24).
narrative_ontology:measurement(comp_su_t15, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 15, 0.23).
narrative_ontology:measurement(comp_su_t20, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(comp_su_t25, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 25, 0.22).
narrative_ontology:measurement(comp_su_t30, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 30, 0.22).
narrative_ontology:measurement(comp_su_t40, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 40, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__near_miss_as_bridge, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_retention_exercise__near_miss_as_bridge, 0.12).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise__catastrophe_as_necessary).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise__simulation_as_sufficient).

% DUAL FORMULATION NOTE:
% This constraint is one reading (near_miss_as_bridge) of a triadic kernel contest: 'competence_retention_exercise'. The kernel is the standing commitment to maintain safety competence in low-incident-rate high-reliability organizations. This reading asserts near-miss investigation and simulator integration solves the competence problem without catastrophe. Sibling reading catastrophe_as_necessary asserts only catastrophic learning events provide irreducible stakes; sibling reading simulation_as_sufficient asserts high-fidelity simulation alone suffices. The three readings share the same referent (the competence maintenance challenge) but diverge on what constitutes sufficient learning feedback. They are not alternative measurements of one constraint — they are structurally distinct claims with different ε values, beneficiary/victim structures, and enforcement mechanisms. Linked via affects_constraints: near-miss integration creates institutional pressure against both catastrophe-necessity doctrine (by providing an alternative learning pathway) and simulation-only doctrine (by insisting real-world validation is necessary).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_retention_exercise__near_miss_as_bridge, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
