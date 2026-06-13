% ============================================================================
% CONSTRAINT STORY: competence_occupation__hybrid_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__hybrid_occupation, []).

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
 *   constraint_id: competence_occupation__hybrid_occupation
 *   human_readable: Competence Occupation via Hybrid Training Mechanisms
 *   domain: safety/organizational/epistemic
 *
 * SUMMARY:
 *   High-reliability organizations (aviation, nuclear, emergency response,
 *   healthcare) face a persistent constraint: personnel must occupy and
 *   maintain genuine competence in safety-critical skills under stress, but
 *   no consensus exists on the optimal configuration of training mechanisms
 *   to achieve this. Training administration mandates a multi-mechanism
 *   regime (simulator + classroom refresher + line audit + procedural drill)
 *   to cover skill decay, varied context, and regulatory compliance. The
 *   hybrid reading holds that multiple mechanisms are NECESSARY because each
 *   mechanism captures a different competence boundary (simulator tests
 *   manual control, classroom tests knowledge retention, line audit tests
 *   real-world judgment, procedure reinforcement tests memory under
 *   operational pressure) and no single mechanism is sufficient. The
 *   constraint is structurally extractive because: (1) the regime absorbs
 *   substantial operational time and administrative cost with no clear
 *   optimality proof; (2) training administration controls the regime design
 *   and collects institutional credit regardless of outcome; (3) operational
 *   personnel are identity-locked (exit means leaving their profession) and
 *   bear the training burden without voice in configuration; (4) regulatory
 *   authority benefits from documented compliance without scrutinizing the
 *   regime's actual efficacy. The claim/metric divergence is deliberate: the
 *   constraint is CLAIMED as tangled_rope (genuine coordination problem +
 *   asymmetric extraction) but the theater_ratio (0.41) and
 *   suppression_requirement (0.52) metrics suggest substantial performative
 *   maintenance — some training elements are compliance theater, some address
 *   genuine skill decay, with no consensus on the boundary.
 *
 * KEY AGENTS:
 *   - training_administration: institutional agenda-setter; controls regime design and compliance measurement
 *   - operational_personnel: moderate-power payers; identity-locked; bear training burden without design voice
 *   - regulatory_authority: institutional beneficiary; mandates standards but not mechanisms; receives compliance documentation
 *   - front_line_supervisors: powerful payers; observe field-training correlation empirically; cannot redesign regime
 *   - incident_investigation_bodies: institutional observers; examine post-incident training compliance; find mixed causality
 *   - research_community: organized observers; structurally excluded from operational data and regime modification authority
 *   - alternative_training_frameworks: moderate-power excluded; barred by regulatory lock-in despite potentially superior cost-benefit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, 0.68).
domain_priors:suppression_score(competence_occupation__hybrid_occupation, 0.52).
domain_priors:theater_ratio(competence_occupation__hybrid_occupation, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__hybrid_occupation, tangled_rope).
narrative_ontology:human_readable(competence_occupation__hybrid_occupation, "Competence Occupation via Hybrid Training Mechanisms").
narrative_ontology:topic_domain(competence_occupation__hybrid_occupation, "safety/organizational/epistemic").

domain_priors:requires_active_enforcement(competence_occupation__hybrid_occupation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__hybrid_occupation, 'df8e569e-5900-41f0-bd1d-90d6b3fd2a45').
narrative_ontology:cs_kernel_codification('df8e569e-5900-41f0-bd1d-90d6b3fd2a45', distributed).
narrative_ontology:cs_authority_grounding('df8e569e-5900-41f0-bd1d-90d6b3fd2a45', extraction).
narrative_ontology:cs_interpretation_layer_present('df8e569e-5900-41f0-bd1d-90d6b3fd2a45').
narrative_ontology:cs_reading_relation('df8e569e-5900-41f0-bd1d-90d6b3fd2a45', competence_occupation__simulation_sufficiency, influences).
narrative_ontology:cs_reading_relation('df8e569e-5900-41f0-bd1d-90d6b3fd2a45', competence_occupation__real_incident_necessity, influences).
narrative_ontology:cs_axiom('df8e569e-5900-41f0-bd1d-90d6b3fd2a45', foundational, multi_mechanism_necessity).
narrative_ontology:cs_axiom_status(multi_mechanism_necessity, holdable).
narrative_ontology:cs_axiom_grounding('df8e569e-5900-41f0-bd1d-90d6b3fd2a45', multi_mechanism_necessity, empirically_contingent).
narrative_ontology:cs_axiom('df8e569e-5900-41f0-bd1d-90d6b3fd2a45', foundational, configuration_optimization_unsettled).
narrative_ontology:cs_axiom_status(configuration_optimization_unsettled, holdable).
narrative_ontology:cs_axiom_grounding('df8e569e-5900-41f0-bd1d-90d6b3fd2a45', configuration_optimization_unsettled, conventional).
narrative_ontology:cs_reference_frame('df8e569e-5900-41f0-bd1d-90d6b3fd2a45', evidence_driven_multi_mechanism_optimization).
narrative_ontology:cs_drift_state('df8e569e-5900-41f0-bd1d-90d6b3fd2a45', contemporary_administrative_lock_in, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('df8e569e-5900-41f0-bd1d-90d6b3fd2a45', '').
narrative_ontology:cs_kernel_id(competence_occupation__hybrid_occupation, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, training_administration).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, institutional_safety_posture).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, operational_personnel).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, front_line_staff).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, regulatory_authority).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, front_line_supervisors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, schedules, and mandates the training regime (simulations, refresher courses, line audits, procedural reinforcement drills). Justifies the multi-mechanism approach as necessary to cover skill decay, real-world variety, and regulatory compliance. Collects institutional credit for safety posture; controls budget, evaluation metrics, and certification standards. Can adapt the regime without leaving their role.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, training_administration, agenda_setter,
    institutional, generational, arbitrage, global).

% Must continuously cycle through multiple training modalities (simulator sessions, classroom refreshers, line audits, procedural drills) to maintain certification and remain employable. Time spent in training is time not doing productive work. The regime is presented as necessary for their safety and competence; they experience it as administratively intensive, often repetitive, and without clear connection between mechanism and actual skill maintenance. Exit means leaving the profession.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, operational_personnel, payer,
    moderate, biographical, identity_locked, regional).

% Mandates competence occupation standards but does not specify the precise mechanism mix. Receives documented evidence of multi-method training completion as proof of regulatory compliance. Benefits from the perception that rigorous, diverse training prevents incidents; bears no direct cost of the training burden itself. Can adjust regulatory language but does not manage training operations.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, regulatory_authority, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, regulatory_authority, agenda_setter).

% Manage the operational consequence: personnel are unavailable during training cycles, which compresses operational capacity. They see some training as valuable, some as redundant box-checking. They cannot redesign the regime, only schedule around it. They observe which training modalities correlate with actual field performance and which do not.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, front_line_supervisors, payer,
    powerful, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, front_line_supervisors, observer).

% Post-incident, examine whether the failed personnel had completed all mandated training elements. Often find that incidents occur despite full training compliance, or that a single modality gap (e.g., a missed line audit) correlates with failure while other completed elements did not. Their findings shape regulatory and administrative pressure but do not resolve the underlying question of optimal configuration.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, incident_investigation_bodies, observer,
    institutional, generational, analytical, national).

% Would study skill decay curves, simulator-to-field transfer, and optimal refresher intervals to ground the multi-mechanism design in evidence; is systematically excluded from real-time operational data, access to repeated incident investigations, and authority to modify regimes for experimental purposes. Their absence ensures the training design persists as administrative tradition rather than evidence-driven practice.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, research_community, excluded,
    organized, generational, trapped, global).

% Competing frameworks (simulation-only, incident-response learning, peer-mentorship, just-in-time training) exist in other domains or institutions but are structurally barred from adoption in highly-regulated sectors because the current multi-mechanism mandate is codified in regulation and institutional policy. Their exclusion is what enforcement machinery exists to maintain.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, alternative_training_frameworks, excluded,
    moderate, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__hybrid_occupation, training_administration).
narrative_ontology:fixing_cost_class(competence_occupation__hybrid_occupation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a shared understanding of what 'competence occupation' means and provides verifiable evidence that personnel meet that standard through multiple measurement channels (simulator performance, classroom assessment, field observation, procedure mastery). Solves the problem: how does an organization ensure its personnel remain genuinely capable despite time, stress, and skill decay?
% TRANSFER_FUNCTION: Moves time, attention, and budget from operational work to training administration. Personnel undergo recurring training cycles; supervisors manage scheduling friction; training administration allocates resources; regulatory authority receives compliance documentation.
% ABSENT_VOICES: Operational personnel could articulate which training modalities actually change their field behavior; research communities could provide evidence on skill decay rates and transfer efficacy; alternative training frameworks from other domains could suggest lower-cost configurations. All three are structurally absent from the design process: personnel lack voice in regime design, research access is restricted, and competing frameworks are barred by regulatory lock-in.
% DISAPPEARANCE_RATIONALE: If the multi-mechanism mandate vanished overnight, organizations would shift to lower-cost training (likely simulator-only or incident-response-based), regulatory compliance would be redefined around outcome measures rather than activity logs, and operational capacity would increase. The world would reorganize around a flatter, less-documented training architecture.
% FOUNDING_PROBLEM: Historical incidents showed that personnel who had not recently exercised critical skills in varied contexts (simulator, classroom, field observation, procedure reinforcement) failed under stress. The founding problem: how to ensure competence occupation under realistic operational and stress conditions without waiting for actual incidents.
% FOUNDING_PROBLEM_CORROBORATION: Training administration and regulatory authority assert the founding problem remains live: incidents still occur, skill decay is documented, and varied exercise is necessary. Operational personnel and supervisors assert that incidents often occur despite full training compliance and that the founding problem (ensuring genuine competence) is not solved by the current mechanism mix. Research bodies, when allowed to analyze data, produce mixed findings: some modalities correlate with field performance, others do not, but no consensus emerges on optimal configuration. Legislative oversight bodies have begun questioning whether the multi-mechanism regime is evidence-justified or administrative tradition.
narrative_ontology:disappearance_verdict(competence_occupation__hybrid_occupation, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__hybrid_occupation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__hybrid_occupation, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(competence_occupation__hybrid_occupation, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__hybrid_occupation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_occupation__hybrid_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.58 to 0.68 over the interval (0→40) as administrative overhead accumulates and the regime becomes inertial — new mechanisms are added but old ones are never removed, creating a ratchet effect. Theater ratio rises from 0.32 to 0.41, indicating growing performative activity: newer audits and refreshers become primarily compliance documentation rather than skill validation, especially after the first 15 years when training administration has established full monitoring infrastructure. Suppression is moderate (0.44→0.52) because operational personnel cannot exit without leaving their profession (identity-lock), so the regime's persistence does not require high coercive force — the lock itself is the suppression. Accessibility collapse (0.71) is moderately high because the competence occupation kernel is genuinely necessary (incidents do occur when personnel are out of practice) but the specific multi-mechanism configuration is contestable — alternatives exist (simulation-only, incident-response-based) but are barred by regulatory codification, so the choice set for any given organization is largely predetermined. Resistance (0.58) is moderate because operational personnel resist the time burden and supervisors resist the scheduling impact, but the resistance is diffuse and lacks institutional power — no unified coalition forms because training administration and regulatory authority align on the regime's necessity and operational voices are institutionally subordinate.
 *
 * PERSPECTIVAL GAP:
 *   Training administration and regulatory authority should compute as beneficiaries seeing genuine coordination (competence occupation IS a real problem; the regime addresses it with multiple mechanisms covering different failure modes). Operational personnel should compute as targets experiencing enforced extraction (they bear the time cost and identity-lock prevents exit; the regime's necessity claim is not in dispute, but its configuration is arbitrary and extractive). Front-line supervisors sit in a dual position: they see both the genuine coordination problem (incompetent personnel fail under stress) and the performative over-bureaucratization (compliance theater). The engine should compute divergent types across seats: agenda-setter seat as tangled_rope with net benefit; payer seat as snare-inflected rope (genuine coordination necessity mixed with extractive configuration lock). Research community and alternative frameworks, being excluded, should not compute at all in the seat-and-directionality frame — their absence is itself the structural fact.
 *
 * DIRECTIONALITY LOGIC:
 *   Training administration (institutional, arbitrage exit) → d ≈ 0.15 (beneficiary: controls regime, collects institutional credit, can adapt design or leave). Operational personnel (moderate, identity-locked) → d ≈ 0.75 (target: must undergo training, exit costs entire profession identity, no design voice). Regulatory authority (institutional, analytical) → d ≈ 0.25 (beneficiary: receives compliance documentation, bears no training cost, can adjust regulatory language). Front-line supervisors (powerful, constrained) → d ≈ 0.60 (target: bear scheduling friction, cannot redesign, but have some informal influence over local prioritization). Research community and alternative frameworks (excluded, trapped) → not seated in directionality frame (they are absent from the decision structure). The multi-seat d profile captures why the regime persists despite pushback: training administration (d=0.15) and regulatory authority (d=0.25) have low extraction from their perspective and see genuine coordination value; operational personnel (d=0.75) bear high extraction but lack institutional power to change the design; supervisors (d=0.60) feel the friction but are not the primary target.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid_occupation reading asserts that competence occupation requires multi-mechanism exercise — no single observable (simulator performance, line audit score, classroom exam) is sufficient to verify genuine occupation. This is a DIFFERENT claim from saying each mechanism must be used forever or that all mechanisms carry equal weight. The mandatrophy risk is that the founding problem (ensuring genuine competence under stress) is being substituted with the administrative problem (documenting completion of all mandated elements). Incidents occur despite full training compliance, suggesting the regime is not measuring what it claims to measure. The theater_ratio (0.41, rising) indicates growing performative activity: newer elements (e.g., quarterly refreshers added after the first incident wave) are primarily compliance documentation. A mandatrophy reading would hold that (1) the founding problem is solved or mitigated (competence occupation is approximately maintained; incidents are now rare), but (2) the regime persists because training administration benefits from its continuation and regulatory authority has no mechanism to retire it. The measurement series supports this: base_extractiveness stabilizes at 0.68 after year 25, indicating the regime reaches equilibrium as an extractive arrangement, no longer defending against a live threat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanism_sufficiency_boundary,
    'Is each of the four mechanisms (simulator, classroom, line audit, procedure drill) independently necessary to occupy the competence kernel, or is there a proper subset of mechanisms that would be sufficient?',
    'Controlled experimental variation: disable one mechanism at a time in a pilot organization and measure field performance and incident rates over 2-3 years. Cross-compare with organizations maintaining all four mechanisms at equivalent competence levels.',
    'If a subset (e.g., simulator + line audit) proves sufficient, the regime could be streamlined, cutting training load and extractiveness substantially. If all four are independently necessary, the current multi-mechanism mandate is justified and extractiveness reflects genuine coordination cost rather than overhead. The finding would also validate or invalidate the simulation_sufficiency reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mechanism_sufficiency_boundary, empirical, 'Whether the four-mechanism configuration is optimally tight or over-provisioned.').

omega_variable(
    skill_decay_curves_unknown,
    'What are the actual skill decay curves for this domain''s critical competencies, and how do decay rates vary by mechanism (simulator vs. field vs. classroom)?',
    'Longitudinal study tracking personnel skill levels across time and mechanism type, with validated field performance measures (incident causality analysis, peer review, near-miss correlation). This data is typically restricted to regulatory bodies and training administration.',
    'If decay curves are known and uneven (e.g., procedure memory decays faster than manual control), the regime could be personalized (some personnel get procedure-focused refreshers, others get simulator-focused) and optimized. If decay curves are unknown or uniform, the current uniform multi-mechanism regime is rationally justified. The absence of this data is a structural fact enabling mandatrophy: without decay curves, the regime cannot be proven inefficient and thus persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(skill_decay_curves_unknown, empirical, 'The epistemic gap that sustains the regime''s inertia and prevents optimization.').

omega_variable(
    theater_vs_substance_boundary,
    'Of the theater_ratio (0.41), which training elements are performative compliance theater and which are substantive skill validation? Are newer elements (added post-incident) more theatrical than founding elements?',
    'Post-incident investigation data correlation: do personnel who failed a particular element have higher incident rates than personnel who failed other elements? Supervisor and personnel interview data on which elements they perceive as skill-relevant vs. box-checking.',
    'If theater elements are identifiable and can be retired, extractiveness could drop 15-25 points and the regime would remain coordinate. If all elements are equally theatrical or equally substantive, the current ratio is correct and theater is a side effect of administrative layering, not a sign of remedial opportunity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_vs_substance_boundary, empirical, 'Whether the theater_ratio (0.41) represents true performative creep or legitimate multi-mechanism necessity.').

omega_variable(
    identity_lock_mechanism_contestable,
    'Is the identity-lock on operational personnel (exit = leaving profession) inherent to the role or constructed by professional credentialing, social norms, and economic dependency?',
    'Longitudinal career-history data: what fraction of certified personnel leave the profession after 10, 20, 30 years? What are their stated reasons? Do mid-career exits correlate with training burden, career progression, or other factors? Cross-institutional comparison of retention rates.',
    'If the lock is constructed (e.g., professional identity is artificially fused with the job through credentialing and social status), the suppression could be reduced by decoupling identity, which would change exit_options from identity_locked to constrained or mobile, raising d and making the regime''s extractiveness more visible. If the lock is inherent (the skills are economically valuable and transferable only within the domain), suppression is correctly assessed as structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_contestable, empirical, 'Whether identity-lock is structural to the role or a governable feature of the regime.').

omega_variable(
    regulatory_lock_in_contestable,
    'Are alternative training frameworks (simulation-only, incident-response, peer-mentorship, just-in-time) genuinely less safe or are they barred by regulatory path-dependence and training-administration institutional interests?',
    'Natural experiment from jurisdictions with less prescriptive regulations (e.g., some international maritime regulations allow framework flexibility). Compare incident rates and competence measures across jurisdictions with different training regime prescriptions.',
    'If alternatives prove equally safe, the current regulatory mandate is driven by institutional inertia and path-dependence rather than evidence. This would support the mandatrophy reading (the regime persists despite obsolescence). If alternatives have higher incident rates, the multi-mechanism mandate is evidence-justified and the regime is genuine coordination, not extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_lock_in_contestable, empirical, 'Whether regulatory lock-in creates structural barriers to more efficient training frameworks.').

omega_variable(
    research_exclusion_necessary,
    'Is the systematic exclusion of research communities from operational data and regime-modification authority necessary for safety/security, or is it an institutional self-protection mechanism?',
    'Pilot program: grant a research team access to anonymized incident investigation data, training records, and operational metrics for one organization. Allow them to recommend (but not implement) regime modifications. Compare recommendation quality and implementation barriers to internal training-administration assessments.',
    'If research teams produce recommendations that training administration suppresses despite merit, it indicates the exclusion serves institutional interests (maintaining control and budget capture). If research recommendations prove lower-quality or unsafe, the exclusion is justified. Either finding clarifies whether the research_community''s absence is structural necessity or structural extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(research_exclusion_necessary, conceptual, 'Whether research exclusion is a necessary safeguard or an institutional power-preservation mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__hybrid_occupation, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__hybrid_occupation, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(comp_tr_t0, observed).
narrative_ontology:measurement(comp_tr_t5, competence_occupation__hybrid_occupation, theater_ratio, 5, 0.35).
narrative_ontology:measurement_basis(comp_tr_t5, observed).
narrative_ontology:measurement(comp_tr_t10, competence_occupation__hybrid_occupation, theater_ratio, 10, 0.37).
narrative_ontology:measurement_basis(comp_tr_t10, observed).
narrative_ontology:measurement(comp_tr_t15, competence_occupation__hybrid_occupation, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(comp_tr_t15, observed).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__hybrid_occupation, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(comp_tr_t20, observed).
narrative_ontology:measurement(comp_tr_t25, competence_occupation__hybrid_occupation, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(comp_tr_t25, observed).
narrative_ontology:measurement(comp_tr_t30, competence_occupation__hybrid_occupation, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(comp_tr_t30, observed).
narrative_ontology:measurement(comp_tr_t40, competence_occupation__hybrid_occupation, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(comp_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__hybrid_occupation, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(comp_be_t0, observed).
narrative_ontology:measurement(comp_be_t5, competence_occupation__hybrid_occupation, base_extractiveness, 5, 0.61).
narrative_ontology:measurement_basis(comp_be_t5, observed).
narrative_ontology:measurement(comp_be_t10, competence_occupation__hybrid_occupation, base_extractiveness, 10, 0.64).
narrative_ontology:measurement_basis(comp_be_t10, observed).
narrative_ontology:measurement(comp_be_t15, competence_occupation__hybrid_occupation, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(comp_be_t15, observed).
narrative_ontology:measurement(comp_be_t20, competence_occupation__hybrid_occupation, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(comp_be_t20, observed).
narrative_ontology:measurement(comp_be_t25, competence_occupation__hybrid_occupation, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(comp_be_t25, observed).
narrative_ontology:measurement(comp_be_t30, competence_occupation__hybrid_occupation, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(comp_be_t30, observed).
narrative_ontology:measurement(comp_be_t40, competence_occupation__hybrid_occupation, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(comp_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__hybrid_occupation, suppression_requirement, 0, 0.44).
narrative_ontology:measurement_basis(comp_su_t0, observed).
narrative_ontology:measurement(comp_su_t5, competence_occupation__hybrid_occupation, suppression_requirement, 5, 0.47).
narrative_ontology:measurement_basis(comp_su_t5, observed).
narrative_ontology:measurement(comp_su_t10, competence_occupation__hybrid_occupation, suppression_requirement, 10, 0.49).
narrative_ontology:measurement_basis(comp_su_t10, observed).
narrative_ontology:measurement(comp_su_t15, competence_occupation__hybrid_occupation, suppression_requirement, 15, 0.5).
narrative_ontology:measurement_basis(comp_su_t15, observed).
narrative_ontology:measurement(comp_su_t20, competence_occupation__hybrid_occupation, suppression_requirement, 20, 0.51).
narrative_ontology:measurement_basis(comp_su_t20, observed).
narrative_ontology:measurement(comp_su_t25, competence_occupation__hybrid_occupation, suppression_requirement, 25, 0.52).
narrative_ontology:measurement_basis(comp_su_t25, observed).
narrative_ontology:measurement(comp_su_t30, competence_occupation__hybrid_occupation, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(comp_su_t30, observed).
narrative_ontology:measurement(comp_su_t40, competence_occupation__hybrid_occupation, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(comp_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__hybrid_occupation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_occupation__hybrid_occupation, 0.18).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, competence_occupation__real_incident_necessity).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, incident_causality_attribution).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, regulatory_mandate_decay).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'competence_occupation'. Three readings coexist: hybrid_occupation (this file; multi-mechanism necessary, no consensus on optimality), simulation_sufficiency (simulator-only is sufficient; one mechanism is cheaper), real_incident_necessity (only incidents provide authentic conditions; drills are surrogates). The hybrid reading influences both siblings: it asserts multi-mechanism adequacy (pressuring simulation_sufficiency to prove simulator alone fails) and it shifts burden of proof onto real_incident_necessity (incidents are rare despite multi-mechanism training, so incidents are not the necessary condition for competence occupation). Each reading has its own epsilon value, its own beneficiary/victim structure, and its own classification; they are not observer-dependent cuts on one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_occupation__hybrid_occupation, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
