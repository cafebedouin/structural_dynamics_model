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
 *   constraint_id: competence_retention_exercise__catastrophe_as_necessary
 *   human_readable: Catastrophe-as-Necessary Competence Maintenance Constraint
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint instantiates one reading of a contested kernel about how
 *   organizational competence in high-reliability domains is validated and
 *   maintained. The reading claims: only actual catastrophic events provide
 *   the organizational learning and visceral stakes required to maintain
 *   genuine competence; simulation is rehearsal but not the real thing. Under
 *   this reading, organizations become vulnerable precisely when they appear
 *   safest — during long incident-free periods — and competence decay becomes
 *   visible only catastrophically. The regulatory framework,
 *   incident-response profession, and safety-culture literature increasingly
 *   embed this reading, creating institutional incentives that treat real
 *   catastrophes as necessary system resets rather than preventable failures.
 *   Sibling readings contest this frame: one argues that near-miss incidents
 *   and minor failures provide sufficient real-world feedback; another claims
 *   that high-fidelity simulation constitutes genuine competence validation.
 *   This story authors ONLY the catastrophe-as-necessary reading as a
 *   structurally distinct constraint with its own beneficiaries, victims, and
 *   ε value.
 *
 * KEY AGENTS:
 *   - Operational organizations (aviation, nuclear, chemical, medical): pay the direct cost of the constraint — organizational disruption, trauma, reputational damage, litigation following catastrophic events
 *   - Regulatory framework and incident-response bodies: agenda-setters and beneficiaries — enforce the constraint by investigating catastrophes and implicitly validating it as the competence test
 *   - Safety-culture advocates and incident-response professionals: beneficiaries — gain institutional authority, consulting demand, and expanded jurisdiction through the constraint's operation
 *   - Public at large: powerless payers — bear casualties, infrastructure damage, and environmental harm treated as necessary learning moments
 *   - Simulation training industry: excluded — delegitimized under this reading as 'mere rehearsal' rather than genuine competence testing
 *   - Near-miss researchers: excluded — their evidence is reframed as secondary, not 'real' enough to validate competence
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
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__catastrophe_as_necessary, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__catastrophe_as_necessary, "Catastrophe-as-Necessary Competence Maintenance Constraint").
narrative_ontology:topic_domain(competence_retention_exercise__catastrophe_as_necessary, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_retention_exercise__catastrophe_as_necessary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__catastrophe_as_necessary, '23f8666c-96c2-4567-a02b-c389ccf520d4').
narrative_ontology:cs_kernel_codification('23f8666c-96c2-4567-a02b-c389ccf520d4', distributed).
narrative_ontology:cs_authority_grounding('23f8666c-96c2-4567-a02b-c389ccf520d4', extraction).
narrative_ontology:cs_interpretation_layer_present('23f8666c-96c2-4567-a02b-c389ccf520d4').
narrative_ontology:cs_reading_relation('23f8666c-96c2-4567-a02b-c389ccf520d4', competence_retention_exercise__near_miss_as_bridge, coexists_with).
narrative_ontology:cs_reading_relation('23f8666c-96c2-4567-a02b-c389ccf520d4', competence_retention_exercise__simulation_as_sufficient, influences).
narrative_ontology:cs_axiom('23f8666c-96c2-4567-a02b-c389ccf520d4', foundational, catastrophe_necessity_for_visceral_stakes).
narrative_ontology:cs_axiom_status(catastrophe_necessity_for_visceral_stakes, holdable).
narrative_ontology:cs_axiom_grounding('23f8666c-96c2-4567-a02b-c389ccf520d4', catastrophe_necessity_for_visceral_stakes, empirically_contingent).
narrative_ontology:cs_axiom('23f8666c-96c2-4567-a02b-c389ccf520d4', foundational, simulation_false_confidence_generation).
narrative_ontology:cs_axiom_status(simulation_false_confidence_generation, holdable).
narrative_ontology:cs_axiom_grounding('23f8666c-96c2-4567-a02b-c389ccf520d4', simulation_false_confidence_generation, empirically_contingent).
narrative_ontology:cs_reference_frame('23f8666c-96c2-4567-a02b-c389ccf520d4', real_catastrophe_as_competence_test).
narrative_ontology:cs_drift_state('23f8666c-96c2-4567-a02b-c389ccf520d4', contemporary_simulation_advancement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('23f8666c-96c2-4567-a02b-c389ccf520d4', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, incident_response_regulators).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, safety_culture_advocates).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, operational_organizations).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, public_at_large).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate high-reliability systems and bear the direct costs of the constraint: organizational trauma, reputational damage, litigation, and regulatory penalties following catastrophic events. They cannot exit the domain and must demonstrate competence under regulatory frameworks that implicitly require real-catastrophe validation.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, operational_organizations, payer,
    organized, generational, constrained, global).

% Administers the constraint through accident investigations, root-cause analyses, incident-response protocols, and certification standards. Post-catastrophe investigations implicitly validate the catastrophe-as-necessary framing by treating real events as the authoritative test of competence and organizational learning.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, regulatory_framework, agenda_setter,
    institutional, generational, analytical, global).

% Safety regulators, accident investigators, and incident-review boards gain institutional authority, expanded jurisdiction, and justified funding through investigation and analysis of catastrophic events. The constraint validates their role and creates institutional demand for their expertise.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, incident_response_regulators, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__catastrophe_as_necessary, incident_response_regulators, agenda_setter).

% Safety professionals, human-factors researchers, and organizational behavior consultants benefit from endorsing the constraint. It validates their expertise, justifies expanded safety budgets in operational organizations, and creates demand for their post-incident recovery consulting services.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, safety_culture_advocates, beneficiary,
    organized, biographical, mobile, global).

% Bears the direct harm of catastrophic events: casualties, environmental damage, infrastructure destruction, and long-term trauma. Under this reading, these harms are reframed as necessary learning moments rather than preventable failures. They have no exit from reliance on high-reliability systems and no participation in regulatory framing.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, public_at_large, payer,
    powerless, biographical, trapped, global).

% Flight simulators, nuclear plant simulators, surgical simulations, and other high-fidelity training platforms are structurally delegitimized under this reading as 'mere rehearsal' and sources of false confidence. Their competence validation outputs are excluded from the decision-making frame that determines whether competence is adequately maintained.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, simulation_training_industry, excluded,
    organized, biographical, constrained, global).

% Organizational researchers and safety scientists arguing that near-miss incidents provide sufficient real-world feedback are structurally excluded from framing the competence question under this reading. Their research is treated as secondary evidence, not 'real' enough to count as genuine competence testing.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, near_miss_researchers, excluded,
    moderate, biographical, mobile, global).

% Leadership teams within operational organizations observe the constraint from high power but limited agency. They can absorb the regulatory framing (catastrophes are necessary) or advocate for alternatives (simulation, near-miss learning), but the professional and regulatory consensus increasingly enforces the catastrophe-as-necessary reading. They face legal and reputational consequences if catastrophes occur after they had publicly advocated that simulation or near-miss learning were sufficient.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, operational_leadership, observer,
    powerful, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__catastrophe_as_necessary, incident_response_regulators).
narrative_ontology:fixing_cost_class(competence_retention_exercise__catastrophe_as_necessary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified, institutionally consistent framework for validating organizational competence in high-stakes domains: a single, rigorous test (real catastrophic pressure) that demonstrates whether organizations have maintained genuine, embodied competence and situational awareness. Rather than competing validation systems (simulator performance scores, near-miss incident counts, training metrics), the constraint establishes one canonical test.
% TRANSFER_FUNCTION: Transfers organizational and public risk from regulatory bodies and safety professionals to operational organizations and the public at large. Regulatory agencies gain institutional authority, expanded jurisdiction, and justified budget increases through investigation and remediation of catastrophes. Operational organizations and affected populations bear the direct cost: organizational trauma, reputational damage, litigation, casualties, and infrastructure destruction, reframed as necessary learning rather than preventable harm.
% ABSENT_VOICES: Simulation industry participants who argue that high-fidelity simulation equals real-event training are structurally excluded. Near-miss researchers and continuous-improvement advocates advocating that minor failures and near-misses provide sufficient real-world feedback are delegitimized as offering secondary evidence. Operational employees and community members who have survived catastrophic events and contest the framing ('this was preventable, not necessary') lack institutional standing in the regulatory discourse that affirms the constraint. Prevention-focused safety advocates who prioritize avoiding all catastrophes (rather than using them as learning moments) are excluded from the frame.
% DISAPPEARANCE_RATIONALE: If this constraint vanished — if regulators accepted that high-fidelity simulation, continuous near-miss learning, and ongoing competence monitoring were sufficient — operational organizations would shift investment from catastrophe-response planning and post-incident remediation to prevention infrastructure. Regulatory bodies would redirect toward continuous monitoring and real-time intervention rather than post-catastrophe investigation and authority expansion. The institutional power of incident-response bodies would diminish. Simulation and near-miss research would become the dominant validation mechanisms. Organizational learning apparatus would reorganize around preventing catastrophes rather than treating them as necessary competence tests.
% FOUNDING_PROBLEM: In the 1970s–1980s, investigations of major catastrophes (Tenerife aviation disaster, Three Mile Island, Bhopal) revealed that operator knowledge, procedural discipline, decision-making capability, and situational awareness had degraded invisibly during long incident-free periods, despite certification and training records showing compliance. Organizations appeared safest when they had become most vulnerable. The founding problem: how can regulators and operational leadership validate that competence is genuinely maintained when no incidents occur to test it? Simulation training scores and procedural compliance records did not capture this invisible decay.
% FOUNDING_PROBLEM_CORROBORATION: High-reliability-organization researchers (LaPorte, Roberts, Weick) documented the original finding from multiple catastrophe investigations. However, contemporary organizational learning literature contests whether the founding problem persists: researchers in resilience engineering, continuous-improvement systems, and simulator-based training argue that modern high-fidelity simulation with continuous refresher training, near-miss learning systems, and ongoing competence monitoring can detect and correct competence decay without requiring real catastrophes. Regulatory agencies and incident-response bodies affirm the founding problem as persistent and justify the catastrophe-as-necessary reading through post-catastrophe investigations. Safety researchers and simulation advocates dispute whether the founding problem has been solved by modern continuous-monitoring infrastructure; they argue it has, and that catastrophes are now preventable harms rather than necessary learning events.
narrative_ontology:disappearance_verdict(competence_retention_exercise__catastrophe_as_necessary, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__catastrophe_as_necessary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__catastrophe_as_necessary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_retention_exercise__catastrophe_as_necessary, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__catastrophe_as_necessary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.68) because the constraint transfers organizational and public risk to operational organizations and affected populations, while concentrating institutional authority and funding expansion in the regulatory apparatus. This transfer is presented as coordination (unified competence testing) but operates asymmetrically: regulators gain legitimacy and jurisdiction through catastrophes, while operational organizations bear the actual cost. Suppression is higher (0.71) because the constraint actively excludes alternative validation mechanisms (simulation, near-miss learning) from the professional and regulatory discourse. Theater ratio rises over the interval (0.32 → 0.52) as the constraint's operation shifts from genuine competence validation toward performance of regulatory authority: post-catastrophe investigations become increasingly theatrical — they affirm the constraint's necessity while regulatory bodies extract authority and expanded jurisdiction. The measurement grid is shared across all three metrics at every time point. Accessibility collapse (0.61) reflects that alternative competence-validation mechanisms are suppressed but not completely unavailable — organizations can still advocate for simulation-based approaches, but the institutional consensus delegitimizes them. Resistance (0.58) is moderate: operational organizations resist the constraint's framing, as do simulation advocates and near-miss researchers, but the regulatory consensus is sufficiently entrenched that resistance operates at the margins.
 *
 * PERSPECTIVAL GAP:
 *   The regulatory agenda-setter and safety-culture beneficiary seats experience this constraint as coordination with high legitimacy — a unified, rigorous competence-testing standard. The operational organization seat and public-at-large seats experience it as enforced extraction: the cost of demonstrating competence is paid by those who suffer catastrophes, while the institutional benefits accrue to regulators. The engine computes per-seat directionality from this structural asymmetry: regulators face low d (beneficiary directionality) while operational organizations and the public face high d (target directionality). The simulation industry and near-miss researchers experience it as institutional delegitimization — exclusion from the frame that determines competence validity. The divergence between the claimed type (tangled rope — coordination with extraction) and the measured metrics reflects the structural reality: the constraint does coordinate competence validation (genuine coordination function, beneficiaries exist), but it does so through mechanisms (real catastrophes as the test) that extract from non-beneficiaries as the price of that coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   The regulatory framework and incident-response bodies are the structural beneficiaries: they gain institutional authority, jurisdiction expansion, and justified funding increases through investigation and remediation of catastrophic events. Their directionality is low (near 0.0 — full beneficiary). The safety-culture advocates and consulting industry are secondary beneficiaries with somewhat higher directionality (constrained mobile organizations with generational time horizons). Operational organizations are the primary targets: they pay through operational disruption, organizational trauma, reputational damage, and litigation following real catastrophes. Their directionality is high (near 1.0 — full target), modulated slightly downward because they are 'organized' power (not 'powerless'); organized actors retain more leverage than individual actors. The public at large are powerless targets: they bear casualties and infrastructure damage with 'trapped' exit options (no exit from reliance on high-reliability systems, no participation in regulatory framing). Their directionality approaches 1.0 (full target). The simulation industry and near-miss researchers are excluded rather than coordinated — their directionality is neither beneficiary nor target but 'excluded' (not a directionality value per se, but structurally relevant: exclusion is a form of suppression that prevents alternative framings from competing).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves via the tangled-rope classification: it genuinely coordinates competence validation (the founding problem — how to validate competence during incident-free periods — is real, and the constraint's founding response was a legitimate coordination solution). Simultaneously, it asymmetrically extracts from operational organizations and the public: the cost of catastrophic events is borne by those organizations and affected populations, while regulatory authority and institutional expansion flow to beneficiaries. The classification prevents the false positive of reading this as pure coordination (rope) by naming the victim structure: operational organizations and the public bear real costs, not in payment for a service but as the price of the constraint's enforcement mechanism. It also prevents the false positive of reading this as pure extraction (snare) by acknowledging the genuine coordination function: a unified competence-testing framework is valuable, and the founding problem was real. The tangled-rope classification captures that this is coordination with asymmetric enforcement — the constraint solves a genuine problem and extracts through its solution mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_boundary_ambiguity,
    'Is this constraint one reading of a unified kernel (competence validation standards), or are the three readings instantiating fundamentally incommensurable commitment systems (medical ethics vs. organizational efficiency vs. prevention philosophy)?',
    'Structural analysis of whether the three readings can coexist in a single institutional framework (e.g., one organization holding all three readings for different functions) or whether adopting one reading logically foreclosed the others.',
    'If readings coexist within one framework: the kernel is unified and the readings are policy options. If they are incommensurable: the kernel label is a misnomer and the constraint should be decomposed into three independent constraints with no shared identity. Current organizational practice (regulatory bodies de facto endorsing catastrophe-as-necessary while safety researchers advocate near-miss-as-bridge) suggests coexistence within institutionally segmented frames.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_boundary_ambiguity, conceptual, 'Whether the three readings share a unified kernel or are incommensurable commitment systems.').

omega_variable(
    competence_decay_invisibility,
    'Does competence actually decay invisibly during incident-free periods in modern, high-fidelity simulation-trained organizations, or has continuous monitoring and simulator-based refresher training eliminated this decay?',
    'Longitudinal study of operator performance decay in organizations using modern continuous-competence monitoring vs. organizations using catastrophe-incident-based validation. Measure: hidden failures detected in simulations, near-miss incidents, and operator error rates during incident-free periods.',
    'If decay persists despite continuous monitoring: the founding problem remains live and the catastrophe-as-necessary reading retains structural validity. If decay is adequately detected and corrected through simulation: the founding problem is dead and the constraint persists via mandatrophy (institutional inertia rather than functional necessity).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competence_decay_invisibility, empirical, 'Whether the original founding problem (invisible competence decay during incident-free periods) persists in contemporary high-reliability organizations with modern training infrastructure.').

omega_variable(
    simulation_fidelity_threshold,
    'Is there a structural limit to how closely simulation can replicate the visceral, cognitive, and social-emotional pressure of real catastrophic events, or can sufficiently high-fidelity simulation equal real-event training?',
    'Neuroscience and organizational psychology research on stress-induced learning, embodied cognition, and decision-making under maximal pressure. Empirical comparison: operator performance in post-catastrophe-incident contexts vs. operator performance in maximally realistic simulator scenarios.',
    'If simulation cannot equal real-event pressure (structural limit): this reading''s core premise holds (real events necessary). If simulation can approach parity (with sufficient investment in high-fidelity technology): this reading''s framing is technology-contingent, not structurally necessary — the sibling simulation-as-sufficient reading becomes empirically plausible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fidelity_threshold, empirical, 'Whether simulation can replicate the cognitive and emotional pressure of real catastrophes sufficiently to equal real-event training at the threshold of genuinely high-fidelity technology.').

omega_variable(
    regulatory_authority_incentive_alignment,
    'Do regulatory bodies and incident-response professionals benefit institutionally from catastrophes in ways that create bias toward catastrophe-as-necessary framing, independent of whether catastrophes are genuinely necessary for competence validation?',
    'Institutional analysis: track expansion of regulatory jurisdiction, budget allocation, and personnel growth following major catastrophic events vs. following periods of incident-free safe operation. Survey incident investigators and regulators on the perceived legitimacy of alternative validation mechanisms (simulation, near-miss learning) independent of their institutional incentives.',
    'If regulatory bodies benefit substantially from catastrophes in ways independent of competence validation: the constraint''s persistence may be partly mandatrophy (institutional inertia and rent-seeking) rather than functional necessity. If regulators are structurally indifferent to whether catastrophes occur: the catastrophe-as-necessary framing is more likely reflecting genuine competence requirements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_authority_incentive_alignment, conceptual, 'Whether regulatory authority expansion incentives create bias in favor of catastrophe-as-necessary framing, independent of functional necessity.').

omega_variable(
    near_miss_sufficiency_threshold,
    'Is there a quantitative threshold of near-miss incidents and minor failures that would provide equivalent learning to one major catastrophe for the purpose of competence validation?',
    'Comparative analysis of organizational learning from near-miss datasets vs. learning from catastrophe incident investigations. Measure: systemic corrections implemented, competence improvements measured, behavioral change in personnel, and recurrence of similar failure modes.',
    'If near-miss learning provides equivalent organizational learning per unit cost: the sibling near-miss-as-bridge reading becomes empirically superior and this reading''s catastrophe-necessity becomes contingent on economics (catastrophes are necessary only if near-miss learning infrastructure is unavailable). If catastrophe learning provides disproportionate insight: the catastrophe-as-necessary reading is empirically validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(near_miss_sufficiency_threshold, empirical, 'Whether near-miss incidents and minor failures can provide equivalent organizational learning to catastrophes for competence validation purposes.').

omega_variable(
    reading_kernel_alternative_framing,
    'Is competence validation the correct kernel frame, or is the actual contested commitment ''how much organizational and public risk is acceptable to incur in service of maintaining regulatory authority''?',
    'Genealogical analysis of how the three readings'' origin narratives frame the problem they claim to solve. Interview stakeholders from each reading on the foundational problem the constraint addresses.',
    'If the kernel is truly competence validation: the three readings address that problem differently. If the actual kernel is risk-authority tradeoff: the three readings mask a fundamental disagreement about institutional power and accountability that cannot be resolved by empirical competence data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_alternative_framing, conceptual, 'Whether the contested kernel is genuinely competence validation or is actually a disagreement about institutional authority and acceptable risk.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__catastrophe_as_necessary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 0, 0.32).
narrative_ontology:measurement(comp_tr_t5, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 5, 0.38).
narrative_ontology:measurement(comp_tr_t10, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 10, 0.44).
narrative_ontology:measurement(comp_tr_t15, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 15, 0.48).
narrative_ontology:measurement(comp_tr_t25, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 25, 0.5).
narrative_ontology:measurement(comp_tr_t35, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 35, 0.52).
narrative_ontology:measurement(comp_tr_t40, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 40, 0.52).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(comp_be_t5, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(comp_be_t10, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(comp_be_t15, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(comp_be_t25, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 25, 0.65).
narrative_ontology:measurement(comp_be_t35, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(comp_be_t40, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(comp_su_t5, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(comp_su_t10, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(comp_su_t15, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 15, 0.67).
narrative_ontology:measurement(comp_su_t25, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 25, 0.69).
narrative_ontology:measurement(comp_su_t35, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 35, 0.7).
narrative_ontology:measurement(comp_su_t40, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__catastrophe_as_necessary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_retention_exercise__catastrophe_as_necessary, 0.14).
narrative_ontology:affects_constraint(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise__near_miss_as_bridge).
narrative_ontology:affects_constraint(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise__simulation_as_sufficient).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'competence_retention_exercise'. The kernel family includes three structurally distinct constraint stories: catastrophe_as_necessary (this file — high extraction, asymmetric enforcement), near_miss_as_bridge (moderate extraction, continuous feedback mechanisms), and simulation_as_sufficient (low extraction, technology-based validation). Each reading instantiates a different ε value for the same kernel because each reading's mechanism of competence validation differs structurally. The three readings coexist in institutional practice (regulators de facto endorse catastrophe-as-necessary, safety researchers advocate near-miss-as-bridge, simulation industry pushes simulation-as-sufficient). No single reading has foreclosed the others; they compete via institutional influence, regulatory pressure, and resource allocation. This reading influences the others by establishing catastrophe investigation and incident-response authority as the institutional standard against which simulation fidelity and near-miss sufficiency are measured.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
