% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__existential_risk_reading, []).

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
 *   constraint_id: ai_alignment_priority__existential_risk_reading
 *   human_readable: AI Alignment via Existential Risk Prioritization
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   The existential-risk reading of AI alignment frames the safety problem as
 *   preventing catastrophic loss of control over superintelligent systems
 *   whose arrival may be imminent and irreversible. This reading organizes
 *   research resources, institutional priorities, and policy direction around
 *   the hypothesis that existential risk dominates all other AI-related harms
 *   and justifies aggressive capability research to understand and control
 *   future superintelligence. The reading is one of three structurally
 *   distinct framings of 'alignment': the integrated reading treats present
 *   and existential harms as complementary; the nearterm-harms reading
 *   prioritizes present discrimination and labor displacement as the
 *   justice-relevant safety problem. This story instantiates ONLY the
 *   existential-risk reading as a clean, ε-invariant constraint. The
 *   structural delta (high ε on speculative capabilities, victim set as
 *   undifferentiated future humanity, beneficiary as long-term future,
 *   resource flow to capability research) is authored here. The sibling
 *   readings are separate constraint stories.
 *
 * KEY AGENTS:
 *   - Existential-risk researchers: frame and enforce the definition; set research priorities
 *   - Long-term future (abstract beneficiary): frames resource allocation as future-oriented
 *   - Capability-research institutions: receive disproportionate funding and prestige
 *   - Present-marginalized populations: systematically deprioritized; experience harms from deployed systems
 *   - Nearterm-safety researchers: career incentivized toward existential focus; present-harm research marginalized
 *   - Funding bodies: allocate billions according to existential-risk criterion
 *   - Integrated and nearterm researchers: excluded from priority-setting; voices sidelined
 *   - Regulators: observe how framing shapes policy; pressured toward light-touch governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, 0.82).
domain_priors:suppression_score(ai_alignment_priority__existential_risk_reading, 0.71).
domain_priors:theater_ratio(ai_alignment_priority__existential_risk_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, accessibility_collapse, 0.69).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__existential_risk_reading, "AI Alignment via Existential Risk Prioritization").
narrative_ontology:topic_domain(ai_alignment_priority__existential_risk_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__existential_risk_reading, '8a5976a6-15e4-45a3-8546-7b85d410349d').
narrative_ontology:cs_kernel_codification('8a5976a6-15e4-45a3-8546-7b85d410349d', distributed).
narrative_ontology:cs_authority_grounding('8a5976a6-15e4-45a3-8546-7b85d410349d', extraction).
narrative_ontology:cs_interpretation_layer_present('8a5976a6-15e4-45a3-8546-7b85d410349d').
narrative_ontology:cs_reading_relation('8a5976a6-15e4-45a3-8546-7b85d410349d', ai_alignment_priority__integrated_reading, forecloses).
narrative_ontology:cs_reading_relation('8a5976a6-15e4-45a3-8546-7b85d410349d', ai_alignment_priority__nearterm_harms_reading, coexists_with).
narrative_ontology:cs_axiom('8a5976a6-15e4-45a3-8546-7b85d410349d', foundational, superintelligence_existential_dominance).
narrative_ontology:cs_axiom_status(superintelligence_existential_dominance, holdable).
narrative_ontology:cs_axiom_grounding('8a5976a6-15e4-45a3-8546-7b85d410349d', superintelligence_existential_dominance, empirically_contingent).
narrative_ontology:cs_axiom('8a5976a6-15e4-45a3-8546-7b85d410349d', foundational, control_loss_irreversibility).
narrative_ontology:cs_axiom_status(control_loss_irreversibility, holdable).
narrative_ontology:cs_axiom_grounding('8a5976a6-15e4-45a3-8546-7b85d410349d', control_loss_irreversibility, deontological).
narrative_ontology:cs_reference_frame('8a5976a6-15e4-45a3-8546-7b85d410349d', bounded_rationality_superintelligence_threat).
narrative_ontology:cs_drift_state('8a5976a6-15e4-45a3-8546-7b85d410349d', contemporary_ai_capability_acceleration, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8a5976a6-15e4-45a3-8546-7b85d410349d', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(ai_alignment_priority__existential_risk_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, long_term_future_humanity).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, capability_research_institutions).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, present_marginalized_populations).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, nearterm_safety_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, major_ai_capability_labs).
narrative_ontology:constraint_vindicates(ai_alignment_priority__existential_risk_reading, superintelligence_existential_risk_dominates).
narrative_ontology:constraint_vindicates(ai_alignment_priority__existential_risk_reading, control_loss_irreversibility_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Frame alignment as primarily a problem of preventing catastrophic loss of control over future superintelligent systems. Set research priorities toward adversarial red-teaming, capability limitation, and specification robustness. Argue that addressing present harms dilutes resources from existential-risk reduction. Control the definition of 'alignment' in major funding discussions and institutional policies.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, existential_risk_researchers, agenda_setter,
    organized, civilizational, mobile, global).

% An abstract beneficiary: the constraint frames present resource allocation as advancing the interests of all humanity in the distant future by preventing a speculative but high-consequence existential failure mode. No contemporaneous agent collects this benefit; the framing allows extraction from present research priorities without present accountability.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, long_term_future_humanity, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ai_alignment_priority__existential_risk_reading, long_term_future_humanity).

% Receive disproportionate funding, talent, and institutional prestige under the existential-risk framing because capability research is presented as essential to understanding failure modes and designing controls. The framing legitimates scaling up AI capabilities under the banner of safety research.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, capability_research_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Experience algorithmic discrimination, exclusion from AI-driven services, wage suppression from automation, and data extraction without meaningful recourse. The constraint deprioritizes research into these present harms by characterizing them as second-order to the existential-risk problem. They cannot exit the systems that harm them and lack power to reframe the research agenda.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, present_marginalized_populations, payer,
    powerless, biographical, trapped, global).

% Work on deployed-AI harms, fairness, interpretability, and present-day safety but struggle to secure funding and institutional recognition. The constraint redirects research resources and attention toward speculative existential scenarios, treating present-harm research as a distraction from 'real alignment.' Career advancement in AI safety increasingly requires accepting the existential-risk framing or accepting marginalization.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, nearterm_safety_researchers, payer,
    moderate, biographical, constrained, global).

% Benefit from the existential-risk framing because it legitimates aggressive capability scaling: the narrative holds that controlling dangerous capabilities requires first building them and understanding their properties. Present-harm concerns would constrain deployment and capability expansion; existential-risk focus preserves the research and deployment trajectory.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, major_ai_capability_labs, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, major_ai_capability_labs, agenda_setter).

% Govern resource distribution to AI safety and alignment research. The constraint operates through their adoption of existential-risk prioritization as the criterion for funding allocation. They distribute billions in research support according to this framework without systematically measuring present-harm reduction outcomes.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, funding_allocation_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Propose frameworks that treat present harms and existential risks as linked rather than competing, or argue for equal prioritization of immediate justice concerns. Their voices are structurally sidelined by the binary framing (existential vs. distraction) the constraint enforces. They are not invited to major policy discussions or granted proportional funding despite the theoretical coherence of their positions.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, integrated_and_nearterm_researchers, excluded,
    moderate, biographical, constrained, global).

% Observe how the alignment discourse shapes policy and resource allocation. The constraint influences their regulatory choices: existential-risk framing suggests light-touch regulation and capability acceleration as the path to safety, while present-harm concerns suggest precautionary governance and capability constraints.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, competition_authorities_and_regulators, observer,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__existential_risk_reading, capability_research_institutions).
narrative_ontology:fixing_cost_class(ai_alignment_priority__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global research effort toward a shared hypothesis: that superintelligent AI systems pose an existential risk and that prevention requires understanding and controlling their capabilities. Solves the collective-action problem of fragmented research groups by organizing them around a common threat model and funding mechanism.
% TRANSFER_FUNCTION: Moves research attention, institutional prestige, and funding (billions annually) from present-harm reduction (fairness, privacy, labor displacement) to existential-risk reduction (capability limitation, specification robustness, adversarial red-teaming). Transfers authority over 'alignment' definition from diverse stakeholders to existential-risk researchers and capability institutions. Transfers present suffering as an acceptable cost of future safety.
% ABSENT_VOICES: Present victims of algorithmic harms (marginalized populations subjected to discriminatory systems, workers displaced by automation) are structurally excluded from the research priority-setting process. Communities harmed by current AI deployment are not consulted in setting safety research agendas. Researchers studying present-day AI harms lack institutional representation in alignment governance.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared and the existential-risk prioritization frame dissolved, funding would rebalance toward present-harm reduction, capability-limitation research would intensify, deployment timelines would slow, and marginalized communities would regain voice in AI governance. The research agenda would pluralize around multiple harm models rather than organizing around a single existential-risk hypothesis.
% FOUNDING_PROBLEM: Advanced AI systems might develop capabilities that humans can no longer predict or control, resulting in civilizational-level harm or extinction. The founding problem assumes such superintelligence is likely, consequences are irreversible, and prevention requires dedicating substantial resources to understanding and constraining future AI capabilities before they exist.
% FOUNDING_PROBLEM_CORROBORATION: Existential-risk researchers and major AI labs attest the problem is live and severe. Present-harm researchers, affected communities, and several independent policy analysts attest the founding problem is speculative, its probability is highly uncertain, and the resource allocation it justifies is incommensurate with the empirical evidence. Academic surveys show deep uncertainty about superintelligence timelines and extinction risk magnitudes; no external authority with independent verification power corroborates the problem statement as presented.
narrative_ontology:disappearance_verdict(ai_alignment_priority__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__existential_risk_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint redirects finite research resources and attention away from present-harm reduction toward speculative existential scenarios, imposing costs on present-day victims and nearterm researchers without their consent or compensation. The measurement series shows rising extractiveness (0.68→0.82) indicating that the framing has strengthened its institutional hold: more funding bodies adopt existential-risk criteria, more institutions reorganize around capability research, more careers become locked into the existential narrative. Suppression is high (0.71) because the constraint actively excludes competing framings from major funding and policy discussions—the binary framing (existential vs. distraction) suppresses integrated and nearterm-harm discourse by institutional gatekeeping, not by open argumentation. Theater ratio is moderate (0.28) because the constraint performs genuine research coordination (real coordination on a real threat model) but an increasing share of activity defends the prioritization narrative rather than producing novel safety insights—as the framing ossifies, more effort goes to socialization and exclusion, less to novel investigation.
 *
 * PERSPECTIVAL GAP:
 *   Existential-risk researchers compute this as rope (genuine coordination around a shared threat model; participants are net beneficiaries of organized research direction). Capability institutions compute it as rope or beneficiary (they collect prestige and funding legitimately). Present victims and nearterm researchers compute it as snare or tangled_rope (they bear costs without choice or voice; the coordination function serves others' interests). Funding bodies compute it as rope or agenda-setter (coordinating research efficiently, though by whose criteria?). The engine will compute per-seat types from the structural data—the perspectival gap is precisely where the multiple computations diverge.
 *
 * DIRECTIONALITY LOGIC:
 *   Existential-risk researchers sit near the agenda-setter end (they set definitions, control funding criteria, enforce institutional priorities—high power, mobile exits via prestige and funding capture). Capability institutions sit near beneficiary/beneficiary-agent end (they collect funding, prestige, and deployment legitimacy without running the research direction—moderate power, arbitrage exits). Nearterm-safety researchers and present victims sit near the target end (they bear the cost of deprioritized work and unaddressed harms; they have low power and constrained exits—career lock-in for researchers, trapped for marginalized populations). The abstract beneficiary (long-term future) creates a structurally asymmetric dynamic: the constraint justifies present extraction by invoking an undifferentiated future that cannot contest the interpretation. Identity-lock operates on nearterm researchers: career paths have reorganized around existential-risk credentials, making exit into present-harm research professionally costly. Suppression operates on integrated voices: the binary framing (existential OR distraction) makes simultaneous prioritization seem incoherent, suppressing the third position without requiring explicit exclusion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (superintelligence existential risk) is contested in its severity, likelihood, and the causal link between present research and future control. The constraint justifies present extraction (redirected resources, career lock-in, marginalized communities) by claiming to solve this contested founding problem. If the founding problem is substantially weaker than asserted, or if present research does not causally advance future superintelligence control, the constraint transitions from tangled_rope (coordination with extraction justified by real risk) to snare (extraction justified by false urgency). The mandatrophy signal emerges if the founding problem status remains contested while the extracted resources grow—the extraction persists despite unresolved evidentiary grounds. The fact that no external independent authority corroborates the severity claim (only existential-risk researchers do) is a mandatrophy warning sign.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    superintelligence_timeline_uncertainty,
    'What is the actual probability distribution of advanced AI systems acquiring general-purpose superintelligence capabilities? Is superintelligence likely within 5–50 years as the existential-risk framing assumes, or is it more speculative?',
    'Empirical progress in AI capabilities over the next 10 years; expert forecasting updates from independent research communities; cognitive science progress on understanding general intelligence. The constraint''s extractiveness is highly sensitive to this distribution: if superintelligence remains speculative (>50% deep uncertainty), the present-day extraction cost is less justified.',
    'If superintelligence is substantially less likely or further in the future than currently assumed, the constraint reclassifies from tangled_rope (justified extraction) to snare (unjustified extraction by false urgency). Present-harm research becomes relatively more valuable, and resource reallocation becomes justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(superintelligence_timeline_uncertainty, empirical, 'Whether the founding problem''s severity and timeline justify the extracted resources.').

omega_variable(
    causal_linkage_present_to_future_control,
    'Does present research into AI capabilities and control actually reduce the probability of catastrophic loss of control over future superintelligent systems, or does it advance capability development without proportionate control advances?',
    'Systematic literature review of capability research vs. control research output; causal analysis from independent research institutions tracking whether capability papers predominantly advance control understanding or primarily advance raw capability; future evidence from superintelligent system development (if achieved) of whether present research causally contributed to safety outcomes.',
    'If present capability research does not causally advance future superintelligence control (or advances it less effectively than present-harm reduction would advance present safety), the constraint''s resource extraction is misallocated and reclassifies as snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_linkage_present_to_future_control, empirical, 'Whether research investments in the constraint actually advance its stated problem-solving goal.').

omega_variable(
    abstract_beneficiary_asymmetry,
    'Is the invocation of ''long-term future humanity'' as the constraint''s primary beneficiary a legitimate framing device for intergenerational resource allocation, or does it mask present extractive redistribution by placing the benefit outside present accountability?',
    'Philosophical analysis of legitimacy conditions for abstract beneficiaries; empirical study of how the abstract beneficiary framing affects present stakeholder willingness to accept extraction; counterfactual analysis: what would the constraint look like if present populations were given voice in defining ''long-term benefit''?',
    'If the abstract beneficiary is substantively indistinguishable from a cover story for present-actor interest, the constraint''s legitimacy shifts from coordination-with-extraction to pure-extraction, and the classification moves toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(abstract_beneficiary_asymmetry, conceptual, 'Whether abstract future beneficiaries can legitimately justify present extraction without present accountability.').

omega_variable(
    suppression_mechanism_institutional_or_internalized,
    'Is the suppression of nearterm-harm and integrated-research voices achieved primarily through institutional gatekeeping (funding bodies, hiring practices, publication venues) or through internalized acceptance of the existential-risk narrative by those voices themselves?',
    'Ethnographic study of how nearterm researchers adopt or resist the existential-risk framing; analysis of career trajectories and institutional barriers; post-constraint scenarios where institutional gatekeeping is removed but the researchers themselves still adopt the existential framing (indicating internalization).',
    'If suppression is primarily internalized, the constraint''s effective suppression is higher than structural measures suggest—the excluded voices carry the suppression with them even after institutional barriers are removed. If primarily institutional, removal of gatekeeping would rapidly pluralize the research agenda.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_institutional_or_internalized, empirical, 'Whether suppression is structural or internalized among nearterm researchers.').

omega_variable(
    kernel_contest_boundary,
    'Is the contest between existential-risk, nearterm-harm, and integrated readings of alignment fundamentally a disagreement about values (what should we prioritize), about empirics (what risks are likely), or about institutional authority (who decides)?',
    'Discourse analysis of published positions from each reading; stakeholder interviews exploring whether the dispute is resolvable by evidence or reflects deeper value/authority conflicts; empirical test of whether updating parties on technical evidence shifts positions.',
    'If the contest is empirical, consensus is possible and the constraint may stabilize (or dissolve if evidence favors rivals). If the contest is about values or authority, no empirical resolution is possible; the constraint persists as a power negotiation, and classification depends on whether the negotiated outcome is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_boundary, conceptual, 'Whether the kernel contest reflects empirical or value/authority disagreements.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__existential_risk_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__existential_risk_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(ai_a_tr_t0, observed).
narrative_ontology:measurement(ai_a_tr_t5, ai_alignment_priority__existential_risk_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement_basis(ai_a_tr_t5, observed).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_priority__existential_risk_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement_basis(ai_a_tr_t10, observed).
narrative_ontology:measurement(ai_a_tr_t15, ai_alignment_priority__existential_risk_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement_basis(ai_a_tr_t15, observed).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_priority__existential_risk_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(ai_a_tr_t20, observed).
narrative_ontology:measurement(ai_a_tr_t25, ai_alignment_priority__existential_risk_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(ai_a_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__existential_risk_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(ai_a_be_t0, observed).
narrative_ontology:measurement(ai_a_be_t5, ai_alignment_priority__existential_risk_reading, base_extractiveness, 5, 0.72).
narrative_ontology:measurement_basis(ai_a_be_t5, observed).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_priority__existential_risk_reading, base_extractiveness, 10, 0.76).
narrative_ontology:measurement_basis(ai_a_be_t10, observed).
narrative_ontology:measurement(ai_a_be_t15, ai_alignment_priority__existential_risk_reading, base_extractiveness, 15, 0.79).
narrative_ontology:measurement_basis(ai_a_be_t15, observed).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_priority__existential_risk_reading, base_extractiveness, 20, 0.81).
narrative_ontology:measurement_basis(ai_a_be_t20, observed).
narrative_ontology:measurement(ai_a_be_t25, ai_alignment_priority__existential_risk_reading, base_extractiveness, 25, 0.82).
narrative_ontology:measurement_basis(ai_a_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__existential_risk_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(ai_a_su_t0, observed).
narrative_ontology:measurement(ai_a_su_t5, ai_alignment_priority__existential_risk_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement_basis(ai_a_su_t5, observed).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_priority__existential_risk_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(ai_a_su_t10, observed).
narrative_ontology:measurement(ai_a_su_t15, ai_alignment_priority__existential_risk_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(ai_a_su_t15, observed).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_priority__existential_risk_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(ai_a_su_t20, observed).
narrative_ontology:measurement(ai_a_su_t25, ai_alignment_priority__existential_risk_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(ai_a_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__existential_risk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_alignment_priority__existential_risk_reading, 0.18).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_alignment_priority__integrated_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_alignment_priority__nearterm_harms_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_deployment_governance__precautionary_approach).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, algorithmic_discrimination__systemic_harm).

% DUAL FORMULATION NOTE:
% This constraint is part of the ai_alignment_priority kernel family. The existential-risk reading instantiates alignment as superintelligence-control prevention; it forecloses the integrated reading within the same framework (the binary 'existential OR distraction' logic rules out simultaneous prioritization) but coexists with the nearterm-harms reading as competing institutional positions. The network edges capture structural influence: this reading's resource allocation mechanism influences funding for the sibling readings and downstream constraints like ai_deployment_governance and algorithmic_discrimination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_priority__existential_risk_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
