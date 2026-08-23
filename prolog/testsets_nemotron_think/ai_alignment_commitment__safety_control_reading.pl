% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__safety_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_commitment__safety_control_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_alignment_commitment__safety_control_reading
 *   human_readable: AI Alignment as Catastrophic Control Prevention
 *   domain: technology_ethics/ai_governance/risk_assessment
 *
 * SUMMARY:
 *   The 'safety_control_reading' of AI alignment frames the field's purpose
 *   as preventing catastrophic loss of control over advanced AI systems — a
 *   speculative future scenario where AI capabilities exceed human ability to
 *   steer or shut them down. This reading, rooted in early 2000s
 *   rationalist/longtermist communities (MIRI, FHI, LessWrong), has captured
 *   the 'alignment' label and its associated resources: billions in
 *   philanthropic funding, elite talent pipelines, and increasing policy
 *   influence (e.g., US Executive Order on AI, UK AI Safety Summit). The
 *   constraint operates by defining 'real alignment' exclusively as technical
 *   control research, rendering work on present-day harms (bias,
 *   discrimination, labor, power concentration) as 'not alignment' and
 *   therefore lower priority. This is a tangled rope: it coordinates genuine
 *   safety research (a real coordination function for a non-zero risk) but
 *   extracts asymmetrically from communities addressing harms that are
 *   occurring now, using active enforcement (funding gates, hiring norms,
 *   conference norms, policy framing) to maintain the boundary.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__safety_control_reading, 0.78).
domain_priors:suppression_score(ai_alignment_commitment__safety_control_reading, 0.72).
domain_priors:theater_ratio(ai_alignment_commitment__safety_control_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__safety_control_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__safety_control_reading, "AI Alignment as Catastrophic Control Prevention").
narrative_ontology:topic_domain(ai_alignment_commitment__safety_control_reading, "technology_ethics/ai_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_alignment_commitment__safety_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__safety_control_reading, '3d4173a3-5ce6-4e6f-aed4-af3cd590f493').
narrative_ontology:cs_kernel_codification('3d4173a3-5ce6-4e6f-aed4-af3cd590f493', distributed).
narrative_ontology:cs_authority_grounding('3d4173a3-5ce6-4e6f-aed4-af3cd590f493', expertise).
narrative_ontology:cs_interpretation_layer_present('3d4173a3-5ce6-4e6f-aed4-af3cd590f493').
narrative_ontology:cs_reading_relation('3d4173a3-5ce6-4e6f-aed4-af3cd590f493', ai_alignment_commitment__ethics_justice_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d4173a3-5ce6-4e6f-aed4-af3cd590f493', ai_alignment_commitment__integrated_reading, influences).
narrative_ontology:cs_axiom('3d4173a3-5ce6-4e6f-aed4-af3cd590f493', foundational, existential_risk_lexical_priority).
narrative_ontology:cs_axiom_status(existential_risk_lexical_priority, holdable).
narrative_ontology:cs_axiom_grounding('3d4173a3-5ce6-4e6f-aed4-af3cd590f493', existential_risk_lexical_priority, deontological).
narrative_ontology:cs_axiom('3d4173a3-5ce6-4e6f-aed4-af3cd590f493', foundational, speculative_future_lives_moral_weight).
narrative_ontology:cs_axiom_status(speculative_future_lives_moral_weight, holdable).
narrative_ontology:cs_axiom_grounding('3d4173a3-5ce6-4e6f-aed4-af3cd590f493', speculative_future_lives_moral_weight, deontological).
narrative_ontology:cs_reference_frame('3d4173a3-5ce6-4e6f-aed4-af3cd590f493', early_ai_safety_foundational_framing).
narrative_ontology:cs_drift_state('3d4173a3-5ce6-4e6f-aed4-af3cd590f493', post_chatgpt_scaling_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3d4173a3-5ce6-4e6f-aed4-af3cd590f493', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__safety_control_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, ai_safety_researchers).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, longtermist_institutions).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, frontier_ai_labs).
narrative_ontology:constraint_victim(ai_alignment_commitment__safety_control_reading, present_harm_mitigation_communities).
narrative_ontology:constraint_victim(ai_alignment_commitment__safety_control_reading, algorithmic_fairness_researchers).
narrative_ontology:constraint_victim(ai_alignment_commitment__safety_control_reading, marginalized_communities_affected_by_current_ai).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__safety_control_reading, existential_risk_primacy).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__safety_control_reading, longtermist_moral_calculus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive dedicated funding streams, career tracks, and institutional recognition for work framed as preventing existential catastrophe. Their research agenda is set by longtermist priorities (interpretability, scalable oversight, value learning). Exit means leaving the field entirely or accepting marginalization within mainstream ML.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, ai_safety_researchers, beneficiary,
    organized, biographical, constrained, global).

% Control major funding pools (Open Philanthropy, LTFF, SFF), set research priorities through grantmaking and talent pipelines, and influence policy via government advisory roles. They define what counts as 'alignment' and capture the field's legitimacy. Exit options include shifting to other cause areas or leveraging policy access.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, longtermist_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__safety_control_reading, longtermist_institutions, beneficiary).

% Gain regulatory cover and talent pipeline by funding safety teams and adopting safety frameworks. The catastrophic framing lets them present voluntary commitments as sufficient governance while resisting near-term regulation. They can exit by open-sourcing or relocating, but benefit from the framing's dominance.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, frontier_ai_labs, beneficiary,
    institutional, biographical, arbitrage, global).

% Communities working on bias, discrimination, labor displacement, privacy violations, and environmental impacts of current AI systems. See funding, policy attention, and talent diverted to speculative risks. Their work is framed as 'near-term' or 'non-existential' and deprioritized. Exit means accepting marginalization or reframing work in longtermist terms.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, present_harm_mitigation_communities, payer,
    organized, biographical, constrained, global).

% Academic and industry researchers focused on measurable harms from deployed systems. Face hiring freezes, grant rejections, and conference marginalization when their work doesn't engage catastrophic risk frames. Many are women and minorities whose lived experience informs their research. Exit often means leaving the field.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, algorithmic_fairness_researchers, payer,
    moderate, biographical, constrained, global).

% People experiencing algorithmic discrimination in hiring, lending, policing, healthcare, and housing today. Their harms are dismissed as 'not existential' and therefore lower priority. They have no voice in alignment funding or priority-setting. Exit is impossible — they live the harms regardless of framing.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, marginalized_communities_affected_by_current_ai, excluded,
    powerless, immediate, trapped, global).

% Scholars from philosophy, law, sociology, and critical theory who center justice, rights, and power in AI governance. Structurally excluded from safety-dominated venues and funding. Their frameworks (distributive justice, participatory governance) are treated as orthogonal or hostile to 'technical alignment.'
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, ai_ethics_scholars, excluded,
    moderate, biographical, constrained, global).

% Government officials allocating AI R&D budgets and drafting regulation. Receive competing testimony from safety and ethics communities. Increasingly adopt safety language (e.g., 'frontier model regulation') while ethics provisions are weakened or deferred. Their analytical seat shapes which framing becomes law.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, policy_makers, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global research effort and resource allocation toward preventing loss-of-control scenarios in advanced AI systems, creating shared technical vocabulary, evaluation benchmarks, and talent pipeline for a problem defined as civilization-scale.
% TRANSFER_FUNCTION: Moves billions in philanthropic and public funding, top-tier research talent, and policy attention from measurable present-day AI harms (bias, discrimination, labor displacement, concentration of power) to speculative catastrophic risk prevention, justified by expected-value calculations over astronomical future populations.
% ABSENT_VOICES: Global Majority communities experiencing AI-driven extraction and surveillance; workers displaced by automation without safety nets; communities subject to algorithmic governance in welfare, policing, migration; near-term risk researchers whose funding was redirected; civil society organizations excluded from 'frontier AI' summits.
% DISAPPEARANCE_RATIONALE: If the catastrophic control framing vanished overnight, philanthropic funding would likely revert to broader AI ethics and governance portfolios; policy agendas would rebalance toward accountability, transparency, and redress for deployed systems; research talent would redistribute across near-term and long-term priorities; the 'alignment' field would fracture into multiple legitimate research programs.
% FOUNDING_PROBLEM: Early AI safety field (2000s-2010s, rooted in LessWrong/MIRI/FHI communities) identified the speculative risk of superintelligent AI systems escaping human control as the central technical problem of the century, arguing that solving 'the alignment problem' was a necessary condition for any positive future.
% FOUNDING_PROBLEM_CORROBORATION: Foundational figures (Yudkowsky, Bostrom, Russell) attest the control problem remains live and has grown more urgent with scaling. AI ethics pioneers (Buolamwini, Gebru, Noble, Benjamin) and Global South scholars attest the founding problem was always a narrow framing that erased existing power analysis, and that the field's persistence reflects resource capture not problem validity. Independent policy analysts (e.g., at Ada Lovelace Institute, Data & Society) document the measurable resource shift from present-harm to speculative-risk work since 2018.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__safety_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__safety_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__safety_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_commitment__safety_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__safety_control_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__safety_control_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_commitment__safety_control_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_commitment__safety_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the framing diverts the overwhelming majority of 'alignment' resources from present harm mitigation to speculative scenarios — a zero-sum shift documented in funding data and hiring trends. Suppression (0.72) is high because the constraint's persistence depends on actively marginalizing alternative framings: ethics researchers face grant rejections, conference rejections, and professional stigma for not engaging the catastrophic frame. Theater ratio (0.48) is moderate-rising: genuine technical work on interpretability and oversight exists, but a growing share of activity is performative 'safety washing' by labs and performative 'x-risk' signaling by funders. Accessibility collapse (0.75) is high because the lexical priority argument ('if extinction risk is non-zero, it dominates all else') makes alternatives structurally invisible within the frame. Resistance (0.58) is significant: the AI ethics community, labor organizers, Global South advocates, and critical scholars actively contest the framing, but lack the resource base to dislodge it.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (longtermist institutions), the constraint is a rope: genuine coordination to solve the most important problem. From the payer seats (present-harm researchers, marginalized communities), it is a snare: extraction disguised as coordination, enforced by funding gates and epistemic closure. From the excluded seats (ethics scholars, Global Majority), it is a piton: a degraded institutional form that performs 'safety' while entrenching power. The engine will compute this divergence from the structural data — the claimed_type (tangled_rope) reflects the analytical seat's view that both coordination and extraction are structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   Longtermist institutions are the structural beneficiaries (d ≈ 0.1): they control funding, define the research agenda, and capture policy access. AI safety researchers are beneficiaries (d ≈ 0.2): they gain careers and status within the frame but have constrained exit (leaving means losing the frame's resources). Frontier labs are beneficiaries with arbitrage exit (d ≈ 0.15): they extract regulatory cover and talent but can pivot. Present-harm communities are payers (d ≈ 0.85): they lose funding, attention, and policy priority with trapped or constrained exit. Marginalized communities are excluded (d ≈ 0.95): they bear the harms the frame renders invisible with no voice. Policy makers sit near analytical (d ≈ 0.5) but are pulled toward the beneficiary side by institutional incentives to fund 'frontier' agendas.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (superintelligence control) was always speculative. Whether it remains live is contested: safety proponents argue capabilities progress validates urgency; critics argue the framing was always a resource-capture vehicle that ignores the actual politics of AI deployment. The mandate has not been resolved — it has expanded. The original technical problem (value alignment for a singleton AGI) has metastasized into a vast field covering 'scalable oversight,' 'interpretability,' 'constitutional AI,' etc., each requiring more resources. The coordination function (preventing catastrophe) is real but the extraction function (diverting resources from present justice) has grown larger. This is mandatrophy: the arrangement persists because the beneficiaries control the definition of the problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is the ''ai_alignment_commitment'' a single kernel with multiple readings, or are these fundamentally different constraints masquerading under one label?',
    'Apply ε-invariance test: if measuring ''alignment'' via catastrophic risk prevention yields high ε while measuring via bias prevention yields low ε, they are distinct constraints. Track whether the same institutions, funding, and talent flow to both framings.',
    'If distinct constraints, each gets its own story and classification. The safety_control_reading would be tangled_rope (coordination + extraction); the ethics_justice_reading might be rope or scaffold. The integrated_reading would be a separate constraint claiming to solve both.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether the kernel label ''AI alignment'' covers one constraint or several structurally distinct ones.').

omega_variable(
    catastrophic_risk_epistemic_status,
    'Is the catastrophic loss-of-control scenario a genuine existential risk with non-negligible probability, or a speculative narrative that functions as a resource-capture mechanism?',
    'Forecasting track records of safety community vs. critics; empirical scaling evidence for/against sharp capability jumps; independent expert elicitation on P(doom) with calibrated forecasters.',
    'If genuine high probability, the extraction from present harms may be justified as tragic triage (rope-like). If speculative/low probability, the extraction is unjustified (snare-like). The engine''s classification will reflect this through χ at different seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(catastrophic_risk_epistemic_status, empirical, 'Epistemic status of the catastrophic risk claim that grounds the reading''s lexical priority argument.').

omega_variable(
    resource_diversion_zero_sum,
    'Is the resource shift from present-harm to catastrophic-risk work zero-sum (fixed pie) or does the safety framing grow the total AI governance pie?',
    'Track total AI governance funding over time; compare counterfactual funding for ethics work in absence of safety framing; analyze whether safety funding crowds in new donors (tech billionaires) who wouldn''t fund ethics.',
    'If zero-sum, extraction is direct transfer from victims to beneficiaries. If positive-sum, the coordination function may be net-beneficial even for some payer seats. Affects χ magnitude and classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_diversion_zero_sum, empirical, 'Whether the safety framing''s resource capture is zero-sum or expands total governance investment.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the marginalization of ethics/justice work structural (funding gates, hiring norms) or internalized (researchers self-censor, reframe work in safety terms to survive)?',
    'Post-exit suppression trajectory: track researchers who left safety-dominated venues — do they regain voice? Survey researchers on perceived pressure to adopt safety framing. Analyze conference CFP language shifts over time.',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint travels with the agent. If purely structural, exit (moving to different venues/funders) reduces χ. Affects directionality for payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the marginalization of present-harm work.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__safety_control_reading, 2015, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_alignment_safety_control_tr_t2015, ai_alignment_commitment__safety_control_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(ai_alignment_safety_control_tr_t2017, ai_alignment_commitment__safety_control_reading, theater_ratio, 2017, 0.28).
narrative_ontology:measurement(ai_alignment_safety_control_tr_t2019, ai_alignment_commitment__safety_control_reading, theater_ratio, 2019, 0.35).
narrative_ontology:measurement(ai_alignment_safety_control_tr_t2021, ai_alignment_commitment__safety_control_reading, theater_ratio, 2021, 0.42).
narrative_ontology:measurement(ai_alignment_safety_control_tr_t2023, ai_alignment_commitment__safety_control_reading, theater_ratio, 2023, 0.46).
narrative_ontology:measurement(ai_alignment_safety_control_tr_t2024, ai_alignment_commitment__safety_control_reading, theater_ratio, 2024, 0.48).

% Extraction over time
narrative_ontology:measurement(ai_alignment_safety_control_be_t2015, ai_alignment_commitment__safety_control_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(ai_alignment_safety_control_be_t2017, ai_alignment_commitment__safety_control_reading, base_extractiveness, 2017, 0.42).
narrative_ontology:measurement(ai_alignment_safety_control_be_t2019, ai_alignment_commitment__safety_control_reading, base_extractiveness, 2019, 0.55).
narrative_ontology:measurement(ai_alignment_safety_control_be_t2021, ai_alignment_commitment__safety_control_reading, base_extractiveness, 2021, 0.65).
narrative_ontology:measurement(ai_alignment_safety_control_be_t2023, ai_alignment_commitment__safety_control_reading, base_extractiveness, 2023, 0.73).
narrative_ontology:measurement(ai_alignment_safety_control_be_t2024, ai_alignment_commitment__safety_control_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(ai_alignment_safety_control_su_t2015, ai_alignment_commitment__safety_control_reading, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement(ai_alignment_safety_control_su_t2017, ai_alignment_commitment__safety_control_reading, suppression_requirement, 2017, 0.45).
narrative_ontology:measurement(ai_alignment_safety_control_su_t2019, ai_alignment_commitment__safety_control_reading, suppression_requirement, 2019, 0.55).
narrative_ontology:measurement(ai_alignment_safety_control_su_t2021, ai_alignment_commitment__safety_control_reading, suppression_requirement, 2021, 0.62).
narrative_ontology:measurement(ai_alignment_safety_control_su_t2023, ai_alignment_commitment__safety_control_reading, suppression_requirement, 2023, 0.68).
narrative_ontology:measurement(ai_alignment_safety_control_su_t2024, ai_alignment_commitment__safety_control_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__safety_control_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_alignment_commitment__safety_control_reading, 0.08).
narrative_ontology:affects_constraint(ai_alignment_commitment__safety_control_reading, ai_alignment_commitment__ethics_justice_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__safety_control_reading, ai_alignment_commitment__integrated_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ai_alignment_commitment kernel. The ethics_justice_reading defines alignment as preventing present-day harm reproduction; the integrated_reading claims both are required. This safety_control_reading captures the 'alignment' label and its resource base, making the ethics_justice_reading a marginalized alternative and the integrated_reading a reactive synthesis. The ε values differ sharply: safety_control ε≈0.78 (high extraction from present needs); ethics_justice ε would be lower (coordination around measurable harms); integrated ε depends on whether it achieves genuine synthesis or becomes a three-way extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_commitment__safety_control_reading, institutional, 0.15).
constraint_indexing:directionality_override(ai_alignment_commitment__safety_control_reading, organized, 0.2).
constraint_indexing:directionality_override(ai_alignment_commitment__safety_control_reading, moderate, 0.75).
constraint_indexing:directionality_override(ai_alignment_commitment__safety_control_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
