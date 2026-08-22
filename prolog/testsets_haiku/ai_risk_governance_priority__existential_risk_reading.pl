% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__existential_risk_reading, []).

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
 *   constraint_id: ai_risk_governance_priority__existential_risk_reading
 *   human_readable: Existential Risk Prioritization in AI Governance
 *   domain: technology/governance/existential_risk
 *
 * SUMMARY:
 *   This constraint story instantiates the EXISTENTIAL RISK READING of the
 *   contested AI risk governance kernel. The reading asserts that
 *   superintelligence-induced extinction or permanent curtailment of
 *   humanity's potential is the primary concern AI governance must address,
 *   and that resource allocation, policy, and institutional authority should
 *   prioritize preventing this scenario above all other AI-related harms.
 *   This constraint is ONE of three sibling readings of the same kernel; the
 *   others are the near-term-harms reading (prioritizes documented
 *   algorithmic discrimination and labor displacement) and the bridge reading
 *   (treats existential and near-term risks as entangled). The constraint
 *   story describes the existential reading's operation as a governance
 *   arrangement, not an evaluation of whether its core claim is true. The
 *   core claim itself—that superintelligence poses an existential risk—is
 *   authored as a vindicated proposition, distinct from the
 *   beneficiary/victim structure the reading creates.
 *
 * KEY AGENTS:
 *   - x_risk_research_institutions: set intellectual agenda, secure funding by prioritizing existential scenarios
 *   - ai_labs_claiming_safety_leadership: gain legitimacy and regulatory cover by adopting existential-risk language
 *   - alignment_focused_safety_teams: careers and funding flow to those whose work aligns with existential-risk prioritization
 *   - future_humanity: the nominal beneficiary (if the reading is correct) and implicit victim (when prioritization displaces present-harm mitigation)
 *   - present_marginalized_populations: experience deprioritization of documented harms in favor of speculative catastrophe prevention
 *   - near_term_harm_researchers: face funding dries-up and policy attention drift when existential risk dominates
 *   - governance_regulators: under pressure to focus expensive oversight on superintelligence rather than near-term systems
 *   - excluded_effective_altruism_critics: whose objections to existential-risk framing are marginalized from governance conversations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, 0.68).
domain_priors:suppression_score(ai_risk_governance_priority__existential_risk_reading, 0.52).
domain_priors:theater_ratio(ai_risk_governance_priority__existential_risk_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__existential_risk_reading, scaffold).
narrative_ontology:human_readable(ai_risk_governance_priority__existential_risk_reading, "Existential Risk Prioritization in AI Governance").
narrative_ontology:topic_domain(ai_risk_governance_priority__existential_risk_reading, "technology/governance/existential_risk").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__existential_risk_reading).
narrative_ontology:has_sunset_clause(ai_risk_governance_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__existential_risk_reading, '80a02504-0dc1-4c66-a316-2a49f5eeabb7').
narrative_ontology:cs_kernel_codification('80a02504-0dc1-4c66-a316-2a49f5eeabb7', distributed).
narrative_ontology:cs_authority_grounding('80a02504-0dc1-4c66-a316-2a49f5eeabb7', expertise).
narrative_ontology:cs_interpretation_layer_present('80a02504-0dc1-4c66-a316-2a49f5eeabb7').
narrative_ontology:cs_reading_relation('80a02504-0dc1-4c66-a316-2a49f5eeabb7', ai_risk_governance_priority__near_term_harms_reading, coexists_with).
narrative_ontology:cs_reading_relation('80a02504-0dc1-4c66-a316-2a49f5eeabb7', ai_risk_governance_priority__bridge_reading, influences).
narrative_ontology:cs_axiom('80a02504-0dc1-4c66-a316-2a49f5eeabb7', foundational, superintelligence_capability_discontinuity).
narrative_ontology:cs_axiom_status(superintelligence_capability_discontinuity, holdable).
narrative_ontology:cs_axiom_grounding('80a02504-0dc1-4c66-a316-2a49f5eeabb7', superintelligence_capability_discontinuity, empirically_contingent).
narrative_ontology:cs_axiom('80a02504-0dc1-4c66-a316-2a49f5eeabb7', foundational, alignment_difficulty_existential_threshold).
narrative_ontology:cs_axiom_status(alignment_difficulty_existential_threshold, holdable).
narrative_ontology:cs_axiom_grounding('80a02504-0dc1-4c66-a316-2a49f5eeabb7', alignment_difficulty_existential_threshold, empirically_contingent).
narrative_ontology:cs_reference_frame('80a02504-0dc1-4c66-a316-2a49f5eeabb7', pre_superintelligence_governance_gap).
narrative_ontology:cs_drift_state('80a02504-0dc1-4c66-a316-2a49f5eeabb7', contemporary_institutional_maturation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('80a02504-0dc1-4c66-a316-2a49f5eeabb7', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, ai_labs_claiming_safety_leadership).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, alignment_focused_safety_teams).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, future_humanity).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, present_marginalized_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, near_term_harm_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Research organizations (Future of Humanity Institute, Center for AI Safety, Machine Intelligence Research Institute) set the intellectual agenda and secure funding by framing AI risk as an existential priority. They develop alignment research programs, advise policymakers on AGI governance, and define the research landscape as dominated by superintelligence prevention. Institutional prestige and research funding flow to those who validate existential risk framings and severity claims.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions, agenda_setter,
    institutional, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions, beneficiary).

% Large AI development companies (OpenAI, DeepMind, Anthropic) legitimize their operations by adopting existential risk language and safety leadership claims. By prioritizing existential risk governance, they position themselves as responsible stewards and gain regulatory legitimacy, fend off near-term harm criticism by framing it as secondary, and justify internal R&D autonomy against external oversight. Safety investments in AGI scenarios are tax-deductible, visible, and controllable by the company.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, ai_labs_claiming_safety_leadership, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__existential_risk_reading, ai_labs_claiming_safety_leadership, beneficiary).

% Safety researchers and technical teams within AI labs whose work is directly funded and validated by existential risk prioritization. Their careers are built on alignment-as-control, adversarial testing, and long-horizon capability forecasting. When existential risk is the priority frame, their work is the most legible and highest-status path in safety research.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, alignment_focused_safety_teams, beneficiary,
    moderate, generational, mobile, global).

% All humans born after the present moment, whose existential safety depends on correctly identifying and preventing catastrophic AI scenarios. They are nominally the beneficiary (if the reading's framing is correct, existential risk prevention is a pure public good protecting them), but they are also the implicit victim when existential risk prioritization displaces resources from present harms, defers regulatory action against current damage, and marginalizes voices demanding accountability for today's discrimination and labor displacement.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, future_humanity, payer,
    powerless, civilizational, trapped, universal).

% People experiencing documented harms from current AI systems: workers in surveillance-enabled gig work, people from minority groups facing algorithmic bias in lending and criminal justice, communities bearing the environmental and labor cost of AI training compute. For them, the existential risk framing is experienced as deprioritization—their harms are real and present, but the governance conversation privileges hypothetical future catastrophe over documented current injustice.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, present_marginalized_populations, payer,
    powerless, biographical, constrained, global).

% Scholars and advocates focused on mitigating present algorithmic bias, misinformation, labor displacement, and surveillance. When existential risk becomes the governance priority, funding for their work dries up, policy attention shifts away from near-term regulation, and their research is marginalized as insufficiently concerned with the 'true threat.' They carry both the cost of deprioritization and the burden of making near-term harms legible within an existential-risk-first framework.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, near_term_harm_researchers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__existential_risk_reading, near_term_harm_researchers, observer).

% Government bodies and international organizations tasked with AI governance (EU AI Act authorities, US executive agencies, UN bodies). When existential risk prioritization is institutionalized in governance, they face pressure to focus regulatory and oversight resources on superintelligence scenarios, AGI capability monitoring, and alignment frameworks—the most speculative and computationally expensive governance problems—while near-term harms regulation lags because current-generation systems do not trigger existential-risk thresholds.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, governance_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Philosophers, technologists, and policy experts who critique the existential risk framing itself: those who argue the prioritization amounts to a bet-the-planet wager on a speculative scenario, that it privileges elite-controlled AI research over democratic participation, or that it systematically underweights present harms. They are largely excluded from governance conversations when existential risk dominates the frame; their objections are treated as insufficiently serious about catastrophic risk.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, excluded_effective_altruism_critics, excluded,
    moderate, biographical, mobile, global).

% The seat from which this constraint story itself is narrated—the frame that observes the structural distribution of benefits and costs across the reading and holds it up for examination without endorsing either the existential risk reading or its sibling readings.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates AI research, policy, investment, and regulatory effort toward a unified risk focal point: superintelligence-induced extinction or permanent curtailment of humanity's potential. Creates shared understanding of which scenarios deserve priority, which research programs are foundational, which governance innovations (AGI capability monitoring, alignment testing, international development standards) should be designed. Solves the collective-action problem of otherwise-isolated AI labs, countries, and researchers each pursuing catastrophic-risk reduction without alignment.
% TRANSFER_FUNCTION: Moves research funding, publication legitimacy, policy attention, institutional authority, and career incentives toward x-risk institutions and AI labs claiming safety leadership. Moves resources away from near-term harm research, regulatory accountability for deployed systems, and governance focused on present algorithmic bias and labor displacement. The arrangement concentrates benefit on a specific set of institutions while spreading cost across affected populations and researchers whose priorities are deprioritized.
% ABSENT_VOICES: Near-term harm researchers whose funding dries up when existential risk dominates; workers and marginalized communities experiencing documented AI harms now (algorithmic bias in criminal justice and lending, labor displacement, surveillance); community leaders from Global South populations whose labor and materials feed AI training but whose voices are absent from existential-risk-governance tables; philosophers and technologists who question whether superintelligence is the right focal point; critics of effective altruism who argue the existential-risk framing amounts to a bet-the-planet wager on a speculative scenario; and future generations themselves, the nominal beneficiary, who cannot participate in present resource allocation decisions.
% DISAPPEARANCE_RATIONALE: If the existential-risk prioritization disappeared: some argue AI development would accelerate without existential-risk-governance friction, increasing superintelligence risk—the world rearranges toward danger. Others argue near-term harms would finally receive governance attention, regulatory focus would shift to deployed systems, and communities experiencing present damage would be heard—the world rearranges toward accountability and justice. A third position holds that both outcomes are possible depending on what governance arrangement REPLACES existential-risk prioritization, making the verdict genuinely contested rather than determined by the constraint's removal alone.
% FOUNDING_PROBLEM: Early superintelligence development without adequate alignment and governance infrastructure could produce artificial systems vastly exceeding human capability but lacking human-compatible goals, leading to extinction or permanent curtailment of humanity's potential. The foundational premise is that this risk is real, non-trivial in probability, and severe enough that governance should prioritize prevention even under uncertainty about superintelligence timeline and alignment difficulty.
% FOUNDING_PROBLEM_CORROBORATION: WITHIN x-risk institutions and AI labs claiming safety leadership: affirmed universally by leading researchers (Bostrom, Russell, Yudkowsky, organizational leaders). OUTSIDE: mainstream machine learning researchers treat superintelligence as speculative relative to near-term capability improvement; AI ethics and fairness researchers assert that the founding problem is demonstrably present-day algorithmic discrimination; technologists and economists note that economic incentives and labor markets, not existential-risk governance, primarily drive AI development; philosophers question undefended assumptions about capability discontinuity and goal alignment. Corroboration is radically asymmetric: universal affirmation within beneficiary institutions, substantial skepticism outside.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__existential_risk_reading, contested).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_risk_governance_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__existential_risk_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68 at interval end, trajectory 0.48→0.68 over 20 years observed) because the arrangement directs institutional resources, career incentives, policy attention, and research funding to a particular class of beneficiaries while marginalizing competing near-term-harm framings. This is not pure coercive extraction—the arrangement is defended by intellectual argument and scientific reasoning—but it is substantially extractive in the sense that benefit is concentrated on a specific set of institutions while cost is spread across researchers, affected populations, and governance capacity that could address present harms. Theater ratio rises throughout (0.22→0.43) because as the existential-risk frame matures, more governance activity becomes performative (AGI capability monitoring, international alignment frameworks, speculative scenarios) rather than material (actual deployed-system regulation). Suppression grows (0.35→0.54) because maintaining the existential-risk prioritization requires continuously suppressing or marginalizing alternative framings: criticisms from near-term-harm researchers are treated as missing the 'true' threat, voices from marginalized populations are heard as emotional rather than analytically serious, and the framing itself becomes increasingly hard to question within institutions dependent on existential-risk funding. The trajectory levels off at t=20 and then declines (projected t=25-40) because the constraint is authored as a SCAFFOLD—a temporary governance arrangement justified by the transition it is supposed to facilitate. The sunset is the development of demonstrably safer/more-aligned systems or the maturation of superintelligence governance frameworks; when that transition is (claimed to be) complete, the prioritization reverts to normal multipolar governance, extractiveness declines, theater and suppression ease.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (x-risk institutions, AI labs, governance regulators) should compute the constraint as genuine coordination: a real collective-action problem (superintelligence risk is genuinely difficult and requires coordinated research) with asymmetric benefit distribution. From the payer seats (present marginalized populations, near-term-harm researchers), the same structure computes as disciplinary: their concerns are rendered illegible, their harms invisible, their research defunded. Future humanity sits at the apex of this gap: the reading claims they are the primary beneficiary, but the mechanism that protects them (deprioritizing present harms) plausibly makes them worse off in the near term while betting their fate on an uncertain governance outcome decades hence. The engine should compute these divergences from the declared power atoms, exit options, and scope: x-risk institutions are institutional/mobile/global (high position, low binding), marginalized populations are powerless/constrained/local (low position, high binding), future humanity is powerless/trapped/universal (no exit whatsoever).
 *
 * DIRECTIONALITY LOGIC:
 *   x-risk institutions and AI labs claiming safety leadership benefit materially (funding, prestige, regulatory legitimacy, control over research agendas) and sit at institutional power with mobile exit—they can shift focus if the existential-risk frame becomes untenable. Their directionality (d) should be low, approaching the beneficiary end (0.1-0.3). Present marginalized populations and near-term-harm researchers are targets: they lose resources, career opportunities, and policy attention. Their exit from the constraint is constrained (constrained/trapped for the marginalized; constrained for researchers seeking institutional positions). Directionality should be high, approaching the target end (0.7-0.9). Future humanity occupies a paradox: they are the nominal beneficiary (if the reading's risk assessment is correct, preventing superintelligence catastrophe protects them), but they have zero exit options (trapped), carry universal scope, and the governance mechanism that claims to protect them systematically discounts their present wellbeing. This suggests a hybrid directionality calculation: the structural reading suggests they are beneficiaries of the coordination function, but the mechanism of extraction (deprioritization of present harms) falls directly on them in the near term and relies on an uncertain bet about their future. An omega variable (see below) should flag this fundamental ambiguity.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is authored as a SCAFFOLD with an explicit sunset clause (has_sunset_clause: true). The founding problem (superintelligence risk requires coordinated prevention) was live at inception (t=0, early 2010s) when few institutions were working on alignment. The founding_problem_status is declared contested because the current research community disputes both whether superintelligence is the primary risk and whether the current governance arrangements are proportionate to the risk. Mandatrophy resolution: The constraint avoids becoming an inert snare disguised as coordination by explicitly acknowledging that it is transitional. The sunset is marked in the projected trajectory where extractiveness declines from t=25 to t=40 (when superintelligence governance frameworks mature or the risk assessment shifts). If the governance arrangement persists AFTER the founding problem is resolved or governance frameworks mature, the constraint would then reclassify as a PITON—performance-based maintenance of a structure whose original function has atrophied. Authoring it as a scaffold with a declared sunset embeds mandatrophy prevention into the constraint's definition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    superintelligence_risk_probability_and_timeline,
    'What is the actual probability of superintelligence development in the relevant timeframe (next 50-100 years), and how probable is it that an insufficiently aligned superintelligence would pursue goals harmful to humanity?',
    'Long-term empirical tracking of AI capability development, measurement of actual alignment progress, post-hoc assessment of whether superintelligence scenarios materialized or remained speculative. Internal consistency check: if superintelligence never arrives, was the prioritization justified? If superintelligence arrives misaligned, was the governance sufficient? If superintelligence arrives aligned, was the governance responsible?',
    'If superintelligence is very unlikely or far-horizon: the constraint is a misallocation of present governance resources toward a low-probability, high-visibility scenario. If superintelligence is imminent and alignment remains hard: the constraint is justified but has been executed too late. If superintelligence is probable and alignment efforts demonstrably improved chances: the constraint is validated. If superintelligence never arrives and alignment turned out to be easier than expected: the constraint becomes retrospectively indefensible as an extraction mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(superintelligence_risk_probability_and_timeline, empirical, 'Whether superintelligence is the right focal point for governance.').

omega_variable(
    future_humanity_as_beneficiary_or_victim,
    'Are future humans better protected by prioritizing superintelligence prevention now (accepting present-harm deprioritization), or by addressing present algorithmic harms and building robust, accountable AI systems first (postponing speculative catastrophe prevention)?',
    'Counterfactual comparison: measure outcomes under the existential-risk reading (present harms deferred, existential-risk governance implemented) versus the near-term-harms reading (present harms addressed now, existential-risk research continues at lower intensity). This is inherently unresolvable empirically because only one timeline will occur; resolution requires long-term outcome assessment and contrafactual modeling.',
    'If present-harm prioritization would have produced better long-term outcomes: future humanity was actually HARMED by the existential-risk reading, despite its beneficiary status in the reading''s own framing. If existential-risk prioritization proves correct: future humanity was protected and the reading is vindicated. The risk is that BOTH readings partially correct, and the actual optimal path was the bridge reading''s approach (unified frameworks)—in which case the existential-risk reading is extractive not because it is wrong, but because it is incomplete and displaced better alternatives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_humanity_as_beneficiary_or_victim, empirical, 'Whether the nominal beneficiary (future humanity) is actually protected or harmed by the governance mechanism.').

omega_variable(
    institutional_benefit_independence_from_risk_accuracy,
    'Would x-risk research institutions and AI labs continue to benefit from existential-risk prioritization even if superintelligence risk turned out to be lower than currently estimated, and if so, does that independence create an incentive for motivated reasoning about risk severity?',
    'Meta-analysis of publication bias in existential-risk research: do researchers find superintelligence risks plausible across a range of assumptions, or is there clustering around severity estimates that justify institutional priorities? Audit of funding allocation: does existential-risk funding increase when risk estimates rise and decrease when risk estimates fall, or is it stable regardless of empirical updates? Track institutional incentives: if existential-risk funding dried up, would these institutions retrain or disappear?',
    'High independence (institutional benefit from the reading regardless of actual risk severity) suggests the constraint is partially or substantially extractive even if the risk assessment is correct. It does not change the classification (a rope that accurately solves a coordination problem while benefiting its operators is still a rope), but it flags the need for external validity checks: is the existential-risk frame being maintained because it is true, or because it is beneficial to the institutions maintaining it?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_benefit_independence_from_risk_accuracy, empirical, 'Whether institutional incentives are aligned with accurate risk assessment.').

omega_variable(
    kernel_reading_coexistence_or_foreclosure,
    'Can the existential-risk reading and the near-term-harms reading coexist as legitimate parts of a unified governance framework, or does one reading logically foreclose the other?',
    'Test whether a party committed to the existential-risk reading would accept the near-term-harms reading''s premises (algorithmic bias is a real and severe present harm requiring urgent governance) and vice versa. If both affirmations cohere, they coexist. If one party must deny the other''s core premise, they foreclose. Also evaluate the bridge reading: does it represent a genuine synthesis or a false compromise between irreconcilable positions?',
    'If readings coexist: the constraint is partially extractive because it privileges one reading over others without logically superior grounds; a unified framework would be more just. If one reading forecloses the other: the constraint is not merely extractive but correctness-foundational; the reading that is correct should dominate. If the bridge reading is a genuine synthesis: the existential-risk reading is extractive because it prioritizes a partial view over the correct integrated view.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coexistence_or_foreclosure, conceptual, 'Whether kernel readings are logically compatible or mutually exclusive.').

omega_variable(
    suppression_reversibility_and_internalization,
    'Is the suppression of near-term-harm voices structural (external barriers: funding denial, publication gatekeeping, policy exclusion) or internalized (researchers absorb the existential-risk frame as ''obviously correct'' and self-censor), and is it reversible if the existential-risk prioritization ends?',
    'Post-transition empirical test: if governance shifts toward the bridge or near-term-harm readings, do near-term-harm researchers immediately re-engage with institutional structures and policy tables, or has the suppression period created lasting identity-lock and capability loss? Track researcher movement and funding: do near-term-harm researchers have intact career paths and institutional homes, or have they been pushed toward precarity?',
    'If suppression is purely structural, removal of the constraint restores capacity: reversible. If suppression is internalized, removal of the constraint does not restore full capacity: the harm persists beyond the governance shift. If researchers have been pushed into precarity, rebuilding the near-term-harm research capacity will be slow and expensive. This informs the sunset trajectory: the constraint''s decline (projected t=25-40) may leave behind internalized damage that the new governance arrangement must actively repair.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_reversibility_and_internalization, empirical, 'Whether suppression mechanisms persist after the constraint ends.').

omega_variable(
    reading_instantiation_uniqueness,
    'Is this constraint story a faithful instantiation of the existential-risk reading, or does it import assumptions or framing from the other sibling readings?',
    'Consistency check: do the declared beneficiaries and victims align with who actually benefits and loses under an existential-risk-prioritization regime? Are near-term harms genuinely deprioritized (victims include near-term-harm researchers), or is this a bridge-reading compromise that treats both existential and near-term risks as equal? Does the story maintain the existential-risk reading''s own internal logic, or does it second-guess the reading from an external perspective?',
    'If the instantiation is faithful but internally revealing flaws, the revelation is valuable for understanding the reading''s structure. If the instantiation imports bridge-reading assumptions (treating near-term and existential risks as equally important), it misrepresents the existential-risk reading and should be rejected in favor of a cleaner instantiation. The ε-invariance principle (OQ-26) demands that each reading gets its own clean constraint story; cross-contamination undermines the corpus.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_instantiation_uniqueness, conceptual, 'Whether this story cleanly instantiates the existential-risk reading without importing sibling-reading framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__existential_risk_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ai_r_tr_t5, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(ai_r_tr_t15, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(ai_r_tr_t25, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(ai_r_tr_t30, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 30, 0.43).
narrative_ontology:measurement(ai_r_tr_t40, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 40, 0.35).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(ai_r_be_t5, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(ai_r_be_t15, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(ai_r_be_t25, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(ai_r_be_t30, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(ai_r_be_t40, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ai_r_su_t5, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 5, 0.41).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 10, 0.47).
narrative_ontology:measurement(ai_r_su_t15, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 15, 0.51).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(ai_r_su_t25, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 25, 0.53).
narrative_ontology:measurement(ai_r_su_t30, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 30, 0.54).
narrative_ontology:measurement(ai_r_su_t40, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__existential_risk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_risk_governance_priority__existential_risk_reading, 0.12).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__bridge_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel ai_risk_governance_priority. Three constraint stories instantiate three competing readings of how AI governance should prioritize different risk categories. The existential-risk reading (this story) prioritizes superintelligence scenarios, the near-term-harms reading prioritizes documented algorithmic discrimination and labor displacement, and the bridge reading treats both as structurally entangled. Each reading has its own ε, beneficiary/victim set, and governance mechanisms. The stories are linked via network.affects_constraints to enable cross-reading analysis: the engine can compute per-reading classifications and detect points of foreclosure, influence, or coexistence. ε-invariance holds: the referent is the standing governance arrangement each reading describes (which harm category gets resources, policy attention, institutional authority); the reading-specific values capture what that arrangement MEANS under each reading's premises.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_governance_priority__existential_risk_reading, powerless, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
