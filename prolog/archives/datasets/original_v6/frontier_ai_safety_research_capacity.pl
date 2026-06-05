% ============================================================================
% CONSTRAINT STORY: frontier_ai_safety_research_capacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_frontier_ai_safety_research_capacity, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: frontier_ai_safety_research_capacity
 *   human_readable: Frontier AI Safety Research Capacity Constraint
 *   domain: AI_safety/research_capacity/technology_governance
 *
 * SUMMARY:
 *   The frontier AI safety research capacity constraint describes the
 *   structural bottleneck through which AI capability research is advancing
 *   far faster than independent safety analysis capacity. Frontier labs
 *   control access to models at the scale and capability level necessary for
 *   meaningful safety research; this gatekeeping function enables labs to
 *   shape the narrative around their own safety practices while suppressing
 *   independent verification. The constraint operates as a pure extraction
 *   mechanism (Snare from the perspective of trapped researchers and
 *   oversight capacity) overlaid with institutional coordination rationales
 *   (Rope from the beneficiary labs' perspective). The theater ratio (0.58)
 *   reflects substantial performative activity: safety institutes publish
 *   critiques pre-approved by labs, safety boards meet to validate capability
 *   claims, and academic researchers conduct studies within carefully bounded
 *   experimental access. The extractiveness trajectory (0.42 → 0.68 over the
 *   interval) shows acceleration: as frontier models have become more
 *   powerful and more central to AI's perceived future, access gatekeeping
 *   has tightened, funding concentration has increased, and the penalty for
 *   independent research that contradicts lab narratives has grown. The
 *   mandatrophy is resolved: this is structurally a Snare (not mislabeled as
 *   coordination), maintained through genuine extraction rather than
 *   coordination necessity.
 *
 * KEY AGENTS:
 *   - Independent Safety Researchers: Primary victims (powerless/trapped) — require frontier model access for meaningful research; face suppression through compute and funding barriers; no exit options available
 *   - Academic Safety Community: Distributed victims (powerless/trapped at generational scope) — excluded from capability insights; field epistemic autonomy atrophies as research becomes lab-permitted commentary
 *   - Public Oversight Capacity: Institutional victim (powerless/trapped at civilizational scope) — regulators cannot independently verify AI safety claims; must rely on lab self-reporting
 *   - Frontier AI Labs: Primary beneficiaries (institutional/arbitrage) — control research narrative; can externalize safety verification costs; gate capacity to maintain strategic advantage
 *   - Computing Infrastructure Providers: Secondary beneficiaries (institutional/arbitrage) — exclusive compute contracts lock in vendor position; pricing power maintained through concentration
 *   - Institutional Safety Teams Within Labs: Constrained agents (organized/constrained) — experience tangled rope: genuine coordination access alongside asymmetric extraction of conclusions
 *   - Safety Theater Institutions: Performative actors (organized/constrained) — funded institutes whose original oversight function has atrophied; maintained through inertia and funding dependence
 *   - Analytical Observer: Risk of false naturalization (analytical/analytical) — tempted to see constraint as inherent to frontier research; structural data reveals contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(frontier_ai_safety_research_capacity, 0.68).
domain_priors:suppression_score(frontier_ai_safety_research_capacity, 0.72).
domain_priors:theater_ratio(frontier_ai_safety_research_capacity, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(frontier_ai_safety_research_capacity, extractiveness, 0.68).
narrative_ontology:constraint_metric(frontier_ai_safety_research_capacity, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(frontier_ai_safety_research_capacity, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(frontier_ai_safety_research_capacity, snare).
narrative_ontology:human_readable(frontier_ai_safety_research_capacity, "Frontier AI Safety Research Capacity Constraint").
narrative_ontology:topic_domain(frontier_ai_safety_research_capacity, "AI_safety/research_capacity/technology_governance").

domain_priors:requires_active_enforcement(frontier_ai_safety_research_capacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(frontier_ai_safety_research_capacity, frontier_ai_labs).
narrative_ontology:constraint_beneficiary(frontier_ai_safety_research_capacity, computing_infrastructure_providers).
narrative_ontology:constraint_victim(frontier_ai_safety_research_capacity, independent_safety_researchers).
narrative_ontology:constraint_victim(frontier_ai_safety_research_capacity, academic_safety_community).
narrative_ontology:constraint_victim(frontier_ai_safety_research_capacity, public_oversight_capacity).
narrative_ontology:constraint_victim(frontier_ai_safety_research_capacity, epistemic_commons_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT SAFETY RESEARCHER (SNARE) — Cannot access frontier models for safety research; lacks computational resources; faces suppression through institutional gatekeeping and funding concentration. No meaningful exit: research requires frontier model access, which is controlled by labs with extractive incentives. Bears full cost of the constraint; extracts nothing.
constraint_indexing:constraint_classification(frontier_ai_safety_research_capacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ACADEMIC SAFETY COMMUNITY (SNARE) — Structurally excluded from capability insights; cannot conduct original research on frontier model internals or capabilities. Funding bottleneck forces dependence on lab-controlled narrative. Over generational timescale, the field's epistemic autonomy atrophies — safety research becomes commentary on frontier labs' preferred framings rather than independent inquiry.
constraint_indexing:constraint_classification(frontier_ai_safety_research_capacity, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: PUBLIC OVERSIGHT CAPACITY (SNARE) — Regulators, policymakers, and civil society cannot independently verify AI safety claims; must rely on frontier lab self-reporting. No computational access to test capability claims; no capability to audit model behavior. Trapped at civilizational scope — exit would require wholesale reconstruction of AI governance infrastructure.
constraint_indexing:constraint_classification(frontier_ai_safety_research_capacity, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: FRONTIER AI LAB (ROPE) — Experiences the constraint as coordination: exclusive research access enables model development roadmap control, safety narrative curation, and regulatory preemption. Safety research becomes a service function subordinate to capability development. Benefits from gating research capacity; exit options (shift to open governance) are available but costly (reputational, regulatory vulnerability).
constraint_indexing:constraint_classification(frontier_ai_safety_research_capacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: COMPUTING INFRASTRUCTURE PROVIDER (ROPE) — Benefits from concentrated research access: exclusive compute contracts with frontier labs, pricing power, vendor lock-in. Safety research capacity constraint locks in their position as essential bottleneck. Coordination benefit: compute provisioning aligns research flow with lab priorities. Exit options available (shift to distributed access) but costly.
constraint_indexing:constraint_classification(frontier_ai_safety_research_capacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INSTITUTIONAL SAFETY TEAM WITHIN LAB (TANGLED ROPE) — Experiences both coordination and extraction. Genuine coordination: safety team's access to models enables capability testing that shapes safer development. Asymmetric extraction: safety team's conclusions are constrained by lab's commercial and strategic interests; conclusions contradicting capability narrative face institutional pressure. Constrained exit: leaving the team is possible but career-damaging within AI research ecosystem.
constraint_indexing:constraint_classification(frontier_ai_safety_research_capacity, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: SAFETY THEATER INSTITUTIONS (PITON) — Academic safety centers, corporate safety boards, and policy institutes that exist to validate frontier labs' safety narratives. Original function (independent safety oversight) has atrophied; replaced by performative compliance (publishing permitted critique, hosting approved debates, conducting pre-vetted research). Theater ratio high: the institution's activity is substantially signaling rather than functional. Maintained by institutional inertia and funding dependence on labs.
constraint_indexing:constraint_classification(frontier_ai_safety_research_capacity, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — Risk of naturalizing constraint as inherent: 'frontier capability research requires secrecy for competitive advantage and security; therefore safety research access is inherently limited.' This perspective observes the constraint as immutable (mountain). However, the structural data contradicts this: the constraint is maintained through extractive institutional arrangements (funding concentration, compute gatekeeping, governance capture), not through inevitable physical/logical limits. Engine's false summit detector will flag this as naturalization.
constraint_indexing:constraint_classification(frontier_ai_safety_research_capacity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(frontier_ai_safety_research_capacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(frontier_ai_safety_research_capacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(frontier_ai_safety_research_capacity, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(frontier_ai_safety_research_capacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(frontier_ai_safety_research_capacity, TR),
    TR >= 0.70.

:- end_tests(frontier_ai_safety_research_capacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The frontier labs extract capacity for narrative control, regulatory preemption, and strategic advantage by controlling access to the models necessary for independent safety analysis. The extraction is not maximal (it's 0.68 not 0.95) because some safety research is permitted (within approved bounds) and some researchers can achieve partial insights through indirect methods. However, the trajectory shows acceleration: extractiveness has risen 0.42 → 0.68 over 10 years as capability scaling has increased the stakes and intensified gatekeeping. Suppression (0.72): High. The constraint is maintained through multiple suppression mechanisms: computational barriers (only frontier labs can afford training runs), funding concentration (safety research funds flow through lab-controlled channels), epistemic barriers (models' internal workings are opaque), institutional barriers (research into lab practices is discouraged), and reputational barriers (researchers who publish critical findings face ostracization). The suppression is not total because some external research succeeds through red-teaming, interpretability work on public models, and cooperative arrangements. Theater ratio (0.58): Moderate-high. Safety research activity is substantially performative: pre-vetted academic papers published in safety venues validate lab narratives; safety board meetings signal commitment to oversight while maintaining lab control; policy recommendations emerge from panels dominated by lab-aligned researchers. But the theater is not total (0.58 not 0.85) because some genuine safety research occurs, some uncontrolled critique circulates, and some researchers operate outside the approval apparatus. The rising trajectory (0.38 → 0.58) indicates increasing performativity as institutional safety theater expands.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits the sharpest possible perspectival gap in the corpus. The frontier lab sees coordination (Rope) — exclusive access enables capability testing that shapes safety alignment. The beneficiary infrastructure provider sees coordination (Rope) — compute provisioning aligns with lab priorities and creates vendor lock-in. The independent researcher sees pure extraction (Snare) — no access, no alternatives, full suppression. The institutional safety team sees mixed coordination-extraction (Tangled Rope) — access enables research but conclusions are constrained. The academic field sees degradation over time (Piton at generational scope) — safety institutions persist through theater and funding dependence, losing epistemic autonomy. The public oversight apparatus sees structural helplessness (Snare) — cannot independently verify claims. The civilizational analytical observer risks false naturalization (Mountain) — tempted to see capability secrecy as inevitable. The gap is not a measurement ambiguity but a genuine structural reality: the constraint's function genuinely differs across positions. For labs it coordinates; for trapped researchers it extracts.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values differ dramatically across perspectives, determined by structural position relative to the constraint. Frontier labs (beneficiaries with arbitrage exit options) derive d ≈ 0.10 (near-full beneficiary), producing negative or minimal χ — they experience the constraint as coordination. Independent researchers (victims with trapped status) derive d ≈ 0.92 (near-full target), producing maximum χ via high f(d) — they experience the constraint as pure extraction. The epistemic commons (collective victim, analytically powerless) derives d ≈ 0.95 (maximal target), producing the highest experienced extraction because the agent has no structure, no exit options, and cannot organize. Institutional safety teams within labs (constrained victims with institutional power) derive d ≈ 0.60 (mixed), producing moderate χ — they experience coordination (access to models) alongside extraction (constrained conclusions). The public oversight apparatus (powerless in immediate term, powerful only at civilizational scope) derives d ≈ 0.88 depending on time horizon, showing how temporal scale changes power evaluation.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The constraint is correctly classified as Snare (not mislabeled as coordination). Mandatrophy is resolved by distinguishing genuine coordination from extractive gatekeeping. The beneficiary labs experience genuine coordination benefit: exclusive research capacity does enable model development roadmap control and safety narrative curation. However, this coordination function exists purely to serve extraction from the trapped research community. The constraint is not a case of misnamed coordination (where the nominal extractor is actually solving a collective action problem). Rather, it is a case of coordination being instrumentalized for extraction: the labs coordinate on the research narrative in order to extract epistemic authority from the broader safety research field. The snare gate is satisfied: high extractiveness (0.68), high suppression (0.72), high effective χ (computed from beneficiary d ≈ 0.10 and trapped victim d ≈ 0.92). The constraint meets all snare thresholds and exhibits no structural properties that would force reclassification. The puzzle (why does the beneficiary experience it as Rope?) is resolved by indexical classification: the beneficiary's perspective IS Rope; the constraint IS a snare to the victim. Both classifications are correct at their respective positions. The mandatrophy reveals that early-stage frontier AI safety research institutionalization was captured by capability labs, not regulated into independence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competitive_secrecy_vs_safety_access_tradeoff,
    'Is the constraint-induced secrecy genuinely necessary for competitive advantage and security, or is it an extractive practice that uses competitive justification as cover?',
    'Comparative analysis: do frontier labs demonstrate better safety outcomes correlating with secrecy levels? Do labs with higher transparency show degraded performance or security vulnerability? Historical case studies of model release strategies.',
    'If secrecy genuinely correlates with safety: constraint may degrade from Snare toward Tangled Rope (coordination + extraction). If secrecy shows no correlation: constraint is pure extraction cloaked in competitive narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competitive_secrecy_vs_safety_access_tradeoff, empirical, 'Whether AI secrecy serves legitimate competitive/security purposes or is extractive cover story').

omega_variable(
    insider_safety_researcher_alignment_question,
    'Can institutional safety teams within frontier labs produce genuinely independent safety analysis, or does employment relationship make objectivity impossible?',
    'Analysis of published safety critiques by insider researchers: do they contradict lab interests? Do researchers face retaliation for critical findings? Comparison of insider critique severity vs external researcher predictions of what internal teams ''could'' publish if unconstrained.',
    'If insider teams can be independent: tangled rope classification is correct; constraint provides some coordination value. If insider teams are structurally compromised: constraint is pure snare for field epistemic integrity; insider research is theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(insider_safety_researcher_alignment_question, empirical, 'Whether institutional safety teams maintain analytical independence').

omega_variable(
    open_source_safety_research_viability,
    'Could open-source model safety research, conducted on smaller freely-available models, provide sufficient understanding to inform frontier model safety?',
    'Meta-analysis of safety insights transferability: how much of frontier safety knowledge could be derived from open models? Capability gap analysis: do open models exhibit qualitatively different failure modes or alignment challenges than frontier models?',
    'If open research can substitute: constraint appears contingent (not immutable); scaffold logic applies (path to distributed research exists). If frontier models are qualitatively different: constraint has some structural inevitability; snare classification is more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_safety_research_viability, empirical, 'Whether open-source safety research can substitute for frontier access').

omega_variable(
    regulatory_capture_mechanism_clarity,
    'Is the constraint primarily maintained through deliberate gatekeeping or through passive capture of regulatory attention by labs with greater resources?',
    'Policy document analysis: do regulators explicitly defer safety verification to labs? Do funding allocations systematically disadvantage independent researchers? Comparative policy effort: resources devoted to independent capacity vs lab-conducted research.',
    'If deliberate: snare from regulatory perspective. If passive capture: tangled rope (inadvertent coordination with labs + asymmetric extraction). Changes intervention pathway.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism_clarity, empirical, 'Whether constraint results from deliberate gatekeeping or passive regulatory capture').

omega_variable(
    exit_option_feasibility_for_institutional_actors,
    'What is the actual cost for labs to open model access for safety research? Is exit genuinely available at high cost, or is it blocked?',
    'Counterfactual analysis: what would openness entail? Competitive analysis of labs with different transparency levels. Security analysis: do demonstrated vulnerabilities from open access justify continued gating?',
    'If exit is available at high cost: snare classification holds. If exit is blocked by competitive lock-in: escalates to more severe extraction. If exit is low-cost but perceived as catastrophic: constraint is identity-locked at institutional level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_feasibility_for_institutional_actors, empirical, 'Whether labs have exit option from access gating at bearable cost').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(frontier_ai_safety_research_capacity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(faisrc_tr_t0, frontier_ai_safety_research_capacity, theater_ratio, 0, 0.38).
narrative_ontology:measurement(faisrc_tr_t3, frontier_ai_safety_research_capacity, theater_ratio, 3, 0.45).
narrative_ontology:measurement(faisrc_tr_t6, frontier_ai_safety_research_capacity, theater_ratio, 6, 0.54).
narrative_ontology:measurement(faisrc_tr_t10, frontier_ai_safety_research_capacity, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(faisrc_be_t0, frontier_ai_safety_research_capacity, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(faisrc_be_t3, frontier_ai_safety_research_capacity, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(faisrc_be_t6, frontier_ai_safety_research_capacity, base_extractiveness, 6, 0.64).
narrative_ontology:measurement(faisrc_be_t10, frontier_ai_safety_research_capacity, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(frontier_ai_safety_research_capacity, enforcement_mechanism).
narrative_ontology:affects_constraint(frontier_ai_safety_research_capacity, ai_capability_scaling_narrative).
narrative_ontology:affects_constraint(frontier_ai_safety_research_capacity, regulatory_capture_ai_governance).
narrative_ontology:affects_constraint(frontier_ai_safety_research_capacity, epistemic_autonomy_research_fields).

% DUAL FORMULATION NOTE:
% This constraint is part of a constraint family describing AI safety research ecosystem dynamics. It is upstream of specific safety capability gaps (measurement and mitigation of particular failure modes) but downstream of broader AI governance capacity constraints. The frontier_ai_safety_research_capacity constraint focuses on who can conduct research; downstream constraints focus on what research reveals and how it influences development choices.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(frontier_ai_safety_research_capacity, powerless, 0.95).
constraint_indexing:directionality_override(frontier_ai_safety_research_capacity, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
