% ============================================================================
% CONSTRAINT STORY: ai_capability_assessment_bottleneck
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_capability_assessment_bottleneck, []).

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
 *   constraint_id: ai_capability_assessment_bottleneck
 *   human_readable: AI Capability Assessment Bottleneck
 *   domain: artificial_intelligence/governance/verification
 *
 * SUMMARY:
 *   The AI capability assessment bottleneck represents a structural
 *   constraint where frontier AI laboratory networks control the narrative,
 *   methodology, and attestation of their own capabilities, creating
 *   asymmetric information conditions that extend across governance, safety
 *   research, and policy communities. The bottleneck combines genuine
 *   technical difficulty (complex AI systems have emergent properties that
 *   are hard to characterize) with institutional gatekeeping (frontier labs
 *   have incentives to control capability narrative for funding, policy, and
 *   recruitment purposes). Unlike the quantum materials verification
 *   bottleneck (which has scientific consensus on measurement principles),
 *   the AI capability bottleneck lacks agreed-upon assessment frameworks,
 *   enabling frontier labs to define 'capability' itself. The constraint
 *   exhibits all six DR types depending on perspective: frontier labs
 *   experience it as pure coordination (Rope), downstream safety communities
 *   as pure extraction (Snare), governance actors as temporary problems being
 *   solved by open-source alternatives (Scaffold), academic credentialing as
 *   degraded theater (Piton), mixed institutional relationships as hybrid
 *   extraction-coordination (Tangled Rope), and the civilizational analytical
 *   view risks mistaking contingent gatekeeping for immutable complexity
 *   (false summit Mountain). The theater ratio (0.79) reflects that academic
 *   papers, benchmark scores, and public capability announcements are
 *   substantially performative — they establish credibility and narrative
 *   control rather than conveying independent verification. The rising
 *   measurements indicate increasing gatekeeping intensity as frontier
 *   capabilities approach societal relevance and policy salience increases
 *   competitive pressure to control the assessment narrative.
 *
 * KEY AGENTS:
 *   - Frontier AI Laboratory Networks: Primary beneficiary (institutional/arbitrage) — controls capability narrative, assessment frameworks, and policy influence; benefits from information asymmetry and mystery around model capabilities
 *   - Capability Assessment Gatekeepers: Secondary beneficiary (institutional/arbitrage) — defines evaluation standards, consulting authority, institutional prestige; coordinating with frontier labs through assessment framework design
 *   - Downstream Safety and Governance Communities: Primary victim (powerless/trapped) — cannot exit dependence on lab-provided assessment information; bears costs of misinformation and delayed/inadequate governance response
 *   - Non-Frontier Policymakers and Regulators: Secondary victim (powerless/trapped) — asymmetric information access; must regulate based on lab-controlled narrative
 *   - Independent AI Safety Researchers: Mixed agent (moderate/constrained) — can conduct some independent verification but access is conditional; research agenda shaped by what frontier labs permit testing
 *   - Open-Source AI Coalition: Organized coalition (organized/constrained) — building parallel verification pathways; has exit strategy but constrained by resource barriers and technical challenges
 *   - Academic Credentialing System: Institutional theater (institutional/arbitrage) — maintains performative legitimation of frontier lab claims; benefits from centrality to AI governance narrative despite limited functional verification
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing gatekeeping as inherent complexity; false summit detector applies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_capability_assessment_bottleneck, 0.58).
domain_priors:suppression_score(ai_capability_assessment_bottleneck, 0.68).
domain_priors:theater_ratio(ai_capability_assessment_bottleneck, 0.79).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_capability_assessment_bottleneck, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_capability_assessment_bottleneck, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ai_capability_assessment_bottleneck, theater_ratio, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_capability_assessment_bottleneck, tangled_rope).
narrative_ontology:human_readable(ai_capability_assessment_bottleneck, "AI Capability Assessment Bottleneck").
narrative_ontology:topic_domain(ai_capability_assessment_bottleneck, "artificial_intelligence/governance/verification").

domain_priors:requires_active_enforcement(ai_capability_assessment_bottleneck).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_capability_assessment_bottleneck, frontier_ai_labs).
narrative_ontology:constraint_beneficiary(ai_capability_assessment_bottleneck, capability_assessment_gatekeepers).
narrative_ontology:constraint_victim(ai_capability_assessment_bottleneck, society_governance_capacity).
narrative_ontology:constraint_victim(ai_capability_assessment_bottleneck, downstream_safety_verification).
narrative_ontology:constraint_victim(ai_capability_assessment_bottleneck, non_frontier_ai_developers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOWNSTREAM SAFETY VERIFICATION (SNARE) — Cannot exit the bottleneck. Governance, safety, and alignment communities are trapped: they must assess capabilities they cannot independently verify, using frameworks gatekept by the very labs building the systems. No alternative verification pathway exists. Bears full cost of misinformation from frontier labs. Maximum experienced extraction — abstract safety commons has no organized escape route.
constraint_indexing:constraint_classification(ai_capability_assessment_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-FRONTIER POLICYMAKERS (SNARE) — Trapped by asymmetric information. Cannot independently assess AI capabilities, must rely on lab-provided benchmarks and safety evidence. Cannot demand verification without lab cooperation. No credible exit option — abandoning AI governance is politically infeasible; building independent assessment capacity takes decades. Bears costs of delayed/inadequate regulation while labs capture policy narrative.
constraint_indexing:constraint_classification(ai_capability_assessment_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: INDEPENDENT SAFETY RESEARCHERS (TANGLED ROPE) — Constrained but not trapped. Can access frontier models through API or research agreements, enabling some independent verification. But access is conditional on cooperation; labs can revoke access, modify model behavior for evaluators, or restrict testing scope. Benefits from collaborative research pathways but also bears extraction: research agendas are shaped by what labs permit testing, publication can be delayed or blocked, findings are gatekept by capability assessors. Moderate extraction with genuine coordination elements.
constraint_indexing:constraint_classification(ai_capability_assessment_bottleneck, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FRONTIER AI LABS (ROPE) — Benefits from the bottleneck. Experiences the assessment constraint as coordination mechanism: publishing capability claims establishes priority and funding advantage; controlling assessment narrative shapes AI development trajectory and policy direction; maintaining mystery around capabilities sustains investor confidence and recruitment narrative. Net beneficiary — extraction runs toward these agents. Low perceived extraction because they define the assessment framework.
constraint_indexing:constraint_classification(ai_capability_assessment_bottleneck, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ASSESSMENT GATEKEEPERS (ROPE) — Institutional beneficiary. Controls the definition of 'capability', the evaluation methodologies, and the attestation of AI safety/capabilities. Experiences the bottleneck as pure coordination: setting evaluation standards creates value through reduced uncertainty. Benefits from information asymmetry through consulting contracts, policy influence, and institutional prestige. Exit is available — can shift assessment frameworks or share methodology — but has no incentive. Net beneficiary at low cost.
constraint_indexing:constraint_classification(ai_capability_assessment_bottleneck, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN-SOURCE / TRANSPARENCY MOVEMENT (SCAFFOLD) — Organized agents (open-source communities, transparency advocates, academic institutions) are building parallel verification pathways: open-weights models, red-teaming methodologies, published safety audits, decentralized capability evaluation. See the bottleneck as temporary — as open-source capabilities advance and evaluation standards become public goods, the gatekeeping mechanism loses force. Estimated sunset: 5-15 years as model weights become openly available and evaluation methodologies standardize.
constraint_indexing:constraint_classification(ai_capability_assessment_bottleneck, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ACADEMIC LEGITIMATION THEATER (PITON) — Traditional peer review and academic credentialing for AI capabilities claims is substantially performative. Academic reviewers cannot verify training compute, dataset composition, or instruction-tuning methodology from a paper alone. The review ritual persists because it provides institutional authority to capability claims, but the actual verification content is minimal — downstream actors cite papers not because they've been vetted but because they're published. Theater ratio high. The piton classification reflects that academic journals maintain performative legitimation of frontier lab claims without functional verification.
constraint_indexing:constraint_classification(ai_capability_assessment_bottleneck, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / COMPLEXITY VIEW (MOUNTAIN) — From civilizational perspective, some assessment lag is structurally inherent: complex AI systems with emergent capabilities always take time to characterize, and the frontier of capability is by definition not yet understood. The gap between capability and verification is an immutable property of learning systems encountering novel capability regimes. However, this perspective risks naturalizing what may be contingent institutional gatekeeping as inherent complexity. False summit detection will assess whether the bottleneck is natural or constructed.
constraint_indexing:constraint_classification(ai_capability_assessment_bottleneck, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_capability_assessment_bottleneck_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_capability_assessment_bottleneck, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_capability_assessment_bottleneck, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_capability_assessment_bottleneck, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_capability_assessment_bottleneck, TR),
    TR >= 0.70.

:- end_tests(ai_capability_assessment_bottleneck_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. Frontier labs capture policy influence, funding priority, and narrative control over AI development trajectory during the period when AI capabilities are most uncertain and societally consequential. But extraction is not as severe as pure technical gatekeeping would suggest (0.72+) because genuine technical difficulty exists — AI capability emergence is real, measurement is genuinely hard. The 0.58 reflects that a significant portion of the bottleneck is technical complexity, while a meaningful portion is institutional gatekeeping. Suppression (0.68): High. Multiple barriers prevent independent verification: lack of model access, absence of agreed assessment frameworks, specialized knowledge requirements, proprietary training data, computational resource constraints, and career risk for researchers who challenge lab narratives (academic credentialing depends on lab cooperation). These are structural barriers, not just institutional conventions. Theater ratio (0.79): Very high and rising. Academic papers announcing capabilities, benchmark scores, and public safety commitments are substantially performative. They establish institutional legitimacy and shape policy perception without conveying independent verification. The rising trajectory reflects increasing theatricality as AI becomes more policy-salient — capability claims now serve strategic functions (policy influence, recruitment, investor confidence) in addition to scientific communication. The piton perspective captures this degradation: academic peer review no longer provides meaningful verification of AI capabilities; it provides institutional authority to frontier lab claims.
 *
 * PERSPECTIVAL GAP:
 *   The gap between frontier lab Rope and downstream victim Snare is the core diagnostic. Frontier labs experience assessment as coordination because they control the framework — communication and priority are real coordination functions. Victims experience extraction because they cannot verify claims, cannot challenge the framework, and cannot exit the dependence. These are the same constraint producing opposite classifications. The gap reveals that the constraint's classification depends entirely on position: are you defining the assessment or subject to it? The scaffold perspective reveals that the gap may be temporary — if open-source capabilities and standardized evaluation methodologies mature, both the frontier lab's coordination advantage and the victim's extraction burden could decline. The piton perspective explains why the gap persists despite high theater: academic credentialing provides institutional authority to frontier claims, reducing pressure on labs to submit to independent verification. The mountain perspective risks collapsing the gap by treating bottleneck as natural, which would legitimize the frontier labs' narrative control as inherent to complexity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural relationship to the assessment mechanism. Frontier labs and assessment gatekeepers are beneficiaries with arbitrage options (low d, negative chi) — they define the framework and can exit by releasing information or sharing methodology, but have no incentive. Downstream safety communities are victims with no exit (high d, high f(d), high chi) — trapped by information asymmetry with no alternative verification source. Independent researchers are moderate-power victims with constrained exit (medium-high d) — they can conduct some independent verification but face conditional access and agenda-shaping. Policymakers are victims with apparent exit options (could invest in independent assessment capacity) but constrained by political feasibility and path dependence (trapped exit, high d). Open-source coalitions are organized actors with genuine exit strategy visible on the horizon (lower d than initial measure, shifting toward scaffold). The engine derives d from beneficiary/victim declarations and exit_options; these structural relationships are stable across time.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY: Classification varies sharply by perspective (Snare, Rope, Tangled Rope, Scaffold, Piton, false Mountain), indicating genuine structural conflicts of interest and power asymmetry rather than measurement ambiguity. The bottleneck is NOT a case where 'both coordination and extraction occur' at the same time and place. Rather, the SAME structural mechanism (frontier labs' control of assessment frameworks) is experienced as coordination by beneficiaries and extraction by victims. The mandatrophy is resolved by recognizing this perspectival gap as authentic — not a classification error but a diagnostic feature. The claimed_type 'Tangled Rope' represents the analytical aggregate: genuine coordination functions exist (establishing AI capability claims, enabling policy response), but these are paired with asymmetric extraction (information gatekeeping, narrative control, policy influence concentration). The Tangled Rope classification is verified by the presence of beneficiaries (frontier labs, assessment gatekeepers), victims (safety communities, policymakers), and required active enforcement (capacity assessments are not self-executing; they require lab participation and third-party credentialing). The scaffold perspective's sunset logic provides mandatrophy exit: if open-source models and evaluation standards mature, the beneficiary's coordination advantage declines, the victim's extraction burden declines, and the constraint approaches Rope (pure coordination).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    independent_verification_feasibility,
    'Can frontier AI capabilities be independently verified without cooperation from the developing lab, or is verification fundamentally dependent on lab-provided access and transparency?',
    'Empirical test: independent researchers access frontier model through third-party API/open-source approximation; compare capability characterization with lab-published claims. If agreement is high with limited lab input, verification is feasible. If agreement requires lab explanations, verification is dependent.',
    'If feasible: bottleneck is institutional gatekeeping (Snare, Tangled Rope). If dependent: bottleneck reflects genuine verification complexity (Mountain, Scaffold). Classification shifts from extraction-centered to coordination-centered.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(independent_verification_feasibility, empirical, 'Feasibility of independent frontier AI capability verification').

omega_variable(
    emergence_claim_detectability,
    'Can emergent capabilities in AI systems be detected and characterized by observers other than the developing lab, or do they require the lab''s interpretability/understanding of training process?',
    'Collect samples of ''emergent capability'' claims from frontier labs; attempt independent characterization of same capabilities by external researchers with API access only. Measure success rate in replicating emergence observations without lab interpretation.',
    'If high replication: emergence claims are observable phenomena (verification bottleneck is architectural). If low replication: emergence depends on lab interpretation (verification bottleneck is epistemological — requires lab authority). Different classification implications for both.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergence_claim_detectability, empirical, 'Whether emergent AI capabilities are independently detectable or lab-dependent').

omega_variable(
    assessment_framework_commensurability,
    'Are different AI capability assessment frameworks (OpenAI''s evals, Anthropic''s standards, academic benchmarks, government assessments) measuring the same construct, or are they incommensurable framings hiding different underlying claims?',
    'Systematic comparison of assessment outputs across frameworks for identical model. If high correlation: single underlying construct being measured. If low correlation: frameworks are incommensurable — bottleneck is caused by lack of shared assessment language, not inherent verification difficulty.',
    'If incommensurable: bottleneck is partially resolvable through framework standardization (Scaffold). If commensurable: bottleneck reflects genuine measurement difficulty (Mountain element). Mixed classification outcome.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(assessment_framework_commensurability, empirical, 'Commensurability of competing AI capability assessment frameworks').

omega_variable(
    lab_incentive_misalignment_severity,
    'How much of the assessment bottleneck is driven by frontier labs'' economic/political incentives to obscure capability limitations versus genuine difficulty in capability characterization?',
    'Historical analysis of capability claims proven false or retracted; correlation with periods of high funding competition or policy pressure. Benchmark against lab behavior when incentives align with transparency (e.g., safety-focused disclosures).',
    'If heavily incentive-driven (>70%): bottleneck is constructed gatekeeping (Snare dominant). If primarily technical (>70%): bottleneck reflects complexity (Mountain, Scaffold dominant). Most likely mixed outcome shapes mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lab_incentive_misalignment_severity, empirical, 'Degree to which lab incentive misalignment drives the assessment bottleneck').

omega_variable(
    governance_action_credibility_dependence,
    'How much of the bottleneck''s extractive force comes from governance actors'' dependence on frontier lab credibility for policy legitimation versus genuine incapacity to assess independently?',
    'Analyze policy decisions that invoke capability assessments; trace whether decisions would change if different assessment methods produced different conclusions. If policy is sticky to assessment source, dependence is high.',
    'If high dependence: bottleneck is primarily institutional gatekeeping (Snare, Tangled Rope). If low: policymakers have agency but choose to defer (coordination function). Shapes directionality of extraction — is it imposed or negotiated?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_action_credibility_dependence, empirical, 'Governance credibility dependence on frontier lab assessment authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_capability_assessment_bottleneck, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aicab_tr_t0, ai_capability_assessment_bottleneck, theater_ratio, 0, 0.55).
narrative_ontology:measurement(aicab_tr_t3, ai_capability_assessment_bottleneck, theater_ratio, 3, 0.68).
narrative_ontology:measurement(aicab_tr_t6, ai_capability_assessment_bottleneck, theater_ratio, 6, 0.79).
narrative_ontology:measurement(aicab_tr_t9, ai_capability_assessment_bottleneck, theater_ratio, 9, 0.82).

% Extraction over time
narrative_ontology:measurement(aicab_be_t0, ai_capability_assessment_bottleneck, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(aicab_be_t3, ai_capability_assessment_bottleneck, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(aicab_be_t6, ai_capability_assessment_bottleneck, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(aicab_be_t9, ai_capability_assessment_bottleneck, base_extractiveness, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_capability_assessment_bottleneck, information_standard).
narrative_ontology:affects_constraint(ai_capability_assessment_bottleneck, ai_alignment_verification_gap).
narrative_ontology:affects_constraint(ai_capability_assessment_bottleneck, frontier_ai_policy_lag).

% DUAL FORMULATION NOTE:
% The AI capability assessment bottleneck is upstream of AI alignment verification (safety properties depend on capability characterization) and frontier AI policy lag (policy response depends on shared capability assessment). These constraints form a family: bottleneck in capability assessment creates downstream bottlenecks in safety verification and governance response.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_capability_assessment_bottleneck, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
