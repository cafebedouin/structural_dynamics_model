% ============================================================================
% CONSTRAINT STORY: ai_scholar_citation_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_scholar_citation_trap, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_scholar_citation_trap
 *   human_readable: AI Scholarly Citation System (OpenScholar)
 *   domain: technological/academic
 *
 * SUMMARY:
 *   An AI model, OpenScholar, synthesizes scientific research and provides
 *   accurate citations, outperforming general-purpose LLMs. However, its
 *   reliance on a corpus of exclusively open-access papers creates a
 *   structural trap. While it provides a valuable coordination function for
 *   navigating the vast scientific literature, it systematically suppresses
 *   paywalled research. This creates a biased lens on the state of knowledge,
 *   extracting attention and academic capital from excluded authors and
 *   publishers and creating a significant risk of distorting scientific
 *   consensus for researchers who become dependent on the tool.
 *
 * KEY AGENTS:
 *   - Researchers/Students: Primary users, who are both beneficiaries of efficiency and victims of the system's inherent biases (powerless/trapped).
 *   - OpenScholar Developers: Primary beneficiaries, who see their tool as a solution to information overload (institutional/arbitrage).
 *   - Paywalled Publishers/Authors: Primary victims, whose work is structurally excluded and marginalized (institutional/constrained).
 *   - Open Access Advocacy Groups: Secondary beneficiaries, who view the tool as a strategic asset to promote their cause (organized/mobile).
 *   - Academic Epistemic Commons: An abstract victim representing the overall health and integrity of scholarly knowledge.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_scholar_citation_trap, 0.65).
domain_priors:suppression_score(ai_scholar_citation_trap, 0.75).
domain_priors:theater_ratio(ai_scholar_citation_trap, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_scholar_citation_trap, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_scholar_citation_trap, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ai_scholar_citation_trap, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_scholar_citation_trap, snare).
narrative_ontology:human_readable(ai_scholar_citation_trap, "AI Scholarly Citation System (OpenScholar)").
narrative_ontology:topic_domain(ai_scholar_citation_trap, "technological/academic").

domain_priors:requires_active_enforcement(ai_scholar_citation_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_scholar_citation_trap, openschoar_developers).
narrative_ontology:constraint_beneficiary(ai_scholar_citation_trap, open_access_authors).
narrative_ontology:constraint_beneficiary(ai_scholar_citation_trap, researchers_with_limited_budgets).
narrative_ontology:constraint_victim(ai_scholar_citation_trap, authors_in_paywalled_journals).
narrative_ontology:constraint_victim(ai_scholar_citation_trap, paywalled_journal_publishers).
narrative_ontology:constraint_victim(ai_scholar_citation_trap, researchers_relying_on_the_tool).
narrative_ontology:constraint_victim(ai_scholar_citation_trap, academic_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE RESEARCHER (SNARE) — Overwhelmed by the volume of literature, the individual researcher is trapped by the need for efficiency. They cannot manually replicate the AI's scope, making them dependent on its biased lens. The tool's limitations (e.g., exclusion of paywalled research) become invisible constraints on their work. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈1.11.
constraint_indexing:constraint_classification(ai_scholar_citation_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE DEVELOPER (ROPE) — From the perspective of its creators, the system is a pure coordination tool. It solves the collective action problem of navigating an impossibly large body of literature, increasing research velocity and access. The extraction is seen as a byproduct of an incomplete dataset, not a feature of the tool. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.09.
constraint_indexing:constraint_classification(ai_scholar_citation_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE PAYWALLED PUBLISHER (SNARE) — This institutional actor is a victim, constrained from participating in the AI's knowledge base. The system actively suppresses their content, extracting attention and impact and redirecting it to the open-access ecosystem. From their view, it's a coercive tool designed to render their business model obsolete. d≈0.75, f(d)≈1.10, σ=1.2 → χ≈0.86.
constraint_indexing:constraint_classification(ai_scholar_citation_trap, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN ACCESS ADVOCACY (SCAFFOLD) — This organized group sees the tool as a temporary support structure to accelerate the transition to a fully open-access world. The exclusion of paywalled content is a feature, not a bug—a lever to pressure publishers. They perceive a sunset clause: once open access is the norm, the tool's extractive function will wither away. d≈0.15, f(d)≈-0.01, σ=1.2 → χ≈-0.01.
constraint_indexing:constraint_classification(ai_scholar_citation_trap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: THE CYNICAL USER (PITON) — This user sees the system as a degraded ritual. The function of a literature review—critical, comprehensive engagement—is replaced by the performative act of generating an AI summary. The tool's output is a theatrical substitute for scholarship, maintained because it's required or expected, not because it's epistemically sound. The high theater_ratio (0.75) satisfies the piton gate.
constraint_indexing:constraint_classification(ai_scholar_citation_trap, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — The observer sees a system whose coordination function acts as a lure into a structural trap. The systemic exclusion of paywalled knowledge and the amplification of inherent data biases constitute a severe form of extraction from the epistemic commons. The suppression of alternative viewpoints is high, and the long-term cost to academic integrity outweighs the immediate efficiency gains. d≈0.73, f(d)≈1.15, σ=1.2 → χ≈0.90.
constraint_indexing:constraint_classification(ai_scholar_citation_trap, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_scholar_citation_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_scholar_citation_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_scholar_citation_trap, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_scholar_citation_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_scholar_citation_trap, TR),
    TR >= 0.70.

:- end_tests(ai_scholar_citation_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. The system extracts value by systematically marginalizing a significant portion of scholarly work (paywalled research), redirecting citations, attention, and perceived impact toward the open-access corpus it is trained on. Suppression (0.75): High. While researchers can theoretically use other methods, the sheer scale of modern scientific literature makes AI-powered synthesis tools a practical necessity, creating strong pressure for adoption and making it difficult to work outside the system's biased framework. Theater Ratio (0.75): High. As the tool becomes ubiquitous, its use risks becoming a performative substitute for genuine, critical literature review. The act of 'running the AI report' can replace the intellectual labor of synthesis, creating a ritual that mimics scholarship without its epistemic function.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. Developers see a Rope, a tool for coordinating knowledge. Trapped researchers and excluded publishers see a Snare, a coercive system that limits and distorts their work. Open-access advocates see a temporary Scaffold, a tool to build a better future. Cynical users see a Piton, a hollowed-out ritual. The analytical view sides with the Snare classification, judging that the harm from systemic bias and exclusion outweighs the coordination benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (developers, OA authors) have low derived 'd' values, leading to Rope/Scaffold classifications with low or negative effective extraction (χ). Victims (researchers, publishers) have high derived 'd' values due to their trapped/constrained exit options, resulting in high χ and Snare classifications. The system's structure cleanly sorts agents into those who benefit from the coordination and those who are harmed by the extraction it enables.
 *
 * MANDATROPHY ANALYSIS:
 *   This case avoids mandatrophy by demonstrating that a tool with a clear, beneficial coordination function can simultaneously be a high-extraction Snare. The 'trap' is not a subjective feeling but a structural property: the systematic exclusion of a class of knowledge. The analytical classification as Snare is a claim that this extractive harm is the dominant feature of the constraint, not merely an unfortunate side effect of an otherwise benign coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_distortion_impact,
    'What is the measurable impact of excluding paywalled research on the conclusions drawn in AI-generated literature reviews?',
    'Comparative analysis of AI summaries vs. human expert summaries that include paywalled sources across multiple fields. Quantify the rate of altered conclusions or missed critical findings.',
    'If impact is low, the system is closer to a Tangled Rope (high-functioning coordination with side-effects). If high, the Snare classification is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epistemic_distortion_impact, empirical, 'Measures the distortion caused by excluding paywalled research.').

omega_variable(
    skill_atrophy_risk,
    'Does long-term reliance on AI synthesis tools lead to a measurable decline in critical thinking and literature evaluation skills among researchers?',
    'Longitudinal study tracking cohorts of graduate students, comparing those who use AI synthesis tools extensively versus those who use traditional methods.',
    'If skills atrophy, the ''trap'' is deeper than just data bias, affecting the human component of the research ecosystem, reinforcing the Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_atrophy_risk, empirical, 'Assesses the risk of researcher skill degradation from AI over-reliance.').

omega_variable(
    bias_amplification_feedback_loop,
    'Does the system create a feedback loop where AI-surfaced papers get more citations, which then reinforces their importance in the AI''s training data, amplifying initial biases?',
    'Network analysis of citation patterns over time, comparing the citation graph of papers surfaced by the AI versus a control set.',
    'Confirmation of a strong feedback loop would increase the system''s extractiveness score, as it actively manufactures and concentrates academic capital based on arbitrary starting conditions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bias_amplification_feedback_loop, empirical, 'Investigates if the AI creates a citation-bias feedback loop.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_scholar_citation_trap, 2023, 2033).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_scholar_citation_trap, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ai_s_tr_t5, ai_scholar_citation_trap, theater_ratio, 5, 0.4).
narrative_ontology:measurement(ai_s_tr_t10, ai_scholar_citation_trap, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_scholar_citation_trap, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(ai_s_be_t5, ai_scholar_citation_trap, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(ai_s_be_t10, ai_scholar_citation_trap, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_scholar_citation_trap, information_standard).
narrative_ontology:affects_constraint(ai_scholar_citation_trap, academic_publishing_paywalls).
narrative_ontology:affects_constraint(ai_scholar_citation_trap, open_access_mandates).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
