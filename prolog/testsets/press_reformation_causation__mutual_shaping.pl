% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__mutual_shaping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__mutual_shaping, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: press_reformation_causation__mutual_shaping
 *   human_readable: Mutual Shaping of Printing Press and Reformation
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint describes the co-evolutionary relationship between the
 *   printing press and the Protestant Reformation. It posits that the
 *   printing press, initially a technological innovation, created new
 *   affordances (e.g., rapid dissemination of texts, vernacular literacy)
 *   that were actively exploited by Reformation reformers. In turn, the
 *   demands and innovations of the reformers (e.g., need for mass-produced
 *   pamphlets, new typefaces for vernacular languages) further shaped the
 *   development and adoption of printing technology. This is a 'mutual
 *   shaping' perspective, distinct from purely deterministic or purely
 *   agentic views.
 *
 * KEY AGENTS:
 *   - reformation_reformers: Primary beneficiary (moderate/constrained) — exploited the press's capabilities and shaped its development.
 *   - printing_industry: Primary beneficiary (organized/mobile) — adapted technology to meet reformer demands, expanding markets.
 *   - catholic_church: Payer (institutional/constrained) — initially resisted the spread of vernacular texts, but eventually adapted to print culture.
 *   - literate_public: Beneficiary (moderate/mobile) — gained access to new ideas and texts, fostering new forms of agency.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__mutual_shaping, 0.2).
domain_priors:suppression_score(press_reformation_causation__mutual_shaping, 0.1).
domain_priors:theater_ratio(press_reformation_causation__mutual_shaping, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, extractiveness, 0.2).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__mutual_shaping, scaffold).
narrative_ontology:human_readable(press_reformation_causation__mutual_shaping, "Mutual Shaping of Printing Press and Reformation").
narrative_ontology:topic_domain(press_reformation_causation__mutual_shaping, "history_of_technology/religious_history/media_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__mutual_shaping, 'b0caff5e-d427-44e7-83cb-f6adf638821f').
narrative_ontology:cs_kernel_codification('b0caff5e-d427-44e7-83cb-f6adf638821f', implicit).
narrative_ontology:cs_authority_grounding('b0caff5e-d427-44e7-83cb-f6adf638821f', distributed).
narrative_ontology:cs_reading_relation('b0caff5e-d427-44e7-83cb-f6adf638821f', press_reformation_causation__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('b0caff5e-d427-44e7-83cb-f6adf638821f', press_reformation_causation__strategic_deployment, coexists_with).
narrative_ontology:cs_axiom('b0caff5e-d427-44e7-83cb-f6adf638821f', foundational, technology_and_agency_interdependent).
narrative_ontology:cs_axiom_status(technology_and_agency_interdependent, holdable).
narrative_ontology:cs_axiom_grounding('b0caff5e-d427-44e7-83cb-f6adf638821f', technology_and_agency_interdependent, empirically_contingent).
narrative_ontology:cs_axiom('b0caff5e-d427-44e7-83cb-f6adf638821f', foundational, affordances_shape_but_do_not_determine).
narrative_ontology:cs_axiom_status(affordances_shape_but_do_not_determine, holdable).
narrative_ontology:cs_axiom_grounding('b0caff5e-d427-44e7-83cb-f6adf638821f', affordances_shape_but_do_not_determine, empirically_contingent).
narrative_ontology:cs_reference_frame('b0caff5e-d427-44e7-83cb-f6adf638821f', sociotechnical_coevolution_paradigm).
narrative_ontology:cs_drift_state('b0caff5e-d427-44e7-83cb-f6adf638821f', contemporary_historical_scholarship, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b0caff5e-d427-44e7-83cb-f6adf638821f', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__mutual_shaping, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, reformation_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, printing_industry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, literate_public).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, catholic_church).
narrative_ontology:constraint_vindicates(press_reformation_causation__mutual_shaping, sociotechnical_coevolution_theory).
narrative_ontology:constraint_vindicates(press_reformation_causation__mutual_shaping, actor_network_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exploited the printing press for rapid dissemination of their theological arguments, pamphlets, and vernacular Bibles. Their demands for specific texts and formats influenced the development of the printing industry. Their agency was amplified by the press, but they were also constrained by its technical limitations and the need for capital.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, reformation_reformers, beneficiary,
    moderate, biographical, constrained, regional).

% Adapted printing technology and business models to meet the demands of reformers, expanding their markets and developing new techniques (e.g., mass production of small, cheap pamphlets). They profited from the increased demand for printed materials, but also faced risks from censorship and religious conflict.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, printing_industry, beneficiary,
    organized, generational, mobile, continental).

% Initially resisted the spread of vernacular texts and dissenting ideas facilitated by the press, attempting censorship and control. They bore the cost of losing their monopoly on information and eventually had to adapt their own communication strategies to the new print culture.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, catholic_church, payer,
    institutional, civilizational, constrained, global).

% Gained unprecedented access to religious texts, political pamphlets, and new ideas, fostering individual interpretation and critical thought. This increased literacy and engagement, but also exposed them to new forms of propaganda and religious conflict.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, literate_public, beneficiary,
    moderate, biographical, mobile, regional).

% Analyze the complex interplay between technology and society in the Reformation era, debating the causal weight of each factor. Their role is to interpret the historical record and construct explanatory models.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, historical_scholars, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causation__mutual_shaping, diffuse).
narrative_ontology:fixing_cost_class(press_reformation_causation__mutual_shaping, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitated the rapid and widespread dissemination of ideas and texts, coordinating the intellectual and religious discourse across Europe during a period of profound change. It enabled a shared, if contested, information environment.
% TRANSFER_FUNCTION: Transferred information, theological arguments, and cultural influence from authors and printers to a broader public, and in turn, transferred demand and innovation from reformers to the printing industry.
% ABSENT_VOICES: Those who were illiterate or lacked access to printed materials were excluded from direct participation in the print-mediated discourse, their perspectives shaped by oral traditions or mediated interpretations. Their voices would highlight the uneven distribution of the press's benefits.
% DISAPPEARANCE_RATIONALE: If the mutual shaping between the press and Reformation had not occurred, the Reformation itself would have unfolded very differently, likely slower and more localized, or perhaps not at all in its historical form. The development of printing technology would also have taken a different trajectory, lacking the specific demands and innovations driven by religious conflict.
% FOUNDING_PROBLEM: The problem of disseminating complex theological arguments and vernacular scripture rapidly and widely to challenge established religious authority and foster new forms of religious practice.
% FOUNDING_PROBLEM_CORROBORATION: The specific problem of disseminating Reformation ideas via early modern print technology is historically resolved. While the broader problem of information dissemination remains, the historical context and technological constraints that defined the 'founding problem' are no longer live. Historical scholars, independent of religious institutions, corroborate that the specific conditions of the 16th century are no longer present.
narrative_ontology:disappearance_verdict(press_reformation_causation__mutual_shaping, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__mutual_shaping, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__mutual_shaping, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(press_reformation_causation__mutual_shaping, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__mutual_shaping_tests).
:- end_tests(press_reformation_causation__mutual_shaping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Scaffold because the printing press provided a temporary, enabling structure that facilitated the Reformation, but its specific form and impact were not predetermined. It was a support that was built upon and modified by the agents using it. Extractiveness is low (0.2) because the primary function was enabling, not extracting. Suppression is low (0.1) as the technology itself did not inherently suppress alternatives, but rather created new ones. Theater ratio is low (0.05) as the press's function was genuinely transformative, not performative. Accessibility collapse is moderate (0.3) because while the press opened new avenues, it also created new barriers (e.g., literacy requirements, access to printing presses). Resistance is low (0.15) because the technology itself was not the object of resistance, but rather its content and implications.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the reformers, the press was a tool that amplified their message and enabled their agency. From the perspective of the printing industry, it was a market opportunity shaped by demand. The Catholic Church initially experienced it as a disruptive force, but later adapted to its realities. The 'mutual shaping' perspective attempts to integrate these views, seeing the press as an emergent property of their interaction.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformation reformers and the printing industry are beneficiaries (d near 0.0-0.2) as they directly gained from the co-evolution. The Catholic Church is a payer (d near 0.8) as it bore the costs of adapting to a new media landscape that challenged its authority. The literate public is a beneficiary (d near 0.1) through increased access to information.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as it describes a historical causal relationship rather than an ongoing institutional arrangement. However, mislabeling it as purely deterministic (mountain) or purely agentic (rope) would obscure the dynamic interplay and the emergent properties of the system, leading to an incomplete understanding of historical change. The scaffold classification correctly captures its temporary, enabling, and co-constructed nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_direction_ambiguity,
    'Is the relationship between the printing press and the Reformation primarily one of mutual shaping, or does one factor exert dominant causal influence?',
    'Detailed historical counterfactual analysis, examining periods where one factor was present without the other, or where interventions on one factor did not produce expected changes in the other.',
    'If a dominant causal factor is identified (e.g., technology as primary driver), the constraint would shift towards a more deterministic framing (e.g., ''technological_determinism'' reading), altering its classification from scaffold to a more fixed type like mountain or rope, depending on the nature of the determinism. If agency is found to be dominant, it would shift towards ''strategic_deployment'', potentially a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_direction_ambiguity, empirical, 'Ambiguity in the primary causal direction between technology and social change.').

omega_variable(
    kernel_reading_distinction,
    'This constraint is one reading (''mutual_shaping'') of the ''press_reformation_causation'' kernel. What specific structural elements would change if a sibling reading (e.g., ''technological_determinism'' or ''strategic_deployment'') were adopted?',
    'Comparative historical analysis of the evidence cited by proponents of each reading, focusing on how each reading frames the agency of reformers, the autonomy of technology, and the nature of historical contingency.',
    'Adopting ''technological_determinism'' would increase the ''emerges_naturally'' metric and reduce ''resistance'', shifting the constraint towards a mountain. Adopting ''strategic_deployment'' would increase ''resistance'' and ''extractiveness'' (as agency becomes more central to rent-seeking), shifting it towards a rope or tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Distinguishing between different interpretations of the press-Reformation causal kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__mutual_shaping, 1450, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t0, press_reformation_causation__mutual_shaping, theater_ratio, 0, 0.02).
narrative_ontology:measurement(pres_tr_t50, press_reformation_causation__mutual_shaping, theater_ratio, 50, 0.04).
narrative_ontology:measurement(pres_tr_t100, press_reformation_causation__mutual_shaping, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(pres_be_t0, press_reformation_causation__mutual_shaping, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(pres_be_t50, press_reformation_causation__mutual_shaping, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(pres_be_t100, press_reformation_causation__mutual_shaping, base_extractiveness, 100, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t0, press_reformation_causation__mutual_shaping, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(pres_su_t50, press_reformation_causation__mutual_shaping, suppression_requirement, 50, 0.08).
narrative_ontology:measurement(pres_su_t100, press_reformation_causation__mutual_shaping, suppression_requirement, 100, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__mutual_shaping, information_standard).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, press_reformation_causation__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, press_reformation_causation__strategic_deployment).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'press_reformation_causation' kernel, emphasizing mutual shaping. Sibling readings include 'technological_determinism' and 'strategic_deployment', which offer alternative causal framings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
