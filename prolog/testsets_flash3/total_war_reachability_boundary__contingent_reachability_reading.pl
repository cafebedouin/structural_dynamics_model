% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contingent_reachability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__contingent_reachability_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: total_war_reachability_boundary__contingent_reachability_reading
 *   human_readable: Contingent Reachability of Total War (Technology-Dependent)
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   This constraint represents the reading that the reachability of total war
 *   is not fixed but is contingent on technological developments. The current
 *   'contraction' of strategic space (making total war seem less feasible) is
 *   viewed as a temporary equilibrium, a 'piton' of atrophied capability that
 *   could reverse with new technologies. This perspective frames the
 *   constraint as a scaffold, a temporary support for strategic stability
 *   that is inherently unstable and dependent on the current technological
 *   balance, rather than a permanent feature of the international system. It
 *   implies an ongoing arms race and a continuous re-evaluation of
 *   deterrence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, 0.4).
domain_priors:suppression_score(total_war_reachability_boundary__contingent_reachability_reading, 0.6).
domain_priors:theater_ratio(total_war_reachability_boundary__contingent_reachability_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contingent_reachability_reading, scaffold).
narrative_ontology:human_readable(total_war_reachability_boundary__contingent_reachability_reading, "Contingent Reachability of Total War (Technology-Dependent)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contingent_reachability_reading, "international_relations/strategic_studies/nuclear_deterrence").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__contingent_reachability_reading).
narrative_ontology:has_sunset_clause(total_war_reachability_boundary__contingent_reachability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contingent_reachability_reading, '3598f94b-14ec-4400-9bee-adea6a475f5d').
narrative_ontology:cs_kernel_codification('3598f94b-14ec-4400-9bee-adea6a475f5d', implicit).
narrative_ontology:cs_authority_grounding('3598f94b-14ec-4400-9bee-adea6a475f5d', practice).
narrative_ontology:cs_interpretation_layer_present('3598f94b-14ec-4400-9bee-adea6a475f5d').
narrative_ontology:cs_reading_relation('3598f94b-14ec-4400-9bee-adea6a475f5d', total_war_reachability_boundary__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('3598f94b-14ec-4400-9bee-adea6a475f5d', total_war_reachability_boundary__dropping_reading, coexists_with).
narrative_ontology:cs_axiom('3598f94b-14ec-4400-9bee-adea6a475f5d', foundational, total_war_reachability_is_technology_contingent).
narrative_ontology:cs_axiom_status(total_war_reachability_is_technology_contingent, holdable).
narrative_ontology:cs_axiom_grounding('3598f94b-14ec-4400-9bee-adea6a475f5d', total_war_reachability_is_technology_contingent, empirically_contingent).
narrative_ontology:cs_axiom('3598f94b-14ec-4400-9bee-adea6a475f5d', secondary, current_strategic_stability_is_temporary).
narrative_ontology:cs_axiom_status(current_strategic_stability_is_temporary, holdable).
narrative_ontology:cs_axiom_grounding('3598f94b-14ec-4400-9bee-adea6a475f5d', current_strategic_stability_is_temporary, empirically_contingent).
narrative_ontology:cs_reference_frame('3598f94b-14ec-4400-9bee-adea6a475f5d', dynamic_technological_equilibrium).
narrative_ontology:cs_drift_state('3598f94b-14ec-4400-9bee-adea6a475f5d', contemporary_arms_race_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3598f94b-14ec-4400-9bee-adea6a475f5d', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, states_investing_in_destabilizing_tech).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, military_industrial_complexes).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, global_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states benefit from the perception that technological advancements can shift the strategic balance, potentially enabling new forms of total war or rendering existing deterrence obsolete. They invest in offensive and defensive technologies that could alter reachability.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, states_investing_in_destabilizing_tech, beneficiary,
    institutional, generational, mobile, global).

% Bears the ultimate cost if the reachability boundary shifts and deterrence fails, leading to total war. Also pays indirectly through resource allocation to military spending and the psychological burden of existential threat.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, global_population, payer,
    powerless, generational, trapped, global).

% Profits from the continuous development and deployment of new military technologies, driven by the perceived contingency of total war reachability. Their existence reinforces the idea that technological shifts are always possible.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, military_industrial_complexes, beneficiary,
    organized, biographical, mobile, global).

% Study the interplay of technology, doctrine, and strategic stability. They provide assessments on the likelihood of shifts in total war reachability and the implications for deterrence, often influencing policy debates.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, strategic_analysts, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Temporarily coordinates state behavior around a perceived, technology-dependent boundary of total war, encouraging investment in specific military technologies and strategic doctrines.
% TRANSFER_FUNCTION: Transfers resources (funding, scientific talent) from general societal welfare to military research and development, driven by the perceived contingency of total war reachability. It also transfers the burden of existential risk to the global population.
% ABSENT_VOICES: Future generations, who would bear the consequences of a failed deterrence or an escalated arms race, are absent. They would argue for a more stable, less technology-dependent strategic environment.
% DISAPPEARANCE_RATIONALE: If the belief in technology-dependent, reversible total war reachability vanished, the strategic landscape would fundamentally alter. States would either disinvest in destabilizing technologies (if total war was seen as impossible) or invest even more heavily in absolute defense (if total war was seen as imminent and unavoidable, regardless of tech). The current arms race dynamics would cease, and resource allocation would shift dramatically.
% FOUNDING_PROBLEM: The problem of maintaining deterrence in a rapidly evolving technological landscape, where new capabilities (e.g., hypersonic weapons, advanced cyber warfare) could theoretically alter the feasibility or nature of total war.
% FOUNDING_PROBLEM_CORROBORATION: Military strategists and defense contractors consistently attest to the live status of this problem, citing ongoing technological advancements. Independent academic research and international arms control bodies also acknowledge the potential for technological shifts to impact strategic stability, though they may differ on the interpretation of 'reachability'.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contingent_reachability_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contingent_reachability_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contingent_reachability_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_reachability_boundary__contingent_reachability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contingent_reachability_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).
:- end_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.4) is moderate, reflecting the diversion of resources into military R&D and the psychological cost of perpetual strategic uncertainty. Suppression (0.6) is present as states actively suppress information about technological breakthroughs and maintain a veil of secrecy around capabilities, preventing a clear assessment of the 'reachability boundary'. The theater ratio (0.2) is low, as the investments in technology are genuinely aimed at maintaining or shifting strategic advantage, not merely for show. The claimed type is 'scaffold' because this reading views the current strategic stability as a temporary, technology-dependent arrangement, not a permanent state. It has a sunset clause because its validity is tied to the current technological equilibrium, which is inherently transient.
 *
 * PERSPECTIVAL GAP:
 *   States investing in destabilizing technologies perceive this constraint as a dynamic opportunity, justifying their R&D. The global population, however, experiences it as a constant threat and a drain on resources. Strategic analysts view it as a complex, evolving problem requiring continuous monitoring and adaptation.
 *
 * DIRECTIONALITY LOGIC:
 *   States investing in destabilizing technologies and military-industrial complexes are beneficiaries, as the constraint justifies their existence and funding. The global population is the victim, bearing the costs of arms races and existential risk. Strategic analysts are observers, analyzing the dynamics without direct benefit or cost from the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the current strategic stability as a permanent 'mountain' or 'rope' (as in the sibling readings). By identifying it as a 'scaffold' with a sunset clause, it highlights the temporary and technology-dependent nature of the constraint, forcing a continuous re-evaluation of its mandate rather than assuming its permanence. The 'piton' aspect (atrophied capability) suggests that the current state is not robust and could easily revert with technological shifts, preventing complacency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_determinism_vs_agency,
    'To what extent is the ''reachability boundary'' of total war truly determined by technology, versus being a product of political choices and strategic doctrines?',
    'Historical analysis of past technological shifts and their actual impact on strategic stability, coupled with counterfactual analysis of alternative political and doctrinal choices.',
    'If technology is less deterministic, the constraint''s ''scaffold'' nature is more a product of human choice than an inevitable consequence, implying greater agency in shaping strategic stability. If more deterministic, the ''scaffold'' is a more accurate reflection of an underlying physical reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_determinism_vs_agency, conceptual, 'Ambiguity regarding the causal weight of technology versus human agency in defining total war reachability.').

omega_variable(
    sunset_clause_enforceability,
    'Given the inherent uncertainty of technological development, how can a ''sunset clause'' for this strategic scaffold be practically defined and enforced?',
    'Development of international agreements or verification regimes that explicitly link strategic stability to specific technological thresholds or capabilities, with mechanisms for renegotiation or dissolution.',
    'If a sunset clause is practically unenforceable, the ''scaffold'' risks becoming an indefinite ''tangled rope'' or ''snare'', perpetuating arms races under the guise of temporary stability. If enforceable, it reinforces the temporary nature of the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_clause_enforceability, preference, 'The practical enforceability of the scaffold''s inherent transience.').

omega_variable(
    kernel_identity_contingent_reachability,
    'This constraint is a reading of the ''total_war_reachability_boundary'' kernel. What specific structural elements would change if a sibling reading (e.g., ''contraction_reading'' or ''dropping_reading'') were adopted?',
    'Comparing the core axioms and reference frames of the ''contingent_reachability_reading'' with those of its siblings, identifying points of direct contradiction or significant divergence in their structural implications.',
    'Adopting the ''contraction_reading'' would likely shift the claimed_type towards ''mountain'' or ''rope'' due to a perceived permanent contraction of strategic space, reducing extractiveness and suppression. Adopting the ''dropping_reading'' would emphasize deterrence as a ''rope'', focusing on coordination rather than technological contingency. This reading''s ''scaffold'' classification and higher extractiveness/suppression are directly tied to its core premise of technology-dependent, reversible reachability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_contingent_reachability, conceptual, 'Documents this constraint as one reading of the ''total_war_reachability_boundary'' kernel, highlighting its distinct structural implications compared to sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contingent_reachability_reading, 1991, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1991, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 1991, 0.1).
narrative_ontology:measurement(tota_tr_t2000, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(tota_tr_t2010, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(tota_tr_t2024, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(tota_be_t1991, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 1991, 0.3).
narrative_ontology:measurement(tota_be_t2000, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(tota_be_t2010, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement(tota_be_t2024, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2024, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1991, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 1991, 0.5).
narrative_ontology:measurement(tota_su_t2000, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(tota_su_t2010, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(tota_su_t2024, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
