% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__drop_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__drop_reading
 *   human_readable: Dueling's Structural Legitimacy (Drop Reading)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This constraint story, 'Dueling's Structural Legitimacy (Drop Reading)',
 *   analyzes the historical period where dueling, while increasingly rare in
 *   practice, retained its conceptual legitimacy as a means of honor defense
 *   among certain elite social strata. The decline in actual duels is
 *   attributed primarily to rising external costs (legal penalties, social
 *   ostracism, financial ruin) rather than a fundamental redefinition of
 *   honor itself. The constraint is claimed as a Rope, reflecting its
 *   coordination function for honor-bound elites, even as its practical
 *   application diminished. The metrics reflect low extractiveness and
 *   suppression, as the constraint's persistence relied more on conceptual
 *   availability and high deterrent costs than active enforcement or direct
 *   extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__drop_reading, 0.2).
domain_priors:suppression_score(honor_violence_legitimacy__drop_reading, 0.1).
domain_priors:theater_ratio(honor_violence_legitimacy__drop_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__drop_reading, rope).
narrative_ontology:human_readable(honor_violence_legitimacy__drop_reading, "Dueling's Structural Legitimacy (Drop Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__drop_reading, "historical_sociology/legal_anthropology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__drop_reading, '1a537414-74b6-485a-92c8-f1cffeb30d5e').
narrative_ontology:cs_kernel_codification('1a537414-74b6-485a-92c8-f1cffeb30d5e', implicit).
narrative_ontology:cs_authority_grounding('1a537414-74b6-485a-92c8-f1cffeb30d5e', practice).
narrative_ontology:cs_interpretation_layer_present('1a537414-74b6-485a-92c8-f1cffeb30d5e').
narrative_ontology:cs_reading_relation('1a537414-74b6-485a-92c8-f1cffeb30d5e', honor_violence_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('1a537414-74b6-485a-92c8-f1cffeb30d5e', honor_violence_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('1a537414-74b6-485a-92c8-f1cffeb30d5e', foundational, dueling_remains_legitimate_honor_defense).
narrative_ontology:cs_axiom_status(dueling_remains_legitimate_honor_defense, holdable).
narrative_ontology:cs_axiom_grounding('1a537414-74b6-485a-92c8-f1cffeb30d5e', dueling_remains_legitimate_honor_defense, conventional).
narrative_ontology:cs_axiom('1a537414-74b6-485a-92c8-f1cffeb30d5e', secondary, external_costs_are_primary_deterrent).
narrative_ontology:cs_axiom_status(external_costs_are_primary_deterrent, holdable).
narrative_ontology:cs_axiom_grounding('1a537414-74b6-485a-92c8-f1cffeb30d5e', external_costs_are_primary_deterrent, empirically_contingent).
narrative_ontology:cs_reference_frame('1a537414-74b6-485a-92c8-f1cffeb30d5e', honor_code_permitting_dueling).
narrative_ontology:cs_drift_state('1a537414-74b6-485a-92c8-f1cffeb30d5e', late_19th_century, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1a537414-74b6-485a-92c8-f1cffeb30d5e', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, honor_bound_elites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of a social class for whom dueling remained a legitimate, if costly, means of resolving honor disputes. They benefited from the conceptual availability of dueling as a deterrent to insult, even if rarely invoked. The high external costs (legal, social, financial) made actual duels rare, but the option remained part of their honor code.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, honor_bound_elites, beneficiary,
    powerful, generational, constrained, local).

% While officially condemning dueling, legal authorities often applied lenient penalties or looked the other way, reflecting a societal ambivalence that preserved dueling's underlying legitimacy. Their actions contributed to the high external costs (e.g., fines, social ostracism) that deterred duels without fully delegitimizing the practice.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, legal_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Historians and sociologists who analyze the persistence of dueling's conceptual legitimacy despite its practical decline. They observe the mechanisms by which external costs suppressed the practice without altering the underlying honor code.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, social_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a recognized, albeit costly, mechanism for elites to resolve disputes of honor and maintain social standing, coordinating expectations around acceptable responses to perceived insults.
% TRANSFER_FUNCTION: Transferred the burden of maintaining honor from direct violence to the acceptance of high external costs (legal, social, financial) associated with dueling, effectively pricing out the practice for most while preserving its conceptual role.
% ABSENT_VOICES: Victims of dueling (those killed or injured) and their families, who bore the ultimate cost, were largely excluded from the discourse that maintained dueling's legitimacy. Their voices would have challenged the very premise of honor being satisfied by such violence.
% DISAPPEARANCE_RATIONALE: If the conceptual legitimacy of dueling vanished overnight, the world would remain largely unchanged in practice, as dueling was already rare due to external costs. The underlying honor codes would adapt, but the practical impact would be minimal.
% FOUNDING_PROBLEM: The need for a formalized, high-stakes mechanism for elites to defend their honor and resolve grave insults, preventing endless cycles of vendetta or social degradation.
% FOUNDING_PROBLEM_CORROBORATION: Historical records and sociological analyses from independent scholars (outside the honor-bound elites) corroborate that the problem of honor defense evolved beyond dueling, and the practice became obsolete due to changing social norms and legal frameworks, even if its conceptual legitimacy lingered.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__drop_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__drop_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__drop_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_violence_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__drop_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__drop_reading_tests).
:- end_tests(honor_violence_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.2) reflects that the 'cost' of dueling was primarily external and deterrent, rather than a direct transfer of resources through the mechanism itself. Suppression (0.1) is also low because the constraint's persistence was not due to active coercion to duel, but rather the lingering conceptual acceptance of dueling as a valid, if extreme, option. The 'drop reading' emphasizes that the structural legitimacy remained, even as the practice became rare. The decreasing extractiveness over time reflects the increasing external costs making dueling less viable.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of honor-bound elites, the constraint provided a valuable, if rarely used, tool for maintaining social standing. From the perspective of legal authorities, it was a problematic but culturally entrenched practice that required careful management rather than outright abolition. The analytical observer sees the subtle interplay of conceptual legitimacy and practical deterrence.
 *
 * DIRECTIONALITY LOGIC:
 *   Honor-bound elites are beneficiaries because the conceptual availability of dueling served their interests in maintaining honor, even if they rarely engaged in it. Legal authorities, while officially opposing dueling, implicitly benefited from its role in social order by not fully delegitimizing it, thus acting as agenda-setters who managed its decline rather than eradicating it. There are no direct 'victims' of the constraint's *legitimacy* in this reading, only victims of the *practice* of dueling itself.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a Piton or Snare. While dueling became rare, its conceptual legitimacy was not merely theatrical; it genuinely influenced social expectations and provided a 'thinkable' option for honor defense. It was not a Snare because it didn't actively extract from victims through its operation, but rather imposed high deterrent costs. The 'drop reading' specifically argues against a full mandatrophy where the mandate itself became meaningless.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_vs_practical_decline,
    'Did dueling''s decline primarily stem from rising external costs (as this reading suggests) or from a fundamental redefinition of honor that made dueling conceptually unthinkable (as the ''contraction_reading'' suggests)?',
    'Detailed historical-sociological analysis of elite discourse, legal reforms, and cultural narratives surrounding honor during the period of decline. Examination of whether honor codes explicitly excluded violence or merely made it prohibitively expensive.',
    'If the ''contraction_reading'' is more accurate, this constraint would be reclassified towards a Piton (if the conceptual legitimacy truly atrophied) or even a Mountain (if honor itself became a natural law excluding violence). If this ''drop_reading'' holds, the Rope classification remains, emphasizing the coordination function of a costly but legitimate option.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conceptual_vs_practical_decline, conceptual, 'Ambiguity regarding the primary driver of dueling''s decline: external costs vs. conceptual redefinition of honor.').

omega_variable(
    honor_violence_legitimacy_kernel_reading,
    'This constraint is the ''drop_reading'' of the ''honor_violence_legitimacy'' kernel. What would change if a sibling reading were adopted?',
    'Analysis of historical evidence and philosophical arguments supporting the ''contraction_reading'' (honor redefined to exclude violence) or the ''composite_reading'' (both external costs and conceptual redefinition contributed).',
    'The ''contraction_reading'' would likely lead to a reclassification towards a Piton or even a Mountain, as the underlying legitimacy would have atrophied or become a natural law. The ''composite_reading'' would suggest a more complex, hybrid classification, potentially a Tangled Rope, reflecting both coordination and extraction from the evolving honor system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(honor_violence_legitimacy_kernel_reading, conceptual, 'Impact of alternative readings of the ''honor_violence_legitimacy'' kernel on this constraint''s classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__drop_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_violence_legitimacy__drop_reading, base_extractiveness, 1700, 0.25).
narrative_ontology:measurement(hono_be_t1750, honor_violence_legitimacy__drop_reading, base_extractiveness, 1750, 0.2).
narrative_ontology:measurement(hono_be_t1800, honor_violence_legitimacy__drop_reading, base_extractiveness, 1800, 0.15).
narrative_ontology:measurement(hono_be_t1850, honor_violence_legitimacy__drop_reading, base_extractiveness, 1850, 0.1).
narrative_ontology:measurement(hono_be_t1900, honor_violence_legitimacy__drop_reading, base_extractiveness, 1900, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_violence_legitimacy__drop_reading, suppression_requirement, 1700, 0.1).
narrative_ontology:measurement(hono_su_t1750, honor_violence_legitimacy__drop_reading, suppression_requirement, 1750, 0.1).
narrative_ontology:measurement(hono_su_t1800, honor_violence_legitimacy__drop_reading, suppression_requirement, 1800, 0.1).
narrative_ontology:measurement(hono_su_t1850, honor_violence_legitimacy__drop_reading, suppression_requirement, 1850, 0.1).
narrative_ontology:measurement(hono_su_t1900, honor_violence_legitimacy__drop_reading, suppression_requirement, 1900, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__drop_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'honor_violence_legitimacy' kernel. This 'drop_reading' emphasizes the role of external costs in the decline of dueling, while maintaining its structural legitimacy. The 'contraction_reading' focuses on the redefinition of honor itself, and the 'composite_reading' integrates both factors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
