% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__decline_reading, []).

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
 *   constraint_id: honor_satisfaction_mechanism__decline_reading
 *   human_readable: Dueling as a Declining Honor Satisfaction Mechanism
 *   domain: Historical Sociology/Legal History/Normative Systems
 *
 * SUMMARY:
 *   This constraint story analyzes dueling from the 'decline_reading'
 *   perspective, where the practice persisted at a declining frequency until
 *   it reached fringe status. It was a formalized mechanism for honor
 *   satisfaction among elites, characterized by high extraction (risk of
 *   death/injury) and significant social suppression (pressure to
 *   participate, penalties for refusal). Over the specified interval
 *   (1750-1900), its frequency and social legitimacy waned due to legal
 *   prohibitions and changing social norms, but it remained conceptually
 *   available and occasionally practiced.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__decline_reading, 0.65).
domain_priors:suppression_score(honor_satisfaction_mechanism__decline_reading, 0.75).
domain_priors:theater_ratio(honor_satisfaction_mechanism__decline_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__decline_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__decline_reading, "Dueling as a Declining Honor Satisfaction Mechanism").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__decline_reading, "Historical Sociology/Legal History/Normative Systems").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__decline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__decline_reading, '130f9a20-5309-4c45-ad08-8a7ba33806f5').
narrative_ontology:cs_kernel_codification('130f9a20-5309-4c45-ad08-8a7ba33806f5', formalized).
narrative_ontology:cs_authority_grounding('130f9a20-5309-4c45-ad08-8a7ba33806f5', practice).
narrative_ontology:cs_interpretation_layer_present('130f9a20-5309-4c45-ad08-8a7ba33806f5').
narrative_ontology:cs_reading_relation('130f9a20-5309-4c45-ad08-8a7ba33806f5', honor_satisfaction_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('130f9a20-5309-4c45-ad08-8a7ba33806f5', honor_satisfaction_mechanism__composite_reading, coexists_with).
narrative_ontology:cs_axiom('130f9a20-5309-4c45-ad08-8a7ba33806f5', foundational, honor_demands_physical_satisfaction).
narrative_ontology:cs_axiom_status(honor_demands_physical_satisfaction, holdable).
narrative_ontology:cs_axiom_grounding('130f9a20-5309-4c45-ad08-8a7ba33806f5', honor_demands_physical_satisfaction, deontological).
narrative_ontology:cs_axiom('130f9a20-5309-4c45-ad08-8a7ba33806f5', secondary, social_cost_of_dueling_increasing).
narrative_ontology:cs_axiom_status(social_cost_of_dueling_increasing, holdable).
narrative_ontology:cs_axiom_grounding('130f9a20-5309-4c45-ad08-8a7ba33806f5', social_cost_of_dueling_increasing, empirically_contingent).
narrative_ontology:cs_reference_frame('130f9a20-5309-4c45-ad08-8a7ba33806f5', elite_honor_code_supremacy).
narrative_ontology:cs_drift_state('130f9a20-5309-4c45-ad08-8a7ba33806f5', late_19th_century, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('130f9a20-5309-4c45-ad08-8a7ba33806f5', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, duelists_who_win_honor).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, social_elites_maintaining_status).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, duelists_who_lose_life_or_honor).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, families_of_duelists).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, society_at_large).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, bourgeois_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Those who successfully navigated the duel, either by winning or by demonstrating courage, thereby restoring or enhancing their social standing and honor within elite circles. Refusal to duel carried severe social penalties.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, duelists_who_win_honor, beneficiary,
    powerful, biographical, constrained, local).

% The broader aristocratic and gentry classes who, by participating in or sanctioning dueling, reinforced a system of honor that distinguished them from commoners and provided a mechanism for internal status negotiation. They set and upheld the codes of honor.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, social_elites_maintaining_status, agenda_setter,
    institutional, generational, constrained, national).

% Individuals who were killed, injured, or socially ruined by participating in duels. For them, the mechanism of honor satisfaction was a direct and often fatal extraction.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, duelists_who_lose_life_or_honor, payer,
    powerless, immediate, trapped, local).

% Families who bore the social stigma, economic hardship, or grief resulting from a loved one's participation in a duel, regardless of the outcome. They had little agency to prevent or alter the practice.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, families_of_duelists, payer,
    powerless, biographical, trapped, local).

% The broader populace that suffered from the violence, disruption, and perceived injustice of dueling, even if not directly involved. They bore the costs of a system that often circumvented legal justice.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, society_at_large, payer,
    moderate, generational, constrained, national).

% State and judicial bodies that increasingly outlawed dueling and sought to enforce legal monopolies on violence. While they opposed dueling, their authority was often challenged by the entrenched honor system, leading to inconsistent enforcement.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, legal_authorities, agenda_setter,
    institutional, biographical, analytical, national).

% Churches and other religious bodies that consistently condemned dueling as a sin and a violation of divine law. Their moral authority contributed to the long-term decline of the practice.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, religious_institutions, observer,
    institutional, civilizational, analytical, global).

% The rising middle classes who often rejected dueling in favor of legal and commercial forms of dispute resolution, seeing it as an archaic and irrational practice of the aristocracy. Their ascendance contributed to the decline of dueling's social legitimacy.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, bourgeois_class, beneficiary,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a ritualized, albeit violent, mechanism for elite men to resolve disputes of honor, thereby preventing prolonged feuds and maintaining a specific social hierarchy and code of conduct.
% TRANSFER_FUNCTION: Transferred social standing, reputation, and sometimes life or physical integrity, from one party to another, mediated by a formalized contest of arms.
% ABSENT_VOICES: Commoners, women, and those who rejected the elite honor system were excluded from the 'right' to duel and from shaping the norms that sustained it. Their perspectives on its barbarity and irrelevance were systematically marginalized.
% DISAPPEARANCE_RATIONALE: The decline and eventual disappearance of dueling fundamentally reshaped elite social codes, legal systems' monopoly on violence, and the very definition of honor. Its absence necessitated the development of alternative, non-violent mechanisms for dispute resolution and status maintenance.
% FOUNDING_PROBLEM: Unresolved insults and challenges among elites often led to prolonged feuds, social instability, and a lack of clear, recognized mechanisms for restoring perceived honor or settling grievances within their social stratum.
% FOUNDING_PROBLEM_CORROBORATION: Historians and sociologists widely corroborate the historical problem of honor-based feuds and the role dueling played in addressing it, as well as its eventual obsolescence due to changing social norms and legal frameworks. While a small fringe might still romanticize dueling, its original social function is no longer live.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__decline_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__decline_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__decline_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(honor_satisfaction_mechanism__decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__decline_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__decline_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_mechanism__decline_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_mechanism__decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is high because dueling carried severe risks for participants, but it shows a declining trend as the practice became less common and its social costs more apparent. Suppression is also high, reflecting both the social pressure to duel and the increasing legal efforts to suppress it. The 'requires_active_enforcement' flag is true because both the maintenance of the honor code and its eventual suppression required active social and legal mechanisms. Theater ratio remains low, as dueling, even in decline, was a serious affair with real consequences, not mere performance.
 *
 * PERSPECTIVAL GAP:
 *   The 'decline_reading' emphasizes the quantitative reduction in dueling's frequency and social acceptance, while acknowledging its continued conceptual existence. This contrasts with a 'contraction_reading' (where it becomes cognitively unthinkable) or a 'composite_reading' (focusing on multiple causal factors). Each reading offers a distinct structural interpretation of the same historical phenomenon, leading to different emphases on extraction, suppression, and the nature of its persistence.
 *
 * DIRECTIONALITY LOGIC:
 *   Elite duelists who successfully defended their honor, and the broader social elites who maintained their status through the honor system, were the primary beneficiaries. The direct participants who suffered injury or death, their families, and society at large bore the costs. Legal authorities and religious institutions acted as agenda-setters or observers, attempting to shift the constraint's operation or abolish it. The rising bourgeois class benefited from the decline of dueling as it promoted alternative forms of conflict resolution.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_framing_ambiguity,
    'Is the decline of dueling best understood as a gradual reduction in frequency (decline_reading), a categorical shift making it unthinkable (contraction_reading), or a result of multiple interacting mechanisms (composite_reading)?',
    'Detailed historical sociological analysis comparing the prevalence of dueling with changes in cognitive frames and institutional structures across different social strata and time periods.',
    'If the contraction_reading is more accurate, the constraint''s effective suppression (internalized) would be higher, and its eventual disappearance more complete. If the composite_reading is more accurate, the analysis would need to decompose the constraint into sub-mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_ambiguity, conceptual, 'Ambiguity in the primary mechanism of dueling''s historical transformation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the decline of dueling primarily driven by external legal and social suppression, or by an internal shift in elite values and the definition of honor?',
    'Comparative historical analysis of regions with differing legal enforcement regimes and cultural shifts in honor codes. If decline correlates more strongly with legal bans, external suppression is dominant; if with changing values, internal shifts are key.',
    'If external suppression was dominant, the constraint''s persistence was primarily due to active enforcement. If internal shifts were dominant, the constraint''s ''naturalness'' within the elite social system eroded, making it less resilient to external pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for dueling''s decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__decline_reading, 1750, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1750, 0.1).
narrative_ontology:measurement(hono_tr_t1775, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1775, 0.11).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1800, 0.12).
narrative_ontology:measurement(hono_tr_t1825, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1825, 0.13).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1850, 0.14).
narrative_ontology:measurement(hono_tr_t1875, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1875, 0.15).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1900, 0.16).

% Extraction over time
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1750, 0.7).
narrative_ontology:measurement(hono_be_t1775, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1775, 0.68).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1800, 0.66).
narrative_ontology:measurement(hono_be_t1825, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1825, 0.64).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1850, 0.62).
narrative_ontology:measurement(hono_be_t1875, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1875, 0.6).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1900, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1750, 0.8).
narrative_ontology:measurement(hono_su_t1775, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1775, 0.78).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1800, 0.75).
narrative_ontology:measurement(hono_su_t1825, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1825, 0.72).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1850, 0.68).
narrative_ontology:measurement(hono_su_t1875, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1875, 0.65).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1900, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__decline_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism__contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'honor_satisfaction_mechanism' kernel. This 'decline_reading' focuses on the quantitative reduction in dueling's frequency and social legitimacy, while the 'contraction_reading' emphasizes its cognitive impossibility, and the 'composite_reading' analyzes the multiple factors driving its transformation. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
