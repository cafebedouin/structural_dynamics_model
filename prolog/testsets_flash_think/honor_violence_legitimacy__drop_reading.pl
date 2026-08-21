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
 *   This constraint represents the 'drop_reading' of the
 *   'honor_violence_legitimacy' kernel. It posits that dueling, as a
 *   mechanism for resolving honor disputes, remained structurally legitimate
 *   in the conceptual framework of honor during the 18th and 19th centuries,
 *   but its practical occurrence became increasingly rare due to rising
 *   external costs (legal penalties, social stigma) rather than a fundamental
 *   redefinition of honor itself. The constraint's function atrophied, but
 *   its underlying conceptual validity persisted, making it a Piton.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__drop_reading, 0.15).
domain_priors:suppression_score(honor_violence_legitimacy__drop_reading, 0.1).
domain_priors:theater_ratio(honor_violence_legitimacy__drop_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__drop_reading, piton).
narrative_ontology:human_readable(honor_violence_legitimacy__drop_reading, "Dueling's Structural Legitimacy (Drop Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__drop_reading, "historical_sociology/legal_anthropology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__drop_reading, '24fdf8b9-94f0-4572-aca3-0ae3a64a79e4').
narrative_ontology:cs_kernel_codification('24fdf8b9-94f0-4572-aca3-0ae3a64a79e4', implicit).
narrative_ontology:cs_authority_grounding('24fdf8b9-94f0-4572-aca3-0ae3a64a79e4', practice).
narrative_ontology:cs_reading_relation('24fdf8b9-94f0-4572-aca3-0ae3a64a79e4', honor_violence_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('24fdf8b9-94f0-4572-aca3-0ae3a64a79e4', honor_violence_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('24fdf8b9-94f0-4572-aca3-0ae3a64a79e4', foundational, honor_requires_physical_satisfaction).
narrative_ontology:cs_axiom_status(honor_requires_physical_satisfaction, holdable).
narrative_ontology:cs_axiom_grounding('24fdf8b9-94f0-4572-aca3-0ae3a64a79e4', honor_requires_physical_satisfaction, conventional).
narrative_ontology:cs_reference_frame('24fdf8b9-94f0-4572-aca3-0ae3a64a79e4', honor_code_legitimacy_intact).
narrative_ontology:cs_drift_state('24fdf8b9-94f0-4572-aca3-0ae3a64a79e4', late_19th_century, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('24fdf8b9-94f0-4572-aca3-0ae3a64a79e4', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, society_at_large).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, duelists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uphold the honor code that legitimizes dueling, but increasingly face legal and social costs for engaging in the practice. Their power lies in defining honor, but their options for acting on it are constrained by external forces.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, gentlemen_of_honor, agenda_setter,
    powerful, biographical, constrained, national).

% Increasingly criminalize dueling through legislation and enforcement, imposing fines, imprisonment, and social stigma. This raises the practical cost of dueling, making it rare, without necessarily challenging its underlying conceptual legitimacy for some.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, legal_authorities, agenda_setter,
    institutional, generational, mobile, national).

% Benefits from the decline in violence associated with dueling, experiencing greater public order and safety. However, a residual respect for the concept of honor, even if dueling is no longer the primary means of satisfaction, may persist.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, society_at_large, beneficiary,
    organized, generational, mobile, national).

% Individuals who, when faced with an insult to their honor, feel compelled by the prevailing social code to duel, despite the high personal risks (injury, death) and increasing legal penalties. They are trapped by the social expectations of the honor code and the immediate pressure to respond.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, duelists, payer,
    moderate, immediate, trapped, local).

% Study the historical evolution of honor codes and dueling, analyzing the social, legal, and economic factors that led to its decline. They observe the persistence of the legitimacy claim even as the practice atrophied.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, analytical_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__drop_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__drop_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a formal, ritualized mechanism for gentlemen to resolve grave insults and defend their honor, aiming to prevent uncontrolled violence and maintain social hierarchy.
% TRANSFER_FUNCTION: Transferred social status and the resolution of grievances among elites, but also imposed significant risks of death, injury, and increasing legal penalties on participants.
% ABSENT_VOICES: Those who suffered from the violence of dueling (e.g., families of victims, non-elite classes not included in the honor code) were historically excluded from the discourse surrounding its legitimacy. They would have argued for its complete abolition.
% DISAPPEARANCE_RATIONALE: If the structural legitimacy of dueling vanished overnight, the historical understanding of honor, social conflict resolution, and elite identity would be fundamentally altered. While the practice is rare, its conceptual availability still shapes historical narratives.
% FOUNDING_PROBLEM: To provide a formal, ritualized means for gentlemen to defend their honor and resolve grave insults, preventing uncontrolled violence and maintaining social order among elites.
% FOUNDING_PROBLEM_CORROBORATION: Historical legal codes, social histories, and literary accounts from the period corroborate the founding problem and its eventual decline due to external pressures (legal prohibition, social stigma), rather than a fundamental redefinition of honor itself. Independent historical analysis supports that the problem it solved is largely defunct in its original form.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__drop_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__drop_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__drop_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(honor_violence_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__drop_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   The base extractiveness is low (0.15) because the *legitimacy* itself did not directly extract rents, though it imposed social costs on those who felt compelled to duel. Suppression is low (0.10) as there was no active suppression of the *idea* of dueling for honor, but rather external legal and social forces suppressed its *practice*. The theater ratio is high (0.60) and increasing, reflecting that the concept of dueling for honor became more performative or symbolic, rarely acted upon, but still invoked in discussions of honor. Accessibility collapse is moderate (0.40) as alternatives for resolving disputes became more available, but the conceptual option of dueling for honor did not entirely disappear.
 *
 * PERSPECTIVAL GAP:
 *   This reading emphasizes external costs as the primary driver of decline, contrasting with the 'contraction_reading' which focuses on internal conceptual redefinition of honor. The engine's classification as a Piton reflects the atrophy of function despite persistent conceptual legitimacy, a key distinction of this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Gentlemen of honor, as agenda-setters, upheld the code but faced increasing costs for its practice. Legal authorities, also agenda-setters, imposed external costs, effectively making dueling impractical. Society at large benefited from reduced violence. Duelists were the primary payers, trapped by the honor code and immediate social pressure despite rising risks. The legitimacy itself, as the constraint, did not directly benefit a single party in an extractive way, but rather maintained a social order that became increasingly costly to enact.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decline_causation_ambiguity,
    'Was the decline in dueling practice primarily due to external costs (drop_reading) or a redefinition of honor itself (contraction_reading)?',
    'Further historical and sociological analysis of primary sources, focusing on explicit statements about honor''s meaning versus records of legal penalties and social ostracization for dueling.',
    'If external costs were primary, the constraint''s legitimacy remained intact (Piton); if honor was redefined, the constraint itself (the legitimacy) would have atrophied more fundamentally (closer to a Snare or a fully dissolved constraint).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decline_causation_ambiguity, empirical, 'Ambiguity over the primary cause of dueling''s decline.').

omega_variable(
    composite_vs_single_cause,
    'Did external costs and conceptual redefinition operate simultaneously and interactively (composite_reading), or was one mechanism dominant (drop_reading/contraction_reading)?',
    'Quantitative historical analysis correlating changes in legal enforcement and economic conditions with shifts in honor discourse, seeking evidence of co-occurrence and mutual reinforcement.',
    'If a composite mechanism, this reading (drop_reading) would be an incomplete explanation, requiring integration with the contraction_reading for a full picture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(composite_vs_single_cause, conceptual, 'Whether the decline was a single-cause or overdetermined phenomenon.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__drop_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_violence_legitimacy__drop_reading, theater_ratio, 1700, 0.2).
narrative_ontology:measurement(hono_tr_t1740, honor_violence_legitimacy__drop_reading, theater_ratio, 1740, 0.35).
narrative_ontology:measurement(hono_tr_t1780, honor_violence_legitimacy__drop_reading, theater_ratio, 1780, 0.45).
narrative_ontology:measurement(hono_tr_t1820, honor_violence_legitimacy__drop_reading, theater_ratio, 1820, 0.52).
narrative_ontology:measurement(hono_tr_t1860, honor_violence_legitimacy__drop_reading, theater_ratio, 1860, 0.58).
narrative_ontology:measurement(hono_tr_t1900, honor_violence_legitimacy__drop_reading, theater_ratio, 1900, 0.6).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_violence_legitimacy__drop_reading, base_extractiveness, 1700, 0.1).
narrative_ontology:measurement(hono_be_t1740, honor_violence_legitimacy__drop_reading, base_extractiveness, 1740, 0.12).
narrative_ontology:measurement(hono_be_t1780, honor_violence_legitimacy__drop_reading, base_extractiveness, 1780, 0.13).
narrative_ontology:measurement(hono_be_t1820, honor_violence_legitimacy__drop_reading, base_extractiveness, 1820, 0.14).
narrative_ontology:measurement(hono_be_t1860, honor_violence_legitimacy__drop_reading, base_extractiveness, 1860, 0.15).
narrative_ontology:measurement(hono_be_t1900, honor_violence_legitimacy__drop_reading, base_extractiveness, 1900, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_violence_legitimacy__drop_reading, suppression_requirement, 1700, 0.1).
narrative_ontology:measurement(hono_su_t1740, honor_violence_legitimacy__drop_reading, suppression_requirement, 1740, 0.1).
narrative_ontology:measurement(hono_su_t1780, honor_violence_legitimacy__drop_reading, suppression_requirement, 1780, 0.1).
narrative_ontology:measurement(hono_su_t1820, honor_violence_legitimacy__drop_reading, suppression_requirement, 1820, 0.1).
narrative_ontology:measurement(hono_su_t1860, honor_violence_legitimacy__drop_reading, suppression_requirement, 1860, 0.1).
narrative_ontology:measurement(hono_su_t1900, honor_violence_legitimacy__drop_reading, suppression_requirement, 1900, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__drop_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
