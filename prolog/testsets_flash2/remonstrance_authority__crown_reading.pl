% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__crown_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remonstrance_authority__crown_reading, []).

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
 *   constraint_id: remonstrance_authority__crown_reading
 *   human_readable: Remonstrance Right as Illegitimate Minoritarian Veto (Crown Reading)
 *   domain: constitutional_history/political_economy/legal_authority
 *
 * SUMMARY:
 *   This constraint story models the 'remonstrance right' from the
 *   perspective of the French Crown during the Ancien Régime, particularly
 *   from the mid-17th century to the French Revolution. From this 'Crown
 *   reading,' the right of parliamentary magistrates to remonstrate (object
 *   to) royal edicts is viewed not as a legitimate constitutional check, but
 *   as an illegitimate minoritarian veto that protects particularist
 *   privileges and obstructs royal authority and necessary fiscal reforms.
 *   The Crown perceives this as a snare, extracting from its ability to
 *   govern effectively and from the broader taxpaying populace, while
 *   benefiting the entrenched interests of the magistrates.
 *
 * KEY AGENTS:
 *   - crown_royal_authority: Agenda setter (institutional/constrained)
 *   - parliamentary_magistrates: Payer (organized/identity_locked)
 *   - taxpaying_populace: Payer (powerless/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__crown_reading, 0.85).
domain_priors:suppression_score(remonstrance_authority__crown_reading, 0.7).
domain_priors:theater_ratio(remonstrance_authority__crown_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__crown_reading, snare).
narrative_ontology:human_readable(remonstrance_authority__crown_reading, "Remonstrance Right as Illegitimate Minoritarian Veto (Crown Reading)").
narrative_ontology:topic_domain(remonstrance_authority__crown_reading, "constitutional_history/political_economy/legal_authority").

domain_priors:requires_active_enforcement(remonstrance_authority__crown_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__crown_reading, '495cc9f2-d91c-42b7-8978-70defd4c3717').
narrative_ontology:cs_kernel_codification('495cc9f2-d91c-42b7-8978-70defd4c3717', formalized).
narrative_ontology:cs_authority_grounding('495cc9f2-d91c-42b7-8978-70defd4c3717', lineage).
narrative_ontology:cs_interpretation_layer_present('495cc9f2-d91c-42b7-8978-70defd4c3717').
narrative_ontology:cs_reading_relation('495cc9f2-d91c-42b7-8978-70defd4c3717', remonstrance_authority__magistrate_reading, coexists_with).
narrative_ontology:cs_axiom('495cc9f2-d91c-42b7-8978-70defd4c3717', foundational, royal_sovereignty_is_absolute).
narrative_ontology:cs_axiom_status(royal_sovereignty_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('495cc9f2-d91c-42b7-8978-70defd4c3717', royal_sovereignty_is_absolute, deontological).
narrative_ontology:cs_axiom('495cc9f2-d91c-42b7-8978-70defd4c3717', foundational, magistrates_are_royal_agents).
narrative_ontology:cs_axiom_status(magistrates_are_royal_agents, holdable).
narrative_ontology:cs_axiom_grounding('495cc9f2-d91c-42b7-8978-70defd4c3717', magistrates_are_royal_agents, conventional).
narrative_ontology:cs_reference_frame('495cc9f2-d91c-42b7-8978-70defd4c3717', absolute_royal_prerogative).
narrative_ontology:cs_drift_state('495cc9f2-d91c-42b7-8978-70defd4c3717', late_ancien_regime, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('495cc9f2-d91c-42b7-8978-70defd4c3717', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__crown_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, crown_royal_authority).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, parliamentary_magistrates).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, taxpaying_populace).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Crown views the remonstrance as an illegitimate obstruction to its fiscal and legislative prerogatives, undermining the efficiency of governance and the royal authority to act for the common good. It seeks to suppress or bypass this right to consolidate power and revenue.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, crown_royal_authority, agenda_setter,
    institutional, generational, constrained, national).

% Magistrates, particularly those in the Parlements, are seen by the Crown as using the remonstrance to protect their own particularist privileges and obstruct royal reforms. They bear the cost of royal displeasure and attempts to circumvent their authority, but are identity-locked to their role as guardians of ancient law.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, parliamentary_magistrates, payer,
    organized, biographical, identity_locked, national).

% The populace is indirectly affected by the political gridlock and the Crown's attempts to raise revenue through other, often more arbitrary, means when remonstrances block direct fiscal reforms. They bear the costs of an inefficient and contested system without direct recourse.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, taxpaying_populace, payer,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the Crown's perspective, the constraint (remonstrance) is an anti-coordination mechanism, preventing efficient royal administration and fiscal policy necessary for national stability and defense.
% TRANSFER_FUNCTION: The remonstrance, when effective, prevents the transfer of wealth from particularist interests (often protected by magistrates) to the royal treasury, thereby forcing the Crown to seek alternative, often more extractive, means of funding.
% ABSENT_VOICES: The Crown would argue that the 'national interest' or 'common good' is an absent voice, suppressed by the particularist interests of the magistrates. Future generations, who would benefit from a more efficient and centralized state, are also absent.
% DISAPPEARANCE_RATIONALE: If the remonstrance right vanished, the Crown's fiscal and legislative authority would be significantly strengthened, leading to a more centralized and potentially more efficient (from the Crown's perspective) state. This would fundamentally alter the balance of power and the structure of governance.
% FOUNDING_PROBLEM: The remonstrance right emerged from a historical need to check arbitrary royal power and preserve local customs and privileges against centralizing tendencies.
% FOUNDING_PROBLEM_CORROBORATION: From the Crown's perspective, the original problem of arbitrary royal power is no longer the primary issue; instead, the remonstrance has become an anachronistic tool for obstruction. This view is largely self-serving, with little corroboration from independent historical analysis outside of royal apologists.
narrative_ontology:disappearance_verdict(remonstrance_authority__crown_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__crown_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__crown_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(remonstrance_authority__crown_reading, 'none', 1).
narrative_ontology:epsilon_provenance(remonstrance_authority__crown_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remonstrance_authority__crown_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(remonstrance_authority__crown_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(remonstrance_authority__crown_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because the remonstrance, from the Crown's view, diverts resources and authority away from the central government, forcing it into costly and inefficient workarounds. Suppression (0.70) is also high, reflecting the Crown's continuous efforts to overcome or bypass the remonstrances through 'lits de justice' and other coercive measures. Theater ratio is low (0.10) because the Crown sees little performative value in the remonstrance; it is a direct and costly obstruction. The claimed type is 'snare' because the Crown perceives the remonstrance as a mechanism of pure extraction, serving particularist interests under the guise of constitutionalism.
 *
 * PERSPECTIVAL GAP:
 *   The Crown's perspective (this reading) fundamentally clashes with the magistrates' perspective (the 'magistrate_reading'). The Crown experiences the remonstrance as a snare, while the magistrates would experience it as a rope or even a mountain, a fundamental check on arbitrary power. This divergence is central to the kernel contest.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown is the primary beneficiary of its own authority and views the remonstrance as extracting from that authority, thus placing it in the victim set when thwarted. Parliamentary magistrates, by exercising the remonstrance, are seen as benefiting from the preservation of their privileges and obstructing royal will, making them the primary targets/payers from the Crown's perspective. The taxpaying populace is an indirect payer, suffering from the fiscal inefficiencies caused by the political deadlock.
 *
 * MANDATROPHY ANALYSIS:
 *   From the Crown's perspective, the remonstrance right has outlived its original mandate (if it ever had one beyond particularism) and has become a mechanism for obstruction. The classification as a snare prevents mislabeling this as a legitimate coordination mechanism or a temporary support structure, aligning with the Crown's view of it as an illegitimate veto.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_veto_power,
    'Is the remonstrance a legitimate constitutional veto power, or an illegitimate obstruction of royal authority?',
    'Analysis of historical constitutional theory and the actual impact of remonstrances on governance and public welfare, from a neutral historical perspective.',
    'If legitimate, the Crown''s ''snare'' classification is a misreading, and the constraint might be reclassified as a ''rope'' or ''mountain'' from a broader historical view. If illegitimate, the ''snare'' classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_veto_power, conceptual, 'Ambiguity regarding the constitutional legitimacy of the remonstrance right.').

omega_variable(
    particularist_vs_public_interest,
    'To what extent did the remonstrances genuinely protect ancient liberties and the public interest, versus serving the particularist privileges of the magistrates?',
    'Detailed historical research into the content of remonstrances and their outcomes, assessing who benefited and who bore costs in specific instances.',
    'If primarily particularist, the Crown''s ''snare'' reading is strengthened. If genuinely protective of broader liberties, the ''magistrate_reading'' gains credence, and the Crown''s extractiveness claim is weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(particularist_vs_public_interest, empirical, 'The actual beneficiaries of the remonstrance right.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__crown_reading, 1650, 1789).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_tr_t1650, remonstrance_authority__crown_reading, theater_ratio, 1650, 0.15).
narrative_ontology:measurement(remo_tr_t1680, remonstrance_authority__crown_reading, theater_ratio, 1680, 0.12).
narrative_ontology:measurement(remo_tr_t1710, remonstrance_authority__crown_reading, theater_ratio, 1710, 0.1).
narrative_ontology:measurement(remo_tr_t1740, remonstrance_authority__crown_reading, theater_ratio, 1740, 0.1).
narrative_ontology:measurement(remo_tr_t1770, remonstrance_authority__crown_reading, theater_ratio, 1770, 0.1).
narrative_ontology:measurement(remo_tr_t1789, remonstrance_authority__crown_reading, theater_ratio, 1789, 0.1).

% Extraction over time
narrative_ontology:measurement(remo_be_t1650, remonstrance_authority__crown_reading, base_extractiveness, 1650, 0.75).
narrative_ontology:measurement(remo_be_t1680, remonstrance_authority__crown_reading, base_extractiveness, 1680, 0.8).
narrative_ontology:measurement(remo_be_t1710, remonstrance_authority__crown_reading, base_extractiveness, 1710, 0.82).
narrative_ontology:measurement(remo_be_t1740, remonstrance_authority__crown_reading, base_extractiveness, 1740, 0.83).
narrative_ontology:measurement(remo_be_t1770, remonstrance_authority__crown_reading, base_extractiveness, 1770, 0.84).
narrative_ontology:measurement(remo_be_t1789, remonstrance_authority__crown_reading, base_extractiveness, 1789, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(remo_su_t1650, remonstrance_authority__crown_reading, suppression_requirement, 1650, 0.6).
narrative_ontology:measurement(remo_su_t1680, remonstrance_authority__crown_reading, suppression_requirement, 1680, 0.65).
narrative_ontology:measurement(remo_su_t1710, remonstrance_authority__crown_reading, suppression_requirement, 1710, 0.68).
narrative_ontology:measurement(remo_su_t1740, remonstrance_authority__crown_reading, suppression_requirement, 1740, 0.69).
narrative_ontology:measurement(remo_su_t1770, remonstrance_authority__crown_reading, suppression_requirement, 1770, 0.7).
narrative_ontology:measurement(remo_su_t1789, remonstrance_authority__crown_reading, suppression_requirement, 1789, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__crown_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'remonstrance_authority' kernel. The 'magistrate_reading' is a sibling constraint that presents an alternative interpretation of the same historical right.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
