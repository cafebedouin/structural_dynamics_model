% ============================================================================
% CONSTRAINT STORY: reformation_composite__technological_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__technological_mediation_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: reformation_composite__technological_mediation_reading
 *   human_readable: Printing Press as Catalyst for Reformation (Technological Mediation Reading)
 *   domain: historical_epistemology/religious_history/political_economy
 *
 * SUMMARY:
 *   This constraint models the printing press as a fundamental technological
 *   event that mediated the Reformation, transforming local theological
 *   dissent into a continental mass movement. It is a 'technological
 *   mediation' reading of the broader 'Reformation composite' kernel. The
 *   printing press itself, as a physical technology, is treated as a
 *   Mountain: it emerges naturally from human ingenuity, has negligible
 *   extractiveness (the technology itself doesn't extract, though its use
 *   can), and its existence fundamentally alters the landscape for all
 *   actors. Its impact is primarily enabling and disruptive, rather than
 *   extractive in itself. The beneficiaries are those who leveraged its
 *   capabilities (reformers, literate populace, secular princes), while the
 *   Catholic Church hierarchy paid the cost of losing its information
 *   monopoly.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__technological_mediation_reading, 0.05).
domain_priors:suppression_score(reformation_composite__technological_mediation_reading, 0.1).
domain_priors:theater_ratio(reformation_composite__technological_mediation_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__technological_mediation_reading, mountain).
narrative_ontology:human_readable(reformation_composite__technological_mediation_reading, "Printing Press as Catalyst for Reformation (Technological Mediation Reading)").
narrative_ontology:topic_domain(reformation_composite__technological_mediation_reading, "historical_epistemology/religious_history/political_economy").

domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__technological_mediation_reading, 'bd311a1c-9169-4e7d-b1d4-be4514bff152').
narrative_ontology:cs_kernel_codification('bd311a1c-9169-4e7d-b1d4-be4514bff152', implicit).
narrative_ontology:cs_authority_grounding('bd311a1c-9169-4e7d-b1d4-be4514bff152', diffuse_epistemic).
narrative_ontology:cs_reading_relation('bd311a1c-9169-4e7d-b1d4-be4514bff152', reformation_composite__theological_fragmentation_reading, influences).
narrative_ontology:cs_reading_relation('bd311a1c-9169-4e7d-b1d4-be4514bff152', reformation_composite__political_realignment_reading, influences).
narrative_ontology:cs_axiom('bd311a1c-9169-4e7d-b1d4-be4514bff152', foundational, information_dissemination_shapes_social_change).
narrative_ontology:cs_axiom_status(information_dissemination_shapes_social_change, holdable).
narrative_ontology:cs_axiom_grounding('bd311a1c-9169-4e7d-b1d4-be4514bff152', information_dissemination_shapes_social_change, empirically_contingent).
narrative_ontology:cs_axiom('bd311a1c-9169-4e7d-b1d4-be4514bff152', foundational, technological_advances_alter_power_dynamics).
narrative_ontology:cs_axiom_status(technological_advances_alter_power_dynamics, holdable).
narrative_ontology:cs_axiom_grounding('bd311a1c-9169-4e7d-b1d4-be4514bff152', technological_advances_alter_power_dynamics, empirically_contingent).
narrative_ontology:cs_reference_frame('bd311a1c-9169-4e7d-b1d4-be4514bff152', pre_print_information_regime).
narrative_ontology:cs_drift_state('bd311a1c-9169-4e7d-b1d4-be4514bff152', post_gutenberg_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('bd311a1c-9169-4e7d-b1d4-be4514bff152', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(reformation_composite__technological_mediation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, protestant_reformers).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, literate_populace).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, secular_princes).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, catholic_church_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The physical technology itself, enabling rapid, cheap, and widespread dissemination of texts. It sets the 'agenda' by defining the new possibilities for communication and information flow, fundamentally altering the media landscape.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, printing_press_technology, agenda_setter,
    institutional, generational, analytical, continental).

% Benefited immensely from the printing press, which allowed their theological arguments, pamphlets, and Bibles to reach a mass audience quickly and cheaply, bypassing traditional gatekeepers and accelerating the spread of their ideas.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, protestant_reformers, beneficiary,
    organized, biographical, mobile, continental).

% Paid the cost of losing its monopoly on information dissemination. The printing press undermined its control over theological discourse and made censorship far more difficult, leading to a loss of authority and fragmentation of its power.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, catholic_church_hierarchy, payer,
    institutional, generational, constrained, continental).

% Benefited from increased access to texts, including vernacular Bibles and reformist pamphlets, fostering literacy and direct engagement with religious ideas, rather than relying solely on clerical interpretation.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, literate_populace, beneficiary,
    moderate, biographical, mobile, local).

% Benefited by leveraging the printing press to disseminate their own political and religious agendas, often in support of reformers, thereby consolidating their power against both imperial and papal authority. They could arbitrage the new information landscape.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, secular_princes, beneficiary,
    powerful, generational, arbitrage, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The printing press coordinated the rapid, widespread, and standardized dissemination of information across vast geographical areas, enabling a shared intellectual and theological discourse that was previously impossible.
% TRANSFER_FUNCTION: Transferred information, ideas, and religious texts from authors and printers to a mass audience, bypassing traditional, centralized gatekeepers and accelerating the pace of intellectual and social change.
% ABSENT_VOICES: The voices of those who would have preferred to maintain a localized, oral, or manuscript-based religious culture were effectively drowned out by the sheer volume and speed of printed material. Their alternatives were suppressed by the new technological reality.
% DISAPPEARANCE_RATIONALE: If the printing press had never been invented, the Reformation as a continental mass movement would not have occurred in the same way. Local dissent might have persisted, but the rapid, widespread, and standardized dissemination of ideas that characterized the Reformation was fundamentally dependent on this technology. Its 'disappearance' would mean the world would have remained in a pre-print state, which is a different world.
% FOUNDING_PROBLEM: The problem of slow, expensive, and limited information dissemination, which constrained the spread of new ideas and maintained centralized control over knowledge.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology and media universally corroborate that the printing press fundamentally altered information flow, solving the problem of limited dissemination. The problem is 'dead' because the technology permanently changed the landscape, making a return to pre-print conditions impossible.
narrative_ontology:disappearance_verdict(reformation_composite__technological_mediation_reading, world_unchanged).
narrative_ontology:founding_problem_status(reformation_composite__technological_mediation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__technological_mediation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reformation_composite__technological_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__technological_mediation_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__technological_mediation_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, ExtMetricName, E),
    domain_priors:suppression_score(reformation_composite__technological_mediation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(reformation_composite__technological_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The printing press is classified as a Mountain because it is a physical technology that, once invented, fundamentally and irreversibly changed the conditions for information dissemination. It doesn't 'extract' in the way a human-designed institution does; rather, it creates a new physical reality. Its extractiveness is near zero, suppression is low (representing the difficulty of suppressing a widely distributed technology), and theater ratio is zero (it's a tool, not a performance). Accessibility collapse is high because it made previous methods of mass communication obsolete. Resistance is low because the technology itself was not resisted, only its applications. The beneficiaries are those who adapted to and leveraged this new reality, while the Catholic Church, which relied on the previous information regime, became a 'payer' in terms of lost control and authority.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the reformers, the printing press was a liberating force, enabling the spread of 'truth.' From the perspective of the Catholic Church, it was a disruptive force that facilitated heresy and challenged established order. This reading focuses on the structural impact of the technology itself, which is neutral, but its effects were highly asymmetric for different stakeholders.
 *
 * DIRECTIONALITY LOGIC:
 *   The printing_press_technology itself is the 'agenda_setter' by defining the new possibilities. Protestant_reformers, literate_populace, and secular_princes are beneficiaries because the technology enabled their goals and amplified their reach. The catholic_church_hierarchy is a payer because the technology undermined its existing power structures and information monopoly. The directionality for the technology itself is near 0.0 (full beneficiary) as it is an enabling force, while for the Catholic Church it is near 1.0 (full target) due to the disruptive impact.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_determinism_vs_agency,
    'To what extent was the Reformation''s trajectory determined by the printing press, versus the agency of individuals and institutions in utilizing or resisting the technology?',
    'Comparative historical analysis of other periods with new communication technologies, examining cases where similar technologies did not lead to comparable social upheavals, or where human agency successfully suppressed technological impact.',
    'If technological determinism is stronger, this ''mountain'' classification is robust. If human agency is more dominant, the constraint might be reclassified as a ''rope'' or ''tangled_rope'' where choices about technology adoption and use were more central than the technology''s inherent properties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_determinism_vs_agency, conceptual, 'Ambiguity between the inherent power of the technology and the choices made by actors.').

omega_variable(
    causal_primacy_of_technology,
    'Is the printing press truly the ''fundamental'' cause, or merely an amplifier of pre-existing theological and political tensions?',
    'Counterfactual historical analysis: what would the Reformation have looked like without the printing press? If it would have been a minor, localized event, then the technological primacy is strong. If it would have still been a major upheaval, then the technology is an amplifier.',
    'If the printing press is merely an amplifier, the ''mountain'' classification might be too strong, and the constraint might be better understood as a ''rope'' or ''scaffold'' that facilitated, rather than fundamentally caused, the movement. This would shift the focus to the underlying theological/political drivers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causal_primacy_of_technology, empirical, 'Whether technology is a primary cause or an amplifying factor.').

omega_variable(
    beneficiary_naturalness_ambiguity,
    'Is the benefit derived by ''protestant_reformers'' and ''literate_populace'' from the printing press a ''natural'' consequence of the technology, or does it reflect a specific, contingent choice to leverage it for particular ends?',
    'Examining the early adoption patterns and resistance to the printing press. If its use for reformist ends was inevitable given the technology, it''s natural. If it required active, strategic choices, it''s contingent.',
    'If contingent, the ''mountain'' classification with beneficiaries might be a ''false summit,'' suggesting a ''tangled_rope'' where the benefits were actively captured rather than passively received from a natural phenomenon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_naturalness_ambiguity, conceptual, 'Ambiguity regarding the ''naturalness'' of beneficiaries for a mountain constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__technological_mediation_reading, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1450, reformation_composite__technological_mediation_reading, theater_ratio, 1450, 0.0).
narrative_ontology:measurement(refo_tr_t1500, reformation_composite__technological_mediation_reading, theater_ratio, 1500, 0.0).
narrative_ontology:measurement(refo_tr_t1550, reformation_composite__technological_mediation_reading, theater_ratio, 1550, 0.0).
narrative_ontology:measurement(refo_tr_t1600, reformation_composite__technological_mediation_reading, theater_ratio, 1600, 0.0).
narrative_ontology:measurement(refo_tr_t1650, reformation_composite__technological_mediation_reading, theater_ratio, 1650, 0.0).

% Extraction over time
narrative_ontology:measurement(refo_be_t1450, reformation_composite__technological_mediation_reading, base_extractiveness, 1450, 0.0).
narrative_ontology:measurement(refo_be_t1500, reformation_composite__technological_mediation_reading, base_extractiveness, 1500, 0.02).
narrative_ontology:measurement(refo_be_t1550, reformation_composite__technological_mediation_reading, base_extractiveness, 1550, 0.05).
narrative_ontology:measurement(refo_be_t1600, reformation_composite__technological_mediation_reading, base_extractiveness, 1600, 0.05).
narrative_ontology:measurement(refo_be_t1650, reformation_composite__technological_mediation_reading, base_extractiveness, 1650, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1450, reformation_composite__technological_mediation_reading, suppression_requirement, 1450, 0.0).
narrative_ontology:measurement(refo_su_t1500, reformation_composite__technological_mediation_reading, suppression_requirement, 1500, 0.05).
narrative_ontology:measurement(refo_su_t1550, reformation_composite__technological_mediation_reading, suppression_requirement, 1550, 0.1).
narrative_ontology:measurement(refo_su_t1600, reformation_composite__technological_mediation_reading, suppression_requirement, 1600, 0.1).
narrative_ontology:measurement(refo_su_t1650, reformation_composite__technological_mediation_reading, suppression_requirement, 1650, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__technological_mediation_reading, information_standard).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__political_realignment_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'reformation_composite' kernel, focusing on the technological mediation of the printing press. It is linked to sibling readings that emphasize theological and political aspects, as the printing press enabled and shaped these other dimensions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
