% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__strategic_culture_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__strategic_culture_drift, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: total_war_winnability_post1945__strategic_culture_drift
 *   human_readable: Total War Unthinkability via Strategic Culture Drift
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   Post-1945, total war as a deliberate strategic option disappeared from
 *   elite discourse. This constraint story captures the reading that the
 *   disappearance was driven by an ideational shift in strategic
 *   cultureâdefense intellectuals and war colleges progressively treating
 *   total war as analytically unthinkable rather than merely undesirable or
 *   physically impossible. The constraint persists as a piton: the original
 *   coordinating function (preventing catastrophic great-power escalation) is
 *   now handled by nuclear deterrence, but the ideational prohibition
 *   remains, maintained performatively by a professional community heavily
 *   invested in limited-war frameworks. It is one reading of the
 *   total_war_winnability_post1945 kernel, distinct from the normative
 *   reading (legal prohibition) and the structural reading (physical
 *   impossibility via nuclear weapons).
 *
 * KEY AGENTS:
 *   - Defense intellectuals (agenda_setter/beneficiary, institutional, identity_locked): administer strategic discourse through journals, curricula, and advisory roles; could reintroduce total-war thinking but would destroy professional identity.
 *   - National security planners (payer, institutional, constrained): bear the cost of atrophied strategic flexibility and underdeveloped total-war contingency planning.
 *   - Dissident realist strategists (excluded, moderate, constrained): would object to the ideational closure but are professionally marginalized.
 *   - Military history archivists (observer, moderate, analytical): document the gap between physical capability and cultural thinkability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__strategic_culture_drift, 0.42).
domain_priors:suppression_score(total_war_winnability_post1945__strategic_culture_drift, 0.38).
domain_priors:theater_ratio(total_war_winnability_post1945__strategic_culture_drift, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, extractiveness, 0.42).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__strategic_culture_drift, piton).
narrative_ontology:human_readable(total_war_winnability_post1945__strategic_culture_drift, "Total War Unthinkability via Strategic Culture Drift").
narrative_ontology:topic_domain(total_war_winnability_post1945__strategic_culture_drift, "international_relations/strategic_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__strategic_culture_drift, 'cbf5664a-d896-4f47-aca2-c78ee98e4930').
narrative_ontology:cs_kernel_codification('cbf5664a-d896-4f47-aca2-c78ee98e4930', distributed).
narrative_ontology:cs_authority_grounding('cbf5664a-d896-4f47-aca2-c78ee98e4930', expertise).
narrative_ontology:cs_interpretation_layer_present('cbf5664a-d896-4f47-aca2-c78ee98e4930').
narrative_ontology:cs_reading_relation('cbf5664a-d896-4f47-aca2-c78ee98e4930', total_war_winnability_post1945__structural_contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('cbf5664a-d896-4f47-aca2-c78ee98e4930', total_war_winnability_post1945__normative_reading_drop, coexists_with).
narrative_ontology:cs_axiom('cbf5664a-d896-4f47-aca2-c78ee98e4930', foundational, strategic_culture_determines_thinkability).
narrative_ontology:cs_axiom_status(strategic_culture_determines_thinkability, holdable).
narrative_ontology:cs_axiom_grounding('cbf5664a-d896-4f47-aca2-c78ee98e4930', strategic_culture_determines_thinkability, conventional).
narrative_ontology:cs_axiom('cbf5664a-d896-4f47-aca2-c78ee98e4930', foundational, limited_war_paradigm_professionally_binding).
narrative_ontology:cs_axiom_status(limited_war_paradigm_professionally_binding, holdable).
narrative_ontology:cs_axiom_grounding('cbf5664a-d896-4f47-aca2-c78ee98e4930', limited_war_paradigm_professionally_binding, instrumental).
narrative_ontology:cs_reference_frame('cbf5664a-d896-4f47-aca2-c78ee98e4930', classical_total_war_thinkable).
narrative_ontology:cs_drift_state('cbf5664a-d896-4f47-aca2-c78ee98e4930', contemporary_security_studies, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cbf5664a-d896-4f47-aca2-c78ee98e4930', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, defense_intellectuals).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, national_security_planners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Professional community of strategists, war college faculty, and defense analysts whose careers and intellectual frameworks are built around limited-war paradigms. They reproduce the discourse through curricula, journals, and advisory roles, and could reintroduce total-war frameworks but would pay high professional costs in credibility and identity.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, defense_intellectuals, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__strategic_culture_drift, defense_intellectuals, beneficiary).

% Military and civilian strategists responsible for national defense planning. They bear the cost of a constrained option space: total-war scenarios are analytically underdeveloped, force structures are optimized for limited contingencies, and institutional memory for mass mobilization has atrophied, making strategic planning brittle against existential threats.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, national_security_planners, payer,
    institutional, generational, constrained, global).

% Scholars and practitioners who argue for the continued analytical relevance of total-war thinking. They are marginalized in elite journals, denied mainstream academic positions, and excluded from policy advisory roles not because their arguments are empirically refuted but because they violate the professional aesthetic and cultural norms of the strategic studies field.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, dissident_realist_strategists, excluded,
    moderate, biographical, constrained, national).

% Historians and institutional memory specialists who document the actual total-war capabilities and historical conduct of warfare. They observe the growing gap between what states could still physically accomplish and what contemporary strategic culture admits as thinkable or legitimate.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, military_history_archivists, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_winnability_post1945__strategic_culture_drift, diffuse).
narrative_ontology:fixing_cost_class(total_war_winnability_post1945__strategic_culture_drift, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally coordinated great-power relations by rendering total war strategically illegitimate and unthinkable, reducing the risk of catastrophic escalation. That coordination problem is now managed through nuclear deterrence and other mechanisms; the ideational prohibition persists as vestigial professional convention.
% TRANSFER_FUNCTION: Moves intellectual authority, funding, and career advancement from total-war analytical frameworks to limited-war frameworks; moves strategic option space away from national security planners toward an atrophied default.
% ABSENT_VOICES: Dissident realist strategists who would argue for restoring total-war contingency planning are structurally excluded from tenure lines, elite journals, and advisory panels; their absence is mistaken for consensus that total war is no longer relevant.
% DISAPPEARANCE_RATIONALE: If the ideational block vanished overnight, strategic studies curricula would reincorporate total-war history and theory, defense intellectuals would retool around broader contingency frameworks, and national security planning would regenerate analytical capacity for mass mobilization scenarios. The rearrangement is discursive and institutional, not physical.
% FOUNDING_PROBLEM: Prevention of great-power total war in the early nuclear age through institutional and ideational learning that rendered unlimited conflict strategically obsolete.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear strategists attest that deterrence now manages the existential risk; military historians attest that the original problem was specific to the early Cold War power transition; no corroboration from outside the benefiting parties exists that the ideational prohibition remains functionally necessary today.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__strategic_culture_drift, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__strategic_culture_drift, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__strategic_culture_drift, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_winnability_post1945__strategic_culture_drift, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__strategic_culture_drift, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__strategic_culture_drift_tests).
:- end_tests(total_war_winnability_post1945__strategic_culture_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater_ratio is high (0.72) because a large share of contemporary strategic studies activityâconferences, publishing, curriculum designâmaintains a limited-war paradigm whose original referent (preventing total war) is no longer a live coordination problem. Base_extractiveness (0.42) reflects a moderate but real loss: states have lost institutional memory and analytical frameworks for total mobilization. Suppression (0.38) is moderate because enforcement is discursive and professional rather than legal or violent; total-war thinking is ruled out of bounds by peer review and hiring norms, not by state coercion. Resistance (0.18) is low because the costs are diffuse across strategic planning and no concentrated victim group is organized to restore total-war discourse. Accessibility_collapse (0.62) is substantial: once inside the strategic culture, total-war alternatives are nearly invisible as legitimate intellectual moves.
 *
 * PERSPECTIVAL GAP:
 *   From the defense intellectual seat, the arrangement is legitimate professional evolutionâtotal war genuinely became obsolete as an analytical category. From the national security planner seat, the same structure is an unacknowledged constraint that has atrophied their option space without their consent. From the dissident realist seat, it is ideological closure masquerading as maturity. The engine computes these divergences from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Defense intellectuals sit near the beneficiary end: the constraint validates their professional identity, intellectual sunk costs, and career investments. Their exit is identity_locked because leaving the limited-war paradigm would mean abandoning the self-concept that constitutes their professional standing. National security planners sit near the target end: they bear the operational cost of a constrained option space but lack discursive control to change it. Their exit is constrained because they depend on the same defense intellectuals for doctrine and analysis. Dissident realists are excluded from the conversation rather than coordinated; their high directionality is moot because the constraint operates by shutting them out, not by extracting from them directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpreventing great-power total warâwas genuine, but it has been superseded by nuclear deterrence and other structural mechanisms. The ideational prohibition on total-war discourse persists not because it solves a live coordination problem, but because a professional community is identity-locked to the paradigm that grew up around the original problem. This prevents misclassifying the constraint as rope (it does not coordinate a live problem) or as snare (there is no concentrated beneficiary capturing extraction; defense intellectuals benefit in diffuse identity-validation rather than rents). The piton classification captures the atrophy and theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ideational_vs_structural_causality,
    'Is the disappearance of total war from elite discourse caused by an independent ideational shift, or is it an epiphenomenon of structural deterrence and material incapacity?',
    'Comparative case analysis of non-nuclear great-power dyads and pre-nuclear historical periods to isolate the independent effect of strategic culture from material constraints.',
    'If structural, this reading collapses into the structural_contraction reading; if independent, the ideational constraint has genuine causal force and the piton classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideational_vs_structural_causality, conceptual, 'Whether cultural unthinkability is independent of physical impossibility.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of total-war discourse structural (professional gatekeeping, funding control) or internalized (strategists genuinely cannot conceive of total war as relevant)?',
    'Post-exit trajectory analysis: if strategists outside the discipline (e.g., in private sector risk analysis) readily generate total-war scenarios, suppression is structural; if the conceptual block persists across institutional contexts, it is partially internalized.',
    'If internalized, effective suppression exceeds structural measures and the constraint operates more like a cognitive capture than a professional piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of total-war discourse.').

omega_variable(
    piton_vs_snare_beneficiary_concentration,
    'Do defense intellectuals capture concentrated enough benefits from the limited-war paradigm to constitute a snare-like extraction, or are the gains genuinely diffuse identity-validation?',
    'Tracing of funding flows, career premiums, and institutional budgets tied explicitly to limited-war frameworks versus general defense analysis.',
    'If concentrated extraction is found, the constraint reclassifies as snare; if not, piton classification is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(piton_vs_snare_beneficiary_concentration, empirical, 'Whether beneficiary gains are concentrated enough to disqualify piton.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__strategic_culture_drift, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(total_war_strat_cult_tr_t0, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 0, 0.05).
narrative_ontology:measurement(total_war_strat_cult_tr_t10, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 10, 0.15).
narrative_ontology:measurement(total_war_strat_cult_tr_t20, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 20, 0.3).
narrative_ontology:measurement(total_war_strat_cult_tr_t30, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 30, 0.45).
narrative_ontology:measurement(total_war_strat_cult_tr_t40, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 40, 0.55).
narrative_ontology:measurement(total_war_strat_cult_tr_t50, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 50, 0.65).
narrative_ontology:measurement(total_war_strat_cult_tr_t60, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 60, 0.7).
narrative_ontology:measurement(total_war_strat_cult_tr_t70, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 70, 0.72).

% Extraction over time
narrative_ontology:measurement(total_war_strat_cult_be_t0, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(total_war_strat_cult_be_t10, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(total_war_strat_cult_be_t20, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(total_war_strat_cult_be_t30, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(total_war_strat_cult_be_t40, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(total_war_strat_cult_be_t50, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 50, 0.4).
narrative_ontology:measurement(total_war_strat_cult_be_t60, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 60, 0.41).
narrative_ontology:measurement(total_war_strat_cult_be_t70, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 70, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_winnability_post1945__strategic_culture_drift, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, normative_reading_drop).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, structural_contraction_reading).

% DUAL FORMULATION NOTE:
% The natural-language concept 'total war winnability post-1945' decomposes into three structurally distinct constraints per the epsilon-invariance principle: a normative reading (legal prohibition), a structural reading (physical impossibility), and this cultural reading (ideational drift). Each has a different epsilon, beneficiary structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
