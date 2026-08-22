% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__composite_overdetermined_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__composite_overdetermined_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: honor_satisfaction_substrate__composite_overdetermined_reading
 *   human_readable: Honor Satisfaction Constraint (Composite Overdetermined Reading)
 *   domain: social/cultural/legal
 *
 * SUMMARY:
 *   This reading instantiates the honor-satisfaction constraint as a
 *   tangled_rope subject to DUAL, causally-entangled decline mechanisms. The
 *   constraint operated to coordinate masculine status hierarchy (genuine
 *   coordination function) while extracting participation costs (death, legal
 *   jeopardy, psychological burden) from gentlemen obligated to defend honor
 *   through combat. Its decline exhibits BOTH rope-breaking (legal
 *   suppression, state enforcement) AND mountain erosion (delegitimation of
 *   the honor substrate itself via Enlightenment rationalism). Critically,
 *   these mechanisms are not independent: legal suppression worked BECAUSE
 *   the honor code itself was being delegitimated; delegitimation accelerated
 *   BECAUSE legal enforcement had begun; neither mechanism suffices alone to
 *   explain the historical trajectory. The composite reading asserts the
 *   interdependence; the sibling readings attribute causal primacy to one
 *   mechanism or the other.
 *
 * KEY AGENTS:
 *   - Honor code custodians: social authorities (gentry, military, clergy) who define and police the constraint.
 *   - Gentlemen under obligation: moderate-power, identity-locked participants bearing extraction cost and identity benefit simultaneously.
 *   - Families bearing costs: secondary victims with constrained exit.
 *   - State enforcement apparatus: institutional actor imposing exogenous suppression (legal prohibition, criminal prosecution).
 *   - Enlightenment cultural authorities: organized beneficiaries of delegitimation who reshape the cultural frame.
 *   - Excluded commoners: structurally barred from participation, would benefit from constraint erosion but have no voice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, 0.68).
domain_priors:suppression_score(honor_satisfaction_substrate__composite_overdetermined_reading, 0.71).
domain_priors:theater_ratio(honor_satisfaction_substrate__composite_overdetermined_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__composite_overdetermined_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__composite_overdetermined_reading, "Honor Satisfaction Constraint (Composite Overdetermined Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__composite_overdetermined_reading, "social/cultural/legal").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__composite_overdetermined_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__composite_overdetermined_reading, '976d0e8c-c96a-4b98-9366-81bf0d169564').
narrative_ontology:cs_kernel_codification('976d0e8c-c96a-4b98-9366-81bf0d169564', fixed_text).
narrative_ontology:cs_authority_grounding('976d0e8c-c96a-4b98-9366-81bf0d169564', lineage).
narrative_ontology:cs_interpretation_layer_present('976d0e8c-c96a-4b98-9366-81bf0d169564').
narrative_ontology:cs_reading_relation('976d0e8c-c96a-4b98-9366-81bf0d169564', honor_satisfaction_substrate__practice_decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('976d0e8c-c96a-4b98-9366-81bf0d169564', honor_satisfaction_substrate__cultural_contraction_reading, influences).
narrative_ontology:cs_axiom('976d0e8c-c96a-4b98-9366-81bf0d169564', foundational, dual_causal_entanglement_mechanism).
narrative_ontology:cs_axiom_status(dual_causal_entanglement_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('976d0e8c-c96a-4b98-9366-81bf0d169564', dual_causal_entanglement_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('976d0e8c-c96a-4b98-9366-81bf0d169564', foundational, honor_substrate_partially_reversible_under_enforcement).
narrative_ontology:cs_axiom_status(honor_substrate_partially_reversible_under_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('976d0e8c-c96a-4b98-9366-81bf0d169564', honor_substrate_partially_reversible_under_enforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('976d0e8c-c96a-4b98-9366-81bf0d169564', dual_rope_and_mountain_codification).
narrative_ontology:cs_drift_state('976d0e8c-c96a-4b98-9366-81bf0d169564', enlightenment_legal_modernization_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('976d0e8c-c96a-4b98-9366-81bf0d169564', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, honor_code_custodians).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, status_hierarchy_maintainers).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, gentlemen_under_dueling_obligation).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, families_bearing_dueling_costs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, gentlemen_under_dueling_obligation).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, enlightenment_cultural_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Social authorities (upper gentry, military officers, clergy, cultural arbiters) who define and police the honor code. They maintain the framework that makes dueling intelligible as a satisfaction mechanism. They benefit from the deference dueling extracts — it enforces status hierarchy and validates their authority to judge masculine worth.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, honor_code_custodians, agenda_setter,
    organized, generational, identity_locked, national).

% Men of gentle birth and military/civil standing who face the obligation to defend honor through challenge and combat. They bear the direct extraction: risk of death, injury, legal jeopardy, and the psychological cost of participation. Yet they also benefit from the honor-code's legitimacy — accepting the dueling obligation is how they confirm their status as gentlemen rather than commoners. Exit is identity-locked: refusing a challenge means social death, loss of credibility, and exclusion from elite circles.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, gentlemen_under_dueling_obligation, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__composite_overdetermined_reading, gentlemen_under_dueling_obligation, beneficiary).

% Wives, children, parents of dueling men. They bear secondary extraction: grief, economic disruption (loss of income or inheritance), social stigma if a family member is killed or imprisoned. They have constrained exit — they cannot leave the family or control the obligation, though they may pressure for desistance or attempt to mediate reconciliations.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, families_bearing_dueling_costs, payer,
    moderate, generational, constrained, national).

% Legal systems, police, prosecutors, courts imposing prohibitions on dueling, prosecuting violators, and enforcing sentences. They represent the exogenous suppression vector: legal barriers, criminal liability, imprisonment. They do not benefit from dueling itself but benefit from the state's monopoly on violence and rule of law authority.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, state_enforcement_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Philosophers, clergy, publicists, and reform-minded elites who delegitimate the honor code on rational and moral grounds. They benefit from the constraint's decline because its erosion validates their worldview (rationality over honor, dignity over status, written law over custom). They actively shape the endogenous delegitimation by reframing honor itself.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, enlightenment_cultural_authorities, beneficiary,
    organized, generational, mobile, national).

% The vast majority, structurally barred from dueling by birth. They would benefit from the constraint's erosion (it reinforces the social boundary that keeps them subordinate), but they are not in the conversation. Their interests are voiced, if at all, by reform advocates who take up the equality argument.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, commoners_excluded_from_dueling, excluded,
    powerless, biographical, trapped, national).

% The historical sociologist or legal historian examining the constraint's operation and decline. Positioned outside the active mechanisms to trace the causal pathways.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates masculine status hierarchy among gentlemen through a shared framework in which honor insults demand armed satisfaction. The dueling constraint solves the problem of enforcing deference to social rank without formal law: a gentleman who accepts an insult without challenge loses standing; a gentleman who receives satisfaction through combat retains it. The coordination function is the agreement on what honor means and what challenges require.
% TRANSFER_FUNCTION: Transfers authority to adjudicate masculine worth from formal institutions (courts, clergy) to peer-mediated combat. Transfers the psychological and physical burdens of status maintenance from the honor-code custodians to the gentlemen obligated to defend their standing through violence.
% ABSENT_VOICES: Commoners are structurally excluded from the dueling conversation — they cannot issue or receive challenges and have no seat at the table. Women's voices are absent despite bearing substantial secondary extraction (widowhood, economic disruption, social stigma). Enlightenment critics exist as minority voices in early periods; their presence grows louder as decline accelerates, but they enter the conversation as external critics, not as parties initially.
% DISAPPEARANCE_RATIONALE: If dueling and its obligation disappeared overnight, the status hierarchy it enforced would require alternative legitimating mechanisms. Immediate rearrangement: no challenge protocols, no satisfaction through combat, legal jeopardy removed. Deeper rearrangement: whether honor codes themselves could persist without dueling as their enforcement mechanism — the contested question between this reading and the cultural_contraction_reading. The composite reading expects both: the coordination function (status hierarchy maintenance) would find new forms, AND the honor substrate would undergo further delegitimation.
% FOUNDING_PROBLEM: How to adjudicate and enforce honor among peers when formal law and institutions are distant or powerless. The dueling constraint emerged as a solution: honor satisfaction through combat, mediated by codes of conduct that gentlemen agree bind them.
% FOUNDING_PROBLEM_CORROBORATION: Military and gentry sources attest the founding problem was live during the rise of dueling (15th–16th century): distant courts, limited police capacity, honor insults that law could not remedy. By the 18th century, legal enforcement improves and state capacity rises, yet dueling persists — this divergence is precisely what invites contention. Reform advocates attest the founding problem is solved (courts are now proximate, law is effective, rational systems can adjudicate honor); traditionalists attest it persists (some insults remain beyond law's reach, peer judgment remains superior to magistrates). Historical scholars documenting the interval agree both readings were held simultaneously by different stakeholder seats.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__composite_overdetermined_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__composite_overdetermined_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__composite_overdetermined_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is measured at 0.68 (interval endpoint, t=1850). Early in the interval (t=1500), extraction is low (0.35) because dueling was emergent and not yet mandatory — alternatives existed and honor could be maintained through other means. Extraction peaks at t=1750 (0.71) when legal enforcement and participation expectation are both maximal. It then plateaus and declines slightly (0.68 at t=1850) as cultural practice fragmented — participation fell but those still honoring the code faced the same obligation. Suppression follows a steeper rise from 0.15 (t=1500, minimal state intervention) to 0.71 (t=1850, comprehensive legal prohibition and enforcement). Theater ratio rises more gradually (0.05 to 0.42): early dueling was functional; later dueling became increasingly ritualized and performative as the real status-maintenance function was overtaken by legal consequences. Accessibility collapse shows a divergent pattern: the structural and organizational barriers to exit remain high even at t=1850 (0.42–0.44), but individual-level exit options expanded (0.31) as legal alternatives (courts, written vindication, public apology) became available. This non-uniform pattern indicates the composite mechanism: legal alternatives reduced individual accessibility collapse, but identity lock and organizational status hierarchy kept the obligation live at higher levels. The shared time grid ensures every metric is authored at every interval point — no misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The honor-code custodians (agenda-setter) compute the constraint as coordination: a shared frame enabling status maintenance without formal courts. Gentlemen under obligation compute it as extraction with identity lock: they must participate to maintain status, yet face legal jeopardy and death risk. The state enforcement apparatus computes it as violation of rule-of-law authority: private violence to be suppressed. Enlightenment critics compute it as irrational and immoral: the honor substrate itself is the problem to be dismantled. The engine computes per-seat directionality from the structural data: agenda-setters near d=0.2 (beneficiaries), obligated gentlemen near d=0.75 (targets), state apparatus at analytical d, critics at d=0.1 (beneficiaries of delegitimation). This divergence is constitutive of the composite reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Honor code custodians benefit from the constraint's operation (it validates their authority, enforces deference) and have mobile exit (they can shift the code's interpretation or enforcement). Directionality: low (beneficiary end, d≈0.15). Gentlemen under obligation bear substantial extraction costs (death, injury, legal liability) but also benefit from status confirmation that only participation provides. Their exit is identity-locked: refusing a challenge means social death. Directionality: high (target end, d≈0.72). Families bearing costs have constrained exit (cannot leave the family, limited ability to prevent dueling) and bear secondary extraction. Directionality: high (d≈0.68). State enforcement apparatus is the institutional imposer of exogenous suppression; its directionality is analytical (d=0.5, neither beneficiary nor target — it enforces monopoly on violence). Enlightenment critics have mobile exit and benefit from the constraint's delegitimation. Directionality: low (d≈0.12). Commoners are excluded and would benefit from erosion but have no structured relationship. Directionality: not applicable (excluded stakeholders do not feed metric derivation). The composite reading's key structural claim: the interdependence of exogenous suppression and endogenous delegitimation means the constraint cannot be classified solely from the agenda-setter's or a single payer's perspective — the classification must account for how state action AND cultural delegitimation reinforce each other at different levels (individual, organizational, class, structural).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading does NOT trigger mandatrophy resolution. The founding problem (how to adjudicate honor among peers when formal law is distant) remains CONTESTED, not dead. The composite reading acknowledges that the founding problem status diverges across the interval and across stakeholder seats. At t=1500–1600, the problem is live (courts are distant, honor satisfaction through combat is functional). By t=1750–1850, reformers declare it dead (courts are proximate, law is effective) while traditionalists insist it persists (some insults remain beyond law's reach, peer judgment is superior). This contestation is structural to the composite reading's claim: the constraint's decline is overdetermined BECAUSE the founding problem's status is disputed. No single seat unanimously recognizes the founding problem as dead, so no mandatrophy declaration is warranted. The theater ratio rising (0.05 to 0.42) indicates ritual maintenance increasing relative to functional maintenance, but theater alone does not establish mandatrophy — it establishes that enforcement is becoming more theatrical, not that the underlying problem has vanished.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_internalization_mechanism,
    'Is the measured suppression structural (legal enforcement, external barriers) or internalized (the honor code''s own authority, peer judgment, psychological internalization of obligation)?',
    'Historical evidence from diaries, letters, legal proceedings: do declining duelers cite legal fear or loss of honor conviction? Do courts record self-enforced desistance or forced compliance? Do reformed ex-duelers testify to legal coercion or to changed values?',
    'If structural: the constraint could theoretically be restored by eliminating legal suppression. If internalized: the constraint''s erosion reflects genuine delegitimation and would persist even if legal barriers were removed. If both: the composite mechanism holds — legal suppression accelerated delegitimation because it made honor-satisfaction legally untenable, while delegitimation reduced individual resistance to legal enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether suppression is external or internally driven.').

omega_variable(
    causal_independence_assumption,
    'Are exogenous suppression and endogenous delegitimation truly causally entangled (each accelerates the other) or merely concurrent (independently causing decline at different sites)?',
    'Counterfactual analysis: would pure legal suppression (without Enlightenment philosophical challenge) have broken the constraint? Would pure delegitimation (without legal enforcement) have eroded dueling? Historical comparison with jurisdictions that suppressed earlier or delegitimated earlier without the other mechanism.',
    'If independent: the constraint''s decline can be modeled as two additive pressures; classification could treat suppression and delegitimation separately. If entangled: the constraint exhibits composite dynamics (tangled_rope with dual decline mechanisms) and cannot be reduced to either mechanism alone. If entangled, the composite_overdetermined_reading is correct; if independent, the sibling readings'' single-mechanism focus may be more accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(causal_independence_assumption, conceptual, 'Whether the two decline mechanisms are causally independent or mutually reinforcing.').

omega_variable(
    honor_substrate_transformation_irreversibility,
    'Did the honor code itself undergo foundational, irreversible transformation, or did it merely lose institutional enforcement while remaining viable as an alternative frame?',
    'Post-decline dueling events (19th century sporadic challenges, honor-code persistence among military and diplomatic elites): did participants invoke the original honor frame or a modified/nostalgic version? Can the honor code be reconstructed from first principles by contemporary actors, or has it become historically embedded and unrepeatable?',
    'If irreversible: the cultural_contraction_reading is vindicated (the honor substrate is gone, not just suppressed). If reversible: the constraint could theoretically be restored if legal suppression were lifted. If partially reversible: the composite reading is supported (the substrate is transformed but not erased, making the constraint''s decline path-dependent on both suppression and delegitimation).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(honor_substrate_transformation_irreversibility, conceptual, 'Whether honor-code transformation is structural or contingent on enforcement.').

omega_variable(
    kernel_reading_contest_framing,
    'Does this composite reading (''decline is overdetermined by dual, entangled mechanisms'') correctly capture the structural contest between the three kernel readings, or does it misframe the sibling readings'' claims?',
    'Textual analysis of historical source materials (dueling treatises, legal statutes, Enlightenment critiques, military codes, courtroom testimony): which mechanism do primary sources emphasize? Do contemporaries describe decline as enforcement-driven or as value-driven? Do they recognize the entanglement or attribute causality unilaterally?',
    'If this reading misframes the siblings: the cs_structure.reading_relations and axioms need recalibration. If this reading correctly captures the contest: the structural divergence between readings is genuine and the engine''s per-seat classification should diverge significantly across seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_framing, conceptual, 'Whether the three readings genuinely instantiate different constraint structures or represent interpretive disagreement about a single constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__composite_overdetermined_reading, 1500, 1850).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1500, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1500, 0.05).
narrative_ontology:measurement(hono_tr_t1600, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1600, 0.12).
narrative_ontology:measurement(hono_tr_t1700, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1700, 0.28).
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1750, 0.38).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1800, 0.42).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1850, 0.42).

% Extraction over time
narrative_ontology:measurement(hono_be_t1500, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1500, 0.35).
narrative_ontology:measurement(hono_be_t1600, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1600, 0.52).
narrative_ontology:measurement(hono_be_t1700, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1700, 0.64).
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1750, 0.71).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1800, 0.68).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1850, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1500, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1500, 0.15).
narrative_ontology:measurement(hono_su_t1600, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1600, 0.28).
narrative_ontology:measurement(hono_su_t1700, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1700, 0.52).
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1750, 0.68).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1800, 0.71).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1850, 0.71).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1500, tn=1850
narrative_ontology:measurement(hono_grid_01, honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse(class), 1500, 0.71).
narrative_ontology:measurement(hono_grid_02, honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse(class), 1850, 0.44).
narrative_ontology:measurement(hono_grid_03, honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse(individual), 1500, 0.65).
narrative_ontology:measurement(hono_grid_04, honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse(individual), 1850, 0.31).
narrative_ontology:measurement(hono_grid_05, honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse(organizational), 1500, 0.68).
narrative_ontology:measurement(hono_grid_06, honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse(organizational), 1850, 0.38).
narrative_ontology:measurement(hono_grid_07, honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse(structural), 1500, 0.72).
narrative_ontology:measurement(hono_grid_08, honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse(structural), 1850, 0.42).
narrative_ontology:measurement(hono_grid_09, honor_satisfaction_substrate__composite_overdetermined_reading, resistance(class), 1500, 0.25).
narrative_ontology:measurement(hono_grid_10, honor_satisfaction_substrate__composite_overdetermined_reading, resistance(class), 1850, 0.64).
narrative_ontology:measurement(hono_grid_11, honor_satisfaction_substrate__composite_overdetermined_reading, resistance(individual), 1500, 0.28).
narrative_ontology:measurement(hono_grid_12, honor_satisfaction_substrate__composite_overdetermined_reading, resistance(individual), 1850, 0.48).
narrative_ontology:measurement(hono_grid_13, honor_satisfaction_substrate__composite_overdetermined_reading, resistance(organizational), 1500, 0.18).
narrative_ontology:measurement(hono_grid_14, honor_satisfaction_substrate__composite_overdetermined_reading, resistance(organizational), 1850, 0.62).
narrative_ontology:measurement(hono_grid_15, honor_satisfaction_substrate__composite_overdetermined_reading, resistance(structural), 1500, 0.22).
narrative_ontology:measurement(hono_grid_16, honor_satisfaction_substrate__composite_overdetermined_reading, resistance(structural), 1850, 0.58).
narrative_ontology:measurement(hono_grid_17, honor_satisfaction_substrate__composite_overdetermined_reading, stakes_inflation(class), 1500, 0.35).
narrative_ontology:measurement(hono_grid_18, honor_satisfaction_substrate__composite_overdetermined_reading, stakes_inflation(class), 1850, 0.71).
narrative_ontology:measurement(hono_grid_19, honor_satisfaction_substrate__composite_overdetermined_reading, stakes_inflation(individual), 1500, 0.42).
narrative_ontology:measurement(hono_grid_20, honor_satisfaction_substrate__composite_overdetermined_reading, stakes_inflation(individual), 1850, 0.78).
narrative_ontology:measurement(hono_grid_21, honor_satisfaction_substrate__composite_overdetermined_reading, stakes_inflation(organizational), 1500, 0.38).
narrative_ontology:measurement(hono_grid_22, honor_satisfaction_substrate__composite_overdetermined_reading, stakes_inflation(organizational), 1850, 0.74).
narrative_ontology:measurement(hono_grid_23, honor_satisfaction_substrate__composite_overdetermined_reading, stakes_inflation(structural), 1500, 0.32).
narrative_ontology:measurement(hono_grid_24, honor_satisfaction_substrate__composite_overdetermined_reading, stakes_inflation(structural), 1850, 0.68).
narrative_ontology:measurement(hono_grid_25, honor_satisfaction_substrate__composite_overdetermined_reading, suppression(class), 1500, 0.11).
narrative_ontology:measurement(hono_grid_26, honor_satisfaction_substrate__composite_overdetermined_reading, suppression(class), 1850, 0.72).
narrative_ontology:measurement(hono_grid_27, honor_satisfaction_substrate__composite_overdetermined_reading, suppression(individual), 1500, 0.18).
narrative_ontology:measurement(hono_grid_28, honor_satisfaction_substrate__composite_overdetermined_reading, suppression(individual), 1850, 0.74).
narrative_ontology:measurement(hono_grid_29, honor_satisfaction_substrate__composite_overdetermined_reading, suppression(organizational), 1500, 0.12).
narrative_ontology:measurement(hono_grid_30, honor_satisfaction_substrate__composite_overdetermined_reading, suppression(organizational), 1850, 0.68).
narrative_ontology:measurement(hono_grid_31, honor_satisfaction_substrate__composite_overdetermined_reading, suppression(structural), 1500, 0.08).
narrative_ontology:measurement(hono_grid_32, honor_satisfaction_substrate__composite_overdetermined_reading, suppression(structural), 1850, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__composite_overdetermined_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_substrate__composite_overdetermined_reading, 0.12).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate__practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate__cultural_contraction_reading).

% DUAL FORMULATION NOTE:
% The honor_satisfaction_substrate kernel decomposes into three constraint stories (readings) with structurally distinct ε values and causal claims. This reading (composite_overdetermined) claims ε ≈ 0.68 (extraction from dual mechanisms) and attributes decline to entangled suppression and delegitimation. The practice_decline reading claims exogenous suppression is primary and attributes decline to enforcement. The cultural_contraction reading claims endogenous delegitimation (honor substrate transformation) is primary. All three readings share the same referent (the honor-satisfaction mechanism in European dueling societies ~1500–1850) but interpret its decline differently. They are linked by network.affects_constraints to enable comparative analysis of how different framings produce different constraint classifications from the same historical situation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_substrate__composite_overdetermined_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
