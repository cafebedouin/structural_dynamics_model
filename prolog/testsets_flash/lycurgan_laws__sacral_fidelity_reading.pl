% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__sacral_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__sacral_fidelity_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: lycurgan_laws__sacral_fidelity_reading
 *   human_readable: Lycurgan Laws as Sacred, Unchangeable Divine Ordinance (Sacral Fidelity Reading)
 *   domain: political_philosophy/constitutional_theory/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the 'sacral fidelity' reading of the Lycurgan
 *   laws, where they are understood as a divinely ordained, unchangeable
 *   constitution requiring absolute adherence. This reading attributes
 *   Spartan decline to a failure of moral fidelity rather than systemic
 *   flaws, and views the laws' immutability as a virtue. It is a Mountain
 *   because, within this interpretive frame, the laws are treated as an
 *   irreducible, natural (divine) limit on human action, not a human
 *   construct. The high suppression reflects the absolute social control
 *   inherent in this interpretation, which is seen as a feature of divine
 *   order, not a coercive imposition.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__sacral_fidelity_reading, 0.05).
domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, 0.95).
domain_priors:theater_ratio(lycurgan_laws__sacral_fidelity_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__sacral_fidelity_reading, mountain).
narrative_ontology:human_readable(lycurgan_laws__sacral_fidelity_reading, "Lycurgan Laws as Sacred, Unchangeable Divine Ordinance (Sacral Fidelity Reading)").
narrative_ontology:topic_domain(lycurgan_laws__sacral_fidelity_reading, "political_philosophy/constitutional_theory/commitment_systems").

domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__sacral_fidelity_reading, '510f2817-a20b-4c3e-b15a-6bb816640b3a').
narrative_ontology:cs_kernel_codification('510f2817-a20b-4c3e-b15a-6bb816640b3a', fixed_text).
narrative_ontology:cs_authority_grounding('510f2817-a20b-4c3e-b15a-6bb816640b3a', lineage).
narrative_ontology:cs_interpretation_layer_present('510f2817-a20b-4c3e-b15a-6bb816640b3a').
narrative_ontology:cs_reading_relation('510f2817-a20b-4c3e-b15a-6bb816640b3a', lycurgan_laws__demographic_trap_reading, forecloses).
narrative_ontology:cs_reading_relation('510f2817-a20b-4c3e-b15a-6bb816640b3a', lycurgan_laws__adaptive_fiction_reading, forecloses).
narrative_ontology:cs_axiom('510f2817-a20b-4c3e-b15a-6bb816640b3a', foundational, divine_origin_of_laws).
narrative_ontology:cs_axiom_status(divine_origin_of_laws, holdable).
narrative_ontology:cs_axiom_grounding('510f2817-a20b-4c3e-b15a-6bb816640b3a', divine_origin_of_laws, theological).
narrative_ontology:cs_axiom('510f2817-a20b-4c3e-b15a-6bb816640b3a', foundational, immutability_as_virtue).
narrative_ontology:cs_axiom_status(immutability_as_virtue, holdable).
narrative_ontology:cs_axiom_grounding('510f2817-a20b-4c3e-b15a-6bb816640b3a', immutability_as_virtue, deontological).
narrative_ontology:cs_reference_frame('510f2817-a20b-4c3e-b15a-6bb816640b3a', perfect_divine_order).
narrative_ontology:cs_drift_state('510f2817-a20b-4c3e-b15a-6bb816640b3a', spartan_decline_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('510f2817-a20b-4c3e-b15a-6bb816640b3a', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, spartan_ruling_elite).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, spartan_citizens).
narrative_ontology:constraint_vindicates(lycurgan_laws__sacral_fidelity_reading, divine_mandate_theory_of_law).
narrative_ontology:constraint_vindicates(lycurgan_laws__sacral_fidelity_reading, spartan_exceptionalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound by the laws from birth, their entire social and political identity is defined by adherence to the Lycurgan system. They bear the costs of its rigidity, including strict social control and lack of individual freedom, but perceive these as necessary for Spartan virtue and strength.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, spartan_citizens, payer,
    powerless, generational, identity_locked, local).

% Administers and interprets the Lycurgan laws, benefiting from the stability and legitimacy derived from their sacred status. They enforce strict adherence, attributing any societal problems to a failure of fidelity rather than flaws in the laws themselves. Their power is grounded in upholding the divine mandate.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, spartan_ruling_elite, agenda_setter,
    institutional, generational, constrained, local).

% The historical figures and divine patrons (e.g., Lycurgus, Apollo) whose wisdom and authority are invoked to justify the laws' immutability. They 'benefit' from their legacy being preserved as perfect and unassailable.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, spartan_ancestors, beneficiary,
    analytical, civilizational, analytical, local).
narrative_ontology:stakeholder_non_agent(lycurgan_laws__sacral_fidelity_reading, spartan_ancestors).

% Analyze the Lycurgan system from a historical and theoretical perspective, often perpetuating the narrative of its divine origin and unchangeable nature, or critiquing it from a modern viewpoint. This reading aligns with those who emphasize the sacred and immutable aspects.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, spartan_historians_and_philosophers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, divinely sanctioned social and political order, ensuring absolute unity and discipline among citizens by removing any possibility of legal or constitutional revision.
% TRANSFER_FUNCTION: Transfers absolute authority and legitimacy from a divine source (Apollo/Lycurgus) to the Spartan ruling elite, in exchange for the citizens' absolute obedience and sacrifice of individual autonomy.
% ABSENT_VOICES: Any voices advocating for legal reform, adaptation to changing circumstances, or individual liberties are structurally absent, as the very concept of questioning the divine laws is anathema to this reading. Such voices would be deemed impious or seditious.
% DISAPPEARANCE_RATIONALE: If the belief in the Lycurgan laws' sacred, unchangeable nature vanished, the entire Spartan social and political structure would collapse. The ruling elite's legitimacy would evaporate, citizens would demand reforms, and the unique Spartan identity would dissolve, leading to a complete reorganization of their society.
% FOUNDING_PROBLEM: To establish a perfectly ordered, stable, and virtuous society capable of military supremacy, free from internal strife and corruption, by grounding its laws in divine, immutable wisdom.
% FOUNDING_PROBLEM_CORROBORATION: The Spartan ruling elite and traditionalist historians attest that the problem of maintaining virtue and order is always live, and that the laws' immutability is the only solution. This view is corroborated by ancient texts that praise Spartan stability, though modern historians often contest the 'live' status of the original problem in its pure form.
narrative_ontology:disappearance_verdict(lycurgan_laws__sacral_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__sacral_fidelity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__sacral_fidelity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(lycurgan_laws__sacral_fidelity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__sacral_fidelity_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, ExtMetricName, E),
    domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(lycurgan_laws__sacral_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.05) because, from this perspective, the laws are not designed to extract but to perfect society; any 'cost' is seen as a necessary part of divine order. Suppression is very high (0.95) because the laws demand absolute obedience and permit no deviation, which is framed as a natural consequence of their sacred origin. Theater ratio is low (0.05) as the laws are genuinely believed to be functional and divinely inspired, with little performative maintenance. Accessibility collapse is near total (0.98) as no alternatives are conceivable within this framework, and resistance is negligible (0.02) because questioning the laws is sacrilege.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Spartan ruling elite, the laws are a perfect, divinely given framework that ensures stability and virtue. From the perspective of the Spartan citizens, the laws impose extreme discipline and sacrifice, but this is internalized as a necessary condition for their identity and the state's strength. An external observer might see the rigidity as a flaw, but this reading actively forecloses such interpretations.
 *
 * DIRECTIONALITY LOGIC:
 *   The Spartan ruling elite are the agenda-setters and beneficiaries, as their authority is directly derived from and legitimized by the laws' sacred status. Spartan citizens are payers, bearing the costs of absolute adherence, but their identity-locked exit options and deep internalization of the laws' virtue mean their directionality is not purely extractive. The 'ancestors' are a non-agent beneficiary, representing the legacy that is preserved.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively resists any notion of mandatrophy. The laws' mandate is considered eternal and divinely given, thus incapable of outliving its function. Any perceived 'decline' or 'failure' is attributed to a lack of fidelity by the citizens, not to the laws themselves. The classification as a Mountain, despite identifiable beneficiaries, triggers FSM, which is appropriate for a constraint presented as natural law but benefiting specific actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_vs_human_origin,
    'Are the Lycurgan laws truly a divine ordinance, or a human construct attributed to divine origin to secure legitimacy and immutability?',
    'Archaeological discovery of direct divine communication, or a shift in historical consensus regarding the historicity and legislative process of Lycurgus.',
    'If proven human-made, the constraint would shift from a Mountain to a Snare or Tangled Rope, as its persistence would then depend on active enforcement and suppression of alternatives, rather than natural emergence. The beneficiaries would be seen as extracting from a constructed system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_vs_human_origin, conceptual, 'Ambiguity of the laws'' ultimate origin and its implications for their classification.').

omega_variable(
    fidelity_vs_systemic_failure,
    'Was Spartan decline primarily due to a failure of citizen fidelity to the laws, or due to the inherent rigidity and unadaptability of the Lycurgan system itself?',
    'Comparative historical analysis of other rigid constitutional systems and their long-term viability, or counterfactual historical modeling.',
    'If systemic rigidity is identified as the primary cause, this reading''s justification for immutability would be undermined, potentially shifting the constraint towards a Snare or Piton, as its persistence would be seen as detrimental rather than beneficial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fidelity_vs_systemic_failure, empirical, 'Whether Spartan decline was due to moral failure or systemic design flaws.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__sacral_fidelity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(lycu_tr_t25, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 25, 0.05).
narrative_ontology:measurement(lycu_tr_t50, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 50, 0.05).
narrative_ontology:measurement(lycu_tr_t75, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 75, 0.05).
narrative_ontology:measurement(lycu_tr_t100, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(lycu_be_t25, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 25, 0.05).
narrative_ontology:measurement(lycu_be_t50, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 50, 0.05).
narrative_ontology:measurement(lycu_be_t75, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 75, 0.05).
narrative_ontology:measurement(lycu_be_t100, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 100, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(lycu_su_t25, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 25, 0.95).
narrative_ontology:measurement(lycu_su_t50, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 50, 0.95).
narrative_ontology:measurement(lycu_su_t75, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 75, 0.95).
narrative_ontology:measurement(lycu_su_t100, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 100, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
