% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__catastrophe_as_necessary_anchor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__catastrophe_as_necessary_anchor, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: competence_exercise_requirement__catastrophe_as_necessary_anchor
 *   human_readable: Catastrophe as the Necessary Anchor of Operational Competence
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This story instantiates the 'catastrophe as necessary anchor' reading of
 *   a contested kernel about what maintains real operational competence in
 *   high-consequence domains. On this reading, simulation and drills —
 *   however sophisticated — cannot substitute for the genuine stakes and
 *   irreversibility of a real catastrophic event or near-miss; competence
 *   built on simulation alone atrophies during catastrophe-free intervals in
 *   ways that only manifest, often disastrously, when a real event finally
 *   arrives. The doctrine is extractive in a specific, unusual way: its
 *   'coordination function' (a shared standard for proven competence) is
 *   real, but its persistence structurally requires catastrophes to keep
 *   occurring or having occurred, and it distributes status and authority
 *   toward those who happened to be present for one, while quietly degrading
 *   the standing and confidence of everyone who has not — including the
 *   populations who suffer the anchoring events themselves. This is a
 *   distinct constraint from the sibling readings
 *   (simulation_as_adequate_exercise, hybrid_dependency), which have
 *   different beneficiary structures and different empirical exposure — do
 *   not average across them; they are separate files linked via network
 *   edges.
 *
 * KEY AGENTS:
 *   - frontline_operators_between_events: bear anxiety and recertification burden of an unclosable competence gap
 *   - junior_staff_never_exposed_to_a_real_event: structurally excluded from a credential only catastrophe can issue
 *   - populations_exposed_during_the_anchoring_event_itself: bear the literal cost treated as pedagogically necessary
 *   - veteran_operators_with_live_event_experience: primary beneficiaries of elevated status and authority
 *   - regulatory_bodies_citing_incident_data: entrench the doctrine in certification standards
 *   - independent_safety_researchers: analytical observers with no institutional stake in either reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.58).
domain_priors:suppression_score(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.42).
domain_priors:theater_ratio(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, extractiveness, 0.58).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__catastrophe_as_necessary_anchor, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__catastrophe_as_necessary_anchor, "Catastrophe as the Necessary Anchor of Operational Competence").
narrative_ontology:topic_domain(competence_exercise_requirement__catastrophe_as_necessary_anchor, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_requirement__catastrophe_as_necessary_anchor).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__catastrophe_as_necessary_anchor, '015c6891-d15b-4c19-aacb-17da8ca57e03').
narrative_ontology:cs_kernel_codification('015c6891-d15b-4c19-aacb-17da8ca57e03', distributed).
narrative_ontology:cs_authority_grounding('015c6891-d15b-4c19-aacb-17da8ca57e03', practice).
narrative_ontology:cs_interpretation_layer_present('015c6891-d15b-4c19-aacb-17da8ca57e03').
narrative_ontology:cs_reading_relation('015c6891-d15b-4c19-aacb-17da8ca57e03', competence_exercise_requirement__simulation_as_adequate_exercise, forecloses).
narrative_ontology:cs_reading_relation('015c6891-d15b-4c19-aacb-17da8ca57e03', competence_exercise_requirement__hybrid_dependency, influences).
narrative_ontology:cs_axiom('015c6891-d15b-4c19-aacb-17da8ca57e03', foundational, genuine_stakes_are_irreducible_to_simulation).
narrative_ontology:cs_axiom_status(genuine_stakes_are_irreducible_to_simulation, holdable).
narrative_ontology:cs_axiom_grounding('015c6891-d15b-4c19-aacb-17da8ca57e03', genuine_stakes_are_irreducible_to_simulation, empirically_contingent).
narrative_ontology:cs_axiom('015c6891-d15b-4c19-aacb-17da8ca57e03', secondary, competence_decays_without_catastrophic_recency).
narrative_ontology:cs_axiom_status(competence_decays_without_catastrophic_recency, holdable).
narrative_ontology:cs_axiom_grounding('015c6891-d15b-4c19-aacb-17da8ca57e03', competence_decays_without_catastrophic_recency, empirically_contingent).
narrative_ontology:cs_reference_frame('015c6891-d15b-4c19-aacb-17da8ca57e03', post_disaster_investigation_primacy).
narrative_ontology:cs_drift_state('015c6891-d15b-4c19-aacb-17da8ca57e03', high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('015c6891-d15b-4c19-aacb-17da8ca57e03', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, post_incident_safety_consultants).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, regulatory_bodies_citing_incident_data).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, veteran_operators_with_live_event_experience).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, frontline_operators_between_events).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, junior_staff_never_exposed_to_a_real_event).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, populations_exposed_during_the_anchoring_event_itself).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__catastrophe_as_necessary_anchor, muscle_memory_requires_stakes).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__catastrophe_as_necessary_anchor, simulation_fidelity_ceiling_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pilots, control-room operators, and surgical teams who train continuously on simulators and drills but never get the 'real' exercise the doctrine says is necessary. They bear the cost of an institutional belief that their skills are quietly decaying, in the form of intensified recertification, second-guessing, and anxiety about a competence gap they cannot close through practice available to them. They cannot manufacture a real catastrophe to restore their own standing.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, frontline_operators_between_events, payer,
    moderate, biographical, constrained, national).

% New hires and recent graduates are told, implicitly or explicitly, that they lack the 'real' competence of veterans who lived through a disaster or near-miss. Promotion, trust, and leadership roles are gated by possession of this experience, which they structurally cannot acquire on demand. They are trapped in a permanent probationary status relative to a credential only catastrophe can issue.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, junior_staff_never_exposed_to_a_real_event, payer,
    powerless, biographical, trapped, national).

% The passengers, patients, residents, or workers present during the actual catastrophic or near-miss event that the doctrine treats as pedagogically necessary. They bear the literal harm, injury, or death that constitutes the 'exercise.' They have no say in whether their exposure counts as someone else's competence maintenance and no channel to object to being treated as the necessary raw material of institutional learning.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, populations_exposed_during_the_anchoring_event_itself, payer,
    powerless, immediate, trapped, regional).

% Operators who were present for a real catastrophic event or near-miss acquire elevated status, deference in debriefs, and preferential access to leadership tracks because they hold experience the doctrine declares irreplaceable. They can arbitrage this credential across employers and committees; simulation-only peers cannot compete for the same authority regardless of measured skill.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, veteran_operators_with_live_event_experience, beneficiary,
    organized, biographical, arbitrage, national).

% Consultancies and expert-witness practices that build their credibility and fee structure on having analyzed real disasters, not simulated ones. Their market value rises with each new catastrophe and depends on the doctrine that simulation cannot substitute for the genuine article. They are mobile between engagements and industries, exit freely, and have no exposure to the harms the doctrine requires.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, post_incident_safety_consultants, beneficiary,
    organized, generational, mobile, national).

% Regulators write rules and certification standards that explicitly reference real-event data as the evidentiary gold standard, entrenching a doctrine that privileges catastrophe-derived knowledge over simulation-derived knowledge. This gives their standards apparent rigor and insulates them from the charge of being untested, but it also structurally requires catastrophes to keep happening (or having happened) to keep justifying the standard's authority.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, regulatory_bodies_citing_incident_data, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__catastrophe_as_necessary_anchor, regulatory_bodies_citing_incident_data, agenda_setter).

% Companies building high-fidelity simulators have a direct interest in the claim that simulation constitutes adequate exercise, but the catastrophe-as-anchor doctrine structurally discounts their product's evidentiary standing regardless of fidelity improvements. They are not part of the conversation that sets doctrine at the regulatory or institutional level; their counter-evidence rarely reaches the standard-setting table.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, simulation_and_training_technology_vendors, excluded,
    organized, biographical, constrained, global).

% Academic and independent researchers who study whether simulation-trained competence actually degrades faster than catastrophe-anchored competence, without an institutional stake in either answer. They can examine incident investigation reports and near-miss databases across organizations but have no power to compel a change in doctrine.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, independent_safety_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Institutions need a shared, legible standard for what counts as 'proven' competence in high-consequence domains, and citing real-event performance gives evaluators, regulators, and the public a common, hard-to-dispute reference point that simulation performance alone has historically struggled to establish.
% TRANSFER_FUNCTION: Moves authority, promotion opportunity, and reputational capital toward operators and institutions that have lived through a real catastrophic event, and moves anxiety, probationary status, and blame-readiness onto operators and junior staff who have not — while the literal cost of generating the anchoring events falls on the populations exposed during them.
% ABSENT_VOICES: Simulation and training-technology vendors, and the operators who have trained extensively on high-fidelity simulators without a live event, would argue that the doctrine discounts measurable, repeatable competence evidence in favor of anecdote-adjacent prestige; they are largely outside the rooms where certification and promotion criteria are set. Populations exposed during anchoring events have no voice in whether their harm is retrospectively framed as necessary pedagogy.
% DISAPPEARANCE_RATIONALE: If the doctrine that only real catastrophe maintains competence vanished overnight, simulation-based training would gain full evidentiary parity, promotion and certification pathways would open to simulation-only operators, and veteran-experience premiums would collapse — a real rearrangement for career structures and status hierarchies. But safety outcomes themselves are contested: some argue nothing would change because simulation was already doing the real work; others argue a genuine competence gap would surface at the next real event, vindicating the doctrine after the fact. The verdict cannot be settled without the very event the doctrine claims is necessary to settle it.
% FOUNDING_PROBLEM: Early high-reliability domains (aviation, nuclear, surgery) observed that operators who had never faced a real crisis sometimes froze, panicked, or missed cues that simulator-trained responses did not predict — the doctrine was built to explain and correct for a documented gap between simulated performance and real performance under genuine stakes and irreversible consequences.
% FOUNDING_PROBLEM_CORROBORATION: Veteran operators and post-incident consultants attest the gap is still live, citing specific incidents where simulator-trained crews underperformed relative to catastrophe-experienced crews. Independent safety researchers and simulation vendors, positioned outside the beneficiary set, report that high-fidelity simulation with structured debrief has closed much of the historically observed gap in several domains (notably commercial aviation), and that the residual gap may be confounded by selection: operators who survive real catastrophes and remain in the field are a survivorship-biased sample, not a controlled comparison group.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__catastrophe_as_necessary_anchor, contested).
narrative_ontology:founding_problem_status(competence_exercise_requirement__catastrophe_as_necessary_anchor, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__catastrophe_as_necessary_anchor, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that this reading transfers real authority and career advantage toward catastrophe-experienced operators while imposing a permanent, unclosable probationary status on everyone else — including literal harm on populations present during anchoring events. Suppression (0.42) is moderate: no one is legally barred from arguing simulation suffices, but institutional certification pathways and promotion committees structurally discount simulation-only records, which functions as soft suppression of the alternative. Theater ratio (0.31) captures the genuine but partial performative element — post-incident review processes sometimes perform rigor without changing underlying training investment. Resistance (0.55) is substantial because simulation vendors, researchers, and simulation-trained operators actively contest the doctrine's evidentiary basis.
 *
 * DIRECTIONALITY LOGIC:
 *   Veteran operators, post-incident consultants, and regulators who cite incident data sit near the beneficiary end: they collect status, market value, or standard-setting authority from the doctrine's persistence and are not the ones who must supply the next anchoring event. Frontline and junior operators sit near the target end: they pay in unclosable anxiety and blocked advancement, with trapped or constrained exit because professional identity and licensure are locked to the domain. Populations exposed during the anchoring event itself are the most extreme target case — d approaching the full-target end — because they bear irreversible literal harm and have zero voice in whether their exposure counts as necessary pedagogy; they did not choose to be the exercise.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (operators freezing under real stakes despite simulator training) may or may not still be live — hybrid evidence from aviation suggests high-fidelity simulation has substantially closed the gap in some domains, which would make the catastrophe-as-anchor reading a case of mandatrophy: a doctrine whose original justification has partly dissolved but which persists because it confers status on those who hold catastrophe-derived experience and gives regulators an evidentiary shortcut. Declaring founding_problem_status as contested, rather than dead, avoids overclaiming resolution the corpus cannot adjudicate from this seat alone — the mismatch between a 'dead' status and a 'world_rearranges' disappearance verdict is exactly the zombie-doctrine signal the six-questions consumer is built to catch, and this story deliberately leaves that mismatch live rather than resolving it prematurely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    muscle_memory_irreducibility,
    'Is the ''muscle memory under genuine stakes'' component of competence categorically different from simulated competence, or is it a fidelity gap that sufficiently advanced simulation could close?',
    'Longitudinal comparison of operator performance in real events versus performance predicted by high-fidelity simulator assessments, controlling for selection effects (who stays in the field after a real event) and simulator generation.',
    'If the gap is categorical, this reading''s claim is structurally vindicated and the doctrine''s persistence reflects a genuine irreplaceable exercise function. If the gap is a closable fidelity artifact, this reading collapses toward the simulation_as_adequate_exercise sibling and its extraction becomes harder to justify as coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(muscle_memory_irreducibility, empirical, 'Whether real-event exercise is categorically irreplaceable or a fidelity gap.').

omega_variable(
    survivorship_bias_in_veteran_competence,
    'Does the apparent superiority of catastrophe-experienced operators reflect the exercise''s pedagogical value, or survivorship bias — only operators who performed adequately during a real event remain in the field to be observed?',
    'Compare pre-event competence assessments (if available) against post-event outcomes and career trajectories, including operators who left the field after underperforming during a real event.',
    'If survivorship bias dominates, the doctrine mistakes a selection artifact for a training effect, which would reclassify much of the observed beneficiary advantage as unearned rather than as reward for genuine skill acquisition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survivorship_bias_in_veteran_competence, empirical, 'Selection-effect confound in the veteran-competence evidence base.').

omega_variable(
    sibling_reading_disagreement_locus,
    'Where exactly does this reading''s premise diverge from the hybrid_dependency reading — is it a difference of degree (how much real exposure is needed) or a difference of kind (whether simulation contributes anything durable at all)?',
    'Structural comparison of the two readings'' stakeholder sets and beneficiary structures: hybrid_dependency treats simulation as necessary-but-insufficient (a joint-input claim), while this reading treats real events as the only irreducible exercise (simulation''s contribution decays to near-zero without periodic real anchoring). The disagreement is located in whether simulation-derived competence has any independent half-life or fully depends on real-event recency.',
    'If the disagreement is one of degree, the two readings could in principle be reconciled by a single continuous parameter (real-event recency weighting), suggesting they are not truly distinct constraints. If it is a difference of kind, the ε-invariance principle requires they remain permanently separate stories, which this authoring assumes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_locus, conceptual, 'Where this reading''s premise is located relative to the hybrid_dependency sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0, 0.18).
narrative_ontology:measurement(comp_tr_t8, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 8, 0.22).
narrative_ontology:measurement(comp_tr_t16, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 16, 0.26).
narrative_ontology:measurement(comp_tr_t24, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 24, 0.28).
narrative_ontology:measurement(comp_tr_t32, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 32, 0.3).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(comp_be_t8, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(comp_be_t16, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(comp_be_t24, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(comp_be_t32, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 32, 0.57).
narrative_ontology:measurement(comp_be_t40, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(comp_su_t8, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(comp_su_t16, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 16, 0.37).
narrative_ontology:measurement(comp_su_t24, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(comp_su_t32, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 32, 0.41).
narrative_ontology:measurement(comp_su_t40, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__catastrophe_as_necessary_anchor, identity_coordination).
narrative_ontology:boltzmann_floor_override(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.1).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, simulation_as_adequate_exercise).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, hybrid_dependency).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the competence_exercise_requirement kernel. simulation_as_adequate_exercise claims simulation with debriefing is sufficient on its own (lower ε expected: no catastrophe-dependency, no exposed-population victim class). hybrid_dependency claims both simulation and periodic non-jeopardy real-world exposure are jointly necessary (intermediate ε: real exposure required but not necessarily catastrophic). This reading (catastrophe_as_necessary_anchor) has the highest ε and the most severe victim structure because it specifically requires catastrophic or near-miss severity, not merely real-world exposure, and it treats the resulting harm to exposed populations as structurally necessary rather than as an unfortunate byproduct of ordinary operations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
