% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__real_catastrophe_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__real_catastrophe_only, []).

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
 *   constraint_id: competence_exercise_validity__real_catastrophe_only
 *   human_readable: Real-Catastrophe-Only Doctrine of Competence Validation
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   In high-consequence operational domains (emergency response, nuclear
 *   operations, aviation, disaster management), a doctrine persists that
 *   treats survived real catastrophe as the only legitimate proof of operator
 *   and organizational competence. Personnel who have completed extensive
 *   simulation training are nonetheless regarded, under this doctrine, as
 *   fundamentally unproven until tested by an actual disaster. This produces
 *   a status hierarchy anchored on lived catastrophic experience, chronic
 *   devaluation of simulation investment, and — most consequentially — leaves
 *   the public exposed to a system whose readiness is only confirmed after
 *   the harm-producing event has already occurred.
 *
 * KEY AGENTS:
 *   - veteran_incident_commanders: Primary beneficiary (institutional/arbitrage) — authority derives from having survived real catastrophe
 *   - post_incident_review_bodies: Secondary beneficiary (institutional/constrained) — gatekeeping relevance depends on real-event review monopoly
 *   - frontline_operations_staff: Primary target (moderate/trapped) — competence chronically unverifiable through means under their control
 *   - simulation_training_programs: Secondary target (moderate/constrained) — investment devalued by doctrine regardless of fidelity
 *   - public_safety_beneficiaries: Diffuse target (powerless/trapped) — bears the risk of an unverified system until validation-by-disaster occurs
 *   - regulatory_certification_agencies: Analytical observer (institutional/analytical) — adjudicates competing certification standards
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__real_catastrophe_only, 0.58).
domain_priors:suppression_score(competence_exercise_validity__real_catastrophe_only, 0.61).
domain_priors:theater_ratio(competence_exercise_validity__real_catastrophe_only, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, extractiveness, 0.58).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__real_catastrophe_only, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__real_catastrophe_only, "Real-Catastrophe-Only Doctrine of Competence Validation").
narrative_ontology:topic_domain(competence_exercise_validity__real_catastrophe_only, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__real_catastrophe_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__real_catastrophe_only, '9b659dc5-419b-40b1-9e13-e55ccf680fc7').
narrative_ontology:cs_kernel_codification('9b659dc5-419b-40b1-9e13-e55ccf680fc7', distributed).
narrative_ontology:cs_authority_grounding('9b659dc5-419b-40b1-9e13-e55ccf680fc7', practice).
narrative_ontology:cs_interpretation_layer_present('9b659dc5-419b-40b1-9e13-e55ccf680fc7').
narrative_ontology:cs_reading_relation('9b659dc5-419b-40b1-9e13-e55ccf680fc7', competence_exercise_validity__simulation_as_proxy, forecloses).
narrative_ontology:cs_reading_relation('9b659dc5-419b-40b1-9e13-e55ccf680fc7', competence_exercise_validity__continuous_refresh_hybrid, influences).
narrative_ontology:cs_axiom('9b659dc5-419b-40b1-9e13-e55ccf680fc7', foundational, simulation_cannot_replicate_mortal_stakes).
narrative_ontology:cs_axiom_status(simulation_cannot_replicate_mortal_stakes, holdable).
narrative_ontology:cs_axiom_grounding('9b659dc5-419b-40b1-9e13-e55ccf680fc7', simulation_cannot_replicate_mortal_stakes, empirically_contingent).
narrative_ontology:cs_axiom('9b659dc5-419b-40b1-9e13-e55ccf680fc7', foundational, competence_only_confirmed_post_hoc_by_survived_disaster).
narrative_ontology:cs_axiom_status(competence_only_confirmed_post_hoc_by_survived_disaster, holdable).
narrative_ontology:cs_axiom_grounding('9b659dc5-419b-40b1-9e13-e55ccf680fc7', competence_only_confirmed_post_hoc_by_survived_disaster, empirically_contingent).
narrative_ontology:cs_reference_frame('9b659dc5-419b-40b1-9e13-e55ccf680fc7', pre_simulation_apprenticeship_doctrine).
narrative_ontology:cs_drift_state('9b659dc5-419b-40b1-9e13-e55ccf680fc7', high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9b659dc5-419b-40b1-9e13-e55ccf680fc7', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, veteran_incident_commanders).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, post_incident_review_bodies).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, frontline_operations_staff).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, simulation_training_programs).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, public_safety_beneficiaries).
narrative_ontology:constraint_vindicates(competence_exercise_validity__real_catastrophe_only, competence_is_unverifiable_absent_real_disaster).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have personally lived through a real catastrophic event and hold organizational authority partly derived from having 'been tested.' They set training doctrine, arguing that anyone who has not commanded during an actual disaster remains unproven regardless of drill performance. Their institutional standing rises as the doctrine devalues simulation credentials relative to their own lived-through status.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, veteran_incident_commanders, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__real_catastrophe_only, veteran_incident_commanders, agenda_setter).

% Conduct after-action reviews following real disasters and derive their institutional relevance and budget from being the sole body that can certify 'true' competence exercise. If simulation were accepted as sufficient, their gatekeeping function over post-catastrophe review would be diminished.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, post_incident_review_bodies, beneficiary,
    institutional, generational, constrained, national).

% Complete extensive simulation and drill programs but are told, under this doctrine, that their competence remains fundamentally unverified until they survive a real catastrophic event. This creates chronic status anxiety, blocks promotion pathways gated on 'real event' experience, and leaves them structurally unable to prove readiness through any means under their own control.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, frontline_operations_staff, payer,
    moderate, biographical, trapped, regional).

% Invest heavily in high-fidelity drills, tabletop exercises, and simulated crises, only to have their outputs treated as categorically insufficient by doctrine. Funding for simulation infrastructure is chronically undercut relative to funding for real-event response capacity, since the doctrine holds that simulation cannot substitute for the genuine article.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, simulation_training_programs, payer,
    moderate, biographical, constrained, national).

% The public whose safety depends on responder competence. Under this doctrine, competence is only truly known after a real catastrophe has already occurred and been survived or failed — meaning the public bears the risk of an unverified system until the very event that would validate or invalidate it happens, at which point harm has already occurred.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, public_safety_beneficiaries, payer,
    powerless, generational, trapped, regional).

% Set minimum training and certification standards and must decide whether to weight simulation performance or real-incident experience in licensing decisions. They observe the doctrinal dispute and are lobbied by both sides but do not themselves generate the competence-validity claim.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, regulatory_certification_agencies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_validity__real_catastrophe_only, veteran_incident_commanders).
narrative_ontology:fixing_cost_class(competence_exercise_validity__real_catastrophe_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that claims of readiness are not accepted uncritically — that an organization does not declare itself prepared for catastrophic failure modes based solely on artificial exercises that may not capture real chaos, resource scarcity, or human factors under genuine mortal stakes.
% TRANSFER_FUNCTION: Moves institutional credibility, promotion opportunity, and training budget away from personnel and programs whose competence rests on simulation, toward personnel and institutions whose authority rests on having survived an actual catastrophic event — while public safety risk is transferred forward in time to whenever the next real catastrophe occurs.
% ABSENT_VOICES: Frontline staff who have completed rigorous simulation training but lack 'real event' experience have little say in doctrine-setting; simulation designers and human-factors researchers who could show simulation fidelity is sufficient are rarely invited into the review bodies that certify what counts as proven competence.
% DISAPPEARANCE_RATIONALE: If the real-catastrophe-only doctrine vanished, simulation-validated personnel could be promoted and certified on drill performance alone, training budgets would shift substantially toward simulation fidelity, and the status hierarchy currently anchored on 'who has lived through a real disaster' would collapse — reorganizing career pathways and resource allocation across the safety organization.
% FOUNDING_PROBLEM: Early industrial and emergency-response disasters revealed that organizations which had only ever drilled on paper or in tabletop exercises collapsed under the actual chaos, resource failure, and psychological stress of real catastrophic events — drills had systematically failed to predict readiness.
% FOUNDING_PROBLEM_CORROBORATION: Veteran incident commanders and review bodies attest the founding problem remains live, citing historical drill-to-disaster failures. Independent human-factors researchers and simulation-fidelity engineers, outside the beneficiary group, argue that high-fidelity, stress-inoculated simulation has since closed much of that original gap, and that the doctrine now persists more as a status and gatekeeping mechanism than as a live technical necessity.
narrative_ontology:disappearance_verdict(competence_exercise_validity__real_catastrophe_only, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__real_catastrophe_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__real_catastrophe_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_validity__real_catastrophe_only, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__real_catastrophe_only, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__real_catastrophe_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__real_catastrophe_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_validity__real_catastrophe_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the doctrine's function as a status-transfer mechanism: it moves credibility and career opportunity toward those who happen to have survived a real event, independent of whether their underlying competence is actually superior to that of well-drilled simulation-trained personnel. Theater ratio is authored high (0.72) and rising because a growing share of the doctrine's enforcement is ceremonial — invoking 'real experience' in promotion boards and review panels — rather than tied to any demonstrated predictive validity of real-event survival over simulation performance. Suppression (0.61) captures the structural bar the doctrine places on alternative validation pathways: no amount of simulation achievement can satisfy it, which is a closed door rather than a contestable standard.
 *
 * DIRECTIONALITY LOGIC:
 *   Veteran incident commanders and review bodies are structural beneficiaries: their authority and institutional relevance are constituted by the doctrine's premise, so their directionality sits near the beneficiary end. Frontline staff, simulation programs, and the public are targets: staff cannot exit the requirement to have survived a real event (it is definitionally something that either has or has not happened to them), simulation programs cannot argue their way out of categorical exclusion, and the public cannot opt out of bearing the risk of the unverified interim. The public's powerless/trapped position amplifies effective extraction the most despite bearing no direct career cost — the harm is diffuse and delayed rather than acute.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — drills failing to predict real-world catastrophic performance — was genuinely live historically and is not fully resolved; some human-factors gaps between simulation and lived catastrophe likely persist, which is why this is authored as tangled_rope rather than snare: there IS a real coordination function (skepticism toward untested simulation claims) riding alongside the extraction (status economy for survivors, budget starvation for simulation investment). Classifying this as pure snare would erase the genuine epistemic caution the doctrine encodes; classifying it as pure rope would ignore the concentrated status benefit accruing to veteran commanders and review bodies at frontline staff's and the public's expense.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    real_event_survival_predictive_validity,
    'Does having survived a real catastrophic event actually predict superior future competence, compared to high-fidelity simulation performance, or is ''survived a real event'' largely a status marker uncorrelated with retained skill?',
    'Longitudinal outcome studies comparing incident-commander performance in subsequent real events, stratified by whether prior competence signal came from real-event survival versus simulation-only training, controlling for experience recency.',
    'If real-event survival does not out-predict high-fidelity simulation, the doctrine''s coordination justification collapses and the constraint reads much closer to snare; if it does predict better, the tangled_rope reading (genuine coordination function riding alongside extraction) is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(real_event_survival_predictive_validity, empirical, 'Whether real-catastrophe survival predicts competence better than simulation performance.').

omega_variable(
    kernel_reading_selection_basis,
    'Is the choice among the three competence_exercise_validity readings (real_catastrophe_only, simulation_as_proxy, continuous_refresh_hybrid) itself made on evidentiary grounds, or does organizational structure (who controls promotion and review) determine which reading an institution adopts regardless of evidence?',
    'Cross-organizational comparison: do institutions with different power structures (commander-dominated vs. training-department-dominated) adopt systematically different readings even when facing similar catastrophe base rates and similar simulation technology?',
    'If reading adoption tracks institutional power structure rather than evidence, this constraint''s persistence is better explained by incumbent-beneficiary capture than by genuine epistemic superiority of the real-catastrophe-only claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether reading selection within the kernel is evidence-driven or power-driven.').

omega_variable(
    public_risk_pricing_ambiguity,
    'Is the public safety cost of leaving competence unverified-until-disaster a necessary and unavoidable feature of any high-consequence domain, or is it a cost specifically generated by rejecting simulation as sufficient validation when adequate simulation technology exists?',
    'Comparative analysis of domains that have adopted simulation-as-proxy or continuous-refresh-hybrid readings: do their public safety outcomes differ measurably from real-catastrophe-only domains, holding hazard type constant?',
    'If outcomes do not differ, the public cost attributed to this reading is not offset by any competence gain, sharpening the extraction reading; if outcomes are measurably better under real-catastrophe-only, the coordination function is more substantial than assumed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_risk_pricing_ambiguity, empirical, 'Whether the doctrine''s public risk cost is offset by measurable competence gains.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__real_catastrophe_only, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_validity__real_catastrophe_only, theater_ratio, 0, 0.4).
narrative_ontology:measurement(comp_tr_t4, competence_exercise_validity__real_catastrophe_only, theater_ratio, 4, 0.48).
narrative_ontology:measurement(comp_tr_t8, competence_exercise_validity__real_catastrophe_only, theater_ratio, 8, 0.55).
narrative_ontology:measurement(comp_tr_t12, competence_exercise_validity__real_catastrophe_only, theater_ratio, 12, 0.61).
narrative_ontology:measurement(comp_tr_t16, competence_exercise_validity__real_catastrophe_only, theater_ratio, 16, 0.66).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_validity__real_catastrophe_only, theater_ratio, 20, 0.69).
narrative_ontology:measurement(comp_tr_t24, competence_exercise_validity__real_catastrophe_only, theater_ratio, 24, 0.72).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(comp_be_t4, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(comp_be_t8, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(comp_be_t12, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(comp_be_t16, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(comp_be_t20, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(comp_be_t24, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(comp_su_t4, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 4, 0.5).
narrative_ontology:measurement(comp_su_t8, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 8, 0.53).
narrative_ontology:measurement(comp_su_t12, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 12, 0.56).
narrative_ontology:measurement(comp_su_t16, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(comp_su_t20, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(comp_su_t24, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 24, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__real_catastrophe_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(competence_exercise_validity__real_catastrophe_only, 0.08).
narrative_ontology:affects_constraint(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity__simulation_as_proxy).
narrative_ontology:affects_constraint(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity__continuous_refresh_hybrid).

% DUAL FORMULATION NOTE:
% This is one of three linked readings of the competence_exercise_validity kernel. real_catastrophe_only forecloses simulation_as_proxy within a single institutional framework (an organization cannot simultaneously hold that only real disaster validates competence AND that simulation counts as valid proxy-catastrophe — these are direct premise contradictions). It influences (without foreclosing) continuous_refresh_hybrid: by devaluing simulation categorically, real_catastrophe_only reduces institutional appetite for investing in the continuous drill cycles the hybrid reading requires, creating downstream resource pressure without making the hybrid position logically untenable. Each reading carries its own ε, beneficiary/victim structure, and classification per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
