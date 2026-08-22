% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__catastrophe_as_necessary_anchor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: competence_exercise_requirement__catastrophe_as_necessary_anchor
 *   human_readable: Catastrophe-as-Necessary-Anchor Reading of the Competence Exercise Requirement
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   This story instantiates one specific reading of a contested kernel in
 *   high-reliability organization theory: the claim that only real
 *   catastrophic events or genuine near-misses provide the 'irreducible
 *   exercise' needed to maintain operational competence, and that competence
 *   built on simulation alone quietly atrophies during catastrophe-free
 *   periods regardless of simulation quality. This reading is distinct from a
 *   hybrid-dependency reading (which holds both simulation and periodic
 *   real-world anchoring are needed) and from a simulation-adequacy reading
 *   (which holds high-fidelity simulation with debriefing already suffices).
 *   As authored here, the catastrophe-as-anchor reading functions as a
 *   tangled rope: it coordinates genuine attention to a real risk (skill
 *   decay, the gap between 'knowing about' procedures and 'muscle memory'
 *   under real stakes) while asymmetrically extracting status, advancement,
 *   and psychological burden from frontline operators who cannot manufacture
 *   the very anchoring events the standard requires, and while quietly
 *   normalizing the idea that periodic real disasters are functionally
 *   necessary rather than purely tragic.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.58).
domain_priors:suppression_score(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.42).
domain_priors:theater_ratio(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, extractiveness, 0.58).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__catastrophe_as_necessary_anchor, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__catastrophe_as_necessary_anchor, "Catastrophe-as-Necessary-Anchor Reading of the Competence Exercise Requirement").
narrative_ontology:topic_domain(competence_exercise_requirement__catastrophe_as_necessary_anchor, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(competence_exercise_requirement__catastrophe_as_necessary_anchor).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__catastrophe_as_necessary_anchor, '132597bf-0ae0-4c60-a8eb-8e78b7f3e7e9').
narrative_ontology:cs_kernel_codification('132597bf-0ae0-4c60-a8eb-8e78b7f3e7e9', distributed).
narrative_ontology:cs_authority_grounding('132597bf-0ae0-4c60-a8eb-8e78b7f3e7e9', practice).
narrative_ontology:cs_interpretation_layer_present('132597bf-0ae0-4c60-a8eb-8e78b7f3e7e9').
narrative_ontology:cs_reading_relation('132597bf-0ae0-4c60-a8eb-8e78b7f3e7e9', competence_exercise_requirement__simulation_as_adequate_exercise, coexists_with).
narrative_ontology:cs_reading_relation('132597bf-0ae0-4c60-a8eb-8e78b7f3e7e9', competence_exercise_requirement__hybrid_dependency, influences).
narrative_ontology:cs_axiom('132597bf-0ae0-4c60-a8eb-8e78b7f3e7e9', foundational, real_stakes_are_structurally_irreplaceable).
narrative_ontology:cs_axiom_status(real_stakes_are_structurally_irreplaceable, holdable).
narrative_ontology:cs_axiom_grounding('132597bf-0ae0-4c60-a8eb-8e78b7f3e7e9', real_stakes_are_structurally_irreplaceable, empirically_contingent).
narrative_ontology:cs_axiom('132597bf-0ae0-4c60-a8eb-8e78b7f3e7e9', secondary, simulation_fidelity_has_a_hard_ceiling).
narrative_ontology:cs_axiom_status(simulation_fidelity_has_a_hard_ceiling, holdable).
narrative_ontology:cs_axiom_grounding('132597bf-0ae0-4c60-a8eb-8e78b7f3e7e9', simulation_fidelity_has_a_hard_ceiling, empirically_contingent).
narrative_ontology:cs_reference_frame('132597bf-0ae0-4c60-a8eb-8e78b7f3e7e9', post_disaster_reformed_training_doctrine).
narrative_ontology:cs_drift_state('132597bf-0ae0-4c60-a8eb-8e78b7f3e7e9', contemporary_high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('132597bf-0ae0-4c60-a8eb-8e78b7f3e7e9', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, incident_investigation_bodies).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, veteran_operators_with_catastrophe_experience).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, safety_regulators_citing_precedent).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, frontline_operators_without_catastrophe_exposure).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, organizations_between_disasters).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, public_bearing_residual_risk).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__catastrophe_as_necessary_anchor, muscle_memory_requires_real_stakes).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__catastrophe_as_necessary_anchor, simulation_fidelity_ceiling_exists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Derive their institutional mandate and authority from post-catastrophe investigation. Their expertise and continued relevance are anchored in the occurrence and analysis of real events; a world with zero catastrophes for decades would shrink their evidentiary base and their claim to be the arbiters of what 'real' competence looks like.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, incident_investigation_bodies, beneficiary,
    institutional, generational, analytical, national).

% Have lived through a real near-miss or disaster and carry the resulting credibility and often formal seniority ('has seen real fire') that this reading of the kernel legitimizes. They gain relative status when the story holds that only catastrophe-exposure confers real competence, since simulation-only peers cannot claim the same standing.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, veteran_operators_with_catastrophe_experience, beneficiary,
    moderate, biographical, constrained, national).

% Write and enforce training and certification requirements, frequently justified by citation of past catastrophes ('never again'). They administer requirements built on the premise that real-event exposure or its closest proxies are indispensable, and could revise this premise but have institutional and legal-liability reasons not to.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, safety_regulators_citing_precedent, agenda_setter,
    institutional, generational, analytical, national).

% Perform their duties competently by every measurable simulation and audit standard but are structurally denied the status of 'proven' competence because they have never faced a real catastrophic event. Some are held back from advancement, second-guessed after near-misses handled well, or made to feel their skill is unverified until tested by disaster — a test they cannot ethically seek out.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, frontline_operators_without_catastrophe_exposure, payer,
    powerless, biographical, trapped, local).

% Operate long catastrophe-free stretches during which, under this reading, competence is asserted to quietly decay no matter how much simulation investment is made. They pay for continuous simulation programs that this reading treats as necessary-but-insufficient, while having no legitimate way to manufacture the 'real' anchor the reading says is required, leaving them structurally unable to ever fully satisfy the standard.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, organizations_between_disasters, payer,
    moderate, generational, constrained, national).

% Ride on the trains, fly the planes, live near the plants operated by these organizations. If the catastrophe-as-anchor reading is correct, the public periodically bears the cost of the very disasters/near-misses that are held to be the sole legitimate teacher — an uncomfortable implication this reading must own rather than obscure.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, public_bearing_residual_risk, payer,
    powerless, generational, trapped, national).

% Design and validate high-fidelity simulation and debriefing protocols and would argue their instruments already capture the decision-load and stress dynamics this reading claims only catastrophe can teach. Their evidence rarely enters the room where certification standards citing 'nothing like the real thing' are set.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, excluded_simulation_researchers, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__catastrophe_as_necessary_anchor, diffuse).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__catastrophe_as_necessary_anchor, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates organizational attention and resource allocation around the genuine problem that skills atrophy without exercise, and that some forms of decision-load, stress, and consequence-weight may not be reproducible in any simulated environment — directing post-incident learning into standards that shape training and certification going forward.
% TRANSFER_FUNCTION: Transfers status, promotion eligibility, and the presumption of 'proven' competence toward those who have survived or handled a real catastrophic event, and away from those whose competence rests solely on simulation performance — while transferring the cost of maintaining this standard (unresolvable readiness anxiety, blocked advancement, and eventually the human and material cost of the anchoring events themselves) onto frontline operators and the public.
% ABSENT_VOICES: Simulation researchers and instructional-design specialists who can point to controlled evidence of transfer from high-fidelity simulation to real performance are structurally outside the standard-setting conversation, which is dominated by investigators and survivors of real events; their absence keeps the 'nothing substitutes for the real thing' premise from being empirically stress-tested against the counter-evidence they hold.
% DISAPPEARANCE_RATIONALE: If this specific reading (catastrophe-as-necessary-anchor) were abandoned, veteran operators would lose a distinguishing status marker, investigation bodies would need to justify their authority on other grounds, and certification regimes would need new legitimating language — but day-to-day operational safety practice (simulation, audits, checklists) would likely continue largely unchanged, since much of it does not actually depend on this specific premise. Whether the world 'rearranges' or 'stays the same' is itself the contested question between this reading and its siblings.
% FOUNDING_PROBLEM: Early high-reliability domains (aviation, nuclear, maritime) discovered that operators who had only trained in idealized conditions sometimes froze, panicked, or applied wrong procedures when a real catastrophic event's full sensory, temporal, and consequence load hit — training regimes built purely on paper knowledge or low-fidelity drills failed to produce reliable real-world performance.
% FOUNDING_PROBLEM_CORROBORATION: Incident investigation bodies and veteran operators attest the problem remains live, citing specific cases where simulation-trained crews underperformed under real catastrophic stress. Independent training-science researchers (outside the beneficiary set) attest that the gap has narrowed substantially with modern high-fidelity, physiologically-loaded simulation, and that the residual gap this reading points to may be better explained by selection and survivorship bias in which incidents get studied, rather than a structural ceiling on simulation itself.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__catastrophe_as_necessary_anchor, contested).
narrative_ontology:founding_problem_status(competence_exercise_requirement__catastrophe_as_necessary_anchor, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate-high (0.58) and rising because the reading's institutional expression (in accreditation criteria, promotion committees, post-incident status hierarchies) has hardened over time even as more simulation investment fails to fully satisfy it. Theater ratio is substantial and rising (0.55) because much visible 'competence verification' activity under this reading — commemorations of past incidents, veteran-led war-story training modules, ceremonial deference to catastrophe survivors — functions more as status theater than as a measurable competence intervention. Suppression is moderate (0.42): the reading is not coercively enforced in the way a snare would be, but it does structurally block advancement and second-guess simulation-only-trained operators, which counts as real (if soft) suppression of alternative competence narratives.
 *
 * DIRECTIONALITY LOGIC:
 *   Investigation bodies, veteran catastrophe-survivors, and regulators citing precedent sit near the beneficiary end: their authority, status, and institutional relevance are affirmed by this reading being true. Frontline operators without catastrophe exposure, organizations stuck in long safe intervals, and the public who eventually bear any 'anchoring' catastrophe sit near the target end: they pay in blocked status, unsatisfiable readiness anxiety, and ultimately physical risk. The trapped exit options for frontline operators and the public reflect that neither group can opt out of the standard's operation or manufacture compliant real-world exposure on demand.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled rope (rather than snare) preserves the genuine coordination function this reading points to — real physiological and decision-load stress may plausibly not be fully reproducible in simulation, and organizations legitimately do observe skill decay after long safe intervals. Calling it a pure snare would deny that the underlying skill-decay concern is real; calling it a pure rope would ignore that the reading, as institutionally operationalized, extracts status and blocks advancement from operators who did nothing wrong and simply have not been through a disaster. The tangled classification keeps both facts on the table.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    muscle_memory_vs_declarative_knowledge_ambiguity,
    'Is the ''irreducible exercise'' this reading claims only catastrophe provides actually a distinct competence dimension (procedural/embodied ''muscle memory'' under real stakes) that current simulation genuinely cannot reproduce, or is it a post-hoc narrative imposed on outcomes that were actually driven by selection bias in which incidents get studied and mythologized?',
    'Controlled comparison of operator performance metrics (response latency, procedural accuracy, physiological stress markers) across matched real-incident and high-fidelity-simulation cohorts, adjusting for publication/attention bias toward dramatic real events.',
    'If the muscle-memory gap is empirically real and simulation-resistant, this reading''s coordination function is substantially vindicated even though its extraction on non-exposed operators remains a separate problem to fix. If the gap is mostly narrative, the reading collapses toward snare, since its coordination story would be largely cover for status hierarchy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(muscle_memory_vs_declarative_knowledge_ambiguity, empirical, 'Whether the claimed simulation-fidelity ceiling is a real competence dimension or a survivorship narrative.').

omega_variable(
    catastrophe_as_teacher_moral_hazard,
    'Does treating real catastrophes/near-misses as necessary and irreducible teaching events create any institutional disincentive to minimize their frequency or severity, since the reading''s own coordination story depends on their periodic occurrence?',
    'Trace whether organizations operationalizing this reading show measurably reduced investment in catastrophe-prevention relative to organizations operating under the hybrid or simulation-adequacy readings, controlling for domain and regulatory regime.',
    'Evidence of such a disincentive would sharpen the tangled-rope classification toward snare, since it would show the reading''s persistence is partly maintained by a perverse institutional interest in the very events it claims only to learn from, not merely by accurately describing a training limitation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(catastrophe_as_teacher_moral_hazard, conceptual, 'Whether the anchor-necessity narrative creates institutional interest in catastrophe occurrence.').

omega_variable(
    kernel_reading_framing_alternative,
    'This story treats ''catastrophe-as-necessary-anchor'' as the operative reading actually embedded in the certification and status structures described. An alternative framing would treat the operative kernel as ''hybrid_dependency'' already in practice, with ''catastrophe-as-necessary-anchor'' surviving only as informal veteran-culture rhetoric layered on top of a formally hybrid standard.',
    'Textual analysis of actual certification requirements versus informal promotion/status practice: does the formal standard require real-event exposure, or only simulation-plus-periodic-real-operations (hybrid), with catastrophe-veneration existing only informally?',
    'If the formal standard is actually hybrid and only informal culture asserts catastrophe-necessity, this story''s ε and victim set should be read as describing the informal status economy layered on a hybrid formal kernel, not the formal requirement itself — this would not change this story''s authored values but would sharpen the boundary between this reading and the hybrid_dependency sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing_alternative, conceptual, 'Alternative framing: is catastrophe-necessity the formal kernel or informal culture riding a formally hybrid standard?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0, 0.3).
narrative_ontology:measurement(comp_tr_t8, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 8, 0.38).
narrative_ontology:measurement(comp_tr_t16, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 16, 0.45).
narrative_ontology:measurement(comp_tr_t24, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 24, 0.48).
narrative_ontology:measurement(comp_tr_t32, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 32, 0.52).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(comp_be_t8, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(comp_be_t16, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(comp_be_t24, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(comp_be_t32, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(comp_be_t40, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(comp_su_t8, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(comp_su_t16, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(comp_su_t24, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 24, 0.38).
narrative_ontology:measurement(comp_su_t32, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 32, 0.4).
narrative_ontology:measurement(comp_su_t40, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__catastrophe_as_necessary_anchor, identity_coordination).
narrative_ontology:boltzmann_floor_override(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.1).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, simulation_as_adequate_exercise).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, hybrid_dependency).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the competence_exercise_requirement kernel. simulation_as_adequate_exercise authors a much lower ε (simulation with debriefing treated as sufficient, closer to rope). hybrid_dependency authors an intermediate ε (both simulation and non-jeopardy real-world anchoring required, closer to scaffold or rope depending on sunset conditions on the real-anchor requirement). All three share the same underlying kernel — what maintains operational competence — but diverge on whether real catastrophic exposure is necessary, sufficient-when-simulated, or jointly required. Per the ε-invariance principle, these are three separate constraint stories, not one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
