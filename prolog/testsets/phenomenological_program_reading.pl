% ============================================================================
% CONSTRAINT STORY: phenomenological_program_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_phenomenological_program_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: phenomenological_program_reading
 *   human_readable: Agnostic Phenomenological-Program Reading of alpha_m Supercriticality
 *   domain: theoretical_physics/cosmology/speculative_astrophysics
 *
 * SUMMARY:
 *   The alpha_m supercriticality kernel is a single theoretical fact under
 *   contest: whether the value alpha_m ~ 34 implies a minimal field content
 *   that is or is not consistent at the level of nonperturbative bound
 *   states. This story is the phenomenological-program reading: it takes no
 *   position on the field-theory consistency question and instead treats the
 *   observational program (Psyche density constraints, helioseismic bounds,
 *   LRD demographic statistics) as valuable in its own right, on the grounds
 *   that falsifiability and constraint-generation do not require prior
 *   resolution of the UV question. This reading coexists with (does not
 *   resolve) the inconsistency reading, the nonperturbative-matter-sector
 *   reading, and the mirror-sector-alternative reading — each of those is a
 *   separate constraint story with its own epsilon and its own
 *   beneficiary/victim structure.
 *
 * KEY AGENTS:
 *   - observational_program_researchers: agenda_setter, sets and defends the agnostic research program
 *   - psyche_mission_teams, helioseismology_researchers, lrd_demographics_researchers: beneficiaries, gain publishable legitimacy independent of theory resolution
 *   - field_theory_completionists: payer, bears the cost of deferred attention to the consistency question
 *   - grant_and_funding_bodies: observer, allocates resources based on which reading is persuasive
 *   - physics_community_at_large: excluded, cares about the general-principle stakes but is not in the immediate room
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(phenomenological_program_reading, 0.28).
domain_priors:suppression_score(phenomenological_program_reading, 0.22).
domain_priors:theater_ratio(phenomenological_program_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(phenomenological_program_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(phenomenological_program_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(phenomenological_program_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(phenomenological_program_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(phenomenological_program_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(phenomenological_program_reading, rope).
narrative_ontology:human_readable(phenomenological_program_reading, "Agnostic Phenomenological-Program Reading of alpha_m Supercriticality").
narrative_ontology:topic_domain(phenomenological_program_reading, "theoretical_physics/cosmology/speculative_astrophysics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(phenomenological_program_reading, '1cf8e964-b01d-4b77-8c2f-c3159eff3245').
narrative_ontology:cs_kernel_codification('1cf8e964-b01d-4b77-8c2f-c3159eff3245', distributed).
narrative_ontology:cs_authority_grounding('1cf8e964-b01d-4b77-8c2f-c3159eff3245', expertise).
narrative_ontology:cs_interpretation_layer_present('1cf8e964-b01d-4b77-8c2f-c3159eff3245').
narrative_ontology:cs_reading_relation('1cf8e964-b01d-4b77-8c2f-c3159eff3245', phenomenological_program_reading__inconsistency_reading, coexists_with).
narrative_ontology:cs_reading_relation('1cf8e964-b01d-4b77-8c2f-c3159eff3245', phenomenological_program_reading__nonperturbative_matter_sector_reading, coexists_with).
narrative_ontology:cs_reading_relation('1cf8e964-b01d-4b77-8c2f-c3159eff3245', phenomenological_program_reading__mirror_sector_alternative_reading, coexists_with).
narrative_ontology:cs_axiom('1cf8e964-b01d-4b77-8c2f-c3159eff3245', foundational, empirical_value_independent_of_uv_resolution).
narrative_ontology:cs_axiom_status(empirical_value_independent_of_uv_resolution, holdable).
narrative_ontology:cs_axiom_grounding('1cf8e964-b01d-4b77-8c2f-c3159eff3245', empirical_value_independent_of_uv_resolution, instrumental).
narrative_ontology:cs_axiom('1cf8e964-b01d-4b77-8c2f-c3159eff3245', secondary, constraint_generation_is_the_scientific_deliverable).
narrative_ontology:cs_axiom_status(constraint_generation_is_the_scientific_deliverable, holdable).
narrative_ontology:cs_axiom_grounding('1cf8e964-b01d-4b77-8c2f-c3159eff3245', constraint_generation_is_the_scientific_deliverable, conventional).
narrative_ontology:cs_reference_frame('1cf8e964-b01d-4b77-8c2f-c3159eff3245', pre_consistency_verdict_baseline).
narrative_ontology:cs_drift_state('1cf8e964-b01d-4b77-8c2f-c3159eff3245', post_psyche_and_lrd_survey_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1cf8e964-b01d-4b77-8c2f-c3159eff3245', '').
narrative_ontology:cs_kernel_id(phenomenological_program_reading, alpha_m_supercriticality_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(phenomenological_program_reading, observational_program_researchers).
narrative_ontology:constraint_beneficiary(phenomenological_program_reading, psyche_mission_teams).
narrative_ontology:constraint_beneficiary(phenomenological_program_reading, helioseismology_researchers).
narrative_ontology:constraint_beneficiary(phenomenological_program_reading, lrd_demographics_researchers).
narrative_ontology:constraint_victim(phenomenological_program_reading, field_theory_completionists).
narrative_ontology:constraint_vindicates(phenomenological_program_reading, constraint_generation_as_scientific_value).
narrative_ontology:constraint_vindicates(phenomenological_program_reading, falsifiability_independent_of_uv_completion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and run the falsifiable observational program keyed to alpha_m ~ 34 — asteroid Psyche density anomalies, helioseismic constraints, little red dot (LRD) demographic statistics. They benefit from a reading that lets them proceed without waiting on the UV-completion question to be settled; their funding cases and publication output depend on the claim that constraint-generation itself is the deliverable.
narrative_ontology:constraint_stakeholder(phenomenological_program_reading, observational_program_researchers, agenda_setter,
    organized, biographical, mobile, global).

% Analyze mission data for signatures consistent with or excluding the supercritical minimal-content scenario. Their scientific output is legitimated by the agnostic reading: results constrain the parameter space regardless of whether the underlying field theory is later shown consistent or not.
narrative_ontology:constraint_stakeholder(phenomenological_program_reading, psyche_mission_teams, beneficiary,
    organized, biographical, mobile, global).

% Use solar oscillation data to bound the same parameter. They gain publishable, citable results under this reading without needing to adjudicate the bound-state consistency problem — the reading is what makes their null or positive results meaningful independent of theory resolution.
narrative_ontology:constraint_stakeholder(phenomenological_program_reading, helioseismology_researchers, beneficiary,
    organized, biographical, mobile, global).

% Study little red dot population statistics for signatures bearing on alpha_m. Like the other observational communities, their work retains scientific value under the agnostic framing even if the field-theoretic question is never closed.
narrative_ontology:constraint_stakeholder(phenomenological_program_reading, lrd_demographics_researchers, beneficiary,
    organized, generational, mobile, global).

% Theorists whose research program is to resolve whether the minimal content is consistent or inconsistent at the field-theory level — the bound-state problem itself. This reading explicitly defers their question rather than answering it, treating resolution as unnecessary for the program's worth. They bear the cost of reduced attention, funding, and urgency directed at closing the theoretical question, since the phenomenological reading declares that closure is not required for the program to be scientifically valuable.
narrative_ontology:constraint_stakeholder(phenomenological_program_reading, field_theory_completionists, payer,
    moderate, civilizational, constrained, global).

% Evaluate proposals under competing readings of the kernel. Whether they fund observational programs or theoretical consistency work depends partly on which reading of alpha_m supercriticality they find persuasive; the agnostic reading directly shapes their allocation calculus by declaring the observational path fundable independent of theoretical resolution.
narrative_ontology:constraint_stakeholder(phenomenological_program_reading, grant_and_funding_bodies, observer,
    institutional, generational, analytical, global).

% Would want a clear verdict on whether the minimal content is consistent, since that bears on broader theoretical commitments (unitarity, causality, effective field theory validity) beyond this specific application. They are not directly party to the observational program's funding and design decisions but are affected by whether the field accepts deferral as a stable long-term stance.
narrative_ontology:constraint_stakeholder(phenomenological_program_reading, physics_community_at_large, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(phenomenological_program_reading, observational_program_researchers).
narrative_ontology:fixing_cost_class(phenomenological_program_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows a heterogeneous set of observational communities (asteroid science, helioseismology, high-redshift demographics) to coordinate around a single falsifiable parameter (alpha_m ~ 34) and proceed with data collection and constraint-generation without waiting for theorists to resolve whether the minimal field content is UV-consistent.
% TRANSFER_FUNCTION: Shifts scientific attention, funding urgency, and legitimacy away from resolving the bound-state/field-theory consistency problem and toward observational constraint-generation; the theoretical completionists lose claim-priority on the 'this must be settled first' framing, while observational programs gain standing to proceed and publish independent of that resolution.
% ABSENT_VOICES: Field-theory completionists who hold that a phenomenological program built on a possibly-inconsistent field content is scientifically premature are present in the discourse but structurally deprioritized by this reading's framing; the broader physics community that cares about UV completion as a matter of general principle (not tied to this specific application) is largely absent from the immediate stakeholder conversation.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and the field reverted to requiring theoretical resolution before observational work proceeded, the Psyche/helioseismology/LRD programs would lose part of their epistemic warrant for near-term publication and funding, though the underlying data collection might continue under a different justificatory frame. Completionists would view this as restoring proper scientific order; observational researchers would view it as an unwarranted stall. Whether the world 'rearranges' depends on which community's account of scientific practice is taken as authoritative — hence contested rather than settled.
% FOUNDING_PROBLEM: The original problem was that the minimal-content field theory generating alpha_m ~ 34 had an unresolved question — is the bound-state sector consistent or not — that threatened to stall any use of the parameter for observational prediction, since researchers disagreed about whether it was legitimate to draw empirical consequences from a possibly-inconsistent theory.
% FOUNDING_PROBLEM_CORROBORATION: Observational researchers (a benefiting party) attest the founding problem is resolved in the sense that matters practically — falsifiability does not require UV consistency. Field-theory completionists, who are the losing party under this reading, attest the founding problem remains live and structurally unaddressed. No corroboration exists from a party outside both groups; there is no independent arbiter (e.g. a settled meta-theoretical consensus on whether phenomenology can proceed ahead of consistency proofs) that has weighed in from outside the interested communities.
narrative_ontology:disappearance_verdict(phenomenological_program_reading, contested).
narrative_ontology:founding_problem_status(phenomenological_program_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(phenomenological_program_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-11',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(phenomenological_program_reading, 'none', 1).
narrative_ontology:epsilon_provenance(phenomenological_program_reading, 0.28, 'claude-sonnet-5', 'dirac_magnetic_matter_2026_20260811_143746', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(phenomenological_program_reading_tests).
:- end_tests(phenomenological_program_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.28) and rising modestly: the reading itself does not coerce anyone, but as the observational program matures and captures more funding/attention share, it increasingly displaces resources that could go to the consistency-resolution program, which is a real if modest transfer. Suppression is low (0.22) because completionists are free to keep working on the bound-state problem; nothing blocks that research, it simply loses some agenda-setting priority. Theater ratio is low (0.18) because the constraint-generation activity is genuinely functional science, not performance. Accessibility collapse is moderate (0.35): the alternative of insisting on prior theoretical resolution remains available and is actively held by a real constituency, so alternatives have not collapsed. Resistance is moderate (0.42), reflecting the ongoing, articulate objection from completionists that deferral is evasion rather than discipline.
 *
 * PERSPECTIVAL GAP:
 *   From the observational program's seat, this reading is straightforwardly a rope: it coordinates otherwise-disconnected observational communities around a shared falsifiable target and lets genuine science proceed. From the completionist seat, the same reading looks like a quiet reallocation of legitimacy and resources away from the harder, unresolved question — not extraction in the classic rent-collecting sense, but a real opportunity cost imposed without their consent to the framing. The engine should register this asymmetry: no one is coerced, but the payer bears a diffuse, real cost in attention and funding priority.
 *
 * DIRECTIONALITY LOGIC:
 *   Observational researchers and mission/survey teams are declared beneficiaries because the reading's entire function is to legitimate their work as valuable independent of UV completion — this is a low-d, benefit-concentrating structural position. Field-theory completionists are declared victims not because anyone extracts money or labor from them directly, but because the reading structurally deprioritizes the question their careers are built on answering; their d sits toward the target end because the cost (lost priority, lost funding share) flows specifically to them as a group defined by their theoretical commitment. Grant bodies and the broader physics community are positioned as observer/excluded respectively because they are not the ones whose work-product depends on which reading wins, even though their downstream decisions matter.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (can we draw scientific value from alpha_m ~ 34 without resolving field-theory consistency first) has not disappeared, but its status is genuinely contested rather than settled or dead. This reading is not a zombie mandate — the observational programs it authorizes produce real, checkable, falsifiable predictions right now, which is a live function, not inertial theater. What keeps this from being classified as pure extraction is that the coordination function (letting heterogeneous observational communities proceed on a shared falsifiable target) is real and ongoing, not a cover story; what keeps it from being classified as a costless rope is that the deferral genuinely reallocates attention and resources away from a rival, equally legitimate research question with its own constituency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deferral_as_discipline_or_evasion,
    'Is the agnostic stance genuine scientific discipline (proceeding empirically while a hard theoretical question remains open, as with many historical precedents) or is it evasion that allows an unresolved and possibly fatal theoretical defect to be indefinitely deferred while resources continue to flow to the observational program?',
    'Track whether the observational program''s predictions are eventually confronted with a resolved field-theory verdict (from the inconsistency_reading or nonperturbative_matter_sector_reading camps) and whether that resolution, when it arrives, retroactively validates or undercuts the observational results obtained under agnosticism.',
    'If resolution eventually validates the phenomenological approach (observations were meaningful regardless of the theoretical fate), this reading is vindicated as genuine scientific discipline. If resolution shows the minimal content was inconsistent all along in a way that invalidates the observational predictions'' interpretation, the deferral will retrospectively look like resource-misallocation enabled by premature agnosticism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deferral_as_discipline_or_evasion, conceptual, 'Whether agnostic deferral of the field-theory question is disciplined science or motivated evasion.').

omega_variable(
    kernel_reading_disagreement_location,
    'This story is one reading (phenomenological_program_reading) of the alpha_m_supercriticality_kernel; the sibling readings (inconsistency_reading, nonperturbative_matter_sector_reading, mirror_sector_alternative_reading) locate the disagreement at a different point — namely, whether the minimal field content is consistent at all, and if not, what replaces or extends it. Where exactly does the deepest disagreement sit: is it a factual disagreement about a not-yet-computed nonperturbative property, or a disagreement about acceptable scientific method (whether empirical work can proceed ahead of theoretical closure)?',
    'A rigorous nonperturbative calculation (lattice-type or exact bound-state analysis) of the minimal content''s consistency would resolve the factual half. The methodological half (can phenomenology outrun theory) is resolved only by community consensus or by historical precedent-setting, not by any single calculation.',
    'If the disagreement is purely factual and gets resolved, all four readings converge onto whichever the calculation supports, and three of the four stories become historically superseded. If the disagreement is partly methodological, the phenomenological_program_reading could remain defensible even after the factual question is settled, because its central claim is about method, not outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Whether the kernel dispute is fundamentally empirical (a pending calculation) or methodological (whether phenomenology requires prior theoretical closure).').

omega_variable(
    opportunity_cost_magnitude,
    'How large, in real funding and attention terms, is the cost this reading imposes on field-theory completionists — is it a marginal, easily-absorbed reallocation, or a substantial redirection that meaningfully slows resolution of the consistency question?',
    'Comparative analysis of grant allocation and publication volume across the observational programs versus the theoretical consistency research program over the interval, controlled for overall field growth.',
    'A small effect supports classifying this reading as close to a pure rope (low-cost coordination benefit); a large, sustained effect supports treating the victim designation as substantively meaningful rather than nominal, pushing the classification toward tangled_rope territory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(opportunity_cost_magnitude, empirical, 'The real magnitude of the opportunity cost borne by the theoretical-completion research program.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(phenomenological_program_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(phen_tr_t0, phenomenological_program_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(phen_tr_t2, phenomenological_program_reading, theater_ratio, 2, 0.12).
narrative_ontology:measurement(phen_tr_t4, phenomenological_program_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(phen_tr_t6, phenomenological_program_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(phen_tr_t8, phenomenological_program_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(phen_tr_t10, phenomenological_program_reading, theater_ratio, 10, 0.18).

% Extraction over time
narrative_ontology:measurement(phen_be_t0, phenomenological_program_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(phen_be_t2, phenomenological_program_reading, base_extractiveness, 2, 0.18).
narrative_ontology:measurement(phen_be_t4, phenomenological_program_reading, base_extractiveness, 4, 0.22).
narrative_ontology:measurement(phen_be_t6, phenomenological_program_reading, base_extractiveness, 6, 0.24).
narrative_ontology:measurement(phen_be_t8, phenomenological_program_reading, base_extractiveness, 8, 0.26).
narrative_ontology:measurement(phen_be_t10, phenomenological_program_reading, base_extractiveness, 10, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(phenomenological_program_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(phenomenological_program_reading, information_standard).
narrative_ontology:boltzmann_floor_override(phenomenological_program_reading, 0.02).
narrative_ontology:affects_constraint(phenomenological_program_reading, inconsistency_reading).
narrative_ontology:affects_constraint(phenomenological_program_reading, nonperturbative_matter_sector_reading).
narrative_ontology:affects_constraint(phenomenological_program_reading, mirror_sector_alternative_reading).

% DUAL FORMULATION NOTE:
% This story is one of four members of the alpha_m_supercriticality_kernel constraint family. inconsistency_reading holds the minimal content is field-theoretically inconsistent; nonperturbative_matter_sector_reading holds consistency depends on an unspecified nonperturbative extension; mirror_sector_alternative_reading proposes a distinct mirror-sector explanation for the same phenomenology. This reading (phenomenological_program_reading) is agnostic among all three and locates its value in falsifiable constraint-generation rather than in adjudicating them. All four stories share the same underlying kernel fact (the status of alpha_m ~ 34) but author distinct epsilon values, beneficiary/victim structures, and claimed types because they answer structurally different questions about what that fact licenses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
