% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__autonomy_reading, []).

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
 *   constraint_id: end_of_life_authority__autonomy_reading
 *   human_readable: Medical Aid in Dying — Autonomy-Grounded Reading
 *   domain: medical/bioethics/legal
 *
 * SUMMARY:
 *   This story instantiates the autonomy-grounded reading of the end-of-life
 *   authority kernel: individual autonomy is treated as the legitimating
 *   ground for controlling the timing and circumstances of one's own death
 *   when facing unbearable suffering. Under this reading, the coordination
 *   function is real — a lawful, supervised alternative to unsupervised
 *   self-harm — but the extraction is structural: eligibility criteria drawn
 *   narrowly at any given moment necessarily exclude some patients whose
 *   suffering is equally severe, and those patients become a victim class of
 *   the very framework built to honor autonomy. This is a distinct constraint
 *   from the sanctity_reading (which denies the autonomy premise altogether)
 *   and the slippery_slope_mechanism (which is an empirical claim about
 *   eligibility drift over time, not a normative grounding). Each is authored
 *   as its own file with its own epsilon; this file does not average or hedge
 *   across them.
 *
 * KEY AGENTS:
 *   - terminally_ill_patients_seeking_control: primary beneficiary (powerless/trapped) — the autonomy claim is exercised on their behalf when eligible
 *   - suffering_prolonged_patients_denied_access: primary payer (powerless/trapped) — bear the cost of eligibility lines drawn elsewhere
 *   - attending_physicians: agenda_setter administering eligibility, bearing legal and professional exposure
 *   - legislatures_and_courts: institutional agenda_setter defining and revising eligibility criteria over time
 *   - disability_rights_advocates: excluded voice arguing autonomy framing can mask care-access coercion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__autonomy_reading, 0.42).
domain_priors:suppression_score(end_of_life_authority__autonomy_reading, 0.71).
domain_priors:theater_ratio(end_of_life_authority__autonomy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__autonomy_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__autonomy_reading, "Medical Aid in Dying — Autonomy-Grounded Reading").
narrative_ontology:topic_domain(end_of_life_authority__autonomy_reading, "medical/bioethics/legal").

domain_priors:requires_active_enforcement(end_of_life_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__autonomy_reading, 'b0ba428f-c39a-4a2c-a1ac-8ce5d666bf9c').
narrative_ontology:cs_kernel_codification('b0ba428f-c39a-4a2c-a1ac-8ce5d666bf9c', distributed).
narrative_ontology:cs_authority_grounding('b0ba428f-c39a-4a2c-a1ac-8ce5d666bf9c', distributed).
narrative_ontology:cs_reading_relation('b0ba428f-c39a-4a2c-a1ac-8ce5d666bf9c', end_of_life_authority__sanctity_reading, forecloses).
narrative_ontology:cs_reading_relation('b0ba428f-c39a-4a2c-a1ac-8ce5d666bf9c', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('b0ba428f-c39a-4a2c-a1ac-8ce5d666bf9c', foundational, bodily_autonomy_extends_to_death_timing).
narrative_ontology:cs_axiom_status(bodily_autonomy_extends_to_death_timing, holdable).
narrative_ontology:cs_axiom_grounding('b0ba428f-c39a-4a2c-a1ac-8ce5d666bf9c', bodily_autonomy_extends_to_death_timing, deontological).
narrative_ontology:cs_axiom('b0ba428f-c39a-4a2c-a1ac-8ce5d666bf9c', secondary, unbearable_suffering_overrides_default_prohibition).
narrative_ontology:cs_axiom_status(unbearable_suffering_overrides_default_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('b0ba428f-c39a-4a2c-a1ac-8ce5d666bf9c', unbearable_suffering_overrides_default_prohibition, instrumental).
narrative_ontology:cs_reference_frame('b0ba428f-c39a-4a2c-a1ac-8ce5d666bf9c', criminalized_assisted_death_baseline).
narrative_ontology:cs_drift_state('b0ba428f-c39a-4a2c-a1ac-8ce5d666bf9c', contemporary_legalized_jurisdictions, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('b0ba428f-c39a-4a2c-a1ac-8ce5d666bf9c', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__autonomy_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, terminally_ill_patients_seeking_control).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, families_relieved_of_prolonged_dying_burden).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, suffering_prolonged_patients_denied_access).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, patients_in_restrictive_jurisdictions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, attending_physicians).
narrative_ontology:constraint_vindicates(end_of_life_authority__autonomy_reading, bodily_autonomy_extends_to_death_timing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Facing a diagnosed terminal condition with unbearable suffering, they seek legal authority to choose the timing and manner of their death with medical assistance. Where the framework is recognized, they gain access to a lawful, supervised process; where it is not, they face the trap of continued suffering, unsupervised self-harm, or costly travel to jurisdictions that permit it.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, terminally_ill_patients_seeking_control, beneficiary,
    powerless, immediate, trapped, regional).

% Meet the substantive criteria for unbearable suffering but are denied lawful assistance because they fall outside eligibility bounds (non-terminal, contested prognosis, jurisdictional restriction, or procedural delay). They bear the extraction directly: continued suffering imposed by a framework that claims to vindicate their autonomy but withholds the mechanism from them specifically.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, suffering_prolonged_patients_denied_access, payer,
    powerless, immediate, trapped, regional).

% Caregivers and relatives who would otherwise bear extended caregiving burden, financial strain, and the emotional toll of watching prolonged suffering. A lawful, dignified pathway reduces this burden when available to their family member.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, families_relieved_of_prolonged_dying_burden, beneficiary,
    moderate, biographical, constrained, regional).

% Administer eligibility assessments, certify terminal diagnosis and capacity, and perform or prescribe the lethal intervention within statutory bounds. They set the operative agenda of who qualifies, but personally bear legal exposure, conscience conflict, and professional risk if procedures are challenged.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, attending_physicians, agenda_setter,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__autonomy_reading, attending_physicians, payer).

% Define and periodically revise the eligibility criteria (terminal prognosis window, capacity standard, waiting periods, who may request). They hold the structural power to expand or restrict the framework and are the object of ongoing advocacy pressure from both expansion and restriction coalitions.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, legislatures_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% Argue that framing death-timing as pure autonomy obscures how disabled and chronically ill people are steered toward death options under cost pressure and inadequate care access, rather than genuinely free choice. Their objection is frequently raised in public comment and litigation but rarely shapes the operative eligibility text once a framework is codified.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, disability_rights_advocates, excluded,
    organized, generational, constrained, national).

% Provide the alternative pathway of comfort-focused care and observe how legalized assisted dying interacts with palliative care funding and referral patterns. Some report resource competition; others report complementary use. Their institutional position gives them visibility into whether autonomy claims are exercised freely or under care-access duress.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, palliative_and_hospice_providers, observer,
    organized, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__autonomy_reading, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_authority__autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a lawful, medically supervised pathway so that competent, terminally ill individuals experiencing unbearable suffering can exercise control over the timing and manner of their death, replacing unsupervised or violent self-directed alternatives with a regulated clinical process.
% TRANSFER_FUNCTION: Moves decisional authority over the timing of death from a default state of prohibition (where the state and medical profession retain exclusive control) to the individual patient, conditioned on meeting statutory eligibility; where eligibility is narrowly drawn, it also moves the burden of continued suffering onto those who fall just outside the criteria.
% ABSENT_VOICES: Disability rights advocates who argue autonomy framing can mask care-access coercion are heard in hearings but structurally outside the clinical-legal apparatus that sets eligibility; patients whose suffering is severe but non-terminal, or whose capacity is contested, are also absent from the room where their own eligibility is decided.
% DISAPPEARANCE_RATIONALE: If the autonomy-grounded legal authority disappeared overnight, patients currently eligible would lose lawful access to assisted dying, reverting to unsupervised methods, travel-based access in permissive jurisdictions, or continued suffering without recourse; physicians would lose the statutory shield that currently protects participating clinicians; the entire regulatory and eligibility-review apparatus built around it would become moot.
% FOUNDING_PROBLEM: Competent terminally ill patients facing unbearable, unrelievable suffering had no lawful mechanism to end that suffering on their own terms, forcing a choice between prolonged suffering, unsupervised and often violent self-harm, or reliance on clinicians acting outside the law.
% FOUNDING_PROBLEM_CORROBORATION: Patient advocacy groups and participating physicians attest the founding problem remains live and the framework directly addresses it. Independent bioethicists and disability rights organizations — outside the beneficiary set — attest that in several jurisdictions the founding problem has been reframed: eligibility has expanded from terminal-only to broader suffering categories, which they read as evidence the original problem definition is being renegotiated rather than simply solved.
narrative_ontology:disappearance_verdict(end_of_life_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__autonomy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__autonomy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(end_of_life_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__autonomy_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__autonomy_reading_tests).
:- end_tests(end_of_life_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) rather than high because the core mechanism — granting lawful control to eligible patients — is a genuine transfer of authority, not a rent extraction; the extraction that exists is borne specifically by the excluded-but-similarly-situated victim class, not by the general population. Suppression starts high (0.85) reflecting the historical criminalization and professional prohibition regimes that the autonomy framework had to overturn, and gradually eases (to 0.71) as legal recognition spreads and paternalistic restrictions lose force, though it remains substantial because eligibility gatekeeping is itself an active suppressive mechanism against those outside the line. Theater ratio is low and slowly rising, reflecting some accumulating procedural formalism (waiting periods, review boards) that exists alongside genuine function rather than replacing it.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of an eligible, terminally ill patient, the framework reads as a rope: coordination that serves them without coercion. From the seat of a similarly suffering but ineligible patient, the identical legal architecture reads as a tangled rope or worse — a structure that claims to vindicate autonomy while withholding its mechanism from them on criteria they did not choose. The engine computes these divergently from the same structural data; this story authors both seats rather than collapsing them into one verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Eligible patients and their families are coded as beneficiaries with low derived d — the constraint subsidizes their exercise of control. Patients denied access despite comparable suffering are coded as victims with high derived d — the same legal architecture that grants authority to the first group withholds it from the second, and that withholding is the extraction. Physicians and legislatures sit in agenda_setter roles with moderate d, since they administer and can revise the boundary but do not personally collect the constraint's benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — no lawful recourse for competent, terminally suffering patients — could become dead in a given jurisdiction once palliative care sufficiently addresses suffering, or could remain permanently live given the irreducibility of some suffering to palliative management. Classifying this as tangled_rope rather than pure rope or pure snare prevents two mislabeling errors: treating the entire framework as pure extraction (ignoring the genuine autonomy transfer to eligible patients) or treating it as costless pure coordination (ignoring that eligibility boundaries produce an identifiable victim class by construction, not accident).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_reading_kernel_position,
    'Is the autonomy-grounded reading the correct normative frame for end-of-life authority, or does it presuppose a premise (that death-timing control is a coherent extension of bodily autonomy) that the sanctity_reading denies at the root?',
    'No empirical resolution mechanism exists; this is a foundational normative dispute between the autonomy_reading and sanctity_reading siblings within the end_of_life_authority kernel. Resolution mechanism, if any, is political/legal settlement (legislative codification or constitutional adjudication), not evidence.',
    'If the sanctity_reading''s premise is adopted instead, this entire constraint dissolves — there is no autonomy-grounded right to vindicate, and the beneficiary/victim structure authored here does not exist under that framework.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_reading_kernel_position, preference, 'Foundational normative disagreement between autonomy_reading and sanctity_reading siblings.').

omega_variable(
    eligibility_criteria_drift_direction,
    'Does eligibility under the autonomy_reading tend to expand toward the slippery_slope_mechanism''s predicted trajectory (terminal-only to broader suffering categories), or does it stabilize once a mature legal equilibrium is reached?',
    'Longitudinal tracking of eligibility statute amendments across jurisdictions with mature (15+ year) frameworks; compare early-adopter jurisdictions against recent adopters to test whether drift is a universal empirical pattern or jurisdiction-specific.',
    'If criteria systematically expand, the victim set for THIS reading (suffering_prolonged_patients_denied_access) shrinks over time as the boundary moves outward, but the slippery_slope_mechanism sibling constraint''s own extraction profile would rise correspondingly — the two stories are coupled through the shared eligibility boundary even though each retains its own epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eligibility_criteria_drift_direction, empirical, 'Whether eligibility boundary drift is empirically confirmed and how it couples to the sibling slippery-slope constraint.').

omega_variable(
    coercion_vs_genuine_choice,
    'When patients with disabilities or inadequate care access choose assisted dying, is that an exercise of genuine autonomy or a choice structurally coerced by absent care alternatives?',
    'Comparative outcome studies correlating assisted-dying uptake with regional palliative/disability care funding levels; qualitative interview studies with patients and families examining stated reasons.',
    'If choice is substantially coerced by care scarcity in a meaningful subset of cases, the beneficiary group is smaller than authored and a portion of nominal beneficiaries are more accurately victims of an adjacent care-access constraint, not exercisers of autonomy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_genuine_choice, empirical, 'Whether autonomy claims are confounded by care-access coercion for disabled and low-resource patients.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__autonomy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__autonomy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(end__tr_t5, end_of_life_authority__autonomy_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(end__tr_t10, end_of_life_authority__autonomy_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(end__tr_t15, end_of_life_authority__autonomy_reading, theater_ratio, 15, 0.16).
narrative_ontology:measurement(end__tr_t20, end_of_life_authority__autonomy_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(end__tr_t25, end_of_life_authority__autonomy_reading, theater_ratio, 25, 0.2).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__autonomy_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(end__be_t5, end_of_life_authority__autonomy_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(end__be_t10, end_of_life_authority__autonomy_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(end__be_t15, end_of_life_authority__autonomy_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(end__be_t20, end_of_life_authority__autonomy_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(end__be_t25, end_of_life_authority__autonomy_reading, base_extractiveness, 25, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__autonomy_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(end__su_t5, end_of_life_authority__autonomy_reading, suppression_requirement, 5, 0.81).
narrative_ontology:measurement(end__su_t10, end_of_life_authority__autonomy_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(end__su_t15, end_of_life_authority__autonomy_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(end__su_t20, end_of_life_authority__autonomy_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(end__su_t25, end_of_life_authority__autonomy_reading, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__autonomy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(end_of_life_authority__autonomy_reading, 0.12).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the end_of_life_authority kernel. sanctity_reading denies the foundational premise this story asserts (bodily_autonomy_extends_to_death_timing) and is therefore linked via a foreclosing relationship — the two readings cannot coexist within one legal framework, though they coexist across different jurisdictions and political coalitions. slippery_slope_mechanism is an empirical claim about how autonomy-grounded frameworks evolve once codified (eligibility expansion over time); it is downstream of and influenced by this reading's operative choices about where eligibility lines are initially drawn, but does not foreclose or logically depend on this reading's normative validity — it could in principle be true or false independent of whether the autonomy premise is normatively correct.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
