% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__hybrid_reading, []).

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
 *   constraint_id: preparedness_commitment__hybrid_reading
 *   human_readable: Preparedness as Layered Commitment System (Hybrid Reading)
 *   domain: institutional/disaster_memory
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid reading of the
 *   preparedness_commitment kernel: the claim that disaster preparedness is a
 *   layered system in which memorial elements (commemorations, static plans,
 *   archival routines) stabilize long-term political and budgetary
 *   commitment, while competence elements (live exercises, audited response
 *   capacity) maintain actual operational function. The tension between these
 *   layers generates a persistent maintenance cost, extracting attention and
 *   resources from frontline operators. The constraint is claimed as a
 *   tangled rope because it carries a genuine coordination function
 *   (catastrophic risk reduction) alongside asymmetric extraction (the
 *   memorial burden on operators and the public). The metrics and claim are
 *   authored independently: the claim asserts the hybrid structure is
 *   structurally real, while the metrics describe an actively enforced,
 *   moderately extractive arrangement with meaningful performative overhead.
 *
 * KEY AGENTS:
 *   - preparedness_institutions: agenda_setter (institutional/constrained) â designs and enforces the dual-layer system
 *   - frontline_operators: primary payer (organized/constrained) â bears the operational and ceremonial maintenance burden
 *   - disaster_exposed_populations: dual-position beneficiary/payer (powerless/trapped) â receives risk reduction while funding and participating in the regime
 *   - political_authorities: secondary beneficiary (powerful/mobile) â collects legitimacy without operational exposure
 *   - independent_researchers: analytical observer (analytical/analytical) â documents drift between memorial and competence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__hybrid_reading, 0.58).
domain_priors:suppression_score(preparedness_commitment__hybrid_reading, 0.6).
domain_priors:theater_ratio(preparedness_commitment__hybrid_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_commitment__hybrid_reading, "Preparedness as Layered Commitment System (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_commitment__hybrid_reading, "institutional/disaster_memory").

domain_priors:requires_active_enforcement(preparedness_commitment__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__hybrid_reading, '2ea578de-895a-47ec-8d23-bd0e474e3d3a').
narrative_ontology:cs_kernel_codification('2ea578de-895a-47ec-8d23-bd0e474e3d3a', distributed).
narrative_ontology:cs_authority_grounding('2ea578de-895a-47ec-8d23-bd0e474e3d3a', practice).
narrative_ontology:cs_interpretation_layer_present('2ea578de-895a-47ec-8d23-bd0e474e3d3a').
narrative_ontology:cs_reading_relation('2ea578de-895a-47ec-8d23-bd0e474e3d3a', preparedness_commitment__competence_reading, influences).
narrative_ontology:cs_reading_relation('2ea578de-895a-47ec-8d23-bd0e474e3d3a', preparedness_commitment__husk_reading, influences).
narrative_ontology:cs_axiom('2ea578de-895a-47ec-8d23-bd0e474e3d3a', foundational, hybrid_layer_mandate).
narrative_ontology:cs_axiom_status(hybrid_layer_mandate, holdable).
narrative_ontology:cs_axiom_grounding('2ea578de-895a-47ec-8d23-bd0e474e3d3a', hybrid_layer_mandate, instrumental).
narrative_ontology:cs_axiom('2ea578de-895a-47ec-8d23-bd0e474e3d3a', foundational, memorial_as_commitment_anchor).
narrative_ontology:cs_axiom_status(memorial_as_commitment_anchor, holdable).
narrative_ontology:cs_axiom_grounding('2ea578de-895a-47ec-8d23-bd0e474e3d3a', memorial_as_commitment_anchor, empirically_contingent).
narrative_ontology:cs_reference_frame('2ea578de-895a-47ec-8d23-bd0e474e3d3a', dual_layer_equilibrium).
narrative_ontology:cs_drift_state('2ea578de-895a-47ec-8d23-bd0e474e3d3a', inter_disaster_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2ea578de-895a-47ec-8d23-bd0e474e3d3a', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__hybrid_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, disaster_exposed_populations).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, political_authorities).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, preparedness_institutions).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, frontline_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, disaster_exposed_populations).
narrative_ontology:constraint_vindicates(preparedness_commitment__hybrid_reading, institutional_memory_theory).
narrative_ontology:constraint_vindicates(preparedness_commitment__hybrid_reading, layered_resilience_model).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the dual-layer preparedness system by writing emergency plans, conducting drills, maintaining memorials and commemorative infrastructure, and allocating budgets between live exercises and archival routines. Their organizational survival and continued funding depend on the perceived legitimacy of the layered commitment.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, preparedness_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Maintain actual response competence while also executing mandated memorial drills, documentation, and commemorative activities. Experience the tension between functional readiness and symbolic compliance as operational time and attention are diverted to ceremonial maintenance.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, frontline_operators, payer,
    organized, biographical, constrained, regional).

% Reside in hazard zones and depend on institutional response capacity for survival. Fund the system through taxation and participate in mandated drills and commemorations. Cannot easily opt out of the institutional preparedness regime or relocate from exposure.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, disaster_exposed_populations, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__hybrid_reading, disaster_exposed_populations, payer).

% Fund and publicly champion preparedness initiatives, gaining legitimacy from visible memorial events and reassurance narratives. Shift priorities between electoral cycles without bearing personal frontline operational burdens.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, political_authorities, beneficiary,
    powerful, biographical, mobile, national).

% Document the gap between memorial performance and exercised competence across jurisdictions. Provide external evaluation of institutional memory decay without operational stake in the preparedness system.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, independent_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__hybrid_reading, preparedness_institutions).
narrative_ontology:fixing_cost_class(preparedness_commitment__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains societal capacity to respond to large-scale disasters across generational time by preserving both exercised operational skill and the political commitment to fund and staff that skill against natural institutional amnesia.
% TRANSFER_FUNCTION: Moves labor, attention, and material resources from frontline operators and exposed populations into the maintenance of dual-layer preparedness infrastructure; moves budgetary continuity and political legitimacy to preparedness institutions and political authorities.
% ABSENT_VOICES: Future generations who will inherit the current infrastructure but have no seat in today's prioritization; alternatively prepared communities that rely on informal rather than institutional resilience and are treated as non-compliant.
% DISAPPEARANCE_RATIONALE: If the layered commitment system vanished, political attention to preparedness would collapse within electoral cycles, operational competence would decay as exercises ceased, and institutional memory would revert to ad-hoc oral tradition; disaster mortality and damage distributions would shift measurably.
% FOUNDING_PROBLEM: Catastrophic disasters recur on timescales longer than political and institutional memory, causing societies to repeatedly forget lessons, defund response capacity, and suffer preventable losses.
% FOUNDING_PROBLEM_CORROBORATION: Historians of disaster and civil-defense scholars from outside the preparedness funding stream attest to recurrent cycles of institutional amnesia after major events; independent post-disaster inquiries repeatedly identify memory loss as a causal factor.
narrative_ontology:disappearance_verdict(preparedness_commitment__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_commitment__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__hybrid_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_commitment__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_commitment__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the substantial but not total diversion of operational resources into memorial maintenance. Suppression (0.60) captures the active enforcement required to prevent institutional abandonment: drills are mandated, plans must be updated, commemorations are scheduled. Theater ratio (0.35) acknowledges that the memorial layer is partly performative but not empty. Accessibility collapse (0.50) is moderate because informal resilience and alternative response models exist but are marginalized by institutional dominance. Resistance (0.40) reflects frontline pushback against ceremonial burdens and political cycles that defund competence between disasters. The temporal series run on a single shared grid to prevent misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (preparedness institutions) experiences the constraint as necessary institutional architecture balancing memory and skill. The payer seat (frontline operators) experiences it as a steadily accumulating bureaucratic and ceremonial burden that competes with operational readiness. The beneficiary seat (exposed populations) experiences it as background safety infrastructure with occasional intrusive demands. The engine will compute these seats differently: low directionality for institutions and political authorities, high for frontline operators, near-symmetric for exposed populations.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (disaster_exposed_populations, political_authorities, preparedness_institutions) are positioned toward the low-d end because they receive safety, legitimacy, or budgetary flow from the constraint. Victims (frontline_operators) are high-d because their labor is the resource extracted to maintain the dual system. Exposed populations have mixed positionality: they benefit from coordination but are trapped in the hazard zone and tax base, producing a moderate d. No overrides are needed: the structural derivation captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemârecurrent institutional amnesia about disastersâremains live, which prevents piton misclassification: the constraint is not merely inertial. However, the memorial layer risks mandatrophy if it decouples from competence and becomes pure performance (the husk reading). The hybrid reading guards against this by insisting on competence maintenance. The metrics do not show extreme theater (0.35) or collapse into pure performance, supporting the tangled rope classification over piton or snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    memorial_competence_tension,
    'Does the memorial layer genuinely stabilize long-term commitment, or does it increasingly substitute performative memory for exercised competence?',
    'Comparative longitudinal study of jurisdictions with high versus low memorial emphasis, measuring actual response quality and budget stability over multi-decadal spans.',
    'If substitution dominates, the constraint drifts toward the husk reading; if stabilization dominates, the hybrid reading is vindicated as a sustainable tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memorial_competence_tension, empirical, 'Whether memorial elements sustain or displace operational competence').

omega_variable(
    committer_reading_boundary,
    'Can the hybrid reading maintain both layers indefinitely, or does institutional pressure eventually collapse it into either pure competence or pure husk?',
    'Historical case studies of preparedness systems across multi-decadal spans, tracing post-disaster reform cycles.',
    'Determines whether the hybrid reading is a stable equilibrium or a transient metastructure awaiting decomposition into one of its sibling readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_reading_boundary, conceptual, 'Stability of the hybrid reading against sibling attractors').

omega_variable(
    maintenance_cost_asymmetry,
    'Is the maintenance cost borne by frontline operators offset by proportional safety gains, or does the cost structure indicate extraction?',
    'Cost-benefit analysis of memorial mandates against response outcomes and institutional budget flows.',
    'If costs exceed marginal safety gains, the coordination story is cover for institutional extraction; otherwise, the extraction is the necessary price of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maintenance_cost_asymmetry, empirical, 'Whether maintenance cost is extraction or necessary coordination overhead').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__hybrid_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(prep_tr_t5, preparedness_commitment__hybrid_reading, theater_ratio, 5, 0.23).
narrative_ontology:measurement(prep_tr_t10, preparedness_commitment__hybrid_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(prep_tr_t15, preparedness_commitment__hybrid_reading, theater_ratio, 15, 0.29).
narrative_ontology:measurement(prep_tr_t20, preparedness_commitment__hybrid_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement(prep_tr_t25, preparedness_commitment__hybrid_reading, theater_ratio, 25, 0.33).
narrative_ontology:measurement(prep_tr_t30, preparedness_commitment__hybrid_reading, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__hybrid_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(prep_be_t5, preparedness_commitment__hybrid_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(prep_be_t10, preparedness_commitment__hybrid_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(prep_be_t15, preparedness_commitment__hybrid_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement(prep_be_t20, preparedness_commitment__hybrid_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(prep_be_t25, preparedness_commitment__hybrid_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(prep_be_t30, preparedness_commitment__hybrid_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_commitment__hybrid_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(prep_su_t5, preparedness_commitment__hybrid_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(prep_su_t10, preparedness_commitment__hybrid_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(prep_su_t15, preparedness_commitment__hybrid_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(prep_su_t20, preparedness_commitment__hybrid_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(prep_su_t25, preparedness_commitment__hybrid_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(prep_su_t30, preparedness_commitment__hybrid_reading, suppression_requirement, 30, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, husk_reading).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, competence_reading).

% DUAL FORMULATION NOTE:
% The preparedness_commitment kernel decomposes into three structurally distinct constraints: the hybrid reading (both layers functioning, tangled rope), the competence reading (live knowledge only, rope-like), and the husk reading (memorial without competence, snare or piton). Each carries its own epsilon, stakeholders, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
