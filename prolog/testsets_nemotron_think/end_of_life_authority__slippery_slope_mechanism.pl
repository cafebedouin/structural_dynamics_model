% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__slippery_slope_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__slippery_slope_mechanism, []).

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
 *   constraint_id: end_of_life_authority__slippery_slope_mechanism
 *   human_readable: End-of-Life Autonomy Framework Slippery Slope Mechanism
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   The slippery slope mechanism describes an empirically observed pattern:
 *   jurisdictions that legalize assisted dying for competent terminal
 *   patients (autonomy_reading's paradigm case) subsequently expand
 *   eligibility to incompetent patients (dementia, psychiatric conditions)
 *   and non-terminal chronic suffering. This constraint story models the
 *   mechanism itself as a structural dynamic — the autonomy-based framework,
 *   once instantiated, generates internal pressure to expand its scope. The
 *   initial coordination function (protecting competent terminal choice)
 *   becomes the vehicle for a broader extraction: decision-making authority
 *   over vulnerable populations who cannot consent, transferred to
 *   medical-legal institutions. The mechanism requires active enforcement
 *   through evolving legislation, judicial interpretation, and medical
 *   guideline revision. The claimed type is tangled_rope: genuine
 *   coordination for the initial population coexists with asymmetric
 *   extraction from populations added by drift, held in place by active
 *   institutional maintenance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, 0.72).
domain_priors:suppression_score(end_of_life_authority__slippery_slope_mechanism, 0.68).
domain_priors:theater_ratio(end_of_life_authority__slippery_slope_mechanism, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, extractiveness, 0.72).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__slippery_slope_mechanism, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__slippery_slope_mechanism, "End-of-Life Autonomy Framework Slippery Slope Mechanism").
narrative_ontology:topic_domain(end_of_life_authority__slippery_slope_mechanism, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__slippery_slope_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__slippery_slope_mechanism, '5725db84-22fc-45b6-bb24-200b5461c99a').
narrative_ontology:cs_kernel_codification('5725db84-22fc-45b6-bb24-200b5461c99a', formalized).
narrative_ontology:cs_authority_grounding('5725db84-22fc-45b6-bb24-200b5461c99a', lineage).
narrative_ontology:cs_interpretation_layer_present('5725db84-22fc-45b6-bb24-200b5461c99a').
narrative_ontology:cs_reading_relation('5725db84-22fc-45b6-bb24-200b5461c99a', end_of_life_authority__autonomy_reading, influences).
narrative_ontology:cs_reading_relation('5725db84-22fc-45b6-bb24-200b5461c99a', end_of_life_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_axiom('5725db84-22fc-45b6-bb24-200b5461c99a', foundational, autonomy_frameworks_structurally_expand).
narrative_ontology:cs_axiom_status(autonomy_frameworks_structurally_expand, holdable).
narrative_ontology:cs_axiom_grounding('5725db84-22fc-45b6-bb24-200b5461c99a', autonomy_frameworks_structurally_expand, empirically_contingent).
narrative_ontology:cs_axiom('5725db84-22fc-45b6-bb24-200b5461c99a', secondary, bright_line_prohibition_is_unstable).
narrative_ontology:cs_axiom_status(bright_line_prohibition_is_unstable, holdable).
narrative_ontology:cs_axiom_grounding('5725db84-22fc-45b6-bb24-200b5461c99a', bright_line_prohibition_is_unstable, empirically_contingent).
narrative_ontology:cs_reference_frame('5725db84-22fc-45b6-bb24-200b5461c99a', initial_autonomy_framework_competent_terminal_only).
narrative_ontology:cs_drift_state('5725db84-22fc-45b6-bb24-200b5461c99a', contemporary_expanded_eligibility, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5725db84-22fc-45b6-bb24-200b5461c99a', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, competent_terminal_patients).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, medical_institutions).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, incompetent_patients).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, non_terminal_chronic_patients).
narrative_ontology:constraint_vindicates(end_of_life_authority__slippery_slope_mechanism, autonomy_frameworks_expand_beyond_initial_scope).
narrative_ontology:constraint_vindicates(end_of_life_authority__slippery_slope_mechanism, eligibility_criteria_drift_is_structural).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Patients with terminal illness and decision-making capacity who seek legal access to assisted dying. They gain a regulated pathway to control timing and manner of death. Their exit from the constraint is not needed — they are the intended users. However, their access depends on medical certification and institutional approval, creating constrained exit if the system denies them.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, competent_terminal_patients, beneficiary,
    moderate, immediate, constrained, national).

% Patients with dementia, advanced psychiatric illness, or other conditions impairing decision-making capacity. As eligibility expands, they become subject to life-ending decisions made by proxies or review boards using 'best interests' or 'substituted judgment' standards. They cannot consent, cannot exit the jurisdiction's legal framework, and have no effective resistance. The constraint extracts their continued life as a decision variable for others.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, incompetent_patients, payer,
    powerless, biographical, trapped, national).

% Patients with severe chronic suffering (neurodegenerative, psychiatric, chronic pain) but without terminal prognosis. As criteria expand from 'terminal' to 'grievous and irremediable suffering,' they become eligible for assisted dying. Their exit options are constrained by systemic pressures: inadequate palliative care, disability support gaps, caregiver burden, and the normalization of the pathway. They bear the cost of a framework that offers death as a solution to suffering that could be alleviated.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, non_terminal_chronic_patients, payer,
    powerless, biographical, constrained, national).

% Medical colleges, hospitals, ethics committees, and professional bodies that design, administer, and expand the framework. They gain professional authority over life-ending decisions, control over eligibility interpretation, and institutional relevance in end-of-life governance. They benefit from the expansion (broader scope of practice, gatekeeping role) while administering the safeguards that become increasingly performative. They could change the constraint but face no incentive to restrict their own authority.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, medical_institutions, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__slippery_slope_mechanism, medical_institutions, beneficiary).

% Organizations representing disabled people who argue that expansion devalues disabled lives, creates pressure to choose death over costly support, and reflects ableist assumptions about quality of life. They are structurally excluded from the policy design process — their testimony is heard but not incorporated into eligibility criteria. They have analytical exit (they can analyze and oppose) but no structural position to block expansion. Their absence from the beneficiary/victim arrays is itself signal.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, disability_rights_advocates, excluded,
    organized, generational, analytical, national).

% Academic analysts who study the empirical pattern across jurisdictions, model the drift mechanisms, and debate the normative implications. They do not collect or pay under the constraint; they provide the analytical seat from which the slippery slope mechanism is identified and measured. Their exit is analytical — they can change frameworks of analysis but are not subject to the constraint's operation.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, bioethics_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legally regulated, medically supervised pathway for competent terminal patients to control the timing and manner of their death, replacing clandestine or traumatic self-harm with a transparent process involving safeguards, waiting periods, and professional oversight.
% TRANSFER_FUNCTION: Transfers decision-making authority over life-ending from a bright-line prohibition (no one may intentionally end another's life) to a expanding zone of permitted practice: first to competent terminal patients (self-administration), then to proxies for incompetent patients (euthanasia), then to patients with non-terminal chronic suffering. The transfer moves authority from a universal prohibition to institutionalized discretion, concentrating power in medical-legal gatekeepers.
% ABSENT_VOICES: Incompetent patients (by definition cannot speak for themselves in the process); future patients who will face expanded criteria; disability community members systematically excluded from legislative drafting committees; palliative care clinicians who warn of sufficiency gaps but are not voting members of oversight bodies; religious minorities whose sanctity objections are treated as overrideable preferences rather than structural concerns.
% DISAPPEARANCE_RATIONALE: If the slippery slope mechanism were structurally blocked (e.g., by constitutional entrenchment of terminal-only criteria, irreversible sunset clauses, or independent disability rights veto), the autonomy framework would remain restricted to its founding population. The world would rearrange: no expansion to incompetent/non-terminal populations; palliative care investment would face different political economy; disability rights would have stronger structural protection; the sanctity_reading would lose its primary empirical argument against autonomy frameworks.
% FOUNDING_PROBLEM: Competent terminal patients suffering unbearably with no legal recourse to control their death, forcing some into violent or isolated suicide and denying them the option of a peaceful, witnessed, supported dying process.
% FOUNDING_PROBLEM_CORROBORATION: The autonomy_reading proponents (e.g., Dying With Dignity organizations) attest the problem remains live — access is still uneven, safeguards still burdensome. Disability rights organizations (e.g., Not Dead Yet, Council of Canadians with Disabilities) and palliative care physicians attest the founding problem is substantially solved in jurisdictions with mature frameworks, and the arrangement now persists as expansion. Legislative histories in Oregon, Netherlands, Belgium, Canada show initial narrow laws followed by successive expansions — corroboration from outside the beneficiary set that the founding problem status is contested and the drift is structural.
narrative_ontology:disappearance_verdict(end_of_life_authority__slippery_slope_mechanism, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__slippery_slope_mechanism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__slippery_slope_mechanism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_authority__slippery_slope_mechanism, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__slippery_slope_mechanism, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__slippery_slope_mechanism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__slippery_slope_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) reflects the magnitude of authority transfer: from zero state-sanctioned life-ending to broad eligibility including non-terminal incompetent patients. Suppression (0.68) captures how expansion forecloses alternatives — palliative care investment, disability supports, and conscientious objection protections erode as the framework normalizes. Theater ratio (0.48) measures the growing gap between the autonomy justification and the actual operation: safeguards become procedural checkboxes, 'unbearable suffering' detaches from terminal prognosis. Accessibility collapse (0.55) is moderate — alternatives exist but are structurally undermined. Resistance (0.45) reflects organized disability opposition and some medical resistance, but insufficient to halt drift. The temporal series shows monotonic increase across all three metrics over 34 years, consistent with a ratchet mechanism.
 *
 * PERSPECTIVAL GAP:
 *   From the competent terminal patient seat, the constraint appears as rope (coordination delivering desired autonomy). From the incompetent patient seat, it appears as snare (extraction of the prohibition that protected them). From the medical institution seat, it appears as scaffold (transitional framework that legitimizes expanding professional authority). The engine computes these per-seat classifications from the structural data; the divergence is the measurement. The autonomy_reading sees only the initial coordination; the sanctity_reading sees only the extraction; the slippery_slope_mechanism reading sees the dynamic that links them.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent terminal patients are initial beneficiaries (d ~ 0.2): they gain legal access to desired control. Medical institutions are structural beneficiaries (d ~ 0.1): they gain expanded decision-making authority, professional discretion, and institutional relevance. Incompetent patients are full targets (d ~ 0.9): they lose the protection of a bright-line prohibition on life-ending without consent, becoming subject to proxy/substituted judgment standards that track institutional preferences. Non-terminal chronic patients are high targets (d ~ 0.75): they face expanded eligibility without the terminal diagnosis safeguard, with exit options constrained by systemic pressure (cost, caregiver burden, institutional pathway). Disability advocates are excluded (analytical exit, no structural position). Bioethics scholars are observers (analytical exit). The directionality derivation from beneficiary/victim declarations plus exit options matches the structural asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (competent terminal suffering without recourse) is contested as live vs. solved. If solved, the arrangement's persistence and expansion constitute mandatrophy — the mandate (autonomy for the dying) has outlived its function and now serves institutional expansion. The mechanism prevents mislabeling: without the slippery slope lens, the expansion looks like incremental progress (coordination); with it, the pattern reveals extraction layered onto coordination. The mandatrophy is not in the initial law but in the absence of structural brakes on drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the end_of_life_authority kernel, and does it instantiate a structurally separate constraint from autonomy_reading and sanctity_reading?',
    'Compare the ε values, victim/beneficiary structures, and temporal dynamics across the three readings. If each reading produces a different ε on the same referent (the standing arrangement of end-of-life authority), they are distinct constraints per ε-invariance.',
    'If confirmed as distinct, the three readings form a constraint family linked by network.affects_constraints. If not, the slippery slope is a measurement parameter on the autonomy constraint, not a separate constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the slippery slope mechanism is a kernel reading instantiating a separate constraint').

omega_variable(
    expansion_mechanism_ambiguity,
    'What drives the empirical expansion from competent-terminal to incompetent-non-terminal: institutional inertia, cost-containment pressure, ideological capture by sanctity frameworks, or genuine moral reasoning about suffering?',
    'Comparative policy history across jurisdictions: track whether expansion correlates with healthcare cost crises, disability rights movement strength, judicial vs. legislative pathways, or palliative care infrastructure.',
    'If cost-containment drives expansion, the constraint is extractive (snare-flavored). If genuine moral reasoning, it may be scaffold (transitional). If institutional inertia, piton. The mechanism determines the type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expansion_mechanism_ambiguity, empirical, 'Causal driver of eligibility criteria drift').

omega_variable(
    coordination_extraction_boundary,
    'Is the coordination function (competent terminal autonomy) structurally separable from the extraction function (incompetent/non-terminal inclusion), or does the autonomy framework inherently require the expansion to function?',
    'Jurisdictional natural experiments: compare regimes with strict terminal-only criteria (e.g., Oregon 1997-2019) vs. those that expanded (Netherlands, Belgium, Canada). If coordination persists without expansion, they are separable.',
    'If separable, the constraint is tangled_rope (coordination + extraction). If inseparable, the coordination story may be cover for extraction (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether coordination and extraction components are structurally separable').

omega_variable(
    sanctity_vehicle_claim,
    'Does the autonomy framework ''become a vehicle for sanctity concerns'' as claimed, or does it instead displace sanctity frameworks by normalizing life-ending?',
    'Track sanctity-based opposition intensity over time in expanding vs. non-expanding jurisdictions. Measure whether religious/institutional sanctity actors shift from opposing to managing the framework.',
    'If sanctity concerns are vehicle, the constraint serves a hidden normative agenda. If displaced, the constraint is a genuine autonomy expansion with sanctity as residual opposition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sanctity_vehicle_claim, conceptual, 'Whether sanctity concerns drive or are displaced by the expansion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__slippery_slope_mechanism, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eol_slope_tr_t1990, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(eol_slope_tr_t1997, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 1997, 0.15).
narrative_ontology:measurement(eol_slope_tr_t2002, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 2002, 0.25).
narrative_ontology:measurement(eol_slope_tr_t2009, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 2009, 0.35).
narrative_ontology:measurement(eol_slope_tr_t2016, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 2016, 0.42).
narrative_ontology:measurement(eol_slope_tr_t2024, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 2024, 0.48).

% Extraction over time
narrative_ontology:measurement(eol_slope_be_t1990, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 1990, 0.15).
narrative_ontology:measurement(eol_slope_be_t1997, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 1997, 0.25).
narrative_ontology:measurement(eol_slope_be_t2002, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 2002, 0.45).
narrative_ontology:measurement(eol_slope_be_t2009, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 2009, 0.55).
narrative_ontology:measurement(eol_slope_be_t2016, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 2016, 0.65).
narrative_ontology:measurement(eol_slope_be_t2024, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(eol_slope_su_t1990, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 1990, 0.3).
narrative_ontology:measurement(eol_slope_su_t1997, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 1997, 0.4).
narrative_ontology:measurement(eol_slope_su_t2002, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 2002, 0.55).
narrative_ontology:measurement(eol_slope_su_t2009, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 2009, 0.6).
narrative_ontology:measurement(eol_slope_su_t2016, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 2016, 0.65).
narrative_ontology:measurement(eol_slope_su_t2024, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__slippery_slope_mechanism, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(end_of_life_authority__slippery_slope_mechanism, 0.12).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority__sanctity_reading).

% DUAL FORMULATION NOTE:
% The end_of_life_authority kernel decomposes into three constraint stories. The autonomy_reading (coordination for competent terminal) is the upstream claim cited as justification. The slippery_slope_mechanism (this story) is the downstream empirical dynamic that the autonomy structure enables. The sanctity_reading is a normative position that predicts and opposes the expansion. The ε values differ: autonomy_reading ε ≈ 0.15 (initial law), slippery_slope ε ≈ 0.72 (expanded regime), sanctity_reading ε varies by implementation. They are linked: autonomy_reading → slippery_slope_mechanism (enables), sanctity_reading → slippery_slope_mechanism (predicts/opposes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(end_of_life_authority__slippery_slope_mechanism, institutional, 0.15).
constraint_indexing:directionality_override(end_of_life_authority__slippery_slope_mechanism, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
