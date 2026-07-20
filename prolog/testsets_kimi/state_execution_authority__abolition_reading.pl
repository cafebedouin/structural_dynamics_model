% ============================================================================
% CONSTRAINT STORY: state_execution_authority__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__abolition_reading, []).

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
 *   constraint_id: state_execution_authority__abolition_reading
 *   human_readable: State Execution Authority â Abolition Reading
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the abolition reading of the contested
 *   kernel state_execution_authority. The kernel is a stabilized legal
 *   commitment â the state's claimed authority to execute â that
 *   different parties read differently. This reading treats the authority as
 *   categorically impermissible: all executed persons are victims,
 *   retributive and deterrence justifications are rejected as illegitimate,
 *   and wrongful execution is treated as proof of systemic illegitimacy
 *   rather than a remediable defect. The structural delta from sibling
 *   readings is that no executed person is excluded from the victim set and
 *   no party is admitted as a legitimate beneficiary.
 *
 * KEY AGENTS:
 *   - state_execution_apparatus (agenda_setter / institutional / constrained exit): administers executions and defends capital statutes; captures sovereign authority and budget but is not a legitimate beneficiary under this reading.
 *   - executed_persons (payer / powerless / trapped): bear the ultimate cost; include both guilty and wrongfully convicted.
 *   - abolitionist_movement (observer / organized / mobile): litigates and lobbies for repeal; structurally opposed to the constraint.
 *   - crime_victims_families_retributive (excluded / moderate / constrained): their retributive demand is structurally excluded by the categorical prohibition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__abolition_reading, 0.96).
domain_priors:suppression_score(state_execution_authority__abolition_reading, 0.9).
domain_priors:theater_ratio(state_execution_authority__abolition_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, extractiveness, 0.96).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__abolition_reading, snare).
narrative_ontology:human_readable(state_execution_authority__abolition_reading, "State Execution Authority â Abolition Reading").
narrative_ontology:topic_domain(state_execution_authority__abolition_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__abolition_reading, 'd9f8ce71-119a-41d8-9e96-ac2fc8fb797e').
narrative_ontology:cs_kernel_codification('d9f8ce71-119a-41d8-9e96-ac2fc8fb797e', formalized).
narrative_ontology:cs_authority_grounding('d9f8ce71-119a-41d8-9e96-ac2fc8fb797e', lineage).
narrative_ontology:cs_interpretation_layer_present('d9f8ce71-119a-41d8-9e96-ac2fc8fb797e').
narrative_ontology:cs_reading_relation('d9f8ce71-119a-41d8-9e96-ac2fc8fb797e', state_execution_authority__retributive_reading, forecloses).
narrative_ontology:cs_reading_relation('d9f8ce71-119a-41d8-9e96-ac2fc8fb797e', state_execution_authority__deterrence_reading, forecloses).
narrative_ontology:cs_axiom('d9f8ce71-119a-41d8-9e96-ac2fc8fb797e', foundational, execution_categorically_impermissible).
narrative_ontology:cs_axiom_status(execution_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('d9f8ce71-119a-41d8-9e96-ac2fc8fb797e', execution_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('d9f8ce71-119a-41d8-9e96-ac2fc8fb797e', secondary, wrongful_execution_proves_systemic_failure).
narrative_ontology:cs_axiom_status(wrongful_execution_proves_systemic_failure, holdable).
narrative_ontology:cs_axiom_grounding('d9f8ce71-119a-41d8-9e96-ac2fc8fb797e', wrongful_execution_proves_systemic_failure, empirically_contingent).
narrative_ontology:cs_reference_frame('d9f8ce71-119a-41d8-9e96-ac2fc8fb797e', classical_state_punitive_sovereignty).
narrative_ontology:cs_drift_state('d9f8ce71-119a-41d8-9e96-ac2fc8fb797e', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d9f8ce71-119a-41d8-9e96-ac2fc8fb797e', '').
narrative_ontology:cs_kernel_id(state_execution_authority__abolition_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, executed_persons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates prisons, schedules executions, trains execution teams, and defends capital statutes in appellate courts. Derives budgetary allocations and sovereign authority from maintaining the power to terminate condemned lives. Could abolish the practice through legislative or executive action but instead preserves and administers the machinery of death.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, state_execution_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Are condemned through capital trials and direct appeal, then held on death row until the state carries out the sentence. Include both factually guilty and wrongfully convicted individuals. Once final appeal is exhausted and clemency is denied, they are physically trapped with no exit from the execution facility.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, executed_persons, payer,
    powerless, immediate, trapped, local).

% Files constitutional challenges, represents death row inmates, lobbies for repeal statutes, and appeals to international human rights bodies. Operates across jurisdictions, shifting advocacy to where political windows open. Gains no benefit from the constraint's persistence; its structural interest is total dissolution of the authority.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, abolitionist_movement, observer,
    organized, generational, mobile, global).

% Seek execution of murderers as retributive closure or proportional justice. Their demands are channeled through prosecutor victim-services offices and media narratives. Under a categorical abolition framework, their retributive interest is structurally excluded from legitimate state consideration.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, crime_victims_families_retributive, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__abolition_reading, state_execution_apparatus).
narrative_ontology:fixing_cost_class(state_execution_authority__abolition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The abolition reading holds that state execution authority serves no genuine coordination function. Retributive and deterrent claims are rejected as post-hoc rationalizations; the only 'coordination' claimed by proponents â social order through fear â is read as state terror rather than legitimate coordination.
% TRANSFER_FUNCTION: Moves the lives of condemned persons to the state to be destroyed. No legitimate beneficiary receives the transfer. The state claims to deliver justice to victims and safety to society, but the abolition reading rejects these receipts as illegitimate.
% ABSENT_VOICES: Wrongfully convicted persons who are executed cannot speak after the fact; communities subjected to racially disproportionate application are underrepresented in clemency processes; future potential homicide victims invoked as abstract deterrence props have no seat at the decision.
% DISAPPEARANCE_RATIONALE: Death rows would empty, execution infrastructure would be repurposed or abolished, sentencing regimes would shift to life imprisonment, and the state's repertoire of legitimate violence would contract to military and police contexts only. Legal doctrine would reorganize around irrevocable punishment as the categorical limit.
% FOUNDING_PROBLEM: The arrangement was built to respond to heinous crime and to assert sovereign power over life and death; the abolition reading holds that the true founding driver is the demonstration of state supremacy rather than public safety.
% FOUNDING_PROBLEM_CORROBORATION: International human rights bodies and abolitionist jurists attest from outside the state apparatus that the founding problem is either unsolved by execution or imaginary; no corroboration exists from disinterested parties that execution is a necessary response to the problem it claims to solve.
narrative_ontology:disappearance_verdict(state_execution_authority__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__abolition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__abolition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_execution_authority__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__abolition_reading, 0.96, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__abolition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__abolition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-maximum (0.96) because the constraint extracts life itself, the irreducible base, and offers no substitute. Suppression is very high (0.90) because the authority persists only through active enforcement â appeals, execution protocols, and statutory maintenance â while alternatives (life imprisonment) are systematically suppressed as inadequate. Theater ratio is moderate-high (0.58): procedural safeguards and deterrence rhetoric perform legitimacy, but the abolition reading treats these as ritualized cover for state killing. Accessibility collapse is high (0.92) because once sentenced, the condemned face nearly complete closure of alternatives. Resistance is substantial (0.78) due to persistent abolitionist litigation, legislative repeal campaigns, and international pressure.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the constraint as necessary sovereign authority and lawful punishment; the payer seat experiences it as state murder with no exit. The divergence is total â there is no overlapping frame between the apparatus and the condemned. The abolitionist observer seat sees the apparatus as engaged in performative maintenance of an illegitimate power.
 *
 * DIRECTIONALITY LOGIC:
 *   The executed_persons are full targets (d near 1.0): they bear the extraction, are powerless, and are physically trapped. The state_execution_apparatus is the agenda setter and capturer of the extraction (sovereign power, institutional budget), though it is not a beneficiary in the normative sense of the reading. The abolitionist_movement sits at an analytical distance with mobile exit. The retributive_victims_families are excluded: their preferred outcome is structurally barred by the categorical prohibition, but they are not targets of extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â heinous crime and social disorder â is contested rather than solved. The abolition reading holds that the arrangement persists not because it solves the problem but because it demonstrates state supremacy. This prevents mislabeling the constraint as coordination: even if deterrence or retribution were once live justifications, the abolition reading treats them as exhausted or illegitimate, leaving only extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    innocence_error_rate,
    'What is the actual rate of wrongful conviction among capital defendants, and how many executed persons were factually innocent?',
    'Post-hoc DNA testing and conviction-integrity unit review of completed capital cases; statistical modeling of false-conviction rates in death-penalty jurisdictions.',
    'A high innocence rate would corroborate the abolition reading''s claim that the system is structurally fallible; a negligible rate would weaken the systemic-illegitimacy argument without affecting the categorical deontological claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innocence_error_rate, empirical, 'Empirical rate of wrongful execution under capital regimes.').

omega_variable(
    deterrence_empirical_status,
    'Does capital punishment produce a measurable deterrent effect on homicide relative to life imprisonment?',
    'Panel-data econometric studies exploiting jurisdictional variation in capital statutes and sentencing; meta-analysis of homicide rates before and after abolition or reinstatement.',
    'If deterrence is empirically supported, the abolition reading''s rejection of the deterrence justification is weakened as a factual matter, though the categorical deontological prohibition remains intact. If unsupported, the coordination story collapses entirely into extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_empirical_status, empirical, 'Whether capital punishment deters homicide more than non-lethal sentences.').

omega_variable(
    kernel_reading_boundary,
    'Does the abolition reading''s categorical rejection of execution logically foreclose the retributive and deterrence readings, or can a single legal framework compartmentalize these justifications?',
    'Jurisprudential analysis of whether a single constitutional order can simultaneously hold execution to be categorically impermissible and permissible under retributive or deterrent conditions.',
    'If foreclosed, the kernel generates mutually exclusive constraints and the engine must treat them as a hard decomposition. If coexisting is possible, the readings are competing framings within a single polity rather than logical opposites.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether abolition logically forecloses sibling readings or merely coexists with them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__abolition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sea_abolition_tr_t0, state_execution_authority__abolition_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sea_abolition_tr_t10, state_execution_authority__abolition_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement(sea_abolition_tr_t20, state_execution_authority__abolition_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(sea_abolition_tr_t30, state_execution_authority__abolition_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement(sea_abolition_tr_t40, state_execution_authority__abolition_reading, theater_ratio, 40, 0.56).
narrative_ontology:measurement(sea_abolition_tr_t50, state_execution_authority__abolition_reading, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(sea_abolition_be_t0, state_execution_authority__abolition_reading, base_extractiveness, 0, 0.88).
narrative_ontology:measurement(sea_abolition_be_t10, state_execution_authority__abolition_reading, base_extractiveness, 10, 0.9).
narrative_ontology:measurement(sea_abolition_be_t20, state_execution_authority__abolition_reading, base_extractiveness, 20, 0.92).
narrative_ontology:measurement(sea_abolition_be_t30, state_execution_authority__abolition_reading, base_extractiveness, 30, 0.94).
narrative_ontology:measurement(sea_abolition_be_t40, state_execution_authority__abolition_reading, base_extractiveness, 40, 0.95).
narrative_ontology:measurement(sea_abolition_be_t50, state_execution_authority__abolition_reading, base_extractiveness, 50, 0.96).

% Suppression requirement over time
narrative_ontology:measurement(sea_abolition_su_t0, state_execution_authority__abolition_reading, suppression_requirement, 0, 0.82).
narrative_ontology:measurement(sea_abolition_su_t10, state_execution_authority__abolition_reading, suppression_requirement, 10, 0.84).
narrative_ontology:measurement(sea_abolition_su_t20, state_execution_authority__abolition_reading, suppression_requirement, 20, 0.86).
narrative_ontology:measurement(sea_abolition_su_t30, state_execution_authority__abolition_reading, suppression_requirement, 30, 0.88).
narrative_ontology:measurement(sea_abolition_su_t40, state_execution_authority__abolition_reading, suppression_requirement, 40, 0.89).
narrative_ontology:measurement(sea_abolition_su_t50, state_execution_authority__abolition_reading, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__deterrence_reading).

% DUAL FORMULATION NOTE:
% This constraint is the abolition reading of the state_execution_authority kernel. It is structurally distinct from the retributive and deterrence readings: it declares all executed persons victims, rejects all coordination justifications, and asserts categorical foreclosure of sibling readings. The kernel decomposes into at least three epsilon-invariant constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
