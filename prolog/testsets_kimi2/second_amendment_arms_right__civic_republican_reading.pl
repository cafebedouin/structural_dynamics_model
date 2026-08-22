% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__civic_republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__civic_republican_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: second_amendment_arms_right__civic_republican_reading
 *   human_readable: Civic Republican Right to Arms (Militia-Citizen Duty Reading)
 *   domain: constitutional/law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the civic republican reading of the
 *   Second Amendment arms-right kernel: the right to keep and bear arms
 *   protects armed citizenship as a prerequisite for republican
 *   self-governance, situating the individual citizen-militia member between
 *   the libertarian individual and the centralized state. The reading
 *   produces a dual beneficiary/payer structure in which citizens receive a
 *   right contingent on militia duty and training, while regulatory authority
 *   is constrained by a civic-participation norm. The authored metrics are
 *   independent of the claim: the story claims Tangled Rope because the
 *   arrangement simultaneously coordinates collective defense and extracts
 *   compliance costs from the same citizen-militia members, with additional
 *   exclusion costs for the unqualified. The claim/metric gap is deliberate
 *   and diagnostic.
 *
 * KEY AGENTS:
 *   - Citizen-militia members: dual beneficiary/payer (moderate power, constrained exit) â receive the right but bear training and duty costs.
 *   - Militia regulators: agenda-setter (institutional power, constrained exit) â administer qualifications but are bound by the civic-participation norm.
 *   - Unqualified citizens: payer (powerless, trapped exit) â excluded from the right entirely by qualification failures.
 *   - Libertarian individualists and collective-right advocates: excluded (organized power) â structurally marginalized by the civic republican framework.
 *   - Constitutional interpreters: observer (analytical power) â adjudicate among competing readings and track doctrinal drift.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__civic_republican_reading, 0.48).
domain_priors:suppression_score(second_amendment_arms_right__civic_republican_reading, 0.42).
domain_priors:theater_ratio(second_amendment_arms_right__civic_republican_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__civic_republican_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__civic_republican_reading, "Civic Republican Right to Arms (Militia-Citizen Duty Reading)").
narrative_ontology:topic_domain(second_amendment_arms_right__civic_republican_reading, "constitutional/law/political_philosophy").

domain_priors:requires_active_enforcement(second_amendment_arms_right__civic_republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__civic_republican_reading, 'd18044d8-176b-4d6b-a678-64eabdc7f819').
narrative_ontology:cs_kernel_codification('d18044d8-176b-4d6b-a678-64eabdc7f819', fixed_text).
narrative_ontology:cs_authority_grounding('d18044d8-176b-4d6b-a678-64eabdc7f819', lineage).
narrative_ontology:cs_interpretation_layer_present('d18044d8-176b-4d6b-a678-64eabdc7f819').
narrative_ontology:cs_reading_relation('d18044d8-176b-4d6b-a678-64eabdc7f819', second_amendment_arms_right__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('d18044d8-176b-4d6b-a678-64eabdc7f819', second_amendment_arms_right__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('d18044d8-176b-4d6b-a678-64eabdc7f819', foundational, armed_citizenship_republican_prerequisite).
narrative_ontology:cs_axiom_status(armed_citizenship_republican_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('d18044d8-176b-4d6b-a678-64eabdc7f819', armed_citizenship_republican_prerequisite, conventional).
narrative_ontology:cs_axiom('d18044d8-176b-4d6b-a678-64eabdc7f819', secondary, militia_training_as_condition_of_right).
narrative_ontology:cs_axiom_status(militia_training_as_condition_of_right, holdable).
narrative_ontology:cs_axiom_grounding('d18044d8-176b-4d6b-a678-64eabdc7f819', militia_training_as_condition_of_right, conventional).
narrative_ontology:cs_reference_frame('d18044d8-176b-4d6b-a678-64eabdc7f819', armed_civic_virtue_republic).
narrative_ontology:cs_drift_state('d18044d8-176b-4d6b-a678-64eabdc7f819', post_militia_atrophy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d18044d8-176b-4d6b-a678-64eabdc7f819', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, citizen_militia_members).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, citizen_militia_members).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, unqualified_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess a constitutional right to keep and bear arms contingent upon militia participation and the completion of training or qualification requirements. They receive the means of civic participation and collective defense but bear the costs of mandatory training, time commitment, and ongoing regulatory compliance. Opting out means relinquishing the right or facing legal penalties for non-compliance.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, citizen_militia_members, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__civic_republican_reading, citizen_militia_members, payer).

% State or federal authorities who set training curricula, qualification standards, and mobilization protocols for the militia. They are constrained by a constitutional norm that ties regulatory authority to civic participation rather than to a general police power; they cannot prohibit arms outright but must structure access around militia readiness and republican virtue.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, militia_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Citizens who fail training requirements, are deemed unfit for militia service, or are permanently disqualified from bearing arms under a framework that predicates the right on civic duty capacity. They bear the full cost of disarmament without receiving the offsetting right, and have no pathway to alter the qualification criteria.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, unqualified_citizens, payer,
    powerless, biographical, trapped, national).

% Advocates of a pre-political individual right to arms untethered to any civic duty. They view training mandates and militia linkage as unconstitutional infringements on liberty, but are structurally excluded from the civic republican interpretive framework which subordinates individual preference to collective security.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, libertarian_individualists, excluded,
    organized, biographical, constrained, national).

% Proponents of the view that the Second Amendment protects only state militia authority and not individual ownership outside state-organized forces. They are excluded from this reading because the civic republican framework vests the right in the individual citizen-soldier rather than in the state as such.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, collective_right_advocates, excluded,
    organized, generational, analytical, national).

% Judges and legal scholars who adjudicate among competing readings of the Second Amendment. They observe that the civic republican reading produces moderate extraction through qualification gates while preserving a coordination function, and they track how doctrinal drift toward individual-right or collective-right frameworks reshapes the constraint's operation.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, constitutional_interpreters, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Arms possession tied to militia service coordinates a citizenry capable of collective self-defense and resistance to tyranny, grounding republican government in an armed populace rather than in a standing army or a state monopoly on legitimate force.
% TRANSFER_FUNCTION: Moves the costs of military training, time, and regulatory compliance from the general polity to citizen-militia members in exchange for the right to possess arms; simultaneously moves regulatory authority from unrestrained state police power to a civic-participation norm that constrains but does not eliminate gun control.
% ABSENT_VOICES: Libertarian individualists who reject any duty-based predicate for constitutional rights; pacifist citizens who view armed civic virtue as morally objectionable; and collective-right advocates who would centralize all arms authority in the state rather than in the citizen-soldier.
% DISAPPEARANCE_RATIONALE: If the civic republican reading vanished overnight, militia training requirements would lose their constitutional footing, the balance between individual and state power in arms regulation would collapse toward either pure libertarian individualism or state monopoly, and the citizen-militia member's dual status as rights-holder and duty-bearer would dissolve into one of the other readings.
% FOUNDING_PROBLEM: How to secure republican self-governance against standing armies and tyranny without creating either a state monopoly on force or an atomized, unregulated individual arms free-for-all.
% FOUNDING_PROBLEM_CORROBORATION: Civic republican historians and constitutional scholars (e.g., Saul Cornell) attest to the founding problem's historical reality. Libertarian constitutional scholars and modern individual-rights jurists contest its contemporary mapping, arguing the problem has been replaced by private self-defense needs. No uncontested outside corroboration exists: the historical record supports the republican fear of standing armies, but its present relevance is disputed by non-beneficiaries.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__civic_republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__civic_republican_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__civic_republican_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_arms_right__civic_republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__civic_republican_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__civic_republican_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_arms_right__civic_republican_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_arms_right__civic_republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end) because the training and qualification requirements impose real costs on citizen-militia members and exclude a subset of citizens, but the offsetting right to arms and republican coordination function dampen the net extraction. Suppression is moderate (0.42) because the constraint suppresses unregulated individual ownership and bars the unqualified, yet it does not eliminate alternatives entirelyâcompeting individual-right and collective-right readings remain live in constitutional discourse. Theater ratio rises from 0.15 to 0.35 over the interval because the historical militia system has atrophied, increasing the share of performative civic ritual relative to genuine military coordination. Accessibility collapse is only 0.45: the individual-right and collective-right alternatives are highly visible and institutionally active, preventing full collapse. Resistance is 0.55 due to sustained opposition from both libertarian and state-monopoly camps.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (militia regulators) experiences the constraint as a bounded delegation of authority that preserves republican values; the payer seats (citizen-militia members and unqualified citizens) experience it as a conditional entitlement that extracts compliance costs or denies the right outright. The beneficiary sub-seat within the citizen-militia member offsets some extraction, producing a moderated net directionality, whereas the unqualified citizen experiences pure target status. The engine computes this divergence from the structural data rather than from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Citizen-militia members are declared as both beneficiaries and victims, reflecting their dual structural position: they receive the right to arms (beneficiary derivation, low d) and pay the duty of training and service (payer derivation, high d). The engine moderates their effective extraction through the declared secondary_role and constrained exit. Unqualified citizens are pure payers with trapped exit, placing them near the full-target end. Militia regulators are agenda-setters constrained by constitutional text, yielding a near-symmetric directionality. Excluded advocates have analytical or constrained exit but no direct extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The civic republican reading carries a genuine coordination functionâarming and training a citizen militia for republican self-governanceâwhich prevents classification as pure Snare. Simultaneously, the mandatory training and qualification requirements impose asymmetric costs on citizen-militia members and outright exclusion for the unqualified, preventing classification as pure Rope. The dual beneficiary/payer role of the militia member is the structural signature of Tangled Rope: they are coordinated into a collective-security system and simultaneously charged for that coordination through duty-bearing. If the duty component atrophies while the regulatory framework persists, the constraint would drift toward Piton; if the coordination benefit collapsed while extraction remained, it would drift toward Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Does the civic republican reading collapse into the individual right reading once the militia duty component becomes unenforced, or does it remain structurally distinct as a constraint family?',
    'Comparative doctrinal analysis tracking whether courts adopting civic republican language still enforce training and qualification requirements as conditions of the right, or treat them as optional encouragements.',
    'If the duty component is unenforceable in practice, this reading drifts toward a rhetorical cover for extraction or dissolves into the individual right reading; if enforceable, it remains a distinct Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Structural ambiguity between civic republican reading and sibling readings when militia duty is not operational.').

omega_variable(
    duty_burden_asymmetry,
    'Are the training and qualification costs borne by citizen-militia members offset by genuine republican self-governance benefits, or do the costs exceed the coordination gain?',
    'Empirical assessment of militia readiness outcomes, civic participation rates among armed citizens, and comparative analysis of republican stability in jurisdictions with duty-based arms regimes versus individual-right regimes.',
    'If costs exceed coordination gain, the constraint trends toward Snare; if balanced, it remains Tangled Rope; if benefits dominate, it trends toward Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duty_burden_asymmetry, empirical, 'Whether the civic duty extraction exceeds the republican coordination benefit.').

omega_variable(
    regulatory_authority_constrained,
    'Does the civic participation norm actually constrain regulatory overreach, or does it provide a constitutional vocabulary for expansive state control over arms possession?',
    'Case law review examining whether qualification requirements under a civic republican rationale are used to expand or restrict state regulatory power relative to baseline.',
    'If the norm expands state power, the reading operates as extraction via state agenda-setters; if it constrains, the dual beneficiary-payer structure holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_authority_constrained, conceptual, 'Ambiguity about whether the civic participation norm constrains or enables state regulatory extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__civic_republican_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(saar_civic_rep_tr_t0, second_amendment_arms_right__civic_republican_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(saar_civic_rep_tr_t6, second_amendment_arms_right__civic_republican_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(saar_civic_rep_tr_t12, second_amendment_arms_right__civic_republican_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(saar_civic_rep_tr_t18, second_amendment_arms_right__civic_republican_reading, theater_ratio, 18, 0.26).
narrative_ontology:measurement(saar_civic_rep_tr_t24, second_amendment_arms_right__civic_republican_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(saar_civic_rep_tr_t30, second_amendment_arms_right__civic_republican_reading, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(saar_civic_rep_be_t0, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(saar_civic_rep_be_t6, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 6, 0.38).
narrative_ontology:measurement(saar_civic_rep_be_t12, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(saar_civic_rep_be_t18, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 18, 0.45).
narrative_ontology:measurement(saar_civic_rep_be_t24, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement(saar_civic_rep_be_t30, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 30, 0.52).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(second_amendment_arms_right__civic_republican_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__civic_republican_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, collective_right_reading).

% DUAL FORMULATION NOTE:
% This constraint is the civic republican reading of the Second Amendment kernel, positioned between the individual-right reading (lower extraction, individual liberty focus) and the collective-right reading (state authority focus). The three readings form a constraint family decomposed from the natural-language 'Second Amendment' label per the Îµ-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
