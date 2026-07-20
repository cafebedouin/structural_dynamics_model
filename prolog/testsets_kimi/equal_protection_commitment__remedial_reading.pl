% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__remedial_reading, []).

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
 *   constraint_id: equal_protection_commitment__remedial_reading
 *   human_readable: Equal Protection Remedial Reading: Antisubordination
 *   domain: constitutional_law/political_philosophy/social_policy
 *
 * SUMMARY:
 *   This constraint instantiates the remedial reading of the
 *   equal_protection_commitment kernel: the claim that the Equal Protection
 *   Clause forbids the perpetuation of caste systems and permits
 *   race-conscious state measures to dismantle subordination. It is one of
 *   three structurally distinct readings (alongside colorblind_reading and
 *   diversity_reading) that decompose the colloquial label 'equal
 *   protection.' The remedial reading coordinates state remedial action while
 *   extracting preferential opportunity from historically privileged groups.
 *   State actors occupy the beneficiary/agenda-setter position; historically
 *   subordinated groups receive the remedial transfer; historically
 *   privileged groups pay through denied access. The authored metrics
 *   describe a contested, asymmetric structure with rising enforcement
 *   requirements as the reading faces doctrinal erosion.
 *
 * KEY AGENTS:
 *   - state_remedial_agents: Primary agenda-setter and beneficiary (institutional/constrained) â administers race-conscious programs and defends their constitutionality.
 *   - historically_subordinated_groups: Primary beneficiary (powerless/constrained) â receives remedial access, cannot exit subordinated status.
 *   - historically_privileged_groups: Primary target and victim (powerful/mobile) â bears cost of denied preferential access, has resources but faces constrained institutional options.
 *   - colorblind_interpreters: Excluded voice (organized/constrained) â advocates rival constitutional reading, structurally absent from remedial framework.
 *   - constitutional_analysts: Analytical observer (analytical/analytical) â tracks doctrinal drift and seat divergence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, 0.55).
domain_priors:suppression_score(equal_protection_commitment__remedial_reading, 0.7).
domain_priors:theater_ratio(equal_protection_commitment__remedial_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__remedial_reading, "Equal Protection Remedial Reading: Antisubordination").
narrative_ontology:topic_domain(equal_protection_commitment__remedial_reading, "constitutional_law/political_philosophy/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__remedial_reading, 'd005adaf-744c-4c73-b7f3-e1dac36cd1f6').
narrative_ontology:cs_kernel_codification('d005adaf-744c-4c73-b7f3-e1dac36cd1f6', fixed_text).
narrative_ontology:cs_authority_grounding('d005adaf-744c-4c73-b7f3-e1dac36cd1f6', lineage).
narrative_ontology:cs_interpretation_layer_present('d005adaf-744c-4c73-b7f3-e1dac36cd1f6').
narrative_ontology:cs_reading_relation('d005adaf-744c-4c73-b7f3-e1dac36cd1f6', equal_protection_commitment__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('d005adaf-744c-4c73-b7f3-e1dac36cd1f6', equal_protection_commitment__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('d005adaf-744c-4c73-b7f3-e1dac36cd1f6', foundational, antisubordination_mandate).
narrative_ontology:cs_axiom_status(antisubordination_mandate, holdable).
narrative_ontology:cs_axiom_grounding('d005adaf-744c-4c73-b7f3-e1dac36cd1f6', antisubordination_mandate, deontological).
narrative_ontology:cs_axiom('d005adaf-744c-4c73-b7f3-e1dac36cd1f6', foundational, remedial_race_consciousness_permissible).
narrative_ontology:cs_axiom_status(remedial_race_consciousness_permissible, holdable).
narrative_ontology:cs_axiom_grounding('d005adaf-744c-4c73-b7f3-e1dac36cd1f6', remedial_race_consciousness_permissible, instrumental).
narrative_ontology:cs_reference_frame('d005adaf-744c-4c73-b7f3-e1dac36cd1f6', antisubordination_equality).
narrative_ontology:cs_drift_state('d005adaf-744c-4c73-b7f3-e1dac36cd1f6', post_sffa_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('d005adaf-744c-4c73-b7f3-e1dac36cd1f6', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__remedial_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, state_remedial_agents).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, historically_subordinated_groups).
narrative_ontology:constraint_victim(equal_protection_commitment__remedial_reading, historically_privileged_groups).
narrative_ontology:constraint_vindicates(equal_protection_commitment__remedial_reading, antisubordination_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, implement, and litigate race-conscious remedial programs including affirmative action in higher education, minority business set-asides, and targeted outreach. Defend the constitutionality of explicit racial classifications before courts and legislatures. Bear political and administrative costs of program maintenance. Benefit from the constitutional license to use race as a remedial tool.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, state_remedial_agents, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__remedial_reading, state_remedial_agents, beneficiary).

% Receive preferential access to selective educational institutions, public contracts, and employment opportunities as remediation for historical and ongoing caste-like subordination. Experience the constraint as a partial corrective to structural hierarchy; cannot exit the racialized structural position that justifies the remedial measure.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_subordinated_groups, beneficiary,
    powerless, generational, constrained, national).

% Bear the zero-sum cost of race-conscious remedial programs through reduced access to selective institutions, contracts, and positions they would likely obtain under a colorblind allocation. Experience the constraint as a transfer of opportunity based on group membership rather than individual merit. Have resources to seek alternative pathways but face constrained options within the specific institutions governed by the measure.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_privileged_groups, payer,
    powerful, biographical, mobile, national).

% Advance the constitutional argument that all governmental racial classifications are forbidden under the Equal Protection Clause. Are structurally excluded from the remedial reading's framework, which treats their preferred interpretation as constitutionally invalid and a barrier to substantive equality. Their absence from the doctrinal beneficiary set is constitutive of the constraint.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, colorblind_interpreters, excluded,
    organized, generational, constrained, national).

% Track the doctrinal tension between the remedial reading and competing colorblind and diversity readings. Analyze seat divergence, measure enforcement intensification, and assess whether the antisubordination coordination function remains live or has atrophied into performance.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, constitutional_analysts, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Dismantles caste-like social and economic subordination by permitting state actors to use race-conscious measures that formal equality failed to achieve. Coordinates collective remedial action across institutions to accelerate substantive equality.
% TRANSFER_FUNCTION: Moves preferential access to selective institutions, public contracts, and employment from historically privileged groups to historically subordinated groups; moves constitutional interpretive authority to state actors to deploy explicit racial classifications for remedial ends.
% ABSENT_VOICES: Colorblind interpreters who view any racial classification as a constitutional violation; historically privileged individuals who experience the reading as unconstitutional reverse discrimination; conservative legal scholars who reject caste analysis as outside the original meaning of the Fourteenth Amendment.
% DISAPPEARANCE_RATIONALE: If the remedial reading vanished, existing race-conscious affirmative action programs, set-asides, and targeted remedial policies would face immediate constitutional invalidation. State actors would reorganize around either colorblind formal equality or the narrower diversity rationale, and the architecture of race-conscious remediation would collapse.
% FOUNDING_PROBLEM: The failure of formal legal equality to dismantle caste-like social and economic subordination persisting after the Civil War and Reconstruction.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights historians and critical race theorists attest to persistent structural inequality and caste-like dynamics. Conservative legal scholars and some empirical economists contest the framing and extent of ongoing subordination. Corroboration is split across ideological lines; no consensus exists from entirely outside the beneficiary set.
narrative_ontology:disappearance_verdict(equal_protection_commitment__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__remedial_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_commitment__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__remedial_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_commitment__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as tangled_rope because it possesses both a genuine coordination function (dismantling caste/subordination through state action) and asymmetric extraction (historically privileged groups denied access). Extractiveness is authored at 0.55 because the transfer of opportunity is substantial and zero-sum in selective contexts. Suppression is high (0.70) because the constraint must actively suppress the colorblind alternative reading to maintain its constitutional foothold; enforcement intensifies as backlash grows. Theater_ratio is moderate (0.30) because some judicial rhetoric about remedial purpose is performative while actual programs are narrowed. Accessibility_collapse (0.60) reflects that within this reading's framework, colorblind alternatives are treated as constitutionally illegitimate. Resistance (0.75) is high due to persistent legal and political challenge from privileged groups and colorblind advocates.
 *
 * PERSPECTIVAL GAP:
 *   From the state_remedial_agents' seat, the constraint is legitimate coordination: the Constitution authorizes necessary remedial action to achieve real equality. From the historically_privileged_groups' seat, the same structure is active extraction: their individual merit is overridden by group-based preference, and they are denied access they would otherwise obtain. The subordinated groups' seat experiences subsidy (negative effective extraction). The engine computes this divergence from the structural declarations â beneficiary/victim sets and exit options â without reconciling the perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   State actors and subordinated groups are declared beneficiaries, deriving low directionality (near 0.0) and thus damped or negative effective extraction â the constraint subsidizes them. Historically privileged groups are declared victims with mobile exit, deriving high directionality (near 1.0) and amplified effective extraction. Colorblind interpreters are excluded, bearing no directionality computation. The asymmetry is structural: the constraint moves opportunity from victim seats to beneficiary seats through state mediation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure extraction (snare) because a genuine coordination function is present: the dismantling of caste-like subordination is a real collective-action problem that formal equality failed to solve. Conversely, it prevents mislabeling as pure coordination (rope) because the zero-sum transfer of selective access creates identifiable victims among historically privileged groups. The mandatrophy question is whether the founding problem (caste perpetuation) remains live; if it is declared dead while the constraint persists, the reading would drift toward snare or piton dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remedial_endpoint_ambiguity,
    'Is there a defined temporal endpoint at which race-conscious remedial measures are no longer constitutionally necessary under this reading, or do they persist indefinitely?',
    'Empirical measurement of racial inequality gaps combined with judicial doctrine review (e.g., Grutter''s 25-year horizon versus open-ended caste analysis).',
    'If indefinite, the constraint risks drifting from scaffold-like transition toward permanent extraction; a defined endpoint would support scaffold or rope reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_endpoint_ambiguity, empirical, 'Temporal limit of remedial race-consciousness').

omega_variable(
    observer_position_inversion,
    'Does the beneficiary/victim structure invert completely between the remedial reading and the colorblind reading, or do the readings share any coordination function?',
    'Generate the colorblind reading as a separate constraint story and compare structural data (beneficiaries, victims, coordination_function).',
    'Total inversion with no shared coordination confirms the epsilon-invariance decomposition; partial overlap would suggest the kernel has not been fully decomposed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(observer_position_inversion, conceptual, 'Cross-reading beneficiary/victim inversion').

omega_variable(
    caste_persistence_empirical,
    'Does caste-like subordination persist in the United States at a level that justifies ongoing race-conscious remedial state action?',
    'Sociological and economic longitudinal studies of wealth gaps, residential segregation, and institutional representation; judicial notice of social facts.',
    'If subordination has substantially ended, the coordination function is dead and the constraint extracts without justification (mandatrophy/piton); if persistent, the coordination function remains live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caste_persistence_empirical, empirical, 'Empirical foundation for ongoing remedial necessity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__remedial_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epcr_remedial_tr_t0, equal_protection_commitment__remedial_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(epcr_remedial_tr_t10, equal_protection_commitment__remedial_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(epcr_remedial_tr_t20, equal_protection_commitment__remedial_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(epcr_remedial_tr_t30, equal_protection_commitment__remedial_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(epcr_remedial_tr_t40, equal_protection_commitment__remedial_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(epcr_remedial_tr_t50, equal_protection_commitment__remedial_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(epcr_remedial_be_t0, equal_protection_commitment__remedial_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(epcr_remedial_be_t10, equal_protection_commitment__remedial_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(epcr_remedial_be_t20, equal_protection_commitment__remedial_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(epcr_remedial_be_t30, equal_protection_commitment__remedial_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(epcr_remedial_be_t40, equal_protection_commitment__remedial_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(epcr_remedial_be_t50, equal_protection_commitment__remedial_reading, base_extractiveness, 50, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(epcr_remedial_su_t0, equal_protection_commitment__remedial_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(epcr_remedial_su_t10, equal_protection_commitment__remedial_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(epcr_remedial_su_t20, equal_protection_commitment__remedial_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(epcr_remedial_su_t30, equal_protection_commitment__remedial_reading, suppression_requirement, 30, 0.74).
narrative_ontology:measurement(epcr_remedial_su_t40, equal_protection_commitment__remedial_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(epcr_remedial_su_t50, equal_protection_commitment__remedial_reading, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, diversity_reading).

% DUAL FORMULATION NOTE:
% This constraint is the remedial reading of the equal_protection_commitment kernel. The colloquial label 'equal protection regarding race' conflates three structurally distinct constraints: colorblind_reading (forbids all racial classification), diversity_reading (permits race for educational diversity), and remedial_reading (permits race-conscious measures to dismantle subordination). Each reading has distinct beneficiaries, victims, coordination functions, and epsilon values. They are linked by mutual doctrinal contestation over the same constitutional text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
