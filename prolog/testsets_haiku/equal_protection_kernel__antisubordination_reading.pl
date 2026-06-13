% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__antisubordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__antisubordination_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: equal_protection_kernel__antisubordination_reading
 *   human_readable: Equal Protection as Antisubordination Doctrine
 *   domain: constitutional_law/civil_rights
 *
 * SUMMARY:
 *   The antisubordination reading of the Fourteenth Amendment's Equal
 *   Protection Clause holds that the clause targets caste-like hierarchies,
 *   not racial classification per se. Under this reading, state action that
 *   dismantles subordination (affirmative action, remedial education,
 *   integration mandates) is permitted; state action that entrenches
 *   hierarchy is forbidden. The reading creates a structurally asymmetric
 *   doctrine: historically subordinated groups retain robust equal protection
 *   claims against subordinating state action; dominant groups lack equal
 *   protection standing to contest remedial measures. This reading competes
 *   with the colorblind reading (which forbids all racial classification) and
 *   the remedial reading (which permits race-consciousness when remediating
 *   documented historical harm). The constraint is CLAIMED as tangled_rope
 *   because it coordinates a real function (dismantling caste-like hierarchy)
 *   while extracting legal standing from one group to give it to another. The
 *   claim and metrics are authored independently: the extraction score
 *   reflects the legal-standing asymmetry and the constraint's dependence on
 *   active judicial enforcement to maintain the distinction between
 *   subordinating and anti-subordinating state action.
 *
 * KEY AGENTS:
 *   - historically_subordinated_castes: The beneficiaries of antisubordination doctrine — retain standing to challenge subordinating state action and can support remedial measures.
 *   - dominant_group_members: The payers — lose standing to contest race-conscious remedial measures; their constitutional claims against affirmative action fail under this reading.
 *   - state_administrative_apparatus: The agenda setter — has authority to use race-consciousness to dismantle subordination, but must police action that entrenches it.
 *   - colorblind_reading_proponents: Excluded but live competitors — argue the reading impermissibly permits state racial sorting.
 *   - remedial_reading_proponents: Excluded but live competitors — occupy structural middle ground, accepting race-consciousness but grounding it in remedy, not ongoing subordination.
 *   - constitutional_scholarship: Observers curating coherence and mapping implications across readings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__antisubordination_reading, 0.31).
domain_priors:suppression_score(equal_protection_kernel__antisubordination_reading, 0.68).
domain_priors:theater_ratio(equal_protection_kernel__antisubordination_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__antisubordination_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__antisubordination_reading, "Equal Protection as Antisubordination Doctrine").
narrative_ontology:topic_domain(equal_protection_kernel__antisubordination_reading, "constitutional_law/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__antisubordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__antisubordination_reading, '155878aa-1069-4ce8-8648-a2ab902624f9').
narrative_ontology:cs_kernel_codification('155878aa-1069-4ce8-8648-a2ab902624f9', fixed_text).
narrative_ontology:cs_authority_grounding('155878aa-1069-4ce8-8648-a2ab902624f9', lineage).
narrative_ontology:cs_interpretation_layer_present('155878aa-1069-4ce8-8648-a2ab902624f9').
narrative_ontology:cs_reading_relation('155878aa-1069-4ce8-8648-a2ab902624f9', equal_protection_kernel__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('155878aa-1069-4ce8-8648-a2ab902624f9', equal_protection_kernel__remedial_reading, influences).
narrative_ontology:cs_axiom('155878aa-1069-4ce8-8648-a2ab902624f9', foundational, caste_like_subordination_is_constitutional_evil).
narrative_ontology:cs_axiom_status(caste_like_subordination_is_constitutional_evil, holdable).
narrative_ontology:cs_axiom_grounding('155878aa-1069-4ce8-8648-a2ab902624f9', caste_like_subordination_is_constitutional_evil, deontological).
narrative_ontology:cs_axiom('155878aa-1069-4ce8-8648-a2ab902624f9', foundational, state_may_use_race_consciousness_to_dismantle_subordination).
narrative_ontology:cs_axiom_status(state_may_use_race_consciousness_to_dismantle_subordination, holdable).
narrative_ontology:cs_axiom_grounding('155878aa-1069-4ce8-8648-a2ab902624f9', state_may_use_race_consciousness_to_dismantle_subordination, instrumental).
narrative_ontology:cs_reference_frame('155878aa-1069-4ce8-8648-a2ab902624f9', subordination_dismantling_authority).
narrative_ontology:cs_drift_state('155878aa-1069-4ce8-8648-a2ab902624f9', contemporary_colorblind_ascendancy, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('155878aa-1069-4ce8-8648-a2ab902624f9', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__antisubordination_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, historically_subordinated_castes).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, dominant_group_legal_interests).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, dominant_group_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of racial/ethnic groups subjected to caste-like subordination (African Americans, Indigenous peoples, other historically excluded groups). The antisubordination reading permits state action—affirmative action, remedial education policies, integration mandates—specifically to dismantle the subordinating hierarchy they are locked into. Their membership in the subordinated caste is the basis for claiming remedial protection; they cannot exit the identity that triggers subordination.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, historically_subordinated_castes, beneficiary,
    powerless, generational, trapped, national).

% Members of historically dominant groups (whites in racial context, majority religions in religion context) who face exclusion from certain race-conscious or ethnicity-conscious programs. Under the antisubordination reading, they cannot invoke equal protection against remedial programs because their groups were not the targets of subordination; they bear the cost of remedial state action but lack standing to contest it as discrimination.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, dominant_group_members, payer,
    moderate, biographical, constrained, national).

% Legislatures, courts, and enforcement agencies that must determine whether state action entrenches or dismantles subordination. Under this reading, they have affirmative authority to use race-conscious measures (admissions policies, contracting, remedial education, integration) when calibrated to dismantle caste-like hierarchy. They must also police state action that entrenches subordination, including facially neutral policies that perpetuate hierarchy.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, state_administrative_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Jurists, legislators, and advocacy groups who hold the competing colorblind reading—that the Constitution forbids ALL racial classification regardless of purpose. They would argue that the antisubordination reading impermissibly permits state racial sorting and violates the clause's categorical prohibition on race-consciousness. They are excluded from the authoring frame of this reading but remain live litigants in constitutional contests.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, colorblind_reading_proponents, excluded,
    institutional, generational, constrained, national).

% Jurists and scholars holding the remedial reading—that race-conscious state action is permitted when narrowly tailored to remedy documented historical exclusion. They occupy a structural middle position: accepting race-consciousness as the antisubordination reading does, but grounding it in remedy for historical injury rather than in dismantling ongoing subordination. They would contest the antisubordination reading's scope (whether subordination extends beyond documented historical exclusion) and justification.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, remedial_reading_proponents, excluded,
    institutional, generational, constrained, national).

% Legal scholars, civil rights organizations, and academic theorists who map the doctrine's implications and contests. They take testimony from all seat-holders, produce doctrinal analysis, and in effect curate the coherence of each reading. No direct stake in any ruling's outcome, but their framing work shapes how courts and legislatures navigate the reading choice.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, constitutional_scholarship, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_kernel__antisubordination_reading, state_administrative_apparatus).
narrative_ontology:fixing_cost_class(equal_protection_kernel__antisubordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents state from using its enforcement power to entrench caste-like hierarchy; simultaneously authorizes state to use race-conscious measures to dismantle subordination. Solves the coordination problem: how can law serve both as a shield against discriminatory state action AND as a tool to correct discriminatory state structures.
% TRANSFER_FUNCTION: Transfers the burden of constitutional deference: from subordinated groups (who must justify claims of injury in order to mount legal challenge) to dominant groups (who must justify race-conscious state measures in terms of dismantling rather than entrenching subordination). The constraint also transfers legal standing: only subordinated groups retain robust equal protection claims; dominant groups are foreclosed from contesting remedial measures as discrimination.
% ABSENT_VOICES: Color-blind reading proponents are excluded from the authoring frame—they would argue the reading misreads the constitutional text by permitting racial classification. So are remedial reading proponents—they dispute the grounding (ongoing subordination vs. historical injury) and scope (how far remedial authority extends). Non-legal constituencies affected by educational and economic policies (students, workers, families) have no formal seat in the constraint's operation but bear many of its consequences.
% DISAPPEARANCE_RATIONALE: If the antisubordination reading disappeared, equal protection law would collapse into either the colorblind reading (race-consciousness categorically forbidden) or the remedial reading (race-consciousness permitted only when remediating documented historical harm). The antisubordination framing itself—that ongoing subordination justifies race-conscious state action—would cease to structure judicial review. Affirmative action, integration mandates, and remedial education programs would face different doctrinal tests. Educational hierarchies and resource distributions that currently turn on subordination-based reasoning would be re-evaluated under different constitutional logic.
% FOUNDING_PROBLEM: Formal legal equality—a rule that treats all citizens identically—fails to address caste-like systems in which some groups are structurally subordinated despite formal legal equality. The Fourteenth Amendment must forbid state action that perpetuates such subordination, not merely classify on a forbidden basis.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights scholars (Reva Siegel, Catharine MacKinnon, others) and some judicial opinions (e.g., Justice Gorsuch's Bostock majority, broader scholarship on structural inequality) corroborate that caste-like subordination persists and formal equality doctrines miss it. Colorblind reading proponents contest whether this framing is supported by the constitutional text or represents judge-made policy. No universal consensus exists outside the antisubordination reading's own framework.
narrative_ontology:disappearance_verdict(equal_protection_kernel__antisubordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__antisubordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__antisubordination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(equal_protection_kernel__antisubordination_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__antisubordination_reading_tests).
:- end_tests(equal_protection_kernel__antisubordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.31 at interval end) is moderate because the reading does solve a genuine coordination problem (preventing law from entrenching caste-like hierarchy while permitting dismantling), but it does so by redistributing legal standing asymmetrically — a core extraction mechanism. The score increases from 1868 (0.05, when the reading was nascent) through the mid-20th century (0.28–0.32, when the reading gained traction in civil rights scholarship and selective judicial adoption). The suppression requirement is high (0.68) because the reading's persistence depends on actively maintaining the distinction between subordinating and anti-subordinating state action — a distinction courts must police continuously, and one that dominant groups actively contest. The theater ratio is moderate-low (0.22), indicating that the antisubordination framing does substantive legal work (it is not merely performative), but a growing share of the constraint's enforcement activity (post-2000) defends the legal-standing asymmetry against colorblind and remedial challenges rather than directly addressing subordination. The measurement series tracks a single time grid: every metric is authored at every examined time point (1868, 1954, 1978, 2000, 2013, 2026), so temporal analysis has a shared basis.
 *
 * PERSPECTIVAL GAP:
 *   The subordinated-caste seats and the agenda-setter seat should compute very differently in per-seat classification. From the subordinated-caste position, the reading is protective — it recognizes the caste structure and empowers state action to dismantle it. From the dominant-group position, the reading operates as a constraint on legal standing — they cannot invoke equal protection against remedial measures. The state administrative apparatus sees the reading as a tool of authority (it permits race-conscious action) and also as a constraint (it forbids subordinating action). The engine computes these divergences from power, exit, and directionality; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated castes are the structural beneficiaries (d near 0.0–0.2): the reading creates legal standing for them to mount challenges and supports state action on their behalf. Dominant-group members are the payers (d near 0.8–1.0): they lose standing to contest remedial measures and bear the burden of integration/remedial policies. The state apparatus sits near symmetric (d near 0.5): it gains authority to use race-consciousness but also bears the burden of continuous judicial policing to maintain the subordination/remediation distinction. These directionalities emerge from the structural data: beneficiaries (subordinated castes), victims (dominant-group legal interests), exit options (trapped for subordinated groups, constrained for dominant groups), and power differentials (institutional state, moderate-to-powerless subordinated groups).
 *
 * MANDATROPHY ANALYSIS:
 *   The antisubordination reading does not exhibit mandatrophy. Its founding problem (caste-like subordination persists despite formal legal equality) remains live, and the reading's function (distinguishing subordinating from anti-subordinating state action) remains active. The constraint does not persist primarily through inertia or performance — it is actively contested and enforced. However, the reading faces a specific mandatrophy-adjacent dynamic: as the colorblind reading gains traction (especially post-2000), the antisubordination framing's authority erodes, and a growing share of judicial enforcement work goes into defending the legal-standing asymmetry rather than toward substantive subordination analysis. This is not yet full mandatrophy (the function has not atrophied), but it is a cautionary trajectory: the reading persists partly by theatrical maintenance of its core distinction in the face of doctrinal pressure from competitors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subordination_scope_ambiguity,
    'What count as ''caste-like subordination'' under the antisubordination reading? Does the concept extend to all status hierarchies, only to racial/ethnic hierarchy, or only to hierarchies descended from explicit legal subordination (slavery, Jim Crow, caste systems)?',
    'Case law development and scholarly consensus on which groups qualify for antisubordination protection and which state measures count as dismantling vs. entrenching subordination. Historical analysis of which hierarchies operate with caste-like mechanisms (rigid boundaries, intergenerational transmission, social stigma, resource concentration).',
    'A narrow scope (only explicitly historical exclusion) converges the antisubordination reading toward the remedial reading and reduces extractiveness. A broad scope (all status hierarchies) amplifies the reading''s scope and increases its extractiveness by extending legal standing to more groups and authorizing more state measures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_scope_ambiguity, conceptual, 'Whether subordination includes all hierarchies or is limited to those with historical pedigree.').

omega_variable(
    dominant_group_standing_foreclosure,
    'Is the antisubordination reading''s foreclosure of dominant-group equal protection claims logically entailed by the reading''s core premise (that the clause targets subordination, not classification), or is it a separate policy choice about standing and remedies?',
    'Doctrinal analysis: does the antisubordination premise logically imply that dominant groups cannot invoke the clause against remedial measures, or could a reading that targets subordination still permit dominant-group claims on other grounds (e.g., narrow tailoring, less restrictive means)?',
    'If logically entailed, the standing foreclosure is internal to the reading''s structure and not removable without abandoning antisubordination reasoning. If separable, the reading could accommodate dominant-group claims while maintaining subordination-focus, reducing extractiveness and bringing it closer to the remedial reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dominant_group_standing_foreclosure, conceptual, 'Whether dominant-group standing foreclosure is intrinsic to antisubordination logic or contingent on doctrine design.').

omega_variable(
    subordination_entrenching_vs_remediating_distinction,
    'Can courts reliably distinguish state action that entrenches subordination from state action that dismantles it? Or does the distinction collapse into a purely political judgment about which groups deserve protection?',
    'Longitudinal study of how courts apply the distinction in affirmative action cases, remedial education policy, integration mandates, and other race-conscious measures. Analysis of cases where courts disagreed on whether a measure entrenched or remediated subordination.',
    'If courts can reliably make the distinction, the antisubordination reading sustains its doctrine and its authority remains coherent. If the distinction is unstable, the reading devolves into political contestation, extraction rises (the constraint becomes whatever courts assert at any moment), and the reading converges toward piton dynamics (theatrical maintenance of a distinction courts cannot operationalize).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subordination_entrenching_vs_remediating_distinction, empirical, 'Whether the core subordination/remediation distinction is doctrinally operable or collapses into politics.').

omega_variable(
    reading_kernel_ambiguity,
    'Is the Equal Protection Clause a single kernel with multiple readings, or do the antisubordination, remedial, and colorblind positions rest on fundamentally different textual premises that cannot be unified under one clause?',
    'Constitutional history and text analysis: does the clause''s language support one reading as primary, or is multi-reading indeterminacy structural? Does the legislative history of the Fourteenth Amendment favor one reading?',
    'If one reading is textually primary, the others are departures and should be classified as reinterpretations or misreadings, not as coequal readings. If the clause is fundamentally indeterminate, all three readings are live and equally valid, and the kernel itself should be classified as a commitment system under severe stress (drift_state shows authority_erosion or axiom_overriding, not stability).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether the equal protection kernel has one privileged reading or is fundamentally multi-reading.').

omega_variable(
    caste_metaphor_literalism,
    'The antisubordination reading uses ''caste-like'' as a description of social hierarchy. Is this metaphor analytically rigorous, or does it smuggle in assumptions from anthropological caste systems that may not apply to racial hierarchy in the United States?',
    'Comparative analysis of caste and racial hierarchy: do they operate by the same mechanisms (hereditary status, stigma, intergenerational transmission, exclusion from mobility)? Does the metaphor distort or clarify equal protection reasoning?',
    'If the metaphor is loose, the antisubordination reading may overreach by applying caste-logic to hierarchies with different structures, potentially authorizing remedial measures that lack clear subordination grounding. If the metaphor is analytically sound, it supports the reading''s scope and justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caste_metaphor_literalism, empirical, 'Whether caste-like hierarchy is an accurate description of racial subordination in U.S. law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__antisubordination_reading, 1868, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1868, equal_protection_kernel__antisubordination_reading, theater_ratio, 1868, 0.08).
narrative_ontology:measurement_basis(equa_tr_t1868, observed).
narrative_ontology:measurement(equa_tr_t1954, equal_protection_kernel__antisubordination_reading, theater_ratio, 1954, 0.12).
narrative_ontology:measurement_basis(equa_tr_t1954, observed).
narrative_ontology:measurement(equa_tr_t1978, equal_protection_kernel__antisubordination_reading, theater_ratio, 1978, 0.18).
narrative_ontology:measurement_basis(equa_tr_t1978, observed).
narrative_ontology:measurement(equa_tr_t2000, equal_protection_kernel__antisubordination_reading, theater_ratio, 2000, 0.21).
narrative_ontology:measurement_basis(equa_tr_t2000, observed).
narrative_ontology:measurement(equa_tr_t2013, equal_protection_kernel__antisubordination_reading, theater_ratio, 2013, 0.24).
narrative_ontology:measurement_basis(equa_tr_t2013, observed).
narrative_ontology:measurement(equa_tr_t2026, equal_protection_kernel__antisubordination_reading, theater_ratio, 2026, 0.22).
narrative_ontology:measurement_basis(equa_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t1868, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1868, 0.05).
narrative_ontology:measurement_basis(equa_be_t1868, observed).
narrative_ontology:measurement(equa_be_t1954, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1954, 0.12).
narrative_ontology:measurement_basis(equa_be_t1954, observed).
narrative_ontology:measurement(equa_be_t1978, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1978, 0.28).
narrative_ontology:measurement_basis(equa_be_t1978, observed).
narrative_ontology:measurement(equa_be_t2000, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2000, 0.32).
narrative_ontology:measurement_basis(equa_be_t2000, observed).
narrative_ontology:measurement(equa_be_t2013, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2013, 0.29).
narrative_ontology:measurement_basis(equa_be_t2013, observed).
narrative_ontology:measurement(equa_be_t2026, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2026, 0.31).
narrative_ontology:measurement_basis(equa_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1868, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1868, 0.15).
narrative_ontology:measurement_basis(equa_su_t1868, observed).
narrative_ontology:measurement(equa_su_t1954, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1954, 0.35).
narrative_ontology:measurement_basis(equa_su_t1954, observed).
narrative_ontology:measurement(equa_su_t1978, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1978, 0.62).
narrative_ontology:measurement_basis(equa_su_t1978, observed).
narrative_ontology:measurement(equa_su_t2000, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement_basis(equa_su_t2000, observed).
narrative_ontology:measurement(equa_su_t2013, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2013, 0.71).
narrative_ontology:measurement_basis(equa_su_t2013, observed).
narrative_ontology:measurement(equa_su_t2026, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2026, 0.68).
narrative_ontology:measurement_basis(equa_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__antisubordination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_kernel__antisubordination_reading, 0.12).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, equal_protection_kernel__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, equal_protection_kernel__remedial_reading).

% DUAL FORMULATION NOTE:
% The equal_protection_kernel comprises three constraint stories corresponding to three competing readings of the Fourteenth Amendment's Equal Protection Clause: antisubordination_reading (this story, targeting caste-like hierarchy), colorblind_reading (forbidding all racial classification), and remedial_reading (permitting race-consciousness for documented historical remedy). Each reading instantiates a different constraint with a different epsilon, different beneficiary/victim structure, and different effective extraction. The three stories are linked via network.affects_constraints to indicate kernel kinship and doctrinal interference. Per the ε-invariance principle, each reading is authored as a single, self-contained constraint; the contest between readings is recorded in cs_structure.reading_relations and omegas, not in metric hedging within a single story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
