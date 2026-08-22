% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__rhetorical_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__rhetorical_scaffold_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: deferential_realism_ontology__rhetorical_scaffold_reading
 *   human_readable: Deferential Realism Typology as Rhetorical Advocacy Framework
 *   domain: epistemology/institutional_design
 *
 * SUMMARY:
 *   Under this reading, the Deferential Realism constraint typology
 *   (mountain/rope/snare/tangled_rope/scaffold/piton) is not an observational
 *   instrument for discovering fixed institutional categories but a normative
 *   vocabulary for declaring institutional mechanisms illegitimate. The
 *   reading treats 'snare' as a normative classification that precedes and
 *   guides measurement, rather than as a measured outcome that precedes
 *   classification. The typology's value lies entirely in its persuasive
 *   power to reframe institutional debates and shift the burden of proof from
 *   reformers (prove extraction exists) to practitioners (prove you are not
 *   extracting). This reading instantiates one position in a contested
 *   kernel: the rival immutable_diagnostic reading treats the typology as a
 *   fixed measurement tool; the hybrid_pragmatic reading treats it as having
 *   elements of both discovery and normative judgment with the boundary
 *   depending on context.
 *
 * KEY AGENTS:
 *   - policy_advocates_for_structural_reform: agenda-setter, organized power, mobile exit — control the rhetorical frame and decide which mechanisms get classified as snares
 *   - marginalized_constituencies: beneficiary, powerless, trapped exit — gain a vocabulary that validates their experience without requiring empirical proof
 *   - institutional_practitioners: payer, powerful power, constrained exit — must defend against classifications made without prior measurement
 *   - empirical_methodologists: excluded, organized power, mobile exit — would argue for measurement-first epistemic order but are excluded from policy process
 *   - alternative_critique_frameworks: excluded, organized power, mobile exit — competing vocabularies crowded out by the typology's rhetorical hegemony
 *   - policy_scholars_neutral_stance: observer, moderate power, constrained exit — attempt non-partisan analysis but lose authority once policy moves to implementation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__rhetorical_scaffold_reading, 0.82).
domain_priors:suppression_score(deferential_realism_ontology__rhetorical_scaffold_reading, 0.31).
domain_priors:theater_ratio(deferential_realism_ontology__rhetorical_scaffold_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__rhetorical_scaffold_reading, scaffold).
narrative_ontology:human_readable(deferential_realism_ontology__rhetorical_scaffold_reading, "Deferential Realism Typology as Rhetorical Advocacy Framework").
narrative_ontology:topic_domain(deferential_realism_ontology__rhetorical_scaffold_reading, "epistemology/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__rhetorical_scaffold_reading).
narrative_ontology:has_sunset_clause(deferential_realism_ontology__rhetorical_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__rhetorical_scaffold_reading, '07697191-079d-49a0-82e6-b9ea2d277ad0').
narrative_ontology:cs_kernel_codification('07697191-079d-49a0-82e6-b9ea2d277ad0', distributed).
narrative_ontology:cs_authority_grounding('07697191-079d-49a0-82e6-b9ea2d277ad0', distributed).
narrative_ontology:cs_reading_relation('07697191-079d-49a0-82e6-b9ea2d277ad0', deferential_realism_ontology__immutable_diagnostic_reading, coexists_with).
narrative_ontology:cs_reading_relation('07697191-079d-49a0-82e6-b9ea2d277ad0', deferential_realism_ontology__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('07697191-079d-49a0-82e6-b9ea2d277ad0', foundational, normative_judgment_epistemic_priority).
narrative_ontology:cs_axiom_status(normative_judgment_epistemic_priority, holdable).
narrative_ontology:cs_axiom_grounding('07697191-079d-49a0-82e6-b9ea2d277ad0', normative_judgment_epistemic_priority, deontological).
narrative_ontology:cs_axiom('07697191-079d-49a0-82e6-b9ea2d277ad0', foundational, institutional_classification_as_rhetorical_act).
narrative_ontology:cs_axiom_status(institutional_classification_as_rhetorical_act, holdable).
narrative_ontology:cs_axiom_grounding('07697191-079d-49a0-82e6-b9ea2d277ad0', institutional_classification_as_rhetorical_act, instrumental).
narrative_ontology:cs_reference_frame('07697191-079d-49a0-82e6-b9ea2d277ad0', measurement_first_epistemic_order).
narrative_ontology:cs_drift_state('07697191-079d-49a0-82e6-b9ea2d277ad0', rhetorical_inversion_moment, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('07697191-079d-49a0-82e6-b9ea2d277ad0', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, policy_advocates_for_structural_reform).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, marginalized_constituencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, institutional_practitioners).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__rhetorical_scaffold_reading, normative_classification_precedes_measurement).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__rhetorical_scaffold_reading, institutional_taxonomy_as_persuasion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uses the constraint typology (especially 'snare' and 'tangled_rope' labels) as a diagnostic vocabulary to reframe institutional mechanisms as illegitimate extractions rather than natural arrangements. They set the rhetorical frame, choose which mechanisms to apply the typology to, and benefit from the power to declare something extractive without measured proof of cost asymmetry. Can shift to alternative critique vocabularies if the typology loses persuasive power.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, policy_advocates_for_structural_reform, agenda_setter,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__rhetorical_scaffold_reading, policy_advocates_for_structural_reform, beneficiary).

% Gain a vocabulary that names their experience of extraction without requiring them to produce the empirical measurement a neutral observer would demand. The typology's normative stance validates their situational knowledge. They cannot exit the constraint (are structurally dependent on advocacy vocabularies); they benefit from the rhetorical reframing but do not control it.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, marginalized_constituencies, beneficiary,
    powerless, biographical, trapped, national).

% Bear the cost of operating under a typology that classifies their mechanisms as illegitimate without requiring empirical demonstration of extraction. Once an institution is labeled 'snare' under this reading, the burden shifts to them to prove they are not extractive—a structural asymmetry in the frame itself. Their options are defending against the reframing (costly) or accepting the constraint's normative redescription.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, institutional_practitioners, payer,
    powerful, generational, constrained, national).

% Would object that classification without measurement and prior specification of what counts as illegitimate beneficiaries inverts the epistemic order—measurement should ground classification, not follow it. They are excluded from the policy process where the typology's classifications drive remedial action; their voice is treated as naive empiricism rather than valid epistemic critique.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, empirical_methodologists, excluded,
    organized, generational, mobile, national).

% Competing normative vocabularies (Marxist analysis, public-choice economics, utilitarian cost-benefit, capabilities frameworks) are crowded out when the constraint typology becomes the hegemonic reframing tool. They would offer different classifications of the same mechanisms; exclusion is structural—the typology's rhetorical power marginalizes rather than refutes competitors.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, alternative_critique_frameworks, excluded,
    organized, biographical, mobile, national).

% Analyze the typology from a position claiming non-partisanship. They note the scaffold's persuasive architecture but attempt to remain outside the normative commitments. In practice, their exclusion from setting the frame means their observations are treated as less authoritative than the advocates' declarations once policy moves into implementation.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, policy_scholars_neutral_stance, observer,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__rhetorical_scaffold_reading, policy_advocates_for_structural_reform).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__rhetorical_scaffold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes policy critique around a single coherent vocabulary (mountain/rope/snare/tangled_rope/scaffold/piton) that translates diverse institutional complaints into a shared diagnostic language, enabling coalition-building among reform advocates who might otherwise lack common framing.
% TRANSFER_FUNCTION: Transfers interpretive authority over institutional classification from empiricists and neutral methodologists to normative advocates and reform coalitions. The advocates gain the power to declare mechanisms extractive; empiricists and institutional practitioners lose the power to demand prior measurement before classification.
% ABSENT_VOICES: Empirical methodologists and practitioners of the mechanisms being classified would argue that classification without prior specification of legitimate beneficiaries and measurement of extraction inverts proper epistemic order. Alternative critique frameworks (Marxist, public-choice, utilitarian, capabilities-based) would argue the typology's categories obscure rather than reveal by claiming universality across incommensurable normative traditions. These parties are structurally excluded from the policy process where classifications drive remedial action.
% DISAPPEARANCE_RATIONALE: If this rhetorical constraint vanished—if the typology lost persuasive power or was abandoned—policy advocates would reorganize around alternative vocabularies (perhaps returning to raw power analysis, utilitarian metrics, or domain-specific institutional critique). The institutional mechanisms themselves would not change, but the normative frame through which their legitimacy is assessed would shift, altering the political dynamics of reform efforts and the burden-of-proof allocation in policy disputes.
% FOUNDING_PROBLEM: Institutional mechanisms that operate through extraction, coordination failure, or normative ambiguity resist straightforward empirical description. Reform advocates need a vocabulary that can name illegitimacy without being dismissed as subjective politics. The typology was constructed to provide that vocabulary—to make the normative judgment authoritative through structural categories rather than assertion.
% FOUNDING_PROBLEM_CORROBORATION: Policy advocates attest the problem remains live: institutions still evade accountability by hiding extraction in coordination language, and empiricist methodologies still demand proof before reform. However, empirical methodologists and alternative-framework scholars attest the founding problem is misconceived—that the typology solves an artificial problem by inverting the epistemic order and treating normative judgment as prior to measurement. The actual institutional dynamics (extraction, coordination cost, legitimacy dispute) would persist without the typology; the typology merely redistributes interpretive authority.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__rhetorical_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__rhetorical_scaffold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(deferential_realism_ontology__rhetorical_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferential_realism_ontology__rhetorical_scaffold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint transfers interpretive authority from empiricists to normative advocates—the power to declare institutional mechanisms extractive without measurement is a concentrated gain for advocates and a concentrated cost for practitioners. Suppression is moderate-low (0.31) because this reading explicitly names normativity as its operating principle: the constraint does not hide its normative commitments (unlike a snare), but it does suppress the empirical methodology's claim to epistemic priority by inverting the order (normative judgment precedes measurement). Theater ratio is high (0.68) because the constraint's function is overwhelmingly persuasive: the typology's categories perform rhetorical work (delegitimating institutions, validating marginalized experience) rather than discovering institutional facts. The measurement series shows extractiveness and theater rising together over the interval as the typology gains persuasive power and advocates become more confident in declaring mechanisms without measurement. Suppression remains low because the constraint does not depend on hiding its normative structure—its power lies in making normativity explicit and authoritative. This profile matches a scaffold: the constraint is transitional (declared with an explicit sunset clause tied to the success of structural reforms), coordination exists (advocates organize around the shared vocabulary), and asymmetric extraction is visible (advocates gain interpretive authority, practitioners lose burden-of-proof position).
 *
 * PERSPECTIVAL GAP:
 *   The divergence between advocate and practitioner seats is structural: advocates gain authority; practitioners lose burden-of-proof position. This is not resolvable by better measurement—it is the reading's core claim that measurement comes after normative judgment, so practitioners cannot win the dispute by gathering more data. The constraint's persistence depends on the typology remaining persuasive to policy audiences, not on empirical validation. From the advocate seat, this is enabling coordination; from the practitioner seat, this is enforced extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Policy advocates for structural reform are the primary beneficiaries (d near 0.0): they gain concentrated interpretive authority, control the frame, and have mobile exit (can shift to alternative vocabularies if this one loses power). Marginalized constituencies are secondary beneficiaries (d near 0.15): they gain rhetorical validation and vocabulary but do not control the frame and have trapped exit (must depend on advocates to deploy the vocabulary on their behalf). Institutional practitioners are the targets (d near 0.85): they bear the concentrated cost of defending against classifications made without measurement, cannot easily exit (institutions persist), and face constrained exit even if they disagree with the frame. Empiricists and alternative frameworks are excluded rather than classified (their directionality is undefined by the constraint itself, though they would experience d near 1.0 if included—treated as obstacles to the typology's deployment). The low suppression metric reflects that this reading does not require hiding alternatives; it explicitly proposes normativity as prior to measurement. The accessibility_collapse metric is moderate (0.41) because alternatives remain cognitively accessible (people can understand the empirical-first critique or the alternative vocabularies) but are structurally excluded from policy processes where the typology operates.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is explicitly a scaffold with a sunset clause tied to the success of structural reforms. The founding problem is real (institutions do resist accountability, empirical methodologies do demand proof before reform is attempted), but the constraint's solution—inverting the epistemic order so normative judgment precedes measurement—risks two failure modes: (1) if the typology's classifications become detached from actual institutional extraction, the constraint becomes a snare on its own advocates (persuasion replaces reform); (2) if marginalized constituencies gain rhetorical validation without material benefit, the constraint becomes a snare on them (symbolic victories substitute for structural change). Mandatrophy would be declared if the typology persists as a rhetorical frame after the structural reforms it was meant to enable have either succeeded (making the frame unnecessary) or failed (making it a zombie classification). The measurement series shows the constraint rising in extractiveness and theater while remaining constant in suppression, suggesting the typology is gaining persuasive power independent of whether actual institutional change is occurring. This pattern would trigger mandatrophy investigation: is the typology becoming an end in itself rather than a means to reform?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_precedence_empirical_validity,
    'Is a typology''s persuasive power a valid measure of its truth, or does normative declaration without prior measurement constitute methodological inversion?',
    'Comparative institutional analysis: track outcomes of policies derived from typology-based classification versus measurement-first classification over the same institutional domains. If the rhetorical approach produces better-targeted reforms, the normative-first order may be pragmatically justified; if empirical measurement-first approaches produce fewer unintended consequences, the methodological inversion becomes visible.',
    'If normative precedence proves robust, the rhetorical scaffold reading becomes epistemically legitimate and the typology is reclassified as a hybrid pragmatic/normative tool. If measurement-first proves more robust, the reading is revealed as inversion-dependent and becomes classified as a snare on its own advocates (methodological capture by normative commitment).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(normative_precedence_empirical_validity, empirical, 'Whether normative judgment can legitimately precede measurement in institutional classification.').

omega_variable(
    epistemic_authority_redistribution_sustainability,
    'Can a rhetorical vocabulary sustain interpretive authority once its normative commitments are made explicit, or does the persuasive power depend on concealing the normative choice as discovery?',
    'Monitor the typology''s rhetorical effectiveness across contexts where the normative commitments have been explicitly named (by critics) and where the commitments remain implicit. If effectiveness persists when commitments are explicit, the reading is sustainable; if it declines markedly, the reading depends on obscuring its own normativity.',
    'If sustainability requires implicit normativity, the constraint becomes a snare on its own framework (persuasion through concealment). If explicit normativity preserves effectiveness, the reading is robust and the low suppression metric is accurate—people accept the normative frame when they understand they are accepting it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_authority_redistribution_sustainability, empirical, 'Whether the rhetorical constraint depends on hiding its normative commitments.').

omega_variable(
    contested_kernel_reading_coherence,
    'Do the three readings of the deferential_realism_ontology kernel represent genuinely incommensurable epistemologies, or do they exist on a continuum with hybrid positions possible?',
    'Detailed structural comparison of the three readings'' core premises (discovery vs. declaration, empirical measurement vs. normative judgment, fixed referents vs. constructed classifications). If incommensurable, the forecloses relation holds; if on a continuum, coexists_with is more accurate and hybrid pragmatic positions become coherent.',
    'If incommensurable, the three readings partition the epistemic space and one reading''s success entails another''s failure. If on a continuum, the hybrid_pragmatic_reading becomes the dominant position and the rhetorical reading''s influence operates through persuasion rather than logical exclusion of alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contested_kernel_reading_coherence, conceptual, 'Whether the kernel''s contested readings are logically incommensurable or exist on an epistemological continuum.').

omega_variable(
    marginalized_constituencies_agency,
    'Do marginalized constituencies benefit from the typology''s rhetorical power, or are they instrumentalized by advocates who control the frame?',
    'Qualitative study of policy outcomes where the typology''s classification has driven remedial action: track whether margins constituents gain actual material benefit, expanded exit options, or increased decision-making power, versus merely gaining rhetorical validation while material conditions persist unchanged.',
    'If genuine material benefit accrues, the rhetorical scaffold is a legitimate tool of structural reform and the beneficiary designation is accurate. If rhetorical validation persists without material change, the constraint becomes a snare on the marginalized constituencies themselves (conversion of material claims into symbolic victories).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_constituencies_agency, empirical, 'Whether rhetorical validation through the typology translates to material benefit for marginalized constituencies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__rhetorical_scaffold_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement_basis(defe_tr_t0, projected).
narrative_ontology:measurement(defe_tr_t5, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 5, 0.58).
narrative_ontology:measurement_basis(defe_tr_t5, projected).
narrative_ontology:measurement(defe_tr_t10, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 10, 0.63).
narrative_ontology:measurement_basis(defe_tr_t10, observed).
narrative_ontology:measurement(defe_tr_t15, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 15, 0.66).
narrative_ontology:measurement_basis(defe_tr_t15, observed).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 20, 0.67).
narrative_ontology:measurement_basis(defe_tr_t20, observed).
narrative_ontology:measurement(defe_tr_t25, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 25, 0.68).
narrative_ontology:measurement_basis(defe_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement_basis(defe_be_t0, projected).
narrative_ontology:measurement(defe_be_t5, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 5, 0.71).
narrative_ontology:measurement_basis(defe_be_t5, projected).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 10, 0.76).
narrative_ontology:measurement_basis(defe_be_t10, observed).
narrative_ontology:measurement(defe_be_t15, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 15, 0.8).
narrative_ontology:measurement_basis(defe_be_t15, observed).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 20, 0.81).
narrative_ontology:measurement_basis(defe_be_t20, observed).
narrative_ontology:measurement(defe_be_t25, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 25, 0.82).
narrative_ontology:measurement_basis(defe_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(defe_su_t0, projected).
narrative_ontology:measurement(defe_su_t5, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 5, 0.25).
narrative_ontology:measurement_basis(defe_su_t5, projected).
narrative_ontology:measurement(defe_su_t10, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement_basis(defe_su_t10, observed).
narrative_ontology:measurement(defe_su_t15, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 15, 0.3).
narrative_ontology:measurement_basis(defe_su_t15, observed).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 20, 0.31).
narrative_ontology:measurement_basis(defe_su_t20, observed).
narrative_ontology:measurement(defe_su_t25, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 25, 0.31).
narrative_ontology:measurement_basis(defe_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__rhetorical_scaffold_reading, information_standard).
narrative_ontology:boltzmann_floor_override(deferential_realism_ontology__rhetorical_scaffold_reading, 0.25).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology__immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% The deferential_realism_ontology kernel decomposes into three structurally distinct constraints, each representing a different reading of what the typology IS. The rhetorical_scaffold_reading (this story) treats the typology as a normative vocabulary for declaring institutional illegitimacy; the immutable_diagnostic_reading treats it as an observational instrument for measuring institutional facts; the hybrid_pragmatic_reading treats it as having elements of both, with the boundary depending on context. Each reading has its own epsilon (extractiveness of the typology itself, not of the mechanisms it classifies), its own beneficiary/victim structure (who gains interpretive authority vs. who loses burden-of-proof position), and its own classification (scaffold vs. mountain vs. rope). The three readings compete for epistemic authority in policy discourse; the network links show they structurally influence each other (the success of one reading constrains the others) but do not logically foreclose each other (all three remain live options as different parties adopt different readings).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(deferential_realism_ontology__rhetorical_scaffold_reading, organized, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
