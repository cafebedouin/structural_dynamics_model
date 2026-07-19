% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__positivist_reading, []).

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
 *   constraint_id: constitutional_text_authority__positivist_reading
 *   human_readable: Positivist Constitutional Validity Doctrine
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   This constraint story instantiates the positivist reading of
 *   constitutional text authority: the claim that constitutional validity
 *   derives exclusively from formal enactment procedures and institutional
 *   sources, maintaining a strict separation between law and morality. As one
 *   reading of the contested constitutional_text_authority kernel, it
 *   competes with originalist and living constitutionalist readings. The
 *   positivist reading coordinates legal actors around a procedural validity
 *   test but systematically extracts epistemic standing from natural law
 *   adherents and moral reformers by declaring their arguments irrelevant to
 *   constitutional validity. The claim is tangled_rope â genuine
 *   coordination function paired with asymmetric exclusion of moral
 *   reasoning.
 *
 * KEY AGENTS:
 *   - federal_judiciary (institutional/agenda-setter): enforces the positivist validity framework through judicial doctrine and precedent
 *   - state_and_federal_legislatures (institutional/beneficiary): outputs insulated from moral challenge by procedural validity
 *   - legal_profession (organized/beneficiary): gains epistemic autonomy and professional boundary from law/morality separation
 *   - moral_reform_movements (moderate/payer): must translate moral claims into formal legal arguments; structurally disadvantaged
 *   - natural_law_jurists (moderate/payer): excluded from mainstream constitutional validity discourse
 *   - constitutional_scholars (analytical/observer): document the gap between positivist doctrine and practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__positivist_reading, 0.52).
domain_priors:suppression_score(constitutional_text_authority__positivist_reading, 0.48).
domain_priors:theater_ratio(constitutional_text_authority__positivist_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__positivist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__positivist_reading, "Positivist Constitutional Validity Doctrine").
narrative_ontology:topic_domain(constitutional_text_authority__positivist_reading, "legal/constitutional").

domain_priors:requires_active_enforcement(constitutional_text_authority__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__positivist_reading, '9491c0fc-7a82-4802-9c89-af5afeb851d9').
narrative_ontology:cs_kernel_codification('9491c0fc-7a82-4802-9c89-af5afeb851d9', formalized).
narrative_ontology:cs_authority_grounding('9491c0fc-7a82-4802-9c89-af5afeb851d9', lineage).
narrative_ontology:cs_interpretation_layer_present('9491c0fc-7a82-4802-9c89-af5afeb851d9').
narrative_ontology:cs_reading_relation('9491c0fc-7a82-4802-9c89-af5afeb851d9', constitutional_text_authority__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9491c0fc-7a82-4802-9c89-af5afeb851d9', constitutional_text_authority__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_axiom('9491c0fc-7a82-4802-9c89-af5afeb851d9', foundational, validity_from_enactment_procedure_only).
narrative_ontology:cs_axiom_status(validity_from_enactment_procedure_only, holdable).
narrative_ontology:cs_axiom_grounding('9491c0fc-7a82-4802-9c89-af5afeb851d9', validity_from_enactment_procedure_only, conventional).
narrative_ontology:cs_axiom('9491c0fc-7a82-4802-9c89-af5afeb851d9', foundational, law_morality_separation).
narrative_ontology:cs_axiom_status(law_morality_separation, holdable).
narrative_ontology:cs_axiom_grounding('9491c0fc-7a82-4802-9c89-af5afeb851d9', law_morality_separation, conventional).
narrative_ontology:cs_reference_frame('9491c0fc-7a82-4802-9c89-af5afeb851d9', procedural_validity_supremacy).
narrative_ontology:cs_drift_state('9491c0fc-7a82-4802-9c89-af5afeb851d9', contemporary_rights_jurisprudence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9491c0fc-7a82-4802-9c89-af5afeb851d9', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__positivist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, state_and_federal_legislatures).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, legal_profession).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, moral_reform_movements).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, natural_law_jurists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and applies constitutional provisions under the positivist frame, treating validity as established by enactment pedigree and institutional source rather than moral argument. Bound by precedent, bar norms, and the institutional need to present decisions as legally determined rather than morally chosen.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__positivist_reading, federal_judiciary, beneficiary).

% Exercise lawmaking authority whose outputs are treated as constitutionally valid provided they follow correct enactment procedures, insulating their work from judicial override on moral grounds alone.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, state_and_federal_legislatures, beneficiary,
    institutional, generational, constrained, national).

% Operates within a professional domain bounded by formal sources and procedures; gains epistemic authority and autonomy by treating law as a technical field separable from moral philosophy. Professional training and bar identity lock members into the positivist frame.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, legal_profession, beneficiary,
    organized, biographical, constrained, national).

% Seek constitutional change or invalidation of laws on moral grounds such as justice, equality, or human rights. Their arguments are structurally recast as policy preferences or political claims rather than legal arguments, forcing them to find procedural hooks or textual anchors to gain traction.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, moral_reform_movements, payer,
    moderate, biographical, constrained, national).

% Advance theories that ground constitutional validity in moral truth or natural justice. Their scholarship and jurisprudential tradition are systematically excluded from mainstream constitutional validity discourse and treated as philosophy rather than law within the dominant institutional framework.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, natural_law_jurists, payer,
    moderate, generational, identity_locked, national).

% Analyze and critique the positivist framework from comparative and historical perspectives. They document the drift between positivist doctrine and actual judicial practice, tracing the kernel's contested readings without being bound to enforce any single reading.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, procedurally verifiable criterion for constitutional validity that enables legal predictability and resolves interpretive disagreement without requiring resolution of underlying moral controversies.
% TRANSFER_FUNCTION: Moves authority over constitutional meaning from moral argumentation and natural law reasoning to formal institutional enactment procedures; moves the costs of legal indeterminacy onto actors who lack access to formal lawmaking channels or whose claims are grounded in moral principle rather than textual pedigree.
% ABSENT_VOICES: Natural law jurists, moral reformers, and religious authorities who would ground constitutional validity in divine or natural moral order are structurally excluded; their arguments are treated as policy or philosophy rather than law. Critical race and feminist theorists who argue that formal neutrality masks substantive injustice are similarly marginalized in validity discourse.
% DISAPPEARANCE_RATIONALE: If the positivist constraint vanished, courts would lose the doctrinal boundary that separates legal interpretation from moral reasoning; constitutional litigation would openly become a contest over moral values, legislatures would face direct challenges to their authority based on justice rather than procedure, and the legal profession's claim to technical autonomy would collapse.
% FOUNDING_PROBLEM: How to identify valid constitutional law and resolve interpretive disagreement in a morally pluralistic society without empowering judges or officials to impose their own moral views.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the benefiting legal profession attest that formalism emerged to manage class conflict and religious pluralism; critical legal scholars attest the problem was never solved but only displaced into proceduralism. The legal profession's own historical narrative corroborates the pluralism-management origin, though it evaluates the outcome positively.
narrative_ontology:disappearance_verdict(constitutional_text_authority__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__positivist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-19',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text_authority__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__positivist_reading, 0.52, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__positivist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text_authority__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text_authority__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) reflects the systematic channeling of constitutional authority toward formal institutional actors and away from moral-reasoning constituencies. Suppression (0.48) captures the doctrinal exclusion of natural law and moral argument from validity discourse â not physical coercion but structural silencing. Theater ratio (0.32) registers the performative dimension of legal formalism, wherein judges present morally laden decisions as procedurally determined. The measurement series track the entrenchment of legal positivism from the early republic through the twentieth century, with enforcement peaking in the legal-process era and slightly moderating under contemporary substantive-rights challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary and legal profession, the positivist constraint is a necessary coordination mechanism that preserves legal predictability and democratic legitimacy by binding interpreters to enacted text. From moral reform movements and natural law theorists, it is an asymmetric barrier that extracts constitutional standing from morally grounded claims and deposits it in procedural formalities. The engine computes this divergence from the structural data: institutional seats with constrained-but-secure exit map as beneficiaries, while seats locked out of the validity framework map as targets.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal judiciary and legislatures sit near the beneficiary end: the constraint subsidizes their institutional authority by providing a non-moral legitimacy formula. Legal profession sits slightly further out but still beneficiary-side: it gains professional autonomy but pays the cost of internalizing formalist constraints. Moral reform movements and natural law jurists sit near the target end: the constraint extracts their ability to press constitutional claims directly on moral grounds, forcing costly translation into formal legal categories. The directionality is structurally derived from beneficiary/victim declarations and exit modulation â institutional actors have constrained exit (bound by role) but are subsidized by the constraint, while moral-reasoning actors are identity-locked or trapped in an epistemic framework that discounts their core modality.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope rather than snare preserves the genuine coordination function: a legal system does need some procedure for identifying valid law, and positivism provides a relatively transparent one. It does not collapse into pure extraction because the constraint is not merely cover for institutional power â it solves a real collective-action problem (moral pluralism). However, the asymmetric exclusion of moral reasoning from validity discourse, and the insulation of legislative outputs from justice-based challenge, supplies the extraction component that prevents classification as pure rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    positivist_reading_kernel_position,
    'Is the positivist reading of constitutional authority a genuine procedural coordination mechanism or a foreclosure device against moral constitutionalism?',
    'Comparative analysis of jurisdictions with stronger natural law traditions versus strict positivist jurisdictions; measuring whether the positivist constraint reduces or merely displaces moral disagreement.',
    'If purely coordinative, the classification edges toward rope; if primarily a foreclosure device against living constitutionalism, it edges toward tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positivist_reading_kernel_position, conceptual, 'Coordination versus foreclosure function of positivist validity').

omega_variable(
    naturalness_of_law_morality_distinction,
    'Does the law/morality distinction reflect a necessary structural feature of legal systems, or is it a constructed professional boundary that benefits formal legal institutions?',
    'Cross-cultural legal anthropology comparing legal systems with and without strict law/morality separations; historical sociology of the legal profession.',
    'If natural and structural, directionality for legal institutions is lower; if constructed, the constraint functions as institutional extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_law_morality_distinction, empirical, 'Natural versus constructed status of the law-morality boundary').

omega_variable(
    positivist_authority_framing,
    'Is the positivist constraint better framed as authority grounded in the constitutional text''s formal enactment, or in the legal profession''s social practice of recognition?',
    'Internal jurisprudential analysis of whether positivist validity claims collapse into sociological claims about lawyer behavior.',
    'Text-framing strengthens lineage authority; practice-framing shifts authority to the legal profession as beneficiary and may alter directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positivist_authority_framing, conceptual, 'Alternative framing of positivist authority grounding').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__positivist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text_authority__positivist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t20, constitutional_text_authority__positivist_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(cons_tr_t40, constitutional_text_authority__positivist_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(cons_tr_t60, constitutional_text_authority__positivist_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement(cons_tr_t80, constitutional_text_authority__positivist_reading, theater_ratio, 80, 0.32).
narrative_ontology:measurement(cons_tr_t100, constitutional_text_authority__positivist_reading, theater_ratio, 100, 0.32).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text_authority__positivist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cons_be_t20, constitutional_text_authority__positivist_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(cons_be_t40, constitutional_text_authority__positivist_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(cons_be_t60, constitutional_text_authority__positivist_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(cons_be_t80, constitutional_text_authority__positivist_reading, base_extractiveness, 80, 0.54).
narrative_ontology:measurement(cons_be_t100, constitutional_text_authority__positivist_reading, base_extractiveness, 100, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text_authority__positivist_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(cons_su_t20, constitutional_text_authority__positivist_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(cons_su_t40, constitutional_text_authority__positivist_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement(cons_su_t60, constitutional_text_authority__positivist_reading, suppression_requirement, 60, 0.48).
narrative_ontology:measurement(cons_su_t80, constitutional_text_authority__positivist_reading, suppression_requirement, 80, 0.48).
narrative_ontology:measurement(cons_su_t100, constitutional_text_authority__positivist_reading, suppression_requirement, 100, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, originalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the constitutional_text_authority kernel, decomposed from the colloquial label 'constitutional authority' per the epsilon-invariance principle. The positivist reading differs structurally from originalist and living constitutionalist readings in its source of validity (enactment procedure vs. historical meaning vs. contemporary values), producing a distinct beneficiary/victim structure and epsilon profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
