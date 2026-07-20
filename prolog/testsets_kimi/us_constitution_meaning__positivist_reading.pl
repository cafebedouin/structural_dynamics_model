% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__positivist_reading, []).

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
 *   constraint_id: us_constitution_meaning__positivist_reading
 *   human_readable: Positivist Reading of Constitutional Validity
 *   domain: constitutional/law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the positivist reading of the US
 *   Constitution's meaning kernel: constitutional validity is exhaustively
 *   determined by formal enactment procedures (ratification, Article V
 *   amendment) and the institutional authority of the legal system, excluding
 *   external moral principles from validity determination. Judges are
 *   constrained to enforce only what has been textually enacted through
 *   proper procedure. In practice, when the amendment process gridlocks, this
 *   reading collapses into originalism as the only available source of
 *   constraint. The constraint coordinates judicial interpretation around a
 *   closed system of enacted rules while asymmetrically extracting from
 *   litigants whose substantive justice claims lack textual support. It is
 *   often presented as a Mountain of legal necessity but operates as a
 *   Tangled Rope of procedural legitimacy and substantive exclusion.
 *
 * KEY AGENTS:
 *   - federal_judiciary: Primary payer (institutional/constrained) â bound to exclude moral reasoning and decide on enacted text alone.
 *   - substantive_rights_litigants: Primary victim (moderate/constrained) â bear costs when their non-textual moral claims are ruled legally invalid.
 *   - government_enactors: Primary beneficiary (institutional/mobile) â their procedurally compliant enactments are shielded from moral override.
 *   - legal_positivist_academy: Agenda setter (organized/constrained) â trains judges and maintains the methodological boundary between law and morals.
 *   - natural_law_jurists: Excluded voice (moderate/constrained) â structurally marginalized in hiring and argumentation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__positivist_reading, 0.65).
domain_priors:suppression_score(us_constitution_meaning__positivist_reading, 0.58).
domain_priors:theater_ratio(us_constitution_meaning__positivist_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__positivist_reading, "Positivist Reading of Constitutional Validity").
narrative_ontology:topic_domain(us_constitution_meaning__positivist_reading, "constitutional/law/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__positivist_reading, 'd305441b-a5e0-41b4-97af-74a7cd1391b1').
narrative_ontology:cs_kernel_codification('d305441b-a5e0-41b4-97af-74a7cd1391b1', formalized).
narrative_ontology:cs_authority_grounding('d305441b-a5e0-41b4-97af-74a7cd1391b1', lineage).
narrative_ontology:cs_interpretation_layer_present('d305441b-a5e0-41b4-97af-74a7cd1391b1').
narrative_ontology:cs_reading_relation('d305441b-a5e0-41b4-97af-74a7cd1391b1', us_constitution_meaning__originalist_reading, influences).
narrative_ontology:cs_reading_relation('d305441b-a5e0-41b4-97af-74a7cd1391b1', us_constitution_meaning__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_axiom('d305441b-a5e0-41b4-97af-74a7cd1391b1', foundational, constitutional_validity_exhausted_by_enactment).
narrative_ontology:cs_axiom_status(constitutional_validity_exhausted_by_enactment, holdable).
narrative_ontology:cs_axiom_grounding('d305441b-a5e0-41b4-97af-74a7cd1391b1', constitutional_validity_exhausted_by_enactment, conventional).
narrative_ontology:cs_axiom('d305441b-a5e0-41b4-97af-74a7cd1391b1', foundational, moral_principles_irrelevant_to_judicial_validity_determination).
narrative_ontology:cs_axiom_status(moral_principles_irrelevant_to_judicial_validity_determination, holdable).
narrative_ontology:cs_axiom_grounding('d305441b-a5e0-41b4-97af-74a7cd1391b1', moral_principles_irrelevant_to_judicial_validity_determination, conventional).
narrative_ontology:cs_reference_frame('d305441b-a5e0-41b4-97af-74a7cd1391b1', formal_enactment_legitimacy).
narrative_ontology:cs_drift_state('d305441b-a5e0-41b4-97af-74a7cd1391b1', contemporary_constitutional_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d305441b-a5e0-41b4-97af-74a7cd1391b1', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__positivist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, government_enactors).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, substantive_rights_litigants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, federal_judiciary).
narrative_ontology:constraint_vindicates(us_constitution_meaning__positivist_reading, separation_of_law_and_morals_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federal judges are bound to decide constitutional questions by reference to enacted text and formal procedural pedigree alone. They must treat external moral principles as irrelevant to legal validity, even when those principles would support substantive justice. This limits their ability to invalidate laws on moral grounds and channels all constitutional change into the Article V amendment process.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, federal_judiciary, payer,
    institutional, generational, constrained, national).

% Individuals and groups bringing constitutional claims grounded in moral principlesâsuch as human dignity, privacy, or equalityâthat lack explicit textual or formal procedural support. Their claims are systemically disadvantaged because validity is determined by enactment history rather than moral merit.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, substantive_rights_litigants, payer,
    moderate, biographical, constrained, national).

% Legislative and executive actors whose enactments survive constitutional challenge so long as they comply with formal procedures. They are insulated from judicial override based on purely moral criticism, which stabilizes their policy outputs and reduces the risk of courts striking down properly enacted laws.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, government_enactors, beneficiary,
    institutional, generational, mobile, national).

% Law schools, bar examiners, and professional bodies that train judges and lawyers in the positivist framework. They maintain the methodological boundary between law and morals, control hiring and tenure, and treat constitutional law as a closed system of enacted rules and institutional pedigree.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, legal_positivist_academy, agenda_setter,
    organized, generational, constrained, national).

% Scholars and advocates who argue that unjust laws lack validity regardless of procedural pedigree. They are structurally marginalized in mainstream legal education, judicial appointment processes, and constitutional argumentation, and their frameworks are treated as jurisprudentially illegitimate.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, natural_law_jurists, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_meaning__positivist_reading, diffuse).
narrative_ontology:fixing_cost_class(us_constitution_meaning__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates constitutional interpretation across the federal judiciary by supplying a uniform, institutionally accepted test for legal validity: a constitutional norm is valid if and only if it derives from proper enactment procedures and is recognized by the legal system's institutional authority.
% TRANSFER_FUNCTION: Transfers authority from moral reasoning and substantive justice claims to formal enactment procedures and canonical text, moving interpretive power from litigants and individual judges to the constitutional text and the political branches that enacted it.
% ABSENT_VOICES: Natural law jurists and substantive rights advocates who argue that unjust or morally obsolete enactments lack binding force; they are excluded from mainstream legal education, judicial appointments, and standard constitutional argumentation.
% DISAPPEARANCE_RATIONALE: If the positivist constraint vanished overnight, judges would reintroduce moral reasoning into constitutional adjudication; the boundary between law and morals would dissolve, and the institutional legitimacy of properly enacted but substantively unjust laws would erode. Constitutional practice would reorganize around natural-law, originalist, or living-constitutionalist frameworks that permit extra-procedural sources of authority.
% FOUNDING_PROBLEM: How to distinguish valid law from moral opinion or political demand, ensuring that judicial enforcement is tied to democratically enacted text rather than subjective judicial philosophy.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and comparative constitutional scholars outside the positivist academy attest that the founding problem of legal certainty and democratic legitimacy was genuine in the early Republic. Critical legal theorists and natural law jurists from outside the beneficiary set attest that the problem has been superseded by conditions of amendment gridlock and entrenched injustice, making the arrangement a formalist cover story.
narrative_ontology:disappearance_verdict(us_constitution_meaning__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__positivist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_meaning__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__positivist_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__positivist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is substantial because the constraint systematically invalidates justice claims that fail formal textual capture. Suppression (0.58) is moderate-high: the legal academy, bar, and appointment processes actively enforce the exclusion of moral reasoning, though resistance persists in critical and natural law scholarship. Theater_ratio (0.52) is elevated because judicial opinions often perform textual fidelity while covertly relying on moral reasoning in hard cases, especially as amendment gridlock forces the positivist frame toward originalist practice. Accessibility_collapse (0.68) is high: once the positivist framework is accepted, natural law or moral reasoning appears as legally illegitimate. Resistance (0.42) is moderate: recurring critiques from substantive rights movements and marginalized jurisprudential traditions.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (legal positivist academy) and the beneficiary seat (government enactors) experience the constraint as necessary coordination of legal reasoning and legitimacy preservation. The payer seats (federal judiciary, substantive rights litigants) experience it as a rigid boundary that extracts judicial discretion and substantive justice. The engine will compute different per-seat classifications from these structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Government enactors are the structural beneficiary: their laws survive challenge if procedurally proper, so their directionality sits near the beneficiary end (low d). Substantive rights litigants are the structural victim: their claims fail for lack of textual or formal pedigree, so their directionality sits near the target end (high d). The federal judiciary is intermediate: they gain institutional legitimacy from the constraint but lose the power to do substantive justice; their d is near symmetric but slightly target-ward due to the identity-locked nature of judicial role.
 *
 * MANDATROPHY ANALYSIS:
 *   The positivist reading was built to solve the coordination problem of legal certainty and democratic legitimacyâdistinguishing law from morals so that enacted commands are knowable and stable. This founding problem is contested: government enactors and the legal academy claim it is still live, while substantive rights litigants and natural law jurists argue the problem has mutated into a rigid formalism that preserves injustice. The mismatch (founding_problem_status contested + disappearance_verdict world_rearranges) prevents automatic piton classification despite the drift toward originalism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    us_constitution_meaning_positivist_vs_siblings,
    'Does constitutional validity derive solely from formal enactment procedures, or do interpretive methodologies (originalist meaning-fixation or evolving moral principles) introduce additional, non-enacted sources of authority?',
    'Comparative doctrinal analysis tracking whether judicial opinions ground invalidation in procedural pedigree alone or in extra-textual moral or evolutionary claims; empirical study of judicial appointment criteria and confirmation testimony.',
    'If non-enacted sources are structurally necessary for constitutional practice, the positivist reading''s epsilon is higher than its coordination story suggests, and the constraint functions as a Tangled Rope masking substantive extraction through procedural form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_constitution_meaning_positivist_vs_siblings, conceptual, 'Kernel contest between positivist, originalist, and living constitutionalist readings of constitutional authority.').

omega_variable(
    positivism_originalism_collapse,
    'Has the positivist reading collapsed into originalism in practice, making it operationally indistinguishable from the originalist sibling reading?',
    'Examine judicial opinions and appointment records in amendment-gridlocked areas to determine whether positivist judges systematically resort to originalist meaning-fixation when procedural amendment is unavailable.',
    'If collapsed, the constraint story merges with originalism and its independent classification dissolves; if distinct, the positivist reading maintains a separate epsilon and victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positivism_originalism_collapse, empirical, 'Operational collapse of positivism into originalism under amendment gridlock.').

omega_variable(
    moral_exclusion_mechanism,
    'Is the exclusion of moral reasoning from constitutional validity a structural feature of legal practice or an aspirational claim contradicted by judicial behavior in hard cases?',
    'Quantitative content analysis of Supreme Court constitutional opinions for explicit or implicit moral reasoning in invalidation holdings; post-exit behavior of judges who leave the bench and disclose methodological constraints.',
    'If judges routinely smuggle moral reasoning back in, the constraint''s suppression metric overstates its effective force and the theater_ratio rises, indicating a higher extraction-to-coordination ratio than surface compliance suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_exclusion_mechanism, empirical, 'Whether moral reasoning is actually excluded or only formally excluded.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__positivist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_meaning__positivist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_meaning__positivist_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_meaning__positivist_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_meaning__positivist_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_meaning__positivist_reading, theater_ratio, 40, 0.5).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_meaning__positivist_reading, theater_ratio, 50, 0.52).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_meaning__positivist_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(us_c_be_t10, us_constitution_meaning__positivist_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(us_c_be_t20, us_constitution_meaning__positivist_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(us_c_be_t30, us_constitution_meaning__positivist_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(us_c_be_t40, us_constitution_meaning__positivist_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(us_c_be_t50, us_constitution_meaning__positivist_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_meaning__positivist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(us_c_su_t10, us_constitution_meaning__positivist_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(us_c_su_t20, us_constitution_meaning__positivist_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(us_c_su_t30, us_constitution_meaning__positivist_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(us_c_su_t40, us_constitution_meaning__positivist_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(us_c_su_t50, us_constitution_meaning__positivist_reading, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, us_constitution_meaning__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, us_constitution_meaning__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is the positivist reading of the us_constitution_meaning kernel, decomposed from originalist and living constitutionalist readings per the epsilon-invariance principle. Each reading has a distinct epsilon, beneficiary structure, and classification. The positivist reading's authority derives from enactment procedure, whereas the originalist reading's derives from historical public meaning and the living constitutionalist reading's from evolving social norms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
