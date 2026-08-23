% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__living_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__living_reading, []).

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
 *   constraint_id: us_constitution_1787__living_reading
 *   human_readable: Living Constitutionalism — Evolving Meaning Framework
 *   domain: legal/political/constitutional
 *
 * SUMMARY:
 *   Living constitutionalism holds that the Constitution's meaning evolves
 *   with society and that its text provides an aspirational framework rather
 *   than fixed rules. This reading generates an expanding constraint set —
 *   incorporating privacy, dignity, and other unenumerated rights — while
 *   lowering epistemic demands (no need to reconstruct 1787 understandings).
 *   However, it creates vulnerability to elite capture: the 'evolving norms'
 *   that courts identify tend to track the preferences of the legal academy
 *   and progressive policy networks rather than broad democratic consensus.
 *   The constraint is claimed as tangled_rope because it performs genuine
 *   coordination (stable but adaptable governance) while extracting policy
 *   authority from democratic majorities to judicial elites.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__living_reading, 0.58).
domain_priors:suppression_score(us_constitution_1787__living_reading, 0.42).
domain_priors:theater_ratio(us_constitution_1787__living_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__living_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__living_reading, "Living Constitutionalism — Evolving Meaning Framework").
narrative_ontology:topic_domain(us_constitution_1787__living_reading, "legal/political/constitutional").

domain_priors:requires_active_enforcement(us_constitution_1787__living_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__living_reading, 'ac2697bf-8d49-4f8b-ba35-c1753603b49b').
narrative_ontology:cs_kernel_codification('ac2697bf-8d49-4f8b-ba35-c1753603b49b', fixed_text).
narrative_ontology:cs_authority_grounding('ac2697bf-8d49-4f8b-ba35-c1753603b49b', lineage).
narrative_ontology:cs_interpretation_layer_present('ac2697bf-8d49-4f8b-ba35-c1753603b49b').
narrative_ontology:cs_reading_relation('ac2697bf-8d49-4f8b-ba35-c1753603b49b', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ac2697bf-8d49-4f8b-ba35-c1753603b49b', us_constitution_1787__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('ac2697bf-8d49-4f8b-ba35-c1753603b49b', foundational, aspirational_framework_principle).
narrative_ontology:cs_axiom_status(aspirational_framework_principle, holdable).
narrative_ontology:cs_axiom_grounding('ac2697bf-8d49-4f8b-ba35-c1753603b49b', aspirational_framework_principle, conventional).
narrative_ontology:cs_axiom('ac2697bf-8d49-4f8b-ba35-c1753603b49b', secondary, judicial_role_in_identifying_evolving_norms).
narrative_ontology:cs_axiom_status(judicial_role_in_identifying_evolving_norms, holdable).
narrative_ontology:cs_axiom_grounding('ac2697bf-8d49-4f8b-ba35-c1753603b49b', judicial_role_in_identifying_evolving_norms, conventional).
narrative_ontology:cs_reference_frame('ac2697bf-8d49-4f8b-ba35-c1753603b49b', aspirational_constitutional_framework).
narrative_ontology:cs_drift_state('ac2697bf-8d49-4f8b-ba35-c1753603b49b', contemporary_rights_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ac2697bf-8d49-4f8b-ba35-c1753603b49b', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__living_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, progressive_policy_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, legal_academy).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, democratic_majorities).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, originalist_constrained_officials).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, states_subject_to_evolving_norms).
narrative_ontology:constraint_vindicates(us_constitution_1787__living_reading, constitutional_adaptability_principle).
narrative_ontology:constraint_vindicates(us_constitution_1787__living_reading, living_document_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and applies the Constitution through evolving standards; identifies 'evolving norms' and expands rights catalog. Justifies this as fidelity to the Constitution's aspirational character. Holds final say on constitutional meaning in practice.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, federal_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Gain constitutional recognition for new rights claims (privacy, dignity, equality expansions) without needing formal amendment. Use litigation to advance policy goals that lack legislative majorities. Benefit from lower epistemic demands of evolving standards vs. originalist fixed meaning.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, progressive_policy_advocates, beneficiary,
    organized, biographical, mobile, national).

% Produces the theoretical frameworks and 'evolving norms' scholarship that courts cite. Gains professional prestige and influence as the expert class that identifies societal evolution. Career advancement tied to living constitutionalist methodology.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, legal_academy, beneficiary,
    organized, generational, mobile, national).

% See policy preferences overridden by judicial identification of 'evolving norms' that lack democratic ratification. Cannot easily exit — constitutional decisions bind nationally; amending the Constitution is prohibitively difficult. Bear the cost of unelected judges making value choices.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, democratic_majorities, payer,
    powerless, biographical, constrained, national).

% Officials (legislators, executives, lower-court judges) who accept originalist constraint but must operate under living constitutionalist precedent. Professional identity fused to rule-of-law fidelity; exit means abandoning judicial role or accepting illegitimate authority. Bear compliance costs for interpretations they regard as unmoored.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, originalist_constrained_officials, payer,
    moderate, biographical, identity_locked, national).

% State governments subjected to newly declared federal constitutional rights that displace state policy choices. Exit requires constitutional amendment or Supreme Court reversal — both structurally difficult. Bear costs of nationalized rights standards that may not reflect local conditions or values.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, states_subject_to_evolving_norms, payer,
    moderate, generational, constrained, national).

% Advocate fixed-meaning interpretation; would object to evolving standards as judicial usurpation. Structurally excluded from setting the agenda when living constitutionalism dominates; their framework treated as illegitimate by the prevailing interpretive community.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, originalist_judges_scholars, excluded,
    organized, generational, identity_locked, national).

% Analyze living constitutionalism as a global phenomenon — common law constitutionalism, proportionality review, transformative constitutionalism. See U.S. version as one variant; track convergence/divergence with other systems.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable but adaptable constitutional framework that enables governance across changing social conditions without requiring constant formal amendment; coordinates expectations about rights and powers across generations.
% TRANSFER_FUNCTION: Moves interpretive authority and policy-making power from democratic majorities and their representatives to the federal judiciary (especially the Supreme Court), which identifies and enforces 'evolving norms' as constitutional mandates.
% ABSENT_VOICES: Originalist judges and scholars are structurally excluded from the interpretive agenda when living constitutionalism dominates; democratic majorities whose policy preferences are overridden by judicially identified 'evolving norms' have no institutional voice in the interpretation process. Both would object if present.
% DISAPPEARANCE_RATIONALE: If living constitutionalism vanished overnight, the Supreme Court would revert to originalist or textualist methodology; dozens of recognized rights (privacy, dignity, unenumerated equality claims) would lose constitutional footing; legislative majorities would regain policy space; the entire architecture of modern constitutional law would restructure around fixed meaning.
% FOUNDING_PROBLEM: The Founding generation created a Constitution with broad aspirational language ('due process,' 'equal protection,' 'cruel and unusual') that they knew would need to address unforeseen circumstances. The problem was how to keep a 1787 document legitimate and functional across centuries of social transformation without constant formal amendment.
% FOUNDING_PROBLEM_CORROBORATION: The aspirational-framework justification is attested by the Founders' own use of open-ended language and by early Marshall Court opinions (McCulloch v. Maryland) endorsing adaptive interpretation. However, the specific modern form — judicial identification of 'evolving norms' — is contested: originalist scholars (Scalia, Bork, Barnett) and historians (Rakove, Wood) argue the Founders expected amendment, not judicial evolution, to handle change. No consensus exists outside the benefiting parties (judiciary, legal academy, progressive advocates).
narrative_ontology:disappearance_verdict(us_constitution_1787__living_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__living_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__living_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_1787__living_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__living_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__living_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_1787__living_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_1787__living_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) reflects the transfer of policy authority from legislatures to courts via unenumerated rights. Suppression (0.42) is moderate — democratic majorities can theoretically amend the Constitution or appoint different judges, but structural barriers are high. Theater ratio (0.38) captures the performative aspect: opinions cite 'evolving standards of decency' and international law while the actual driver is often the justices' own normative preferences. Accessibility collapse (0.45) and resistance (0.55) reflect that alternatives (originalism, textualism) persist and contest the framework but cannot displace it from the institutional summit.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat, living constitutionalism is coordination — solving the problem of governing with an old text. From democratic majorities' seat, it is extraction — policy preferences overridden by unelected judges. From originalist officials' seat, it is a snare — a constraint they reject but must obey. The engine computes these per-seat types from the structural data; the claimed_type (tangled_rope) reflects the authoring seat's structural judgment that BOTH coordination and extraction are genuinely present.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal judiciary (agenda_setter) sits at d ≈ 0.15 — full beneficiary of interpretive monopoly. Progressive advocates and legal academy (beneficiaries) at d ≈ 0.25-0.35 — gain policy wins without democratic cost. Democratic majorities (payer, powerless) at d ≈ 0.85 — trapped, no exit, bear full extraction. Originalist officials (payer, identity_locked) at d ≈ 0.8 — professional identity fused to originalism makes exit professionally destructive. States (payer, moderate) at d ≈ 0.65 — constrained exit via federalism but ultimately bound by supremacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (adapting an 1787 document to modern conditions) remains live — the Constitution has not been amended since 1992, and social change continues. However, the LIVING CONSTITUTIONALIST SOLUTION may have outlived its coordination function: the 'evolving norms' methodology now primarily serves judicial preference-formation rather than democratic adaptation. The mandatrophy question is whether the coordination justification still holds or whether the arrangement persists as judicial self-empowerment. The founding_problem_status = live but the specific reading's legitimacy is contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is living constitutionalism a genuine reading of the 1787 Constitution''s aspirational language, or a distinct constraint that merely claims the Constitution''s authority?',
    'Comparative analysis of Founding-era statements on constitutional change (amendment vs. judicial adaptation) and early judicial practice. If the Founders expected judicial evolution, the reading is continuous with the kernel; if they expected only amendment, it is a distinct constraint parasitically using the kernel''s authority.',
    'If distinct constraint, the extraction measured here is not coordination-adaptation but pure judicial usurpation — reclassification toward snare. If genuine reading, tangled_rope stands: genuine coordination function (adaptation) coexists with asymmetric extraction (elite capture).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this reading is continuous with the kernel or a parasitic constraint').

omega_variable(
    elite_capture_of_evolving_norms,
    'Do the ''evolving norms'' identified by courts track actual societal consensus or the preferences of the legal-academic-progressive network?',
    'Empirical study of the correlation between Court-identified ''evolving standards'' and (a) public opinion polls, (b) state legislative trends, (c) law review citation networks, (d) elite bar association positions. Divergence from (a) and (b) toward (c) and (d) indicates capture.',
    'If capture is systematic, the coordination function is largely performative — the constraint extracts for a narrow elite under cover of societal evolution. Extraction would be higher than measured; theater ratio would rise. If norms track genuine consensus, coordination function is real and extraction is the cost of democratic lag.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(elite_capture_of_evolving_norms, empirical, 'Whether the evolving-norms methodology is captured by a narrow interpretive community').

omega_variable(
    democratic_legitimacy_deficit,
    'Can a constraint that systematically overrides democratic majorities on value questions maintain legitimacy without formal democratic ratification?',
    'Longitudinal legitimacy surveys, compliance rates with unpopular decisions, Court-packing threats and institutional responses. Track whether the constraint''s persistence depends on the Court''s perceived legitimacy or on the inability of opponents to alter the arrangement.',
    'If legitimacy depends on perceived legitimacy (which fluctuates), the constraint is unstable — a legitimacy crisis could trigger reclassification or structural reform. If persistence is structural (opponents cannot amend or appoint), the constraint is a snare regardless of its coordination claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_legitimacy_deficit, preference, 'Whether the constraint''s persistence is legitimacy-dependent or structurally locked').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of originalist/democratic alternatives structural (Court control of precedent, appointment politics) or internalized (legal profession socialization, professional identity fusion)?',
    'Track originalist judges'' behavior: do they apply living constitutionalist precedent because they must (structural) or because they have internalized the methodology? Measure law school curriculum dominance and clerkship pipeline effects.',
    'If internalized, suppression is higher than measured — the constraint carries its enforcement inside the agents. If purely structural, suppression could drop rapidly with appointment changes. Affects the stability of the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression of alternative interpretive frameworks is structural or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__living_reading, 1787, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1787, us_constitution_1787__living_reading, theater_ratio, 1787, 0.05).
narrative_ontology:measurement(us_c_tr_t1868, us_constitution_1787__living_reading, theater_ratio, 1868, 0.12).
narrative_ontology:measurement(us_c_tr_t1937, us_constitution_1787__living_reading, theater_ratio, 1937, 0.25).
narrative_ontology:measurement(us_c_tr_t1954, us_constitution_1787__living_reading, theater_ratio, 1954, 0.3).
narrative_ontology:measurement(us_c_tr_t1973, us_constitution_1787__living_reading, theater_ratio, 1973, 0.38).
narrative_ontology:measurement(us_c_tr_t1992, us_constitution_1787__living_reading, theater_ratio, 1992, 0.35).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_1787__living_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1787, us_constitution_1787__living_reading, base_extractiveness, 1787, 0.15).
narrative_ontology:measurement(us_c_be_t1868, us_constitution_1787__living_reading, base_extractiveness, 1868, 0.22).
narrative_ontology:measurement(us_c_be_t1937, us_constitution_1787__living_reading, base_extractiveness, 1937, 0.35).
narrative_ontology:measurement(us_c_be_t1954, us_constitution_1787__living_reading, base_extractiveness, 1954, 0.42).
narrative_ontology:measurement(us_c_be_t1973, us_constitution_1787__living_reading, base_extractiveness, 1973, 0.55).
narrative_ontology:measurement(us_c_be_t1992, us_constitution_1787__living_reading, base_extractiveness, 1992, 0.52).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_1787__living_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1787, us_constitution_1787__living_reading, suppression_requirement, 1787, 0.1).
narrative_ontology:measurement(us_c_su_t1868, us_constitution_1787__living_reading, suppression_requirement, 1868, 0.2).
narrative_ontology:measurement(us_c_su_t1937, us_constitution_1787__living_reading, suppression_requirement, 1937, 0.35).
narrative_ontology:measurement(us_c_su_t1954, us_constitution_1787__living_reading, suppression_requirement, 1954, 0.4).
narrative_ontology:measurement(us_c_su_t1973, us_constitution_1787__living_reading, suppression_requirement, 1973, 0.45).
narrative_ontology:measurement(us_c_su_t1992, us_constitution_1787__living_reading, suppression_requirement, 1992, 0.4).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_1787__living_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__living_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(us_constitution_1787__living_reading, 0.08).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__positivist_reading).

% DUAL FORMULATION NOTE:
% This living_reading, originalist_reading, and positivist_reading form a constraint family decomposing the kernel 'us_constitution_1787'. The living_reading expands the constraint set over time (higher ε); originalist_reading freezes ε at founding; positivist_reading limits ε to text + formal amendment. All three claim the same kernel authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_1787__living_reading, powerless, 0.85).
constraint_indexing:directionality_override(us_constitution_1787__living_reading, moderate, 0.65).
constraint_indexing:directionality_override(us_constitution_1787__living_reading, organized, 0.25).
constraint_indexing:directionality_override(us_constitution_1787__living_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
