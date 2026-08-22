% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: us_constitution_meaning__positivist_reading
 *   human_readable: US Constitutional Validity — Positivist Reading
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   The positivist reading of US constitutional validity holds that
 *   constitutional law's authority derives exclusively from formal enactment
 *   procedures (Article V amendment, legislative enactment, judicial
 *   precedent) and the institutional authority of the bodies that perform
 *   them, not from external moral principles. This reading structures the
 *   constitutional order as a closed normative system: validity is a
 *   procedural status, not a moral achievement. The constraint coordinates
 *   governance by providing a determinate rule of recognition (what counts as
 *   constitutional law) while extracting authority from actors who would
 *   ground validity in moral reasoning. Over time, as formal amendment has
 *   become functionally impossible (no successful amendment since 1992, none
 *   addressing structural issues since 1971), the constraint has collapsed
 *   into originalism in practice: judges constrained by enacted text and
 *   original public meaning become the only viable validators, and moral
 *   reasoning is excluded not just from validity determination but from the
 *   interpretive process itself.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__positivist_reading, 0.28).
domain_priors:suppression_score(us_constitution_meaning__positivist_reading, 0.42).
domain_priors:theater_ratio(us_constitution_meaning__positivist_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__positivist_reading, "US Constitutional Validity — Positivist Reading").
narrative_ontology:topic_domain(us_constitution_meaning__positivist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__positivist_reading, '048d68fd-4e8c-4e4e-b06c-95325a4025c6').
narrative_ontology:cs_kernel_codification('048d68fd-4e8c-4e4e-b06c-95325a4025c6', formalized).
narrative_ontology:cs_authority_grounding('048d68fd-4e8c-4e4e-b06c-95325a4025c6', lineage).
narrative_ontology:cs_interpretation_layer_present('048d68fd-4e8c-4e4e-b06c-95325a4025c6').
narrative_ontology:cs_reading_relation('048d68fd-4e8c-4e4e-b06c-95325a4025c6', us_constitution_meaning__originalist_reading, influences).
narrative_ontology:cs_reading_relation('048d68fd-4e8c-4e4e-b06c-95325a4025c6', us_constitution_meaning__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_axiom('048d68fd-4e8c-4e4e-b06c-95325a4025c6', foundational, validity_from_enactment_procedure).
narrative_ontology:cs_axiom_status(validity_from_enactment_procedure, holdable).
narrative_ontology:cs_axiom_grounding('048d68fd-4e8c-4e4e-b06c-95325a4025c6', validity_from_enactment_procedure, conventional).
narrative_ontology:cs_axiom('048d68fd-4e8c-4e4e-b06c-95325a4025c6', foundational, moral_reasoning_excluded_from_validity).
narrative_ontology:cs_axiom_status(moral_reasoning_excluded_from_validity, holdable).
narrative_ontology:cs_axiom_grounding('048d68fd-4e8c-4e4e-b06c-95325a4025c6', moral_reasoning_excluded_from_validity, deontological).
narrative_ontology:cs_reference_frame('048d68fd-4e8c-4e4e-b06c-95325a4025c6', formal_enactment_proceduralism).
narrative_ontology:cs_drift_state('048d68fd-4e8c-4e4e-b06c-95325a4025c6', contemporary_originalist_dominance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('048d68fd-4e8c-4e4e-b06c-95325a4025c6', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__positivist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, procedural_legitimacy_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, institutional_authority_holders).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, substantive_justice_claimants).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, moral_reasoning_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, state_legislatures).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, originalist_judges_scholars).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, state_legislatures).
narrative_ontology:constraint_vindicates(us_constitution_meaning__positivist_reading, legal_positivism).
narrative_ontology:constraint_vindicates(us_constitution_meaning__positivist_reading, formal_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_meaning__positivist_reading, institutional_competence_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authoritatively interprets and applies constitutional text through formal adjudication procedures. Constrained by enacted text, precedent, and formal amendment process. Institutional role depends on maintaining procedural legitimacy as the exclusive validator of constitutional meaning.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Holds formal amendment power (Article V) and legislative authority to structure judicial review. Benefits from procedural legitimacy framework that validates its enactments through formal process rather than substantive moral evaluation. Exit constrained by institutional role but possesses political alternatives.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, congress, agenda_setter,
    institutional, biographical, mobile, national).

% Benefit from clear procedural rules that validate their sovereign authority within the federal system. Also bear costs when federal judicial review invalidates state enactments on procedural grounds. Exit constrained by constitutional structure but retain interstate competition and political alternatives.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, state_legislatures, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__positivist_reading, state_legislatures, payer).

% Advance claims grounded in moral principles (equality, dignity, autonomy) that lack explicit textual support in enacted constitutional provisions. Bear the cost of having such claims excluded from validity determination. Exit options limited to formal amendment (extremely difficult) or persuading judges to incorporate moral reasoning through interpretation.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, substantive_justice_claimants, payer,
    moderate, biographical, constrained, national).

% Academics, activists, and litigators whose professional identity and theoretical commitments are bound to the view that constitutional validity requires moral reasoning. The positivist constraint structurally excludes their framework from authoritative validation. Identity-locked because abandoning the moral-reasoning frame would dissolve their professional self-conception and advocacy strategy.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, moral_reasoning_advocates, payer,
    moderate, generational, identity_locked, national).

% Benefit from the positivist reading's emphasis on formal text and enactment procedures, which aligns with originalism's fixation on historical public meaning. In practice, the positivist constraint collapses into originalism when the amendment process is gridlocked, making originalist judges structural beneficiaries. Mobile exit: can shift to living constitutionalist frameworks if institutional incentives change.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, originalist_judges_scholars, beneficiary,
    organized, generational, mobile, national).

% Advocate for evolving constitutional application responsive to social attitudes and circumstances. The positivist reading's exclusion of moral reasoning from validity determination structurally marginalizes their framework. Constrained exit: institutional positions (judgeships, academic appointments) require operating within the dominant positivist/originalist paradigm.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, living_constitutionalist_judges_scholars, excluded,
    organized, generational, constrained, national).

% Analyze the structural dynamics of constitutional interpretation across frameworks. Neither collect rents nor bear costs from the constraint's operation. Provide the analytical seat from which the constraint's coordination and extraction functions can be assessed without institutional position.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, legal_theory_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, procedurally grounded framework for identifying valid constitutional law that enables coordinated governance across branches and levels of government without requiring consensus on contested moral principles.
% TRANSFER_FUNCTION: Transfers authority to determine constitutional validity from moral-reasoning actors (philosophers, activists, judges employing moral reasoning) to institutional actors operating through formal enactment and adjudication procedures. Moves the power to say 'what the Constitution means' from substantive justice claimants to procedural legitimacy holders.
% ABSENT_VOICES: Future generations whose constitutional rights claims may rest on moral principles not yet recognized in enacted text; marginalized communities whose historical exclusion from formal enactment processes means their justice claims lack textual footing; non-legal moral witnesses (religious leaders, philosophers, community elders) whose authority is structurally excluded from validity determination.
% DISAPPEARANCE_RATIONALE: If the positivist constraint vanished overnight, constitutional validity would become openly contestable on moral grounds. Judges would explicitly invoke moral principles as validity criteria, legislative supremacy would be challenged by substantive justice review, and the institutional settlement that makes constitutional law a distinct domain from moral philosophy would dissolve. The coordination function (determinate validity through procedure) would be lost; the extraction function (excluding moral-reasoning actors) would cease.
% FOUNDING_PROBLEM: The founding problem was establishing a government of laws, not men: creating a constitutional order where validity derives from identifiable, contestable procedures rather than the unconstrained moral judgment of rulers or judges. The positivist reading was built to solve the problem of arbitrary authority by anchoring validity in formal enactment and institutional process.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Federalist Papers (especially Federalist 78 on judicial role) and the constitutional text itself (Article V amendment process, supremacy clause). However, Anti-Federalist critiques (Brutus, Federal Farmer) and subsequent constitutional history (Reconstruction Amendments, Lochner era, Brown v. Board, substantive due process) corroborate that the founding problem was never purely procedural — the Constitution was understood by its framers and ratifiers to embody substantive moral commitments (liberty, equality, republican government) that constrain procedure. No single corroborating source outside the positivist tradition fully endorses the proceduralist reading; the corroboration is split across traditions.
narrative_ontology:disappearance_verdict(us_constitution_meaning__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__positivist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(us_constitution_meaning__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__positivist_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__positivist_reading_tests).
:- end_tests(us_constitution_meaning__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28) reflects the constraint's exclusion of moral-reasoning actors from authoritative validation — a real but moderate transfer of authority. Suppression (0.42) is moderate: moral-reasoning frameworks remain legally and culturally present (academic discourse, dissenting opinions, public advocacy) but are structurally excluded from the official validity calculus. Theater ratio (0.18) is low-moderate: the procedural framework performs genuine coordination (determinate law, institutional stability) but increasingly masks the collapse of the amendment safety valve. Accessibility collapse (0.35) is moderate: alternative validity frameworks (natural law, living constitutionalism, Dworkinian integrity) remain intellectually viable and culturally influential but cannot achieve authoritative validation. Resistance (0.52) is significant: the constraint faces persistent challenge from living constitutionalist judges, moral-reasoning scholars, and social movements demanding substantive justice.
 *
 * PERSPECTIVAL GAP:
 *   From the federal judiciary's seat, the constraint is genuine coordination: it provides the determinate rule of recognition that makes constitutional law possible as a distinct domain. From substantive justice claimants' seat, it is extraction: their moral claims are excluded by a procedural gate they cannot open. From originalists' seat, it is beneficial coordination (the constraint empowers their interpretive method). From living constitutionalists' seat, it is suppressive exclusion. The engine computes this seat divergence from the structural data; the authored claim (tangled_rope) reflects the structural reality that the constraint both coordinates (procedural determinacy) and extracts (excludes moral-reasoning actors).
 *
 * DIRECTIONALITY LOGIC:
 *   Federal judiciary and Congress are agenda-setters with institutional power and constrained exit — they administer and benefit from the procedural framework. State legislatures are dual-positioned: beneficiaries of clear federalism rules, payers when federal review invalidates their enactments. Substantive justice claimants and moral-reasoning advocates are payers with constrained/identity-locked exit — they bear the cost of exclusion from validity determination. Originalist judges/scholars are beneficiaries (the constraint's collapse into originalism serves their framework). Living constitutionalist judges/scholars are excluded — their framework is structurally marginalized. Legal theory observers hold the analytical seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (government of laws, not men) remains live but the positivist solution has developed mandatrophy: the procedural framework persists despite the amendment safety valve's failure. The constraint now primarily functions to entrench originalist interpretation rather than to enable democratic self-governance through formal amendment. The coordination function (determinate validity) is real but has been captured by the extraction function (entrenching a particular interpretive methodology). The constraint is a tangled_rope because it genuinely coordinates governance while asymmetrically extracting authority from moral-reasoning actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    positivist_originalist_collapse_boundary,
    'Is the positivist reading''s practical collapse into originalism under Article V gridlock a contingent historical development or a structural necessity of the positivist framework itself?',
    'Counterfactual analysis: if Article V remained functional (regular amendments addressing contested issues), would positivist judges still converge on originalist methodology? Comparative study of state constitutions with functional amendment processes.',
    'If structural necessity, the positivist reading is internally incoherent — it promises procedural determinacy but delivers originalist constraint. If contingent, the extraction of moral-reasoning authority is a reversible artifact of amendment failure, not a feature of the positivist commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positivist_originalist_collapse_boundary, conceptual, 'Whether the positivist→originalist collapse is structural or contingent.').

omega_variable(
    procedural_substantive_boundary,
    'Can the positivist reading''s distinction between procedural validity and substantive justice be maintained at the boundary cases (e.g., slavery, genocide, systemic oppression) where the enacted text itself embodies profound moral evil?',
    'Historical analysis of positivist responses to Nazi law (Radbruch formula), apartheid South Africa, and US slavery/segregation. Theoretical analysis of inclusive vs. exclusive legal positivism (Hart vs. Raz).',
    'If the boundary collapses (positivists concede moral limits to validity), the constraint''s extraction of moral-reasoning authority is partial and qualified — the constraint is less extractive than measured. If the boundary holds (validity purely procedural even for evil law), extraction is total and the constraint''s legitimacy claim is severely tested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(procedural_substantive_boundary, conceptual, 'Whether the procedural/substantive distinction holds at moral extremes.').

omega_variable(
    amendment_gridlock_extraction,
    'Does Article V''s functional gridlock transform the positivist constraint from a coordination mechanism (democratic self-governance through formal amendment) into an extraction mechanism (entrenching incumbent interpretive authority)?',
    'Empirical analysis of amendment frequency and success rates over time. Counterfactual: if the 27th Amendment (1992) and ERA failure are the only recent data, is gridlock structural or contingent? Comparative analysis with state constitutions.',
    'If gridlock is structural and permanent, the constraint''s coordination function is hollow — it coordinates only around the status quo, extracting authority from those who would change it through moral-political mobilization. If gridlock is contingent, the coordination function remains live and the extraction is temporary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_gridlock_extraction, empirical, 'Whether amendment gridlock hollows out the coordination function.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the kernel ''us_constitution_meaning'' admit a single coherent framing, or do the sibling readings instantiate fundamentally different kernels (validity vs. meaning vs. authority)?',
    'Analyze whether the three readings disagree on the same structural question (what makes constitutional law valid?) or on different questions (what does the text mean? vs. how should it evolve? vs. where does its authority come from?).',
    'If the kernel is underdetermined, the three ''readings'' are not readings of one kernel but constraints on different topics erroneously grouped. This would require decomposition into separate constraint families with distinct ε values and stakeholder structures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel framing itself is coherent across readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__positivist_reading, 1789, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1789, us_constitution_meaning__positivist_reading, theater_ratio, 1789, 0.05).
narrative_ontology:measurement(us_c_tr_t1803, us_constitution_meaning__positivist_reading, theater_ratio, 1803, 0.08).
narrative_ontology:measurement(us_c_tr_t1868, us_constitution_meaning__positivist_reading, theater_ratio, 1868, 0.12).
narrative_ontology:measurement(us_c_tr_t1905, us_constitution_meaning__positivist_reading, theater_ratio, 1905, 0.22).
narrative_ontology:measurement(us_c_tr_t1937, us_constitution_meaning__positivist_reading, theater_ratio, 1937, 0.15).
narrative_ontology:measurement(us_c_tr_t1973, us_constitution_meaning__positivist_reading, theater_ratio, 1973, 0.16).
narrative_ontology:measurement(us_c_tr_t2026, us_constitution_meaning__positivist_reading, theater_ratio, 2026, 0.18).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1789, us_constitution_meaning__positivist_reading, base_extractiveness, 1789, 0.15).
narrative_ontology:measurement(us_c_be_t1803, us_constitution_meaning__positivist_reading, base_extractiveness, 1803, 0.18).
narrative_ontology:measurement(us_c_be_t1868, us_constitution_meaning__positivist_reading, base_extractiveness, 1868, 0.22).
narrative_ontology:measurement(us_c_be_t1905, us_constitution_meaning__positivist_reading, base_extractiveness, 1905, 0.31).
narrative_ontology:measurement(us_c_be_t1937, us_constitution_meaning__positivist_reading, base_extractiveness, 1937, 0.24).
narrative_ontology:measurement(us_c_be_t1973, us_constitution_meaning__positivist_reading, base_extractiveness, 1973, 0.26).
narrative_ontology:measurement(us_c_be_t2026, us_constitution_meaning__positivist_reading, base_extractiveness, 2026, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1789, us_constitution_meaning__positivist_reading, suppression_requirement, 1789, 0.25).
narrative_ontology:measurement(us_c_su_t1803, us_constitution_meaning__positivist_reading, suppression_requirement, 1803, 0.3).
narrative_ontology:measurement(us_c_su_t1868, us_constitution_meaning__positivist_reading, suppression_requirement, 1868, 0.38).
narrative_ontology:measurement(us_c_su_t1905, us_constitution_meaning__positivist_reading, suppression_requirement, 1905, 0.45).
narrative_ontology:measurement(us_c_su_t1937, us_constitution_meaning__positivist_reading, suppression_requirement, 1937, 0.38).
narrative_ontology:measurement(us_c_su_t1973, us_constitution_meaning__positivist_reading, suppression_requirement, 1973, 0.4).
narrative_ontology:measurement(us_c_su_t2026, us_constitution_meaning__positivist_reading, suppression_requirement, 2026, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_meaning__positivist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, us_constitution_meaning__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, us_constitution_meaning__living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, article_v_amendment_process).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, judicial_review_institutional_authority).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, substantive_due_process_doctrine).

% DUAL FORMULATION NOTE:
% This constraint (positivist_reading) is one member of the us_constitution_meaning constraint family. The originalist_reading fixes meaning at ratification; the living_constitutionalist_reading allows evolving application. The positivist_reading fixes validity at formal enactment procedures. All three share the kernel 'us_constitution_meaning' but instantiate different ε values: originalist ε ≈ 0.35 (extracts from living constitutionalists and non-originalist moral reasoning), living constitutionalist ε ≈ 0.22 (extracts from originalists via evolving standards), positivist ε ≈ 0.28 (extracts from all moral-reasoning frameworks). The family is linked by network.affects_constraints in each story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_meaning__positivist_reading, institutional, 0.15).
constraint_indexing:directionality_override(us_constitution_meaning__positivist_reading, moderate, 0.75).
constraint_indexing:directionality_override(us_constitution_meaning__positivist_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
