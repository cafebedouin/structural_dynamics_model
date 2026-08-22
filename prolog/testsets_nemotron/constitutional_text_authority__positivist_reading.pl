% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: constitutional_text_authority__positivist_reading
 *   human_readable: Constitutional Text Authority — Positivist Reading
 *   domain: constitutional_law/legal_theory/interpretive_jurisprudence
 *
 * SUMMARY:
 *   This constraint story captures the positivist reading of constitutional
 *   authority: validity derives exclusively from formal enactment procedures
 *   and institutional sources, not from moral content. The law/morality
 *   distinction is maintained as a structural feature of the legal system.
 *   The constraint operates as a procedural gate — it coordinates legal
 *   practice by giving officials a clear, non-moral criterion for validity,
 *   but it extracts from those whose constitutional claims depend on moral
 *   reasoning (natural law advocates, moral-reading judges, rights
 *   claimants). The constraint has hardened over the post-war period as
 *   formalist doctrine consolidated in academia and courts, with rising
 *   theater as the procedural criterion is ritualistically invoked while
 *   substantive outcomes increasingly track political appointments.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__positivist_reading, 0.42).
domain_priors:suppression_score(constitutional_text_authority__positivist_reading, 0.58).
domain_priors:theater_ratio(constitutional_text_authority__positivist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__positivist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__positivist_reading, "Constitutional Text Authority — Positivist Reading").
narrative_ontology:topic_domain(constitutional_text_authority__positivist_reading, "constitutional_law/legal_theory/interpretive_jurisprudence").

domain_priors:requires_active_enforcement(constitutional_text_authority__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__positivist_reading, '14825a94-69a0-443b-842d-a75cd5a03d31').
narrative_ontology:cs_kernel_codification('14825a94-69a0-443b-842d-a75cd5a03d31', formalized).
narrative_ontology:cs_authority_grounding('14825a94-69a0-443b-842d-a75cd5a03d31', lineage).
narrative_ontology:cs_interpretation_layer_present('14825a94-69a0-443b-842d-a75cd5a03d31').
narrative_ontology:cs_reading_relation('14825a94-69a0-443b-842d-a75cd5a03d31', constitutional_text_authority__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('14825a94-69a0-443b-842d-a75cd5a03d31', constitutional_text_authority__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_axiom('14825a94-69a0-443b-842d-a75cd5a03d31', foundational, validity_from_source_not_content).
narrative_ontology:cs_axiom_status(validity_from_source_not_content, holdable).
narrative_ontology:cs_axiom_grounding('14825a94-69a0-443b-842d-a75cd5a03d31', validity_from_source_not_content, conventional).
narrative_ontology:cs_axiom('14825a94-69a0-443b-842d-a75cd5a03d31', foundational, law_morality_separation_thesis).
narrative_ontology:cs_axiom_status(law_morality_separation_thesis, holdable).
narrative_ontology:cs_axiom_grounding('14825a94-69a0-443b-842d-a75cd5a03d31', law_morality_separation_thesis, conventional).
narrative_ontology:cs_reference_frame('14825a94-69a0-443b-842d-a75cd5a03d31', hartian_legal_positivism).
narrative_ontology:cs_drift_state('14825a94-69a0-443b-842d-a75cd5a03d31', contemporary_judicial_politicization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('14825a94-69a0-443b-842d-a75cd5a03d31', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(constitutional_text_authority__positivist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, legal_formalists).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, institutional_courts).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, legislative_majorities).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, natural_law_advocates).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, moral_reading_judges).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, rights_claimants_excluded_by_procedure).
narrative_ontology:constraint_vindicates(constitutional_text_authority__positivist_reading, separation_of_law_and_morality).
narrative_ontology:constraint_vindicates(constitutional_text_authority__positivist_reading, procedural_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text_authority__positivist_reading, institutional_competence_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Academic and doctrinal actors who build careers on the internal coherence of legal systems. They benefit from a constraint that treats legal reasoning as self-contained and procedurally bounded, giving their expertise a protected domain. Exit is mobile — they could adopt other jurisprudential frameworks, but the institutional rewards for formalist analysis are substantial.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, legal_formalists, beneficiary,
    organized, generational, mobile, national).

% Courts that administer the constraint by policing the boundary between valid enactment and moral aspiration. They benefit from the constraint because it gives them a clear, defensible criterion for decision that insulates them from political attack. They can arbitrage between formalist and other modes of reasoning depending on the case, making their exit options broad.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, institutional_courts, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__positivist_reading, institutional_courts, beneficiary).

% Lawmaking bodies whose enactments gain validity solely from procedural compliance. They benefit because the constraint shields their outputs from moral challenge — once the procedure is followed, the law is valid regardless of its justice. Their exit is constrained: they could adopt a different constitutional theory, but the procedural shield is politically valuable.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, legislative_majorities, beneficiary,
    powerful, biographical, constrained, national).

% Scholars, judges, and advocates who argue that unjust enactments lack legal validity. They pay the cost of exclusion: their arguments are ruled inadmissible in formalist courts, their doctrinal contributions are marginalized, and their professional identity is fused to the rejected position. Exit is identity-locked — abandoning the natural law commitment would dissolve their intellectual project.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, natural_law_advocates, payer,
    moderate, generational, identity_locked, national).

% Judges who believe constitutional interpretation requires moral reasoning. They pay through professional friction: their opinions are treated as activist, their confirmations are contested, and their reasoning is excluded from the formalist canon. Exit is constrained — they can write concurrences, dissent, or shift toward formalism, but each path carries career costs.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, moral_reading_judges, payer,
    organized, biographical, constrained, national).

% Litigants whose claims fail because the constitutional text, procedurally enacted, does not protect their interest — and moral arguments cannot supplement the text. They are trapped: the constraint closes the door they would walk through, and they lack the power to change the enactment or the interpretive rule.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, rights_claimants_excluded_by_procedure, payer,
    powerless, biographical, trapped, national).

% Share the positivist commitment to text-fidelity but ground it in historical public meaning rather than institutional source. They observe the constraint from a neighboring position — convergent on many outcomes, divergent on the source of authority. Their exit is analytical: they evaluate the constraint from outside its operative structure.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, originalist_scholars, observer,
    organized, generational, analytical, national).

% Hold that constitutional meaning evolves with moral progress. They observe the constraint as a rival framework that forecloses their core premise. Their exit is analytical — they engage the positivist reading as an interlocutor, not a participant.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, living_constitutionalist_scholars, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, procedurally verifiable criterion for constitutional validity that enables stable governance without requiring consensus on moral truth. Officials can identify valid law by checking sources; citizens can predict legal consequences by reading enactments.
% TRANSFER_FUNCTION: Moves interpretive authority from moral reasoning to institutional procedure. The power to say what the constitution means shifts from judges exercising moral judgment to the historical record of enactment. Legitimacy rents flow to those who control the procedural gateway (legislatures, formalist courts).
% ABSENT_VOICES: Communities whose constitutional claims rest on moral principles not reflected in the enacted text — indigenous peoples, marginalized groups seeking recognition beyond existing categories, future generations. They are excluded because the constraint defines validity retrospectively: only what was procedurally enacted counts.
% DISAPPEARANCE_RATIONALE: If the positivist constraint vanished, constitutional adjudication would immediately open to moral argument. Courts would have to articulate and defend substantive principles. The professional and institutional division of labor between legislatures and courts would shift. Legitimacy would become contestable in every case rather than settled by pedigree.
% FOUNDING_PROBLEM: The need for a legal system to operate without resolving deep moral disagreements — to provide authoritative settlements that bind even those who dissent from the outcome. The positivist reading was built to solve the problem of legitimate authority in a pluralistic society where moral consensus is impossible.
% FOUNDING_PROBLEM_CORROBORATION: Legal formalists (Hart, Raz, Kelsen) attest the problem is live: pluralism persists, procedural authority remains the only neutral ground. Natural lawyers (Finnis, Dworkin in his later work) and living constitutionalists attest the problem is misdiagnosed: the founding problem was not pluralism but justice, and procedural neutrality masks substantive choices. No consensus exists outside the benefiting formalist tradition.
narrative_ontology:disappearance_verdict(constitutional_text_authority__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__positivist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(constitutional_text_authority__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__positivist_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__positivist_reading_tests).
:- end_tests(constitutional_text_authority__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) is moderate but real: the constraint transfers interpretive authority from moral reasoners to procedural gatekeepers. Suppression (0.58) is higher because the constraint actively excludes moral arguments from validity determinations — not merely ignores them, but rules them inadmissible. Theater (0.25) has risen as formalist language becomes a performative cover for outcomes driven by other factors. Accessibility collapse (0.65) reflects that once the procedural rule is accepted, moral alternatives are structurally invisible within the system. Resistance (0.52) is substantial from natural law and living constitutionalist traditions, but fragmented across different institutional sites.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (institutional courts) experiences the constraint as coordination: it gives them a rule to follow. The payer seats (natural law advocates, moral-reading judges, excluded claimants) experience it as extraction: it takes away the only argument that could validate their claims. The engine computes this divergence from the structural data — the positivist claim that this is pure coordination is the measurement the corpus exists to test.
 *
 * DIRECTIONALITY LOGIC:
 *   Legal formalists and institutional courts are beneficiaries (d near 0.2): the constraint gives them a protected domain of expertise and a defensible decision procedure. Legislative majorities are beneficiaries (d near 0.15): their enactments gain a shield from moral challenge. Natural law advocates are identity-locked payers (d near 0.9): their entire intellectual project is constituted by the rejection of the positivist separation. Moral-reading judges are constrained payers (d near 0.7): they can dissent but cannot change the validity criterion. Rights claimants are trapped payers (d near 1.0): they have no voice in the enactment and no exit from the constraint. Originalist and living constitutionalist scholars are analytical observers (d = 0.5): they engage from outside the constraint's operative structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimate authority amid pluralism) remains contested. The constraint persists not because the problem is solved, but because the benefiting parties (formalist courts, legislative majorities) control the interpretive gateway. The rising theater and extraction over time suggest mandatrophy: the coordination function (neutral procedural criterion) has atrophied while the extraction function (shielding enactments from moral challenge) has grown.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    positivist_originalist_boundary,
    'Is the positivist reading''s convergence with originalism on text-fidelity structurally stable, or does the originalist reliance on historical moral understandings create a fault line that will force divergence?',
    'Track joint citations and doctrinal splits in supreme court opinions over 2025-2040. If originalist opinions increasingly invoke natural law premises that positivist opinions reject, the convergence fractures.',
    'If the convergence fractures, the positivist reading loses its most powerful institutional ally (originalist judges), reducing its coordination function and increasing its extraction profile — it becomes a narrower, more sectarian constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positivist_originalist_boundary, conceptual, 'Whether the positivist-originalist convergence on text-fidelity masks a fundamental disagreement about the source of textual authority').

omega_variable(
    procedural_shield_vs_substantive_outcomes,
    'Does the procedural validity criterion actually constrain outcomes, or has it become a ritual that legitimates predetermined results?',
    'Empirical study of whether formally valid enactments that violate thick moral norms are ever struck down on procedural grounds alone, versus the rate at which procedurally valid enactments are upheld despite moral objections.',
    'If the criterion is purely ritualistic, theater_ratio understates the performative character — the constraint is a piton in positivist clothing. If it genuinely constrains, the coordination function is live.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(procedural_shield_vs_substantive_outcomes, empirical, 'Whether the positivist constraint''s coordination function is genuine or theatrical').

omega_variable(
    kernel_framing_ambiguity,
    'Does the constitutional_text_authority kernel admit a single authoritative reading, or is the kernel itself constituted by the contest among readings?',
    'Institutional history: if constitutional practice treats the contest as permanent (no final adjudicator of the kernel''s meaning), the kernel is distributed. If a supreme court or constitutional convention can settle the reading, the kernel is formalized.',
    'If the kernel is distributed, all three readings are structurally equal — none can claim the kernel''s authority. If formalized, the positivist reading''s claim to be the kernel''s true instantiation is a substantive thesis, not a structural fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the constitutional_text_authority kernel has a determinate meaning independent of the readings that contest it').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__positivist_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(constitutional_text_authority__positivist_reading_tr_t1945, constitutional_text_authority__positivist_reading, theater_ratio, 1945, 0.12).
narrative_ontology:measurement(constitutional_text_authority__positivist_reading_tr_t1965, constitutional_text_authority__positivist_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement(constitutional_text_authority__positivist_reading_tr_t1985, constitutional_text_authority__positivist_reading, theater_ratio, 1985, 0.19).
narrative_ontology:measurement(constitutional_text_authority__positivist_reading_tr_t2005, constitutional_text_authority__positivist_reading, theater_ratio, 2005, 0.22).
narrative_ontology:measurement(constitutional_text_authority__positivist_reading_tr_t2025, constitutional_text_authority__positivist_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(constitutional_text_authority__positivist_reading_be_t1945, constitutional_text_authority__positivist_reading, base_extractiveness, 1945, 0.28).
narrative_ontology:measurement(constitutional_text_authority__positivist_reading_be_t1965, constitutional_text_authority__positivist_reading, base_extractiveness, 1965, 0.32).
narrative_ontology:measurement(constitutional_text_authority__positivist_reading_be_t1985, constitutional_text_authority__positivist_reading, base_extractiveness, 1985, 0.38).
narrative_ontology:measurement(constitutional_text_authority__positivist_reading_be_t2005, constitutional_text_authority__positivist_reading, base_extractiveness, 2005, 0.41).
narrative_ontology:measurement(constitutional_text_authority__positivist_reading_be_t2025, constitutional_text_authority__positivist_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(constitutional_text_authority__positivist_reading_su_t1945, constitutional_text_authority__positivist_reading, suppression_requirement, 1945, 0.45).
narrative_ontology:measurement(constitutional_text_authority__positivist_reading_su_t1965, constitutional_text_authority__positivist_reading, suppression_requirement, 1965, 0.48).
narrative_ontology:measurement(constitutional_text_authority__positivist_reading_su_t1985, constitutional_text_authority__positivist_reading, suppression_requirement, 1985, 0.52).
narrative_ontology:measurement(constitutional_text_authority__positivist_reading_su_t2005, constitutional_text_authority__positivist_reading, suppression_requirement, 2005, 0.55).
narrative_ontology:measurement(constitutional_text_authority__positivist_reading_su_t2025, constitutional_text_authority__positivist_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text_authority__positivist_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, judicial_review_legitimacy).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, statutory_interpretation_canon).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, international_law_domestic_incorporation).

% DUAL FORMULATION NOTE:
% Part of the constitutional_text_authority constraint family with originalist_reading and living_constitutionalist_reading. The positivist reading shares the procedural coordination function with originalism but diverges on moral foundations; it shares the law/morality distinction with analytical jurisprudence but diverges on the role of institutional practice. All three readings link to downstream constraints on judicial legitimacy and statutory interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_text_authority__positivist_reading, institutional, 0.15).
constraint_indexing:directionality_override(constitutional_text_authority__positivist_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
