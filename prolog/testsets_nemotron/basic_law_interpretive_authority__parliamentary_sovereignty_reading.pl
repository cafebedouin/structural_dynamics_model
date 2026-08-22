% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__parliamentary_sovereignty_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: basic_law_interpretive_authority__parliamentary_sovereignty_reading
 *   human_readable: Parliamentary Sovereignty Reading of Constitutional Interpretive Authority
 *   domain: constitutional_law/political_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint story models the parliamentary sovereignty reading of the
 *   contested kernel 'basic_law_interpretive_authority'. The kernel asks:
 *   which institution holds final authority to interpret constitutional
 *   meaning? This reading asserts that the elected legislature, by democratic
 *   mandate and representative accountability, retains final interpretive
 *   authority. It is one of three declared readings — the others being
 *   judicial_supremacy_reading and popular_constitutionalism_reading. The
 *   structural delta for this reading: legislature enters as beneficiary of
 *   institutional authority; judicial independence and rights-protected
 *   minorities enter as victims when legislative override occurs; gridlock
 *   costs fall on the judicial process. The constraint is claimed as
 *   tangled_rope because it performs a genuine coordination function
 *   (democratic governance without judicial veto) while simultaneously
 *   extracting from judicial independence and minority rights — requiring
 *   active enforcement (legislative override mechanisms, court-packing
 *   threats, jurisdictional stripping) to maintain legislative supremacy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.35).
domain_priors:suppression_score(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.45).
domain_priors:theater_ratio(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "Parliamentary Sovereignty Reading of Constitutional Interpretive Authority").
narrative_ontology:topic_domain(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "constitutional_law/political_theory/institutional_design").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__parliamentary_sovereignty_reading, '533649ff-9d64-4f5e-a0ad-8ee30bec7429').
narrative_ontology:cs_kernel_codification('533649ff-9d64-4f5e-a0ad-8ee30bec7429', formalized).
narrative_ontology:cs_authority_grounding('533649ff-9d64-4f5e-a0ad-8ee30bec7429', lineage).
narrative_ontology:cs_interpretation_layer_present('533649ff-9d64-4f5e-a0ad-8ee30bec7429').
narrative_ontology:cs_reading_relation('533649ff-9d64-4f5e-a0ad-8ee30bec7429', basic_law_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('533649ff-9d64-4f5e-a0ad-8ee30bec7429', basic_law_interpretive_authority__popular_constitutionalism_reading, influences).
narrative_ontology:cs_axiom('533649ff-9d64-4f5e-a0ad-8ee30bec7429', foundational, legislative_supremacy_over_interpretation).
narrative_ontology:cs_axiom_status(legislative_supremacy_over_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('533649ff-9d64-4f5e-a0ad-8ee30bec7429', legislative_supremacy_over_interpretation, conventional).
narrative_ontology:cs_axiom('533649ff-9d64-4f5e-a0ad-8ee30bec7429', foundational, democratic_mandate_trumps_legal_expertise).
narrative_ontology:cs_axiom_status(democratic_mandate_trumps_legal_expertise, holdable).
narrative_ontology:cs_axiom_grounding('533649ff-9d64-4f5e-a0ad-8ee30bec7429', democratic_mandate_trumps_legal_expertise, deontological).
narrative_ontology:cs_reference_frame('533649ff-9d64-4f5e-a0ad-8ee30bec7429', parliamentary_sovereignty_framework).
narrative_ontology:cs_drift_state('533649ff-9d64-4f5e-a0ad-8ee30bec7429', contemporary_rights_based_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('533649ff-9d64-4f5e-a0ad-8ee30bec7429', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, governing_party_leadership).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, majoritarian_constituencies).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_independence).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_protected_minorities).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, constitutional_amendment_process).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims final authority to interpret the constitution through democratic mandate. Enacts legislation that may override judicial interpretations, subject to electoral accountability. Controls legislative agenda and can amend or suspend constitutional provisions through prescribed procedures.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature, agenda_setter,
    institutional, generational, constrained, national).

% Directs legislative majorities to advance policy agenda without judicial constraint. Gains institutional authority when courts are subordinated to parliamentary will. Faces electoral accountability but benefits from concentrated interpretive power during tenure.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, governing_party_leadership, beneficiary,
    powerful, biographical, constrained, national).

% Sees legislative supremacy as enabling responsive governance. Policies favored by electoral majorities face fewer veto points. Accountability runs through elections rather than judicial review. Exit is voting or political mobilization.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, majoritarian_constituencies, beneficiary,
    organized, biographical, mobile, national).

% Courts lose final say on constitutional meaning when legislature can override. Judicial review becomes advisory or time-limited. Independence is structurally compromised by legislative supremacy; judges cannot enforce rights against legislative will. Exit is institutional resistance or resignation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_independence, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_independence, excluded).

% Bears costs when legislative majorities override rights-protective judicial interpretations. Constitutional rights become contingent on legislative forbearance. No effective exit — cannot vote out the majority that overrides their protections. Relies on legislative self-restraint or international pressure.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_protected_minorities, payer,
    powerless, biographical, trapped, national).

% Formal amendment procedures are bypassed when legislature claims interpretive finality. The constraint of supermajority requirements and ratification is circumvented by ordinary legislation reinterpreting the constitution. Exit is procedural hardening or entrenchment clauses.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, constitutional_amendment_process, payer,
    moderate, generational, constrained, national).

% Observes the structural tension between democratic legitimacy and rights protection. Tracks how different institutional configurations resolve the interpretive authority question. No direct stake in the constraint's operation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, constitutional_scholars_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the problem of who has the final word on constitutional meaning in a democracy — coordinates governance by vesting interpretive authority in the electorally accountable branch, preventing judicial veto of democratic will.
% TRANSFER_FUNCTION: Transfers final constitutional interpretive authority from an independent judiciary to the elected legislature, moving the power to define rights and governmental limits from courts to parliamentary majorities.
% ABSENT_VOICES: Future generations who inherit the constitutional structure; diaspora and non-citizen residents subject to legislation; international human rights bodies that monitor but cannot enforce; the 'constitutional culture' that expects judicial independence as a norm.
% DISAPPEARANCE_RATIONALE: If legislative interpretive finality vanished overnight, courts would reclaim final authority over constitutional meaning, legislative overrides of judicial decisions would become unconstitutional, and the balance of power would shift to judicial supremacy — the entire institutional geometry of constitutional governance would reorganize.
% FOUNDING_PROBLEM: Post-revolutionary or post-colonial constitutional orders needed to prevent unelected judges from blocking democratic reconstruction. The founding problem was ensuring that the people's representatives could enact transformative legislation without being vetoed by a judiciary tied to the old regime.
% FOUNDING_PROBLEM_CORROBORATION: Founding-era debates and constitutional texts (e.g., UK parliamentary sovereignty tradition, early French revolutionary constitutions, post-colonial African and Asian constitutions) support legislative supremacy as anti-aristocratic. Contemporary constitutional scholars outside the benefiting parties (e.g., Ackerman, Tushnet, Hirschl) argue the founding problem was historically specific and does not justify permanent legislative interpretive finality in mature democracies.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__parliamentary_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).
:- end_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.35) reflects that the constraint transfers meaningful interpretive power from courts to legislature — not total extraction but a significant shift in constitutional authority. Suppression (0.45) is moderate: the constraint suppresses judicial finality and minority exit options but does not eliminate judicial review entirely (often retains weak-form review). Theater ratio (0.25) captures performative democratic rhetoric masking power concentration. Accessibility collapse (0.40) is partial: alternative interpretive arrangements (judicial supremacy, popular constitutionalism) remain intellectually and politically live. Resistance (0.55) is substantial: courts, minority groups, and civil society actively contest legislative interpretive finality.
 *
 * PERSPECTIVAL GAP:
 *   From the legislature's seat (agenda_setter), this is a rope: genuine coordination of democratic governance. From judicial_independence (payer) and rights_protected_minorities (victim), it is a snare: extraction of their constitutional protections. The engine computes this seat divergence from the structural data — the declared roles, power levels, and exit options produce different effective extraction per seat. The claimed type (tangled_rope) acknowledges both coordination and extraction are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   The elected legislature and governing party leadership are structural beneficiaries (d ≈ 0.15–0.25): they gain concentrated interpretive authority. Majoritarian constituencies are moderate beneficiaries (d ≈ 0.35): they gain responsive governance but lose rights protections. Judicial independence is a structural payer (d ≈ 0.85): loses final authority, becomes subordinate. Rights-protected minorities are full targets (d ≈ 0.95): bear concentrated costs with no exit. Constitutional amendment process is a payer (d ≈ 0.70): its procedural constraints are circumvented. The analytical observer sits at d = 0.5.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing old-regime judiciary from blocking democratic reconstruction) was historically real but may be dead in mature democracies. The constraint persists because legislative majorities benefit from interpretive control — a classic mandatrophy pattern where the original justification (democratic reconstruction) is contested but the arrangement continues to extract from judicial independence and minority rights. The 'dead' status of the founding problem would trigger mandatrophy resolution if corroborated externally.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_liveness,
    'Is the founding problem (preventing anti-democratic judicial veto) still live in mature constitutional democracies, or has it become a cover story for legislative power accumulation?',
    'Historical-institutional analysis of whether legislatures in established democracies face actual judicial blocking of democratic mandates, versus using interpretive finality to avoid rights constraints.',
    'If dead, the constraint''s coordination function is vestigial and mandatrophy resolution reclassifies toward snare/piton. If live, the tangled_rope classification holds — genuine coordination persists alongside extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_liveness, conceptual, 'Whether the original justification for legislative interpretive finality remains operative or has atrophied.').

omega_variable(
    judicial_independence_as_victim_or_payer,
    'Is judicial independence properly modeled as a victim (rights-bearing entity harmed) or a payer (institution bearing structural costs)? The schema forces a choice between victim and payer roles for non-agent entities.',
    'Constitutional theory debate: is judicial independence a right of the judiciary as an institution, or a structural guarantee for rights-holders? The answer determines whether courts appear as victims (if independence is their right) or payers (if independence is a procedural cost).',
    'Role assignment changes the directionality derivation and the constraint''s victim structure. Victim status for judicial_independence strengthens the snare-like character; payer status emphasizes institutional cost-bearing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_independence_as_victim_or_payer, conceptual, 'Structural role assignment for judicial independence in the stakeholder surface.').

omega_variable(
    reading_foreclosure_boundary,
    'Does parliamentary sovereignty logically foreclose judicial supremacy within a single constitutional framework, or do they coexist as competing institutional equilibria?',
    'Comparative constitutional law: examine whether any stable constitutional order maintains both legislative finality on some matters and judicial finality on others (e.g., UK''s parliamentary sovereignty with HRA weak-form review vs. US-style strong judicial review).',
    'If forecloses, reading_relation = forecloses; if stable hybrid exists, reading_relation = coexists_with or influences. Determines the structural relationship declared in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_boundary, conceptual, 'Whether the parliamentary sovereignty reading logically eliminates the judicial supremacy reading within one framework.').

omega_variable(
    minority_exit_ambiguity,
    'For rights-protected minorities, is exit truly ''trapped'' (no options) or ''identity_locked'' (exit requires abandoning constitutional identity/commitment)? The distinction affects directionality derivation.',
    'Empirical study of minority political strategies under legislative supremacy: do they pursue constitutional amendment, international litigation, civil resistance, or assimilation? The feasibility of each path calibrates exit_options.',
    'If identity_locked rather than trapped, directionality d is slightly lower (0.9 vs 1.0) — the agent bears extraction but the constraint does not physically prevent exit. Affects effective extraction computation for the victim seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_exit_ambiguity, empirical, 'Exit option classification for rights-protected minorities under legislative interpretive finality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(basi_tr_t60, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(basi_tr_t80, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 80, 0.24).
narrative_ontology:measurement(basi_tr_t100, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 100, 0.25).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 20, 0.25).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 40, 0.3).
narrative_ontology:measurement(basi_be_t60, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 60, 0.33).
narrative_ontology:measurement(basi_be_t80, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 80, 0.35).
narrative_ontology:measurement(basi_be_t100, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 100, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(basi_su_t60, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 60, 0.43).
narrative_ontology:measurement(basi_su_t80, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 80, 0.44).
narrative_ontology:measurement(basi_su_t100, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 100, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% Part of the basic_law_interpretive_authority constraint family. This reading (parliamentary_sovereignty) vests final interpretive authority in the elected legislature. The judicial_supremacy_reading vests it in independent courts. The popular_constitutionalism_reading distributes it across ongoing democratic contestation. The three readings share the kernel but instantiate different constraints with different beneficiary/victim structures and extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(basic_law_interpretive_authority__parliamentary_sovereignty_reading, institutional, 0.15).
constraint_indexing:directionality_override(basic_law_interpretive_authority__parliamentary_sovereignty_reading, powerful, 0.25).
constraint_indexing:directionality_override(basic_law_interpretive_authority__parliamentary_sovereignty_reading, organized, 0.35).
constraint_indexing:directionality_override(basic_law_interpretive_authority__parliamentary_sovereignty_reading, powerless, 0.95).
constraint_indexing:directionality_override(basic_law_interpretive_authority__parliamentary_sovereignty_reading, moderate, 0.7).
constraint_indexing:directionality_override(basic_law_interpretive_authority__parliamentary_sovereignty_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
