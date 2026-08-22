% ============================================================================
% CONSTRAINT STORY: constitutional_text__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__judicial_supremacy_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: constitutional_text__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of Constitutional Text
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   This constraint story models the judicial supremacy reading of
 *   constitutional text: the constitution grants courts final interpretive
 *   authority, and judicial invalidation of legislation is the conclusive
 *   determination of constitutional meaning. This reading is instantiated in
 *   systems like the United States (Marbury v. Madison), Germany
 *   (Constitutional Court), and South Africa. The constraint operates as a
 *   tangled rope: it coordinates by providing stable constitutional meaning
 *   and protecting rights against majoritarian overreach (beneficiaries:
 *   rights-claimants, judicial institutions), but extracts by foreclosing
 *   legislative override and democratic responsiveness (victims: democratic
 *   responsiveness, legislative majorities). Active enforcement is required —
 *   courts must maintain institutional independence and the political
 *   branches must comply with adverse rulings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, 0.38).
domain_priors:suppression_score(constitutional_text__judicial_supremacy_reading, 0.42).
domain_priors:theater_ratio(constitutional_text__judicial_supremacy_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__judicial_supremacy_reading, "Judicial Supremacy Reading of Constitutional Text").
narrative_ontology:topic_domain(constitutional_text__judicial_supremacy_reading, "constitutional_theory/political_philosophy/comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__judicial_supremacy_reading, '6ec8d5da-876c-423c-a8ff-82a797d3099b').
narrative_ontology:cs_kernel_codification('6ec8d5da-876c-423c-a8ff-82a797d3099b', fixed_text).
narrative_ontology:cs_authority_grounding('6ec8d5da-876c-423c-a8ff-82a797d3099b', lineage).
narrative_ontology:cs_interpretation_layer_present('6ec8d5da-876c-423c-a8ff-82a797d3099b').
narrative_ontology:cs_reading_relation('6ec8d5da-876c-423c-a8ff-82a797d3099b', constitutional_text__legislative_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('6ec8d5da-876c-423c-a8ff-82a797d3099b', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('6ec8d5da-876c-423c-a8ff-82a797d3099b', foundational, judicial_finality_as_constitutional_requirement).
narrative_ontology:cs_axiom_status(judicial_finality_as_constitutional_requirement, holdable).
narrative_ontology:cs_axiom_grounding('6ec8d5da-876c-423c-a8ff-82a797d3099b', judicial_finality_as_constitutional_requirement, conventional).
narrative_ontology:cs_axiom('6ec8d5da-876c-423c-a8ff-82a797d3099b', foundational, countermajoritarian_difficulty_as_feature_not_bug).
narrative_ontology:cs_axiom_status(countermajoritarian_difficulty_as_feature_not_bug, holdable).
narrative_ontology:cs_axiom_grounding('6ec8d5da-876c-423c-a8ff-82a797d3099b', countermajoritarian_difficulty_as_feature_not_bug, deontological).
narrative_ontology:cs_reference_frame('6ec8d5da-876c-423c-a8ff-82a797d3099b', marbury_constitutional_supremacy).
narrative_ontology:cs_drift_state('6ec8d5da-876c-423c-a8ff-82a797d3099b', contemporary_rights_jurisprudence_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6ec8d5da-876c-423c-a8ff-82a797d3099b', '').
narrative_ontology:cs_kernel_id(constitutional_text__judicial_supremacy_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, rights_claimants_against_majoritarian_overreach).
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, judicial_institutions).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, democratic_responsiveness).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, legislative_majorities).
narrative_ontology:constraint_vindicates(constitutional_text__judicial_supremacy_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text__judicial_supremacy_reading, judicial_review_as_constitutional_guardian).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts hold final interpretive authority and administer the constraint. They benefit from the institutional prestige and authority grant, but bear legitimacy costs when rulings are perceived as political. Their exit options are high (arbitrage) — individual judges can move to academia, private practice, or international tribunals; the institution itself cannot exit but can modulate its assertiveness.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, judicial_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__judicial_supremacy_reading, judicial_institutions, beneficiary).

% Minority groups, civil liberties organizations, and individuals whose rights claims would lose in majoritarian politics. They gain enforceable constitutional protection against legislative majorities. Exit is constrained: they can seek rights protection in other jurisdictions (forum shopping) or through international bodies, but domestic judicial supremacy is their primary structural guarantee.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, rights_claimants_against_majoritarian_overreach, beneficiary,
    organized, biographical, constrained, national).

% Elected legislatures that lose the ability to enact preferred policies when courts invalidate them. They bear the extraction directly: their democratic mandate is overridden by unelected judges. Exit is constrained: they can pursue constitutional amendment (extremely difficult), court-packing or jurisdiction stripping (politically costly and institutionally dangerous), or wait for judicial turnover (uncertain, slow).
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, legislative_majorities, payer,
    powerful, biographical, constrained, national).

% The systemic capacity of the political system to translate sustained popular majorities into policy outcomes. When courts foreclose legislative action on salient issues, democratic responsiveness is extracted from — the feedback loop between voter preferences and policy results is severed on constitutionalized issues. This is a non-agent entity (a systemic property) but bears the extraction structurally.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, democratic_responsiveness, payer,
    moderate, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(constitutional_text__judicial_supremacy_reading, democratic_responsiveness).

% Academic observers who analyze the constraint's operation across systems and time. They neither collect nor pay; they map the structural relationships. Their analytical exit is costless.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__judicial_supremacy_reading, judicial_institutions).
narrative_ontology:fixing_cost_class(constitutional_text__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides stable, authoritative constitutional meaning that binds all branches; protects fundamental rights against majoritarian drift; resolves constitutional disputes conclusively without political crisis.
% TRANSFER_FUNCTION: Moves final interpretive authority from legislative majorities to courts; moves policy discretion on constitutionalized issues from elected branches to judicial institutions; moves the cost of constitutional error from rights-claimants (who would suffer rights violations) to democratic majorities (who lose preferred policies).
% ABSENT_VOICES: Future generations (who inherit constitutional interpretations they did not authorize), citizens in jurisdictions without strong judicial review (who experience alternative coordination mechanisms), and political minorities who would use legislative override mechanisms to protect their interests (excluded by the constraint's foreclosure of legislative correction).
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, legislative majorities would become the final interpreters of constitutional meaning (subject to amendment processes). Rights-claimants would lose their strongest structural guarantee. Constitutional crises would increase as branches dispute meaning. The political system would reorganize around legislative constitutional interpretation — either through explicit notwithstanding mechanisms, departmentalism, or popular constitutionalism.
% FOUNDING_PROBLEM: Preventing legislative tyranny and majority faction from violating fundamental rights; securing stable constitutional meaning against shifting political majorities; providing a conclusive dispute-resolution mechanism for constitutional questions that avoids political violence or systemic paralysis.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by Federalist Papers (Madison, Hamilton) and the Marbury Court as live. It is contested by: (1) legislative sovereignty theorists (Waldron, Tushnet) who argue the problem was misdiagnosed — legislatures are better rights protectors; (2) popular sovereignty theorists (Ackerman, Levinson) who argue the founding problem was constituent power, not judicial guardianship; (3) comparative evidence from parliamentary supremacy systems (UK, NZ) where rights protection occurs without judicial finality. Corroboration from outside judicial beneficiaries: political scientists (Rosenberg, Whittington) documenting court constraints; historians (Kramer) on popular constitutionalism tradition.
narrative_ontology:disappearance_verdict(constitutional_text__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(constitutional_text__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__judicial_supremacy_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__judicial_supremacy_reading_tests).
:- end_tests(constitutional_text__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects the asymmetric transfer: legislative majorities lose the ability to enact their preferred policies when courts invalidate them, and this loss is not reciprocated by a comparable gain to courts (they gain authority but not policy preferences). Suppression (0.42) is moderate: legislative override is structurally impossible (no notwithstanding clause), but legislatures retain amendment power and appointment influence. Theater ratio (0.25) is low-moderate: judicial review performs genuine rights-protection work, but a growing share of high-profile cases involve culture-war issues where the coordination function is contested. Accessibility collapse (0.35) is moderate: alternative interpretive frameworks (departmentalism, popular constitutionalism) persist in academic and political discourse but have no institutional pathway. Resistance (0.65) is high: court-curbing proposals, jurisdiction stripping, and non-compliance threats are recurring features.
 *
 * PERSPECTIVAL GAP:
 *   From the rights-claimant seat, the constraint appears as a rope (genuine coordination protecting fundamental rights). From the legislative majority seat, it appears as a snare (extraction of democratic authority). From the judicial institution seat, it appears as a tangled rope (coordination function they administer, but with legitimacy maintenance costs). The engine computes these per-seat divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights-claimants and judicial institutions are beneficiaries (d near 0.0): the constraint subsidizes their position by making rights claims judicially enforceable against majorities. Legislative majorities and democratic responsiveness are victims (d near 1.0): they bear the cost of foreclosed policy pathways. The analytical observer seat (constitutional theorists) sees the full structure. Judicial institutions hold a dual position: they benefit from the authority grant (beneficiary) but also bear legitimacy costs when rulings are contested (partial payer).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing legislative tyranny, securing stable constitutional meaning) remains live but contested. The constraint has not resolved its mandatrophy: the coordination function (rights protection) is real, but the extraction component (judicial finality foreclosing legislative correction) has accumulated as the scope of judicial review expanded beyond the founding generation's contemplation. The tangled_rope classification captures this dual character — neither pure coordination nor pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint instantiates the judicial_supremacy_reading of the constitutional_text kernel; how do the sibling readings (legislative_sovereignty_reading, popular_sovereignty_reading) alter the structural classification?',
    'Author separate constraint stories for each sibling reading with their own ε, beneficiaries, victims, and cs_structure; compare computed per-seat types across the kernel family.',
    'If sibling readings compute different constraint types (e.g., legislative_sovereignty_reading as rope or scaffold), the kernel itself is not a single constraint but a family of structurally distinct constraints linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committee structure: judicial_supremacy_reading vs. legislative_sovereignty_reading vs. popular_sovereignty_reading').

omega_variable(
    democratic_responsiveness_as_victim,
    'Is democratic responsiveness a genuine victim (bearing extraction) or a contested normative claim about what the constraint suppresses?',
    'Empirical study of legislative output pre/post judicial invalidation regimes; measure policy responsiveness to electoral shifts under strong vs. weak judicial review.',
    'If democratic responsiveness is structurally extracted from (legislatures cannot enact preferred policies even with sustained majorities), victim declaration stands; if it is a normative disagreement about policy substance, the victim label may be a frame dispute rather than extraction evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_responsiveness_as_victim, empirical, 'Whether democratic responsiveness bears extraction or expresses normative contestation').

omega_variable(
    coordination_vs_extraction_boundary,
    'Does the coordination function (stable constitutional meaning, rights protection against majoritarian drift) genuinely require judicial finality, or is legislative override with supermajority thresholds a viable coordination alternative?',
    'Comparative analysis of constitutional systems with notwithstanding clauses (Canada) or legislative override mechanisms (Israel pre-2023, UK Human Rights Act) vs. strong judicial supremacy systems (US, Germany); measure rights protection outcomes and constitutional stability.',
    'If viable coordination alternatives exist that do not concentrate interpretive authority in courts, the constraint''s extraction component is higher (suppressed alternatives); if judicial finality is structurally necessary for the coordination function, the tangled_rope classification is warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, conceptual, 'Whether judicial finality is necessary for the coordination function or suppresses viable alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__judicial_supremacy_reading, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ct_jsr_tr_t1789, constitutional_text__judicial_supremacy_reading, theater_ratio, 1789, 0.1).
narrative_ontology:measurement(ct_jsr_tr_t1857, constitutional_text__judicial_supremacy_reading, theater_ratio, 1857, 0.15).
narrative_ontology:measurement(ct_jsr_tr_t1937, constitutional_text__judicial_supremacy_reading, theater_ratio, 1937, 0.22).
narrative_ontology:measurement(ct_jsr_tr_t1973, constitutional_text__judicial_supremacy_reading, theater_ratio, 1973, 0.28).
narrative_ontology:measurement(ct_jsr_tr_t2000, constitutional_text__judicial_supremacy_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(ct_jsr_tr_t2024, constitutional_text__judicial_supremacy_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(ct_jsr_be_t1789, constitutional_text__judicial_supremacy_reading, base_extractiveness, 1789, 0.15).
narrative_ontology:measurement(ct_jsr_be_t1857, constitutional_text__judicial_supremacy_reading, base_extractiveness, 1857, 0.25).
narrative_ontology:measurement(ct_jsr_be_t1937, constitutional_text__judicial_supremacy_reading, base_extractiveness, 1937, 0.35).
narrative_ontology:measurement(ct_jsr_be_t1973, constitutional_text__judicial_supremacy_reading, base_extractiveness, 1973, 0.42).
narrative_ontology:measurement(ct_jsr_be_t2000, constitutional_text__judicial_supremacy_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(ct_jsr_be_t2024, constitutional_text__judicial_supremacy_reading, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(ct_jsr_su_t1789, constitutional_text__judicial_supremacy_reading, suppression_requirement, 1789, 0.2).
narrative_ontology:measurement(ct_jsr_su_t1857, constitutional_text__judicial_supremacy_reading, suppression_requirement, 1857, 0.35).
narrative_ontology:measurement(ct_jsr_su_t1937, constitutional_text__judicial_supremacy_reading, suppression_requirement, 1937, 0.5).
narrative_ontology:measurement(ct_jsr_su_t1973, constitutional_text__judicial_supremacy_reading, suppression_requirement, 1973, 0.48).
narrative_ontology:measurement(ct_jsr_su_t2000, constitutional_text__judicial_supremacy_reading, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement(ct_jsr_su_t2024, constitutional_text__judicial_supremacy_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text__judicial_supremacy_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, constitutional_text__legislative_sovereignty_reading).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, constitutional_text__popular_sovereignty_reading).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, judicial_independence_institutional_design).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, constitutional_amendment_difficulty).

% DUAL FORMULATION NOTE:
% The constitutional_text kernel decomposes into three structurally distinct readings with different ε values, beneficiary/victim structures, and constraint types. This story (judicial_supremacy_reading) has ε=0.38 (tangled_rope). The legislative_sovereignty_reading would have lower ε (rope or scaffold) with legislative majorities as beneficiaries. The popular_sovereignty_reading would have variable ε depending on amendment accessibility. All three are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_text__judicial_supremacy_reading, institutional, 0.15).
constraint_indexing:directionality_override(constitutional_text__judicial_supremacy_reading, powerful, 0.85).
constraint_indexing:directionality_override(constitutional_text__judicial_supremacy_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
