% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__constitutional_nullity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__constitutional_nullity_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: statutory_debt_ceiling__constitutional_nullity_reading
 *   human_readable: Statutory Debt Ceiling (Constitutional Nullity Reading)
 *   domain: constitutional_law/political_economy
 *
 * SUMMARY:
 *   The statutory debt ceiling is a fixture of US fiscal governance since
 *   1917, requiring separate congressional authorization for Treasury
 *   borrowing above a specified threshold. The constitutional nullity reading
 *   claims that the 14th Amendment Section 4—which declares 'The validity of
 *   the public debt of the United States ... shall not be
 *   questioned'—constitutionally prohibits any statutory rule that would
 *   allow Congress to invalidate or repudiate the debt, including a rule that
 *   freezes borrowing capacity and forces default-or-capitulation. Under this
 *   reading, the ceiling is not a coordination mechanism and not an
 *   extraction tool; it is a legally void constraint that operates only
 *   through ceremonial deference and political theater. The constraint's
 *   extractive phases (in the alternative snare reading) arise only because
 *   Congress treats a void rule as if it were binding. This reading, if
 *   correct, collapses the constraint into pure theater: 100% of the
 *   ceiling's effect is performative because the constraint has zero legal
 *   force.
 *
 * KEY AGENTS:
 *   - Treasury Department: Institutional actor that must borrow to fund appropriated spending; under nullity reading, authorized to borrow without debt-ceiling approval
 *   - Congress: Institutional actor that appropriates spending and (under this reading) retains only nominal authority to reaffirm or repeal a void ceiling
 *   - Federal Courts: Institutional actor with final authority to adjudicate constitutional claims; decisive for whether nullity reading becomes operative
 *   - Constitutional Legal Order: Non-agent beneficiary entity; the text and supremacy structure itself is what is 'vindicated' by the reading
 *   - Legislative Minority: Excluded agent; loses extraction leverage if the ceiling is constitutionally inoperative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__constitutional_nullity_reading, 0.0).
domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, 0.0).
domain_priors:theater_ratio(statutory_debt_ceiling__constitutional_nullity_reading, 0.92).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 0.92).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__constitutional_nullity_reading, mountain).
narrative_ontology:human_readable(statutory_debt_ceiling__constitutional_nullity_reading, "Statutory Debt Ceiling (Constitutional Nullity Reading)").
narrative_ontology:topic_domain(statutory_debt_ceiling__constitutional_nullity_reading, "constitutional_law/political_economy").

domain_priors:emerges_naturally(statutory_debt_ceiling__constitutional_nullity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__constitutional_nullity_reading, 'ccad4e75-8ae2-4024-8664-b6eb9db112a7').
narrative_ontology:cs_kernel_codification('ccad4e75-8ae2-4024-8664-b6eb9db112a7', formalized).
narrative_ontology:cs_authority_grounding('ccad4e75-8ae2-4024-8664-b6eb9db112a7', lineage).
narrative_ontology:cs_interpretation_layer_present('ccad4e75-8ae2-4024-8664-b6eb9db112a7').
narrative_ontology:cs_reading_relation('ccad4e75-8ae2-4024-8664-b6eb9db112a7', statutory_debt_ceiling__coordination_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('ccad4e75-8ae2-4024-8664-b6eb9db112a7', statutory_debt_ceiling__extraction_snare_reading, forecloses).
narrative_ontology:cs_axiom('ccad4e75-8ae2-4024-8664-b6eb9db112a7', foundational, id_14th_amendment_section_4_binds_congress).
narrative_ontology:cs_axiom_status(id_14th_amendment_section_4_binds_congress, holdable).
narrative_ontology:cs_axiom_grounding('ccad4e75-8ae2-4024-8664-b6eb9db112a7', id_14th_amendment_section_4_binds_congress, deontological).
narrative_ontology:cs_axiom('ccad4e75-8ae2-4024-8664-b6eb9db112a7', foundational, constitutional_text_overrides_statute).
narrative_ontology:cs_axiom_status(constitutional_text_overrides_statute, holdable).
narrative_ontology:cs_axiom_grounding('ccad4e75-8ae2-4024-8664-b6eb9db112a7', constitutional_text_overrides_statute, conventional).
narrative_ontology:cs_reference_frame('ccad4e75-8ae2-4024-8664-b6eb9db112a7', constitutional_textual_supremacy).
narrative_ontology:cs_drift_state('ccad4e75-8ae2-4024-8664-b6eb9db112a7', contemporary_political_extraction_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ccad4e75-8ae2-4024-8664-b6eb9db112a7', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, constitutional_legal_order).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, congress).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, the Treasury has constitutional obligation to borrow as required by law-authorized expenditures, with the debt ceiling inoperative. The department argues debt ceiling authorization votes are ceremonial constraints on a constitutional mandate that overrides them. Its authority to borrow follows from Congress's power of the purse exercised through appropriations, not from a separate debt ceiling permitting vote.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, treasury_department, agenda_setter,
    institutional, generational, analytical, national).

% Under this reading, Congress retains its constitutional power of the purse through appropriations, but the debt ceiling—as a constraint on borrowing to fund those appropriations—is constitutionally void and unenforceable. Congress can choose to repeal the ceiling (as a courtesy to clarity), but failure to do so does not operate as a legal block on Treasury borrowing required by duly enacted appropriations.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, congress, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__constitutional_nullity_reading, congress, beneficiary).

% Would have final authority to adjudicate whether the debt ceiling is constitutionally void. Under the nullity reading, courts apply constitutional supremacy doctrine and find the statute void to the extent it conflicts with Section 4. They provide the enforcement mechanism for this reading—they are not passive observers but the decisive institutional actors.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% The framework of constitutional supremacy is the referent entity vindicated by this reading. Under the nullity interpretation, the constitutional text (14th Amendment Section 4) governs, and ordinary statutory rules that contradict it are void. No agent 'benefits' in the rent-collection sense; rather, the constitutional rule structure is affirmed as operative.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, constitutional_legal_order, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(statutory_debt_ceiling__constitutional_nullity_reading, constitutional_legal_order).

% Interpret the constitutional text and adjudicate competing readings. The nullity reading derives from formalist textual arguments about Section 4 supremacy and the ordinary meaning of 'validity.' Scholars dispute whether the text actually yields this interpretation or whether statutory-level debt ceiling rules sit in a different domain from Section 4's explicit prohibition.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, constitutional_scholars, observer,
    powerful, generational, analytical, national).

% Under the alternative extraction_snare_reading, the debt ceiling is a tool of minority veto. Under the nullity reading, the ceiling is inoperative, so minority leverage disappears—they lose the ability to condition fiscal compliance on extractive concessions. This reading forecloses that extraction mechanism entirely.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, legislative_minority, excluded,
    moderate, biographical, constrained, national).

% Financial markets, foreign holders of US debt, and institutional creditors would argue that debt ceiling uncertainty creates real economic risk and that a clear legal rule—even one invalidating the ceiling—would reduce that risk. They are excluded from the constitutional reading process and would benefit from resolution, but the reading itself makes no claim about their interests.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, default_risk_agents, excluded,
    powerful, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__constitutional_nullity_reading, diffuse).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__constitutional_nullity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None under this reading. The nullity interpretation does not describe a coordination mechanism; it describes a constitutional legal truth-condition. The debt ceiling, if inoperative, coordinates nothing because it has no operative effect.
% TRANSFER_FUNCTION: None under this reading. The nullity interpretation asserts the ceiling has zero extractive effect because it is legally inoperative. Any flow of concessions would arise only from politicians' ceremonial deference to a void constraint, not from the constraint's operation.
% ABSENT_VOICES: Financial markets and foreign creditors would prefer a clear legal rule (whether permissive or prohibitive) to uncertainty. Legislative minorities would object to the loss of ceiling-based veto leverage, but under this reading they have no standing because the constraint is void and cannot operate as a tool. Constitutional scholars who defend the ceiling's validity are excluded from the nullity reading's framework but are present in the larger institutional debate.
% DISAPPEARANCE_RATIONALE: If the debt ceiling disappeared overnight, this reading says the legal outcome would not change—the ceiling is already void, so removing an inoperative rule changes nothing. Congress would still appropriate, Treasury would still borrow to fund those appropriations, and the constitutional balance would remain unchanged. The disappearance would be a clarification, not a rearrangement.
% FOUNDING_PROBLEM: The founding problem differs across the sibling readings. The nullity reading does not frame a coordination or extraction problem—it frames a constitutional-supremacy problem: does a statute (the debt ceiling) override an amendment (Section 4) or vice versa? The textual answer, under this reading, is that the amendment is supreme.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars working in formalist traditions (Balkin, Tribe at times, various law review articles on Amendment primacy) have articulated versions of this argument independent of any political advantage. Courts have not yet adjudicated the question directly, so corroboration comes from scholarly legal analysis and formal textualist methodology, not from institutional practice. The reading remains academic and contestable because no court has yet declared the ceiling void.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__constitutional_nullity_reading, world_unchanged).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__constitutional_nullity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__constitutional_nullity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statutory_debt_ceiling__constitutional_nullity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__constitutional_nullity_reading, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, ExtMetricName, E),
    domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(statutory_debt_ceiling__constitutional_nullity_reading),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is zero because, under this reading, the constraint has no legal force—it is constitutionally inoperative. The authorization votes and compliance with ceiling language are pure theater; Treasury's obligation to borrow comes from the Constitution and appropriations law, not from the ceiling. Theater ratio is extremely high (0.92) because nearly all observed behavior around the ceiling (votes, negotiations, rhetoric) maintains a void constraint's appearance of authority. Congress members vote to 'raise the ceiling' even though, under this reading, they have no power to prevent ceiling-exempt borrowing. The accessibility collapse is extremely high (0.95) because, once the constitutional analysis settles on nullity, the only alternative reading that makes sense (the scaffold reading) would emerge from the same textual analysis—most people educated in constitutional law would accept the supremacy argument or have their own coherent counterargument, not a default confusion. Resistance is very low (0.08) because there is no active resistance to a void constraint—resistance only makes sense if the constraint were operative and someone were trying to oppose it. Under the nullity reading, any observed resistance is resistance to the *performance* of the constraint, not to the constraint itself.
 *
 * PERSPECTIVAL GAP:
 *   The perspective gap between seats is minimal under the nullity reading because all seats should theoretically accept the same constitutional fact. Treasury and Courts compute the constraint identically (void). Congress computes it the same way but faces political pressure to maintain the ceremonial votes anyway. The legislative minority computes it as a loss of leverage, not as a void constraint. The gap is not primarily a structural difference in how seats experience the constraint; it is a gap between the constitutional-legal claim (nullity) and the political-ceremonial practice (maintaining the ceiling's appearance). This is a reading-level gap, not a per-seat gap within the reading's own framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Under the nullity reading, directionality is not applicable in the normal sense because the constraint has zero legal force, so no seat is extracted from. The declared 'beneficiary' (constitutional_legal_order) is not an agent that collects rents—it is the vindicated constitutional principle itself. The treasury benefits incidentally from having autonomy to borrow as appropriated, but this is not an extraction dynamic; it is a constitutional allocation of power. If courts adjudicate the reading and find the ceiling void, all institutional seats would compute d symmetrically around 'neutrality'—the constraint is simply inoperative for everyone.
 *
 * MANDATROPHY ANALYSIS:
 *   The nullity reading resolves any mandatrophy question by asserting the constraint never had valid authority to begin with. The founding problem (keeping fiscal discipline while delegating borrowing to Treasury) would have been solved by the ceiling if it were operative, but the reading asserts it never was operative. Therefore, there is no mandatrophy in the traditional sense (authority outliving function) because there was no authority. The apparent mandatrophy—Congress voting on a ceiling that has zero legal effect—dissolves once the nullity claim is accepted. This is why the reading is so clean as a mountain: if true, it eliminates the entire extraction question by making the constraint legally void.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    section_4_supremacy_interpretation,
    'Does the text of the 14th Amendment Section 4 (''The validity of the public debt ... shall not be questioned'') actually operate as a constitutional prohibition on statutory debt ceilings, or is the ceiling a separate statutory mechanism that sits outside the Section 4 domain?',
    'Supreme Court decision directly adjudicating the constitutional status of the debt ceiling under Section 4, or comprehensive scholarly consensus on originalist and textualist grounds that the Amendment''s language forecloses ceiling-like restrictions.',
    'If Section 4 does prohibit the ceiling, the constraint is constitutionally void and extractiveness drops to zero (as this reading claims). If Section 4 is silent on statutory procedural debt limits, the ceiling remains operative and the constraint reverts to snare or scaffold readings depending on use.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(section_4_supremacy_interpretation, conceptual, 'Whether Section 4 supremacy extends to statutory debt ceilings or is limited to explicit debt-cancellation scenarios.').

omega_variable(
    nullity_vs_voidness_vs_suspension,
    'If courts find the debt ceiling in tension with Section 4, do they rule it constitutionally void (nullity reading), temporarily suspended (emergency reading), or reinterpret it narrowly to avoid conflict (saving construction)?',
    'Judicial reasoning and holding in the first Supreme Court case addressing the question directly; law review analysis of comparable constitutional-statutory conflicts.',
    'Nullity (this reading) means the ceiling has never had legal force and all enforcement has been theatrical. Suspension or narrow reinterpretation would imply the ceiling retains some operative force in normal circumstances. Each outcome maps to a different constraint type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nullity_vs_voidness_vs_suspension, conceptual, 'The legal modality of the ceiling''s constitutional disability (void, suspended, narrowly construed).').

omega_variable(
    reading_observational_bias,
    'Is the nullity reading derived from faithful constitutional textual analysis, or is it a reading chosen to escape the extraction-dynamics of the ceiling-as-snare that dominate contemporary politics?',
    'Scholarly pedigree of the reading prior to recent extraction crises (does the reading appear in pre-2009 constitutional scholarship?); originalist methodology applied consistently to comparable texts (how do courts handle other constitutional prohibitions on statutory procedures?); acceptance among constitutional scholars without political stake in debt-ceiling outcomes.',
    'If the reading is observationally biased toward political preference, it loses credibility as a constitutional claim and becomes a normative proposal rather than a legal truth-condition. If it has genuine scholarly provenance independent of politics, it gains force.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_observational_bias, empirical, 'Whether the nullity reading is a constitutional finding or a political-motivated reinterpretation.').

omega_variable(
    natural_law_vs_constructed_reading,
    'This constraint is authored as a mountain (emerges_naturally = true) with beneficiaries. Does the constitutional nullity claim represent a discovered feature of the Constitution''s actual text and structure, or is it a constructed reading authored by agents who benefit from eliminating the ceiling''s extraction potential?',
    'Comparative textual analysis across constitutional traditions; historical intent evidence from Section 4 ratification; independent scholarly acceptance of the nullity claim prior to the century of extraction dynamics.',
    'If the claim is a genuine constitutional fact independent of political convenience, the mountain classification stands and extractiveness is zero. If the reading is constructed to serve the interest of eliminating extraction leverage, the constraint may be a false summit (beneficiaries present, emerges_naturally = true, but actually a tangled_rope or snare with a naturalness cover story).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_reading, empirical, 'Whether Section 4 supremacy over the debt ceiling is a discovered constitutional fact or an interests-driven reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__constitutional_nullity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 0, 0.88).
narrative_ontology:measurement(stat_tr_t5, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 5, 0.9).
narrative_ontology:measurement(stat_tr_t10, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 10, 0.91).
narrative_ontology:measurement(stat_tr_t15, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 15, 0.92).
narrative_ontology:measurement(stat_tr_t20, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 20, 0.92).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(stat_be_t5, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 5, 0.0).
narrative_ontology:measurement(stat_be_t10, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 10, 0.0).
narrative_ontology:measurement(stat_be_t15, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 15, 0.0).
narrative_ontology:measurement(stat_be_t20, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 20, 0.0).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(statutory_debt_ceiling__constitutional_nullity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__constitutional_nullity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling__extraction_snare_reading).

% DUAL FORMULATION NOTE:
% The statutory debt ceiling is a single kernel interpreted by three distinct readings: (1) constitutional_nullity_reading (this constraint) asserts the ceiling is void under Section 4 of the 14th Amendment; (2) coordination_scaffold_reading interprets it as a procedural coordination mechanism; (3) extraction_snare_reading interprets it as minority leverage. Each reading is a separate constraint story with its own ε, stakeholders, and structure. The nullity reading forecloses the snare reading because it asserts the ceiling is legally inoperative and therefore cannot function as an extraction tool. The scaffold reading coexists with nullity because a scaffold can be void-yet-performed (ceremonial coordination persists even after legal invalidation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
