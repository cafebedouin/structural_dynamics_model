% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__progressive_textualist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__progressive_textualist, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: equality_clause_scope__progressive_textualist
 *   human_readable: Equality Clause Scope - Progressive Textualist Reading
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The progressive textualist reading of the equality clause holds that the
 *   Constitution's text contains an equality principle, but the scope of that
 *   principle—who and what relationships it covers—is fixed at the time of
 *   ratification and can be expanded only through the formal amendment
 *   process, not through judicial reinterpretation. This is ONE reading of a
 *   contested kernel (the equality clause itself). Under this reading, the
 *   14th Amendment's equality language applied to a circumscribed set of
 *   relationships at 1868 (freed persons and their property rights in the
 *   immediate post-slavery context), and courts have authority to interpret
 *   that scope within reason, but expanding the scope to cover women, sexual
 *   minorities, or other groups requires a supermajority democratic
 *   amendment, not a judicial decree. The reading is 'textualist' because it
 *   grounds authority in the written text's amendment, and 'progressive'
 *   because it explicitly acknowledges that the original scope was narrow and
 *   deliberately permits democratic expansion—distinguishing it from the
 *   restrictive originalist reading that denies even the desirability of
 *   expansion, and from the expansive universalist reading that treats
 *   equality as self-evidently unbounded and delegitimizes amendment as an
 *   unnecessary procedural delay.
 *
 * KEY AGENTS:
 *   - Political majorities at amendment moments: control the supermajority gates
 *   - Excluded groups awaiting amendment: bear the cost of non-inclusion while lacking amendment power
 *   - Progressive courts: want interpretive flexibility but are constrained by the reading's authority
 *   - Originalist interpreters: use this reading as a boundary marker
 *   - Universalist advocates: excluded from the amendment process, argue for immediate judicial recognition
 *   - State legislatures and conventions: hold formal veto power over scope expansion
 *   - Constitutional legitimacy tradition: vindicated by the amendment-gating mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__progressive_textualist, 0.58).
domain_priors:suppression_score(equality_clause_scope__progressive_textualist, 0.62).
domain_priors:theater_ratio(equality_clause_scope__progressive_textualist, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, extractiveness, 0.58).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__progressive_textualist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__progressive_textualist, "Equality Clause Scope - Progressive Textualist Reading").
narrative_ontology:topic_domain(equality_clause_scope__progressive_textualist, "constitutional/political").

domain_priors:requires_active_enforcement(equality_clause_scope__progressive_textualist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__progressive_textualist, 'd8d394fa-eb1d-4d56-9cdd-6d05170b3800').
narrative_ontology:cs_kernel_codification('d8d394fa-eb1d-4d56-9cdd-6d05170b3800', fixed_text).
narrative_ontology:cs_authority_grounding('d8d394fa-eb1d-4d56-9cdd-6d05170b3800', lineage).
narrative_ontology:cs_interpretation_layer_present('d8d394fa-eb1d-4d56-9cdd-6d05170b3800').
narrative_ontology:cs_reading_relation('d8d394fa-eb1d-4d56-9cdd-6d05170b3800', equality_clause_scope__restrictive_originalist, coexists_with).
narrative_ontology:cs_reading_relation('d8d394fa-eb1d-4d56-9cdd-6d05170b3800', equality_clause_scope__expansive_universalist, coexists_with).
narrative_ontology:cs_axiom('d8d394fa-eb1d-4d56-9cdd-6d05170b3800', foundational, amendment_supermajority_required_for_scope_expansion).
narrative_ontology:cs_axiom_status(amendment_supermajority_required_for_scope_expansion, holdable).
narrative_ontology:cs_axiom_grounding('d8d394fa-eb1d-4d56-9cdd-6d05170b3800', amendment_supermajority_required_for_scope_expansion, conventional).
narrative_ontology:cs_axiom('d8d394fa-eb1d-4d56-9cdd-6d05170b3800', foundational, original_narrow_scope_permissibly_expandable).
narrative_ontology:cs_axiom_status(original_narrow_scope_permissibly_expandable, holdable).
narrative_ontology:cs_axiom_grounding('d8d394fa-eb1d-4d56-9cdd-6d05170b3800', original_narrow_scope_permissibly_expandable, deontological).
narrative_ontology:cs_reference_frame('d8d394fa-eb1d-4d56-9cdd-6d05170b3800', equality_clause_as_originally_ratified_narrow_scope).
narrative_ontology:cs_drift_state('d8d394fa-eb1d-4d56-9cdd-6d05170b3800', contemporary_democratic_politics, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d8d394fa-eb1d-4d56-9cdd-6d05170b3800', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__progressive_textualist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, political_majorities_at_amendment_moment).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, centrist_constitutional_legitimacy_framework).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, excluded_groups_awaiting_amendment).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, marginalized_communities_with_no_supermajority_support).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, progressive_courts).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, state_legislatures_and_conventions).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, progressive_courts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the formal amendment process through supermajority coalition-building. Has exclusive power to authorize scope expansions by constitutional amendment. Sets the agenda for what equality claims get textually formalized and what claims remain unresolved. Benefits from the constraint because it consolidates their authority and prevents unilateral judicial redefinition.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, political_majorities_at_amendment_moment, agenda_setter,
    institutional, generational, analytical, national).

% Groups claiming they fall within equality's principle but lack the supermajority coalition to amend the text. Bear the cost of continued de jure or de facto exclusion. Can persuade through argument and organizing, but cannot unilaterally amend. Must wait for political majorities to shift or face indefinite non-inclusion.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, excluded_groups_awaiting_amendment, payer,
    powerless, generational, trapped, national).

% Wish to extend equality's application through interpretation but are bound by the reading's constraint. Pay by ceding expansion authority to amendment process. Benefit by being positioned as legitimacy guardians of the constitutional text and by the constraint's explicit acknowledgment that within-scope interpretation is their prerogative.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, progressive_courts, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__progressive_textualist, progressive_courts, beneficiary).

% Use this reading as a boundary marker in jurisprudential debate. Observe that the progressive textualist reading admits original narrowness and permits amendment-driven expansion, which distinguishes it from restrictive originalism. Neither benefit nor pay directly; serve as analytical reference point for the constraint's legitimacy positioning.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, originalist_interpreters, observer,
    institutional, biographical, analytical, national).

% Argue that equality is self-evident universal principle and courts should recognize new applications immediately without awaiting amendment. Are excluded from the amendment process's formal decision-making. Can persuade through public discourse and litigation strategy, but lack the institutional seat that would let them bypass amendment requirement.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, universalist_advocates, excluded,
    moderate, biographical, constrained, national).

% Hold formal veto power over scope expansion through amendment supermajority requirement. Benefit from constraint because it guarantees them a seat at the constitutional-change table and prevents federal courts from unilaterally redefining the equality principle. Are part of the institutional majority coalition that controls scope expansion.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, state_legislatures_and_conventions, beneficiary,
    organized, generational, analytical, national).

% The doctrine that the Constitution's written text is the supreme source of binding authority and that fundamental change requires textual amendment, not judicial reinterpretation. Is vindicated by the constraint's enforcement of amendment-gating as the mechanism for scope expansion.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, constitutional_legitimacy_tradition, beneficiary,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(equality_clause_scope__progressive_textualist, constitutional_legitimacy_tradition).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__progressive_textualist, political_majorities_at_amendment_moment).
narrative_ontology:fixing_cost_class(equality_clause_scope__progressive_textualist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distinguishes between incremental interpretation (within judicial authority) and fundamental scope expansion (requiring democratic amendment). Solves the problem of preventing unilateral redefinition while preserving the possibility of democratic constitutional change.
% TRANSFER_FUNCTION: Moves authority over equality clause scope from courts and excluded groups to political majorities controlling supermajority coalitions. Transfers opportunity for immediate equality recognition into a requirement for democratic consensus-building at the amendment moment.
% ABSENT_VOICES: Judicial expansionists and those in excluded groups who lack supermajority political power. They would argue that equality is self-evident and its application should expand immediately through judicial recognition, that waiting for amendment is unjustly slow, and that amendment-gating enables entrenchment of majority preferences over minority rights. Their voices are excluded from the formal amendment machinery and can only influence through persuasion and litigation strategy.
% DISAPPEARANCE_RATIONALE: If the constraint vanished—if courts could unilaterally recognize equality's scope expansion without awaiting amendment—the separation between interpretive authority (courts) and amendment authority (supermajority) would collapse. Excluded groups would gain immediate voice (courts recognizing their equality) but democratic legislatures would lose veto power over constitutional scope. Constitutional authority would shift decisively toward judicial supremacy, and the amendment process would become advisory rather than controlling for scope questions.
% FOUNDING_PROBLEM: Distinguishing between the Constitution's binding textual authority and the judges' role as its interpreters. Early constitutional crisis: judges faced increasing pressure to recognize equality's application to groups (women, enslaved persons, sexual minorities) excluded by historical practice, but unilateral judicial redefinition would undermine the amendment process as the supreme mechanism for constitutional change. The founding problem was: how can courts honor equality's principle while respecting the democratic amendment process?
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars of the progressive textualist and originalist schools attest the problem is live and foundational. State legislatures attest it by insisting on their amendment veto power. Courts attest it by their patterns of restraint on expansionist readings pending amendment (e.g., explicit statements that equal protection could reach sex discrimination if the Court chose, but waiting for amendment is preferable). Universalist advocates and activists dispute it, arguing the problem is overblown and courts should recognize equality's scope immediately. Independent constitutional historians document the deliberate creation of the amendment process as distinct from interpretation in the Framers' design.
narrative_ontology:disappearance_verdict(equality_clause_scope__progressive_textualist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__progressive_textualist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__progressive_textualist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equality_clause_scope__progressive_textualist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__progressive_textualist, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__progressive_textualist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_clause_scope__progressive_textualist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equality_clause_scope__progressive_textualist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58 at interval end) because the constraint extracts political opportunity from marginalized groups seeking immediate equality recognition—they must wait for amendment rather than persuade courts. Suppression is substantial (0.62): the constraint's persistence depends on actively preventing courts from recognizing equality claims that majorities have not yet amended to include, which means suppressing plausible judicial redefinitions and defending the amendment supermajority as necessary. Theater ratio is low-moderate (0.28): the amendment process is largely functional (real amendments have occurred—19th Amendment for women, 26th for voting age), but there is performative rhetoric around constitutional fidelity that masks the extraction mechanism. The measurement series shows extractiveness rising through the mid-20th century (pressure from civil rights movements, Cold War equality rhetoric) and declining in recent decades (successful amendments like the 26th; reduced pressure from universalist courts due to established precedent). Suppression spiked during the 1960s–90s (active enforcement against expansionist judicial readings, e.g., resistance to sex-discrimination reasoning), then moderated as amendment became politically legible again (multiple successful amendments, reduced institutional conflict). Theater ratio peaked in the 1960s when courts were most visibly restrained, then stabilized as the reading became more normalized.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional majority's seat, this constraint is a virtuous protection: it prevents courts from unilaterally redefining the Constitution and ensures that constitutional change requires democratic legitimacy. From the excluded group's seat, it is a trap: they are denied immediate equality recognition and must wait for a supermajority coalition that may never materialize. From the progressive court's seat, it is a binding limitation: they see equality's logic extending further than the current text admits, but the reading forbids them from following that logic without amendment. The engine will compute different constraint types from these three seats: institutional majorities and states see rope-like coordination (they collectively control the amendment process); excluded groups see snare-like extraction (they are blocked from immediate relief and have no power to amend); courts see tangled rope (they gain legitimacy authority in exchange for accepting extraction—the suppression of their own interpretive impulse). The authored metrics describe the average experience; the per-seat computation reveals the asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Excluded groups are the primary victims: they bear the cost (non-inclusion) and have no meaningful exit (cannot amend on their own authority, constrained to persuasion campaigns that may fail). Political majorities and state legislatures are the primary beneficiaries: they control amendment gates and benefit from the veto power the constraint guarantees. Progressive courts are dual-positioned: they benefit from being cast as the legitimacy guardians of the constitutional text (the constraint vindicates their interpretive authority within bounds), but they pay by forgoing expansionist readings they might otherwise support. Directionality for excluded groups is near 1.0 (full target): high extraction, high suppression, no exit. For institutional majorities, d is near 0.0 (full beneficiary): they control the mechanism, incur no direct cost, can exercise or withhold amendment authority. For courts, d is around 0.5 (symmetric): genuine coordination benefit (their interpretive authority is protected) offset by genuine cost (their expansionist impulse is suppressed). The override mechanism is not needed here because the derivation from beneficiary/victim declarations captures the structure accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem was preventing unilateral judicial redefinition of constitutional scope (live in 1868, urgent during Reconstruction). The problem persists (contested status): majorities and state governments attest it is live; universalist advocates and expansionist courts attest it is largely solved (the constitutional tradition has matured, courts are sophisticated about the boundary). The constraint prevents misclassification of extraction as pure coordination by insisting on the explicit cost: excluded groups are NOT receiving a benefit from the amendment-gating mechanism; they are paying the cost of waiting. A classification of the constraint as pure 'Rope' (coordination without extraction) would be false because it erases the excluded group's situation. A classification as pure 'Snare' (extraction without coordination) misses that the amendment process IS genuinely democratic and occasionally succeeds (19th Amendment, 26th Amendment, state-level expansions). Tangled Rope captures both: there is real coordination (the amendment process solves a genuine problem of distinguishing interpretation from innovation), AND asymmetric extraction (some parties benefit from the amendment gate, others pay the cost of waiting). Mandatrophy is not present here: the constraint's founding problem has not been forgotten or denied; it is actively disputed, which is the normal state of a constitutional constraint in live politics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretation_vs_amendment_boundary,
    'Where is the boundary between incremental interpretation (within authority of courts) and genuine scope expansion (requiring amendment)? Is equal protection for women incremental interpretation of 1868 equality, or a scope expansion?',
    'Historical practice: test the boundary against successful amendments (19th Amendment for women, 26th for voting age) and failed expansionist jurisprudence (courts'' failed attempts to extend equality without amendment). Map which claims courts explicitly refrained from making pending amendment, and which claims courts made confident they were interpreting existing scope.',
    'A narrow boundary (almost all expansion is deemed scope-change requiring amendment) makes the reading more textualist and more extractive of excluded groups. A wide boundary (many expansions are deemed incremental interpretation) weakens the amendment-gating function and approaches universalist territory. The constraint''s extractiveness depends on where the boundary is drawn.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretation_vs_amendment_boundary, empirical, 'The location of the interpretation/amendment boundary determines extractiveness.').

omega_variable(
    supermajority_gating_vs_tyranny_of_majority,
    'Does the supermajority amendment requirement protect constitutional stability and deliberation, or does it enable a kind of tyranny of the entrenched majority, locking excluded groups out of equal citizenship?',
    'Comparative constitutional study: test against constitutions with different amendment thresholds, and against history of groups locked out of amendment windows (how long did women, racial minorities, sexual minorities wait for amendment or interpretation-driven expansion; did the wait cause measurable harm; could interpretation have closed the gap faster).',
    'If amendment-gating is genuinely protective of deliberation and minority rights (minority-protective reading of supermajority), the constraint is moderate tangled rope. If amendment-gating locks groups out of timely rights recognition (majority-entrenchment reading), the constraint is closer to snare. The constraint''s classification per-seat depends on this empirical and normative question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supermajority_gating_vs_tyranny_of_majority, preference, 'Whether amendment-gating is protective deliberation or majority entrenchment.').

omega_variable(
    amendment_as_democratic_legitimacy_vs_veto_by_intensity,
    'Is the supermajority amendment requirement a genuine expression of democratic legitimacy (reflecting actual widespread consensus), or a structural veto granted to geographically dispersed minorities (state legislatures'' supermajority can be a minority of national population)?',
    'Empirical: compare voting power distribution in state legislatures to population distribution; test whether successful amendments reflect national consensus or geographic coalition power. Survey evidence on public opinion vs. amendment passage rates.',
    'If supermajority reflects genuine consensus, the constraint gains democratic legitimacy and is less extractive of excluded groups (they can argue they lack consensus support, not that they are suppressed). If supermajority is a structural veto by geographic minorities, the constraint is more extractive—excluded groups are locked out by a non-representative coalition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_as_democratic_legitimacy_vs_veto_by_intensity, empirical, 'Whether amendment supermajority reflects democratic consensus or geographic veto power.').

omega_variable(
    committer_constraint_ambiguity,
    'Is this reading fundamentally about the equality clause''s scope, or is it fundamentally about the authority to interpret the Constitution (courts vs. amendment process)?',
    'Genealogy: trace the reading''s emergence and defense. If it emerged primarily to resolve questions about whose voice counts in equality recognition (excluded groups'' voice vs. majorities'' voice), it is scope-focused. If it emerged primarily to defend the amendment process against expansionist judicial readings, it is authority-focused. The reading''s framers and current advocates will clarify where they see the core concern.',
    'If scope-focused, the constraint is about WHO the equality clause covers (original scope vs. expanded scope). If authority-focused, the constraint is about WHO decides scope (courts vs. democracies). The framing affects classification: scope-focus aligns with tangled rope (coordination on interpretation + extraction from excluded groups); authority-focus aligns more with piton or even snare (the constraint is machinery for defending institutional authority, not genuinely coordinating on equality''s meaning). This omega documents which framing the progressive textualist reading itself endorses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_constraint_ambiguity, conceptual, 'Whether the reading is fundamentally scope-focused or authority-focused.').

omega_variable(
    textual_amendment_vs_living_tradition,
    'Is the amendment process the only legitimate way to expand scope, or do constitutional traditions legitimately evolve through practices and interpretations that are not formally textual?',
    'Meta-constitutional question: query the reading''s own theory of constitutional legitimacy. Ask whether a practice accepted across generations and generations of interpretation counts as a legitimate scope expansion even absent a formal amendment. The reading''s answer determines whether it truly requires text-change or permits tradition-change.',
    'If only textual amendment counts, the reading is strictly textualist and highly extractive (excluded groups locked out pending text-change). If tradition-change counts, the boundary softens and the constraint becomes closer to rope (interpretation within tradition can expand scope). This is the deepest ambiguity in the reading''s own premises.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_amendment_vs_living_tradition, conceptual, 'Whether the reading permits constitutional evolution through tradition or requires formal amendment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__progressive_textualist, 1868, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1868, equality_clause_scope__progressive_textualist, theater_ratio, 1868, 0.18).
narrative_ontology:measurement(equa_tr_t1920, equality_clause_scope__progressive_textualist, theater_ratio, 1920, 0.22).
narrative_ontology:measurement(equa_tr_t1964, equality_clause_scope__progressive_textualist, theater_ratio, 1964, 0.31).
narrative_ontology:measurement(equa_tr_t1992, equality_clause_scope__progressive_textualist, theater_ratio, 1992, 0.28).
narrative_ontology:measurement(equa_tr_t2012, equality_clause_scope__progressive_textualist, theater_ratio, 2012, 0.26).
narrative_ontology:measurement(equa_tr_t2024, equality_clause_scope__progressive_textualist, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(equa_be_t1868, equality_clause_scope__progressive_textualist, base_extractiveness, 1868, 0.48).
narrative_ontology:measurement(equa_be_t1920, equality_clause_scope__progressive_textualist, base_extractiveness, 1920, 0.54).
narrative_ontology:measurement(equa_be_t1964, equality_clause_scope__progressive_textualist, base_extractiveness, 1964, 0.61).
narrative_ontology:measurement(equa_be_t1992, equality_clause_scope__progressive_textualist, base_extractiveness, 1992, 0.65).
narrative_ontology:measurement(equa_be_t2012, equality_clause_scope__progressive_textualist, base_extractiveness, 2012, 0.62).
narrative_ontology:measurement(equa_be_t2024, equality_clause_scope__progressive_textualist, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1868, equality_clause_scope__progressive_textualist, suppression_requirement, 1868, 0.44).
narrative_ontology:measurement(equa_su_t1920, equality_clause_scope__progressive_textualist, suppression_requirement, 1920, 0.51).
narrative_ontology:measurement(equa_su_t1964, equality_clause_scope__progressive_textualist, suppression_requirement, 1964, 0.67).
narrative_ontology:measurement(equa_su_t1992, equality_clause_scope__progressive_textualist, suppression_requirement, 1992, 0.71).
narrative_ontology:measurement(equa_su_t2012, equality_clause_scope__progressive_textualist, suppression_requirement, 2012, 0.66).
narrative_ontology:measurement(equa_su_t2024, equality_clause_scope__progressive_textualist, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__progressive_textualist, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equality_clause_scope__progressive_textualist, 0.25).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, equality_clause_scope__restrictive_originalist).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, equality_clause_scope__expansive_universalist).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, judicial_supremacy_vs_democratic_amendment).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, constitutional_amendment_supermajority_threshold).

% DUAL FORMULATION NOTE:
% This story is one reading of the equality_clause_scope kernel. The restrictive_originalist and expansive_universalist readings are sibling constraints instantiating different readings of the same kernel text. All three readings share the referent (the equality clause as written), but differ on scope (what it covers) and on the mechanism for scope-change (amendment vs. interpretation vs. narrow original only). The constraint family is linked: each reading influences and coexists with the others. The progressive textualist reading differs structurally from both siblings: it acknowledges the original scope was narrow (distance from restrictive originalism) and permits expansion (distance from expansive universalism), but requires democratic amendment rather than judicial recognition (the distinguishing mechanism). Epsilon values differ across readings because each reading assesses extractiveness of the SAME constraint (the amendment-gating mechanism) from its own lights: restrictive originalists see the gating as protecting legitimate original boundaries (low extraction for them); expansive universalists see it as unjustly suppressing obvious equality (high extraction for them); progressive textualists see it as moderate tangled rope (genuine coordination + asymmetric extraction bundled).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
