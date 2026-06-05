% ============================================================================
% CONSTRAINT STORY: expression_conscience_amendments__establishment_clause
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_expression_conscience_amendments__establishment_clause, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: expression_conscience_amendments__establishment_clause
 *   human_readable: Establishment Clause: State Secularism & Religious Freedom
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   The Establishment Clause of the First Amendment ('Congress shall make no
 *   law respecting an establishment of religion') forbids government
 *   endorsement of, favoritism toward, or institutional entanglement with
 *   religion. This reading instantiates one specific claim within the larger
 *   expression-and-conscience constitutional kernel: that political stability
 *   and genuine religious freedom in pluralistic societies require the state
 *   to remain formally neutral on religious matters — neither endorsing
 *   majoritarian faith traditions nor suppressing minority belief. The
 *   constraint operates through legal prohibition (formal rule against state
 *   religious establishment), institutional enforcement (courts striking down
 *   religious legislation and public religious symbols), and political
 *   norm-setting (treating state secularism as a constitutional commitment).
 *   The beneficiaries are religious minorities and the nonreligious, who are
 *   protected from compulsory participation in state-endorsed religious
 *   ritual. The victims are majoritarian religious groups whose goal of
 *   public religious affirmation is blocked. The extractiveness is moderate
 *   because the clause does accomplish genuine coordination (protecting
 *   religious liberty across traditions) while also performing extraction
 *   (denying majorities access to state machinery for advancing their faith).
 *
 * KEY AGENTS:
 *   - Religious minorities (powerless/trapped): Face compulsory participation in majoritarian religious ritual; Establishment Clause forbids this, but enforcement gaps persist
 *   - Nonreligious citizens (moderate/constrained): Benefit from state secularism; face social stigma and career constraints; organized through civil rights and secular groups
 *   - Civil rights & secular organizations (institutional/arbitrage): Primary institutional beneficiaries; Establishment Clause enables their litigation strategy
 *   - Majoritarian religious groups (powerless/trapped in baseline; organized/constrained when mobilized): Lose access to state apparatus for advancing faith; cannot exit without abandoning goal of public religious affirmation
 *   - Religious majority political coalitions (organized/constrained): Can mobilize legal challenges and ballot initiatives; constrained by the rule but have strategic options
 *   - Courts and legal system (institutional/analytical): Enforce the clause; interpret its scope; mediate the competing claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(expression_conscience_amendments__establishment_clause, 0.38).
domain_priors:suppression_score(expression_conscience_amendments__establishment_clause, 0.52).
domain_priors:theater_ratio(expression_conscience_amendments__establishment_clause, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(expression_conscience_amendments__establishment_clause, extractiveness, 0.38).
narrative_ontology:constraint_metric(expression_conscience_amendments__establishment_clause, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(expression_conscience_amendments__establishment_clause, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(expression_conscience_amendments__establishment_clause, tangled_rope).
narrative_ontology:human_readable(expression_conscience_amendments__establishment_clause, "Establishment Clause: State Secularism & Religious Freedom").
narrative_ontology:topic_domain(expression_conscience_amendments__establishment_clause, "political/legal/constitutional").

domain_priors:requires_active_enforcement(expression_conscience_amendments__establishment_clause).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(expression_conscience_amendments__establishment_clause, 'f3ae38dd-8539-4a56-a4d3-2e4df6c9f7af').
narrative_ontology:cs_kernel_codification('f3ae38dd-8539-4a56-a4d3-2e4df6c9f7af', fixed_text).
narrative_ontology:cs_authority_grounding('f3ae38dd-8539-4a56-a4d3-2e4df6c9f7af', lineage).
narrative_ontology:cs_interpretation_layer_present('f3ae38dd-8539-4a56-a4d3-2e4df6c9f7af').
narrative_ontology:cs_reading_relation('f3ae38dd-8539-4a56-a4d3-2e4df6c9f7af', expression_conscience_amendments__free_exercise_clause, coexists_with).
narrative_ontology:cs_reading_relation('f3ae38dd-8539-4a56-a4d3-2e4df6c9f7af', expression_conscience_amendments__free_speech_clause, coexists_with).
narrative_ontology:cs_reading_relation('f3ae38dd-8539-4a56-a4d3-2e4df6c9f7af', expression_conscience_amendments__free_press_clause, coexists_with).
narrative_ontology:cs_reading_relation('f3ae38dd-8539-4a56-a4d3-2e4df6c9f7af', expression_conscience_amendments__assembly_petition_clause, coexists_with).
narrative_ontology:cs_axiom('f3ae38dd-8539-4a56-a4d3-2e4df6c9f7af', foundational, state_neutrality_enables_religious_freedom).
narrative_ontology:cs_axiom_status(state_neutrality_enables_religious_freedom, holdable).
narrative_ontology:cs_axiom_grounding('f3ae38dd-8539-4a56-a4d3-2e4df6c9f7af', state_neutrality_enables_religious_freedom, deontological).
narrative_ontology:cs_axiom('f3ae38dd-8539-4a56-a4d3-2e4df6c9f7af', foundational, majoritarian_religious_expression_cannot_use_state_machinery).
narrative_ontology:cs_axiom_status(majoritarian_religious_expression_cannot_use_state_machinery, holdable).
narrative_ontology:cs_axiom_grounding('f3ae38dd-8539-4a56-a4d3-2e4df6c9f7af', majoritarian_religious_expression_cannot_use_state_machinery, deontological).
narrative_ontology:cs_reference_frame('f3ae38dd-8539-4a56-a4d3-2e4df6c9f7af', state_religious_neutrality_as_constitutional_commitment).
narrative_ontology:cs_drift_state('f3ae38dd-8539-4a56-a4d3-2e4df6c9f7af', contemporary_legal_pluralism_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('f3ae38dd-8539-4a56-a4d3-2e4df6c9f7af', '').
narrative_ontology:cs_kernel_id(expression_conscience_amendments__establishment_clause, expression_conscience_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(expression_conscience_amendments__establishment_clause, religious_minorities).
narrative_ontology:constraint_beneficiary(expression_conscience_amendments__establishment_clause, nonreligious_citizens).
narrative_ontology:constraint_victim(expression_conscience_amendments__establishment_clause, majoritarian_religious_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RELIGIOUS MINORITY (SNARE) — Faces compulsory participation in state-endorsed majoritarian religious ritual (prayer in schools, religious symbols in courthouses, tax-funded religious education). Cannot exit without abandoning public institutions or violating conscience. The Establishment Clause forbids this extraction, but enforcement gaps leave minorities trapped between legal rights (on paper) and social pressure (in practice). Maximum extraction for those with no structural power to resist.
constraint_indexing:constraint_classification(expression_conscience_amendments__establishment_clause, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NONRELIGIOUS CITIZENS (TANGLED ROPE) — Benefit from the Establishment Clause's prohibition on state religious endorsement (coordination function: public neutrality). But also experience suppression: social stigma against atheism/agnosticism, barriers to holding office in some jurisdictions, constrained exit due to career and family costs. The clause coordinates secularism as a legitimate public stance while suppressing majoritarian pressure — genuine coordination benefit plus asymmetric extraction from those who would enforce religious conformity.
constraint_indexing:constraint_classification(expression_conscience_amendments__establishment_clause, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CIVIL RIGHTS & SECULAR ORGANIZATIONS (ROPE) — Primary institutional beneficiaries. The Establishment Clause enables their existence and litigation strategy (separation of church and state). They experience the constraint as coordination: the clause solves the collective action problem of protecting minority rights against majoritarian religious pressure. Exit option is arbitrage — these organizations can allocate litigation resources across jurisdictions and issue areas. Pure coordination function from this perspective.
constraint_indexing:constraint_classification(expression_conscience_amendments__establishment_clause, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MAJORITARIAN RELIGIOUS GROUPS (SNARE) — Experience the Establishment Clause as extraction: cannot use state machinery to advance their faith tradition (prayer in schools, religious symbols in courthouses, public religious education funded by taxes). The constraint forbids compelled extraction FROM minorities but also forbids voluntary USE of state power FOR majorities' religious goals. Majorities are trapped by the rule itself — cannot exit without abandoning their (legitimate) goal of public religious affirmation. This perspective shows the extraction IS reciprocal but directional: majorities lose access to state apparatus; minorities gain protection from state-enforced conversion pressure.
constraint_indexing:constraint_classification(expression_conscience_amendments__establishment_clause, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: ORGANIZED RELIGIOUS MAJORITIES (TANGLED ROPE) — As organized political actors (evangelical coalitions, Catholic Bishops Conference, LDS leadership), majorities are constrained by but also benefit from the Establishment Clause. Benefit: predictability (they know what state neutrality means — prevents unexpected displacement of their traditions). Constraint: cannot directly legislate religious preference. But organized majorities have strategic options (political coalition-building, ballot initiatives to change the clause itself, public persuasion campaigns). The clause is simultaneously an extraction mechanism (blocks direct state support) and a coordination mechanism (stabilizes pluralism and makes religious practice safe by keeping state out). Moderate experienced extraction because organized agents have exit paths (legal challenges, political mobilization).
constraint_indexing:constraint_classification(expression_conscience_amendments__establishment_clause, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the Establishment Clause can appear to articulate a logical necessity: state neutrality on religion is a structural requirement for both political stability and religious freedom in pluralistic societies. A government that endorses any faith tradition necessarily excludes others; true religious freedom requires state non-endorsement. This reads as a natural law of political science. However, the structural data (beneficiaries, victims, suppression) contradicts the mountain classification — this is a false summit, naturalizing what is actually a contingent choice to distribute power in a specific way (toward protection of minorities, away from majoritarian theocracy).
constraint_indexing:constraint_classification(expression_conscience_amendments__establishment_clause, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(expression_conscience_amendments__establishment_clause_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(expression_conscience_amendments__establishment_clause, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(expression_conscience_amendments__establishment_clause, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(expression_conscience_amendments__establishment_clause_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The Establishment Clause accomplishes two structural things simultaneously: (1) it coordinates religious neutrality as a public norm, protecting both religious minorities and the nonreligious from state-mandated conversion pressure; (2) it extracts from majoritarian religious groups by blocking their access to state power for advancing their faith traditions. Neither effect is zero. The moderate value reflects genuine coordination (the clause solves the collective action problem of protecting minority belief in a democracy) balanced against real extraction (majorities lose access to state endorsement). The extraction is not as severe as a pure snare because it is symmetrical — majorities cannot use state machinery to compel religious conformity; minorities cannot either. Both are blocked. Theater ratio (0.48): Moderate-low. The Establishment Clause has substantial functional content (courts do enforce it, state religious symbols do get removed, religious education does face funding challenges). But enforcement is uneven: some jurisdictions maintain de facto religious establishment through weak enforcement or creative legal reinterpretation. The moderate-rising trajectory reflects increasing professionalization of enforcement over time (civil rights litigation, clearer doctrine) partially offset by strategic legal challenges attempting to narrow the clause's scope. Suppression (0.52): Moderate-high. The clause suppresses majoritarian religious expression in official settings and nonreligious public criticism of religion in contexts where that criticism is framed as hostility to faith. Suppression requirement increases over time as society becomes more pluralistic and minority groups gain institutional voice to challenge state religious involvement.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates a stark perspectival gap. Religious minorities trapped in majoritarian jurisdictions classify the constraint as Snare — the Establishment Clause forbids extraction they experience, but enforcement is incomplete. Majoritarian religious groups classify it as Snare in reverse — they experience the clause itself as extraction, forbidding them access to state machinery. But these are not symmetrical experiences: minorities are protected from compulsion; majorities are prevented from using state power, but not compelled to adopt minority beliefs. Organized religious majorities classify as Tangled Rope because they have strategic options (litigation, political mobilization, constitutional amendment). Civil rights organizations classify as Rope — pure coordination function solving the collective action problem of protecting minority belief. The analytical observer risks classifying as Mountain (state-religion separation as logical necessity), but this is a false summit: the beneficiaries and victims reveal a specific distributional choice, not an immutable principle.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious minorities as victims + trapped exit → high d (≈0.90) → high f(d) → they experience high χ from the constraint. But the Establishment Clause is oriented AGAINST their extraction, so beneficiary status flips: minorities benefit structurally even though they experience trapped exit (they are protected from the snare they would otherwise face without the clause). Nonreligious citizens as beneficiaries + constrained exit → moderate d (≈0.50) → moderate f(d) → moderate experienced χ, experienced as coordination benefit. Majoritarian religious groups as victims + trapped exit → high d → high f(d) → they experience high χ from the constraint (extraction: denied access to state machinery). Organized majorities as victims + constrained/arbitrage exit → lower d → lower experienced χ (they have strategic options). The perspectival gap shows that 'd' is not a property of the agent alone but of the agent's relationship to this specific constraint — majorities are trapped by the clause itself, but organized majorities have exit paths (litigation, political mobilization). The engine derives d from beneficiary/victim declarations and exit options automatically, producing the observed perspectival variation.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy for the Establishment Clause is resolved by recognizing that the constraint simultaneously accomplishes coordination AND extraction. This is not a paradox or a classification failure — it is the core structure of a Tangled Rope. Majorities face extraction (denied state apparatus for religious advancement). Minorities face protection from extraction they would otherwise face (compulsory religious participation). Organized actors face reduced extraction because they have strategic options. The false-summit danger is the analytical observer's perspective, which risks naturalizing the specific distributional choice (state secularism protects religious liberty) as a natural law of politics. The omega variables document that this appearance of necessity is actually contestable: neutrality itself may be impossible, the scope of establishment is ambiguous, and the founding intent is disputed. The constraint is real, but its appearance as natural law is a cover story for a contingent institutional choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    clause_scope_ambiguity,
    'Does ''no establishment'' forbid all government involvement with religion, or only government endorsement/favoritism/entanglement that goes beyond equal treatment?',
    'Jurisprudential analysis of Lemon test vs. endorsement test vs. coercion test; empirical review of Supreme Court doctrine evolution; reconstruction of founding-era intent from sources',
    'Narrow reading (endorsement/entanglement only): private religious expression in public spaces remains constitutional (prayer at graduation, nativity scenes). Broad reading (all entanglement): more comprehensive secularization of public institutions. Different reading yields different suppressiveness and extractiveness scores.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(clause_scope_ambiguity, conceptual, 'Ambiguity in scope of establishment clause prohibition').

omega_variable(
    neutrality_paradox,
    'Can the state be truly neutral on religion, or does neutrality itself constitute a form of secular bias that suppresses majoritarian religious expression?',
    'Philosophical analysis of whether neutrality is coherent; empirical measurement of whose interests are advanced by different regulatory postures; comparative study of pluralistic societies with vs. without formal establishment clauses',
    'If neutrality is truly neutral: clause is coordination mechanism protecting both minorities and religious freedom. If neutrality privileges secular actors: clause becomes extraction mechanism favoring nonreligious groups. Shifts beneficiary/victim classification fundamentally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_paradox, conceptual, 'Whether state neutrality on religion is possible or coherent').

omega_variable(
    compulsion_definition_contest,
    'What counts as ''compulsion'' of religious support? Only direct funding? Indirect funding through tax expenditures? Implicit endorsement through symbols and speech? Social pressure?',
    'Historical tracking of what courts have treated as compulsory; analysis of whether taxpayers can opt-out or redirect religious education funding; measurement of actual coercion experienced by minorities vs. felt suppression by majorities',
    'Narrow definition (direct funding only): many current practices survive constitutional scrutiny. Broad definition (symbols, speech, implicit endorsement): requires extensive de-religionization of public institutions. Changes suppression measurement and victim set.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compulsion_definition_contest, empirical, 'Definition of what constitutes compelled support of religion').

omega_variable(
    founding_intent_drift,
    'Is the Establishment Clause grounded in founding-era intent (narrow wall of separation for religious stability) or in contemporary democratic theory (pluralism requires state neutrality)?',
    'Historical analysis of 18th-century sources (Virginia Statute for Religious Freedom, state ratification debates, Founding-era religious composition); comparison with how courts actually apply the clause today',
    'Originalist reading: clause is narrower, more permissive of religious public expression. Living-constitution reading: clause is broader, more protective of minorities. Different readings emit different constraint stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_intent_drift, conceptual, 'Whether establishment clause is grounded in founding intent or contemporary theory').

omega_variable(
    false_summit_naturalization_mechanism,
    'Is the appearance of natural law (state-religion separation as logical necessity) a cover story for a specific distributional choice that benefits minorities and the nonreligious at expense of majoritarian religious expression?',
    'Comparative constitutional analysis: how do societies with different religious demographics handle establishment? Do majoritarian-religious nations claim establishment is necessary natural law? Do secular/plural nations do the same?',
    'If naturalization: the mountain classification fails and the engine''s false-summit detector should reclassify as tangled_rope. The constraint is revealed as contingent institutional design, not immutable principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalization_mechanism, conceptual, 'Whether natural law framing naturalizes a contingent distributional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(expression_conscience_amendments__establishment_clause, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(expr_tr_t0, expression_conscience_amendments__establishment_clause, theater_ratio, 0, 0.42).
narrative_ontology:measurement(expr_tr_t50, expression_conscience_amendments__establishment_clause, theater_ratio, 50, 0.45).
narrative_ontology:measurement(expr_tr_t100, expression_conscience_amendments__establishment_clause, theater_ratio, 100, 0.48).

% Extraction over time
narrative_ontology:measurement(expr_be_t0, expression_conscience_amendments__establishment_clause, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(expr_be_t50, expression_conscience_amendments__establishment_clause, base_extractiveness, 50, 0.35).
narrative_ontology:measurement(expr_be_t100, expression_conscience_amendments__establishment_clause, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(expr_su_t0, expression_conscience_amendments__establishment_clause, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(expr_su_t50, expression_conscience_amendments__establishment_clause, suppression_requirement, 50, 0.48).
narrative_ontology:measurement(expr_su_t100, expression_conscience_amendments__establishment_clause, suppression_requirement, 100, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(expression_conscience_amendments__establishment_clause, identity_coordination).
narrative_ontology:affects_constraint(expression_conscience_amendments__establishment_clause, expression_conscience_amendments__free_exercise_clause).
narrative_ontology:affects_constraint(expression_conscience_amendments__establishment_clause, expression_conscience_amendments__free_speech_clause).
narrative_ontology:affects_constraint(expression_conscience_amendments__establishment_clause, expression_conscience_amendments__free_press_clause).
narrative_ontology:affects_constraint(expression_conscience_amendments__establishment_clause, expression_conscience_amendments__assembly_petition_clause).

% DUAL FORMULATION NOTE:
% The Establishment Clause is one reading of the expression-and-conscience constitutional kernel. Sibling readings (Free Exercise, Free Speech, Free Press, Assembly/Petition) are separate constraint stories with different ε values reflecting their different structural positions within the kernel. The Establishment Clause reading prioritizes state neutrality and minority protection; sibling readings have different focal points. All five readings coexist in constitutional law but instantiate different structural constraints with different beneficiaries, victims, and extraction mechanisms. Network links preserve the kernel structure: each reading affects the others through shared legitimacy authority (the Constitution itself) and interpretive traditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
