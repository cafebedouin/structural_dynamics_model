% ============================================================================
% CONSTRAINT STORY: constitutional_supremacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_supremacy, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_supremacy
 *   human_readable: The Supremacy of Written Constitutions and Judicial Review
 *   domain: legal/political
 *
 * SUMMARY:
 *   Constitutional supremacy—the doctrine that a written constitution is the
 *   supreme law of the land and that courts may invalidate laws violating
 *   it—establishes one of the fundamental constraints of liberal democratic
 *   governance. Originating from Marbury v. Madison (1803), this constraint
 *   creates a structural tension between democratic majoritarianism (the
 *   people's will expressed through elected legislatures) and constitutional
 *   entrenchment (binding commitments made by past actors that constrain
 *   present majorities). The constraint exhibits six distinct classifications
 *   depending on the observer's structural position. For temporary
 *   legislative majorities, judicial review appears as pure extraction—they
 *   are trapped by judicial supremacy that they cannot override. For judges,
 *   it appears as coordination—the mechanism that legitimates their
 *   institutional role and solves the problem of constitutional fidelity. For
 *   organized reform movements, it appears as scaffold with a sunset
 *   clause—the amendment process provides a real exit mechanism, albeit a
 *   difficult one. For comparative constitutional democracies, the American
 *   commitment to judicial supremacy appears as piton—a historical
 *   contingency maintained through institutional inertia, not inherent
 *   necessity. The constraint's theater ratio has increased over two
 *   centuries as legal doctrine has become more elaborate, producing more
 *   performative constitutional interpretation even as the functional
 *   verification of constitutionality remains contested.
 *
 * KEY AGENTS:
 *   - Legislative Majorities: Primary victim (powerless/trapped) — cannot override judicial determinations; their statutes are subordinate
 *   - Executive Branch: Secondary victim (powerless/trapped) — executive actions face judicial invalidation; enforcement power is constrained
 *   - Judicial Institutions: Primary beneficiary (institutional/arbitrage) — experience constitutional supremacy as establishing their interpretive authority; can choose strategic interpretation or deference
 *   - Constitutional Continuity: Beneficiary (institutional/arbitrage) — abstract institutional value that benefits from being tied to higher law and protected from majoritarian whim
 *   - Constrained Majorities: Mixed (moderate/constrained) — experience both coordination (protection from tyranny) and extraction (inability to execute policy)
 *   - Constitutional Reform Movements: Organized actors (organized/constrained) — face suppression through difficult amendment process but have genuine exit mechanism via constitutional change
 *   - Comparative Democracies: Analytical observer (analytical/analytical) — parliamentary and weaker-review systems function without judicial supremacy, revealing it as contingent choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_supremacy, 0.38).
domain_priors:suppression_score(constitutional_supremacy, 0.48).
domain_priors:theater_ratio(constitutional_supremacy, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_supremacy, extractiveness, 0.38).
narrative_ontology:constraint_metric(constitutional_supremacy, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(constitutional_supremacy, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_supremacy, tangled_rope).
narrative_ontology:human_readable(constitutional_supremacy, "The Supremacy of Written Constitutions and Judicial Review").
narrative_ontology:topic_domain(constitutional_supremacy, "legal/political").

domain_priors:requires_active_enforcement(constitutional_supremacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_supremacy, judicial_institutions).
narrative_ontology:constraint_beneficiary(constitutional_supremacy, constitutional_continuity).
narrative_ontology:constraint_victim(constitutional_supremacy, legislative_majorities).
narrative_ontology:constraint_victim(constitutional_supremacy, executive_prerogative).
narrative_ontology:constraint_victim(constitutional_supremacy, popular_sovereignty_in_moment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIRCUMVENTED LEGISLATOR (SNARE) — A legislature passing a statute faces the hard constraint that courts may invalidate it. The legislative majority has no exit option; they are trapped in a subordinate institutional position. They cannot dissolve the courts, rewrite the constitution by simple majority, or override judicial review. The constraint extracts from them by preventing their will from becoming law.
constraint_indexing:constraint_classification(constitutional_supremacy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FRUSTRATED EXECUTIVE (SNARE) — An executive enforcing or proposing policy faces judicial invalidation. The executive power is checked and constrained at every level. No exit option exists except leaving office. The constraint prevents executive action from being the final word.
constraint_indexing:constraint_classification(constitutional_supremacy, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: JUDICIAL GUARDIAN (ROPE) — Courts experience constitutional supremacy as a coordination mechanism that elevates their institutional role. Judges have arbitrage options: they can interpret the constitution creatively, defer to other branches, or assert supremacy, depending on strategic needs. The constraint benefits them by establishing their institutional supremacy over coordinate branches. Judicial review is coordination logic that solves the problem of constitutional fidelity.
constraint_indexing:constraint_classification(constitutional_supremacy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTRAINED MAJORITY (TANGLED ROPE) — A coalition of voters and legislators holding a temporary electoral majority experiences the constraint as both coordination and extraction. Constitutional supremacy prevents tyranny of the majority (coordination benefit) but also prevents rapid policy change and majoritarian will from being executable (extraction cost). Exit options are constrained: citizens can vote, but constitutional entrenchment limits what voting accomplishes.
constraint_indexing:constraint_classification(constitutional_supremacy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL REFORM MOVEMENT (SCAFFOLD) — Organized movements for constitutional amendment (18th Amendment prohibition, 21st Amendment repeal, civil rights amendments) experience judicial review as a temporary constraint with a sunset clause. The constraint forces slower, more deliberate change via the amendment process rather than legislation, but the amendment process itself is the exit mechanism. High suppression (difficult amendment) but with a genuine exit path. Theater remains moderate because amendment ratification is performative of constitutional legitimacy.
constraint_indexing:constraint_classification(constitutional_supremacy, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: COMPARATIVE OBSERVER (PITON) — From a global/civilizational view, many constitutional democracies function without judicial review (parliamentary supremacy in UK, Commonwealth systems) or with weak review (Canada's notwithstanding clause). The American insistence on judicial supremacy appears as a historical contingency maintained through institutional inertia. Courts perform constitutional guardianship ritually even in cases where legislative intent is clear and amendment would be democratically superior. The constraint persists because no alternative has replaced it, not because it is functionally indispensable.
constraint_indexing:constraint_classification(constitutional_supremacy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CONSTITUTIONAL LOGIC (MOUNTAIN) — From a universal/analytical view, some form of supreme law is structurally necessary in any written constitutional system. If the constitution is not supreme, it is not a binding constraint — it is merely preamble. The logical requirement that a constitution either is or is not supreme appears immutable. However, this perspective risks naturalizing the specific form (judicial supremacy via review) as inherent to constitutionalism itself. The analytical engine will flag this as a false summit: logical necessity of constitutional supremacy does not entail that courts must enforce it.
constraint_indexing:constraint_classification(constitutional_supremacy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_supremacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_supremacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_supremacy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(constitutional_supremacy, TR),
    TR >= 0.70.

:- end_tests(constitutional_supremacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts from legislative majorities by preventing their will from becoming law, but not absolutely. Legislatures can work within judicial constraints, pass statutes as modified by interpretation, and pursue amendment. The extraction is significant but not total—judges do not have unilateral power to rewrite statutes, and legislative intent constrains interpretation. Suppression (0.48): Moderate-high. The constraint suppresses alternatives: legislative majorities cannot simply override the courts, executives cannot ignore judicial orders, and ordinary legislation cannot amend the constitution. But suppression is not total—amendment is possible (though difficult), and reinterpretation over time provides some flexibility. Theater ratio (0.65): Moderate-high. Constitutional interpretation has become increasingly performative. Judges engage in elaborate doctrinal rituals (strict scrutiny, rational basis review, textualism vs. purposivism) to reach conclusions that were often determined by prior political commitments. The theater has increased over the interval as legal doctrine has accumulated and constitutional law has become more specialized and inscrutable to non-lawyers. Claimed type (Tangled Rope): The constraint combines genuine coordination function (constitutional fidelity, protection from tyranny) with asymmetric extraction (majoritarian will is subordinate to judicial interpretation). Both functions are real—this is not a case where one dominates.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the judicial guardian (Rope) and the circumvented legislator (Snare) is maximal. The judge experiences the constraint as coordination—a mechanism that legitimates their role and solves the genuine problem of keeping legislative power within constitutional bounds. The legislator experiences it as pure extraction—their democratic mandate is thwarted by an unelected court interpreting eighteenth-century text. The constrained majority (Tangled Rope) experiences both—they benefit from constitutional protection but suffer from inability to execute policy. The scaffold perspective (Constitutional Reform Movement) identifies a real exit mechanism that pure snare agents lack: constitutional amendment. The piton perspective (Comparative Observer) reveals that judicial supremacy is not necessary to constitutionalism—many democracies function effectively with parliamentary supremacy or weak review. The mountain perspective (Analytical Observer) risks naturalizing a contingent institutional choice as a logical necessity of written constitutionalism.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures the agent's structural position relative to the extraction flow. Judges (institutional/arbitrage) have low d: they benefit from supremacy and can choose their degree of engagement. Legislative majorities (powerless/trapped) have high d: they bear the cost of subordination with no exit. Organized reformers (organized/constrained) have moderate d: they face barriers but have amendment as a real exit mechanism. The analytical observer has high d as well (cannot exit the observation): sees the full structure including false summits. The engine derives d from these exit options and structural positions. No override is needed; the structural data itself produces the perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   Constitutional supremacy resolves the mandatrophy by showing that the constraint is legitimately tangled rope, not pure snare misclassified as rope. The coordination function is genuine: constitutions do need supremacy to be binding, and majoritarian impulses do need checking. The extraction function is also genuine: present majorities are prevented from executing their will by past constitutional choices and judicial interpretation. Both functions coexist. The mandatrophy is resolved by recognizing that this particular constraint cannot be simplified into either pure coordination or pure extraction without losing explanatory power. Judicial review as practiced involves both genuine constitutional fidelity and genuine majoritarian subordination. The theater ratio increasing (0.35→0.65) suggests that the performative aspect is growing, potentially masking the real extraction underneath elaborate doctrinal ritual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_review_necessity,
    'Is judicial review the only mechanism by which written constitutional supremacy can be enforced, or do alternative enforcement mechanisms (legislative self-restraint, public accountability, amendment supermajority requirements) substitute effectively?',
    'Comparative constitutional study of enforcement mechanisms in jurisdictions with and without judicial review; historical analysis of constitutional compliance in periods before Marbury v. Madison and in systems without review courts',
    'If only mechanism: judicial supremacy is logically necessary (Mountain). If substitutes exist: judicial review is a contingent institutional choice (Tangled Rope/Piton).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_review_necessity, conceptual, 'Whether judicial review is necessary to enforce constitutional supremacy').

omega_variable(
    constitutional_amendment_accessibility,
    'Is the amendment process (Article V) sufficiently accessible to function as a genuine exit mechanism for minorities locked out by constitutional interpretation, or is it so procedurally difficult that it is effectively blocked?',
    'Historical frequency and success rates of amendment attempts; empirical study of amendment feasibility for major policy reversals; comparison to amendment accessibility in other democracies',
    'If accessible: scaffold perspective is accurate—amendment provides real exit. If blocked: the constraint is pure snare for majorities wanting to override courts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_amendment_accessibility, empirical, 'Whether constitutional amendment is accessible enough to be a real exit mechanism').

omega_variable(
    majoritarian_tyranny_prevention,
    'How much protection against majoritarian tyranny does judicial review actually provide beyond what legislative supermajority requirements and bicameralism provide?',
    'Historical analysis of tyrannies attempted/prevented by judicial intervention; comparison of outcomes in pure majoritarian vs. constitutionally entrenched systems',
    'If substantial: rope/coordination perspective validated. If minimal: judicial review is primarily extraction from majorities rather than protection against tyranny.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_tyranny_prevention, empirical, 'Actual protective effect of judicial review against majoritarian tyranny').

omega_variable(
    constitutional_entrenchment_cost,
    'What are the true costs of constitutional entrenchment in terms of policy lag, inability to correct errors, and institutional sclerosis?',
    'Empirical comparison of policy responsiveness in high-entrenchment (US, Australia) vs. low-entrenchment (UK, New Zealand) systems; analysis of constitutional amendments that reversed deeply entrenched errors',
    'If costs are high: snare extraction from majorities is severe. If costs are low: coordination benefits outweigh extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_entrenchment_cost, empirical, 'Costs of constitutional entrenchment and policy rigidity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_supremacy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(const_tr_t0, constitutional_supremacy, theater_ratio, 0, 0.35).
narrative_ontology:measurement(const_tr_t5, constitutional_supremacy, theater_ratio, 5, 0.5).
narrative_ontology:measurement(const_tr_t10, constitutional_supremacy, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(const_be_t0, constitutional_supremacy, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(const_be_t5, constitutional_supremacy, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(const_be_t10, constitutional_supremacy, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_supremacy, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_supremacy, separation_of_powers).
narrative_ontology:affects_constraint(constitutional_supremacy, legislative_supremacy_alternative).
narrative_ontology:affects_constraint(constitutional_supremacy, popular_sovereignty_entrenchment).

% DUAL FORMULATION NOTE:
% Constitutional supremacy decomposes into two structurally distinct claims: (1) that written constitutions must be supreme to any ordinary law (ε~0.08, Mountain), and (2) that courts are the proper enforcers of constitutional supremacy through judicial review (ε~0.38, Tangled Rope). Story focuses on (2). Story (1) would emphasize logical necessity of supreme law in any written constitutional system; this story emphasizes the contingent institutional choice to vest enforcement in courts rather than legislatures, voters, or constitutional commissions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
