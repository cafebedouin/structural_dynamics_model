% ============================================================================
% CONSTRAINT STORY: positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_positivist_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: positivist_reading
 *   human_readable: Positivist Constitutional Authority: Procedural Validity Without Moral Content
 *   domain: constitutional_law/legal_philosophy/interpretive_jurisprudence
 *
 * SUMMARY:
 *   The positivist reading of constitutional authority establishes that law
 *   derives validity from formal enactment procedures and institutional
 *   sources, not from moral content or natural law moorings. This constraint
 *   generates a structural tension: it provides institutional clarity and
 *   decision-procedure determinacy (coordination function) while
 *   simultaneously excluding entire categories of constitutional claims from
 *   legitimate adjudication (extraction function). The constraint exhibits
 *   different types from different institutional perspectives. The legal
 *   positivist institution experiences it as pure coordination (Rope) — it
 *   solves the problem of adjudicating contested constitutionalism without
 *   dragging every moral dispute into law. Substantive rights claimants
 *   experience it as pure extraction (Snare) — their core arguments are ruled
 *   out of bounds by the framework itself. Intermediate courts experience it
 *   as hybrid (Tangled Rope) — coordination through procedural clarity,
 *   extraction through discretion limits. Living constitutionalist scholars
 *   building alternative frameworks experience it as temporary (Scaffold) —
 *   as jurisprudential consensus shifts, the positivist reading's
 *   institutional authority erodes. The doctrine itself, when examined at
 *   civilizational scale, exhibits high theater (0.65): courts routinely
 *   invoke moral principles (substantive due process, equal protection's
 *   dignity norm, Eighth Amendment proportionality) while nominally adhering
 *   to positivist methodology. The apparent objectivity of procedural
 *   formalism masks deeply embedded moral reasoning. This constraint is ONE
 *   READING of the contested kernel constitutional_text_authority, which is
 *   also instantiated by originalist_reading and
 *   living_constitutionalist_reading. The positivist reading's distinctive
 *   feature is its rejection of natural law moorings and emphasis on
 *   institutional source as the exhaustive criterion for validity — what
 *   distinguishes it from originalism (which often invokes natural law or
 *   original public meaning) and living constitutionalism (which openly
 *   embraces evolutionary moral reasoning).
 *
 * KEY AGENTS:
 *   - Institutional Legal Positivist Community: Primary beneficiary (institutional/arbitrage) — captures legitimacy benefit of 'objective' procedure; experiences reading as coordination mechanism
 *   - Substantive Rights Claimants: Primary victim (powerless/trapped) — cannot access moral arguments within the positivist frame; trapped by the boundary definition itself
 *   - Intermediate Court Judges: Secondary actors (moderate/constrained) — constrained by precedent and institutional obligation; benefit from clarity; experience mixed coordination-extraction
 *   - Constitutional Amendment Coalition: Organized agents (powerful/mobile) — mobile and powerful but constrained by high procedural bar for formal amendment; see reading as obstacle but can mobilize amendment processes
 *   - Living Constitutionalist Scholars and Judges: Competing institutional network (organized/constrained) — building alternative theoretical frameworks (living constitutionalism, moral constitutionalism) with sunset logic; see positivism as increasingly indefensible
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing strategic institutional choice as logical necessity; false summit candidate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(positivist_reading, 0.38).
domain_priors:suppression_score(positivist_reading, 0.52).
domain_priors:theater_ratio(positivist_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(positivist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(positivist_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(positivist_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(positivist_reading, tangled_rope).
narrative_ontology:human_readable(positivist_reading, "Positivist Constitutional Authority: Procedural Validity Without Moral Content").
narrative_ontology:topic_domain(positivist_reading, "constitutional_law/legal_philosophy/interpretive_jurisprudence").

domain_priors:requires_active_enforcement(positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(positivist_reading, formalized).
narrative_ontology:cs_authority_grounding(positivist_reading, extraction).
narrative_ontology:cs_interpretation_layer_present(positivist_reading).
narrative_ontology:cs_kernel_id(positivist_reading, constitutional_text_authority).
narrative_ontology:cs_reading_relation(positivist_reading, originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation(positivist_reading, living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_axiom(positivist_reading, foundational, law_morality_distinction_exhaustive).
narrative_ontology:cs_axiom_status(law_morality_distinction_exhaustive, holdable).
narrative_ontology:cs_axiom(positivist_reading, foundational, institutional_source_validity_criterion).
narrative_ontology:cs_axiom_status(institutional_source_validity_criterion, holdable).
narrative_ontology:cs_reference_frame(positivist_reading, institutional_legal_positivism).
narrative_ontology:cs_drift_state(positivist_reading, contemporary_jurisprudential_consensus, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(positivist_reading, institutional_legal_authority).
narrative_ontology:constraint_beneficiary(positivist_reading, procedural_formalists).
narrative_ontology:constraint_victim(positivist_reading, moral_constitutional_claims).
narrative_ontology:constraint_victim(positivist_reading, substantive_rights_litigants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBSTANTIVE RIGHTS CLAIMANT (SNARE) — A litigant claiming constitutional protection on moral grounds (e.g., fundamental dignity, natural rights) is trapped by the positivist reading's core mechanism: the claim's moral force is ruled out of bounds by definition. No procedural remedy exists within the constraint itself. Exit requires either abandoning the moral claim entirely (accepting positivism) or exiting the legal system itself (civil disobedience, emigration). Maximum extraction from this agent's perspective — their core argument is suppressed by the framework before adjudication.
constraint_indexing:constraint_classification(positivist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

constraint_indexing:constraint_classification(positivist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LEGAL POSITIVIST INSTITUTION (ROPE) — The institutional legal establishment benefits from the clarity and predictability of positivism: no meta-ethical disputes, no need to resolve deep moral disagreements, clear procedures for constitutional amendment. This perspective experiences the constraint as pure coordination — the mechanism solves the problem of adjudicating contested constitutionalism without dragging every moral dispute into law. Arbitrage exit means the institution can accept or abandon positivism depending on institutional convenience; it experiences the reading as instrumentally beneficial. Zero or negative effective extraction from this perspective.
constraint_indexing:constraint_classification(positivist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSTITUTIONAL AMENDMENT COALITION (TANGLED ROPE) — Organized political actors see the positivist reading as both enabling and constraining. It enables constitutional change only through formal amendment procedures (Article V), which is technically democratic but practically difficult. The reading provides coordination (clear procedure) but also extracts by making informal constitutional development impossible — moral consensus cannot reshape the Constitution without the procedural gauntlet. The coalition is mobile (can organize, lobby, amend formally) but constrained by the high bar positivism sets for validity. Moderate extraction — real agency exists but is significantly channeled.
constraint_indexing:constraint_classification(positivist_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: LIVING CONSTITUTIONALIST SCHOLARS (SCAFFOLD) — Academic and judicial actors promoting alternative readings (living constitutionalism, originalism-with-natural-law) see the positivist constraint as temporary and increasingly indefensible. They are building competing theoretical frameworks (originalist jurisprudence, moral constitutionalism) that provide alternative pathways for constitutional development. Low effective extraction because these agents see a sunset: the positivist reading is losing institutional purchase as jurisprudential consensus shifts. Sunset mechanism: as legal academia converges on living constitutionalism or originalism, the positivist reading's authority erodes. Estimated sunset: 20-30 years for significant doctrinal shift.
constraint_indexing:constraint_classification(positivist_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: POSITIVIST LEGAL DOCTRINE (PITON) — The doctrine itself is substantially performative at civilizational scale. Courts routinely adjudicate constitutional cases by invoking moral principles (substantive due process, equal protection's underlying dignity norms, Eighth Amendment's 'cruel and unusual' standard) while nominally adhering to positivist methodology. The theater ratio reflects this gap: judges perform positivist reasoning in opinions while actually applying moral judgment in outcomes. The doctrine persists through institutional inertia and the appearance of objectivity it provides, not because it successfully prevents moral reasoning from constitutional law. Theater ratio 0.65 reflects that legal formalism has real institutional work (legitimacy theater) even though the substance of constitutional development is substantially driven by evolving moral consensus.
constraint_indexing:constraint_classification(positivist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / LOGICAL NECESSITY VIEW (MOUNTAIN) — From a purely logical perspective, the distinction between law and morality is analytically necessary: any coherent legal system must have a decision procedure that is independent of the substantive rightness of the outcome, or else it collapses into pure moral philosophy. A legal system with a constraint that validity depends on moral content would make law indistinguishable from ethics and would provide no stable ground for adjudication across moral disagreement. From this view, positivism is not contingent but logically required. However, structural data contradicts this mountain classification: the constraint has identifiable beneficiaries (institutional authority, procedural formalists) and victims (moral claimants, substantive rights advocates), which reveals that the 'logical necessity' framing naturalizes what is actually a strategic institutional choice. This is a false summit.
constraint_indexing:constraint_classification(positivist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(positivist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(positivist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(positivist_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(positivist_reading, TR),
    TR >= 0.70.

:- end_tests(positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The positivist reading extracts by suppressing moral arguments from legitimate constitutional discourse, creating a category of constitutional claims that are structurally inadmissible. However, extraction is not severe because: (1) the suppression is not total — informal constitutional development occurs and is eventually formalized; (2) the reading provides genuine coordination benefits (institutional clarity, decision-procedure determinacy); (3) the institutional legal system retains alternative mechanisms for substantive development (judicial interpretation, amendment, practice drift). The moderate value reflects that the reading has both real coordination function and real extraction mechanism. Suppression (0.52): Moderate-high. The boundary between legal and moral reasoning is institutionally enforced through doctrine, standing rules, and the structure of constitutional adjudication. But suppression is not total because: (1) moral reasoning occurs covertly in constitutional law (hidden within due process, equal protection, Eighth Amendment analysis); (2) courts engage in substantive moral judgment while using procedural vocabulary; (3) the boundary is contestable and contested. Theater ratio (0.65): Moderate-high. The doctrine performs 'objectivity' through procedural formalism while actual constitutional development is substantially driven by evolving moral consensus. Courts state positivist reasoning in opinions while outcomes reflect moral judgment. The gap between stated procedure and actual practice — moral reasoning occurring under procedural cover — constitutes the theater. Theater has increased over the measurement interval as the gap between doctrine (procedural formalism) and practice (moral development) has widened.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental perspectival gap is between institutional legal authority (which benefits from the positivist reading's clarity and sees it as coordination) and substantive rights claimants (who are excluded from legitimate discourse by the reading and see it as extraction). The legal positivist institution experiences the constraint as Rope — solving a genuine coordination problem (how to adjudicate without resolving every moral disagreement). The rights claimant experiences it as Snare — trapped by the boundary definition itself, unable to access the core of their argument. No perspectival reconciliation is possible from within a single institutional frame: the reading's core mechanism requires that moral arguments be excluded, which means the rights claimant's perspective is structurally inadmissible. The intermediate judge experiences Tangled Rope — the reading provides real coordination (clarity, procedure) while also constraining discretion. The amendment coalition experiences Tangled Rope from a different angle — real agency (can organize, lobby, amend) but channeled through a high procedural bar. The analytical observer risks seeing Mountain (logical necessity) when the structural data reveals false summit (institutional choice with identifiable beneficiaries). The perspectival distribution suggests that the positivist reading's coherence depends on the exclusion of victim perspectives from institutional adjudication.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from the agent's structural position relative to the extraction flow. Beneficiary institutions (legal positivist community) experience low d ≈ 0.10-0.20 (arbitrage exit, net beneficiary) → low f(d) → near-zero or negative χ (they see coordination). Powerless rights claimants experience high d ≈ 0.90 (trapped exit, victim) → high f(d) ≈ 1.30 → high χ (they see extraction). Moderate judges experience d ≈ 0.55 (constrained exit, mixed victim/beneficiary) → f(d) ≈ 0.75 → moderate χ. Organized amendment coalitions experience d ≈ 0.45 (mobile exit, victim of constraint but with agency) → f(d) ≈ 0.50 → moderate χ. Academic competitors building alternative frameworks experience d ≈ 0.50 (constrained to academic discourse, but scaffold perspective with sunset logic) → f(d) ≈ 0.65 → moderate χ. The perspectival gap derives from the fact that beneficiary institutions see coordination while victim claimants see extraction — they occupy opposite positions in the extraction flow, and the constraint's structure produces opposite experienced types from these positions.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exemplifies mandatrophy through the false summit mechanism. The analytical perspective tempts toward Mountain classification ('the law/morality distinction is logically necessary'), which would dissolve the tension between coordination and extraction by treating the constraint as inevitable natural law. But structural data contradicts the mountain gates: the constraint has identifiable beneficiaries (institutional legal authority, procedural formalists), identifiable victims (substantive rights claimants, moral constitutionalists), and active enforcement mechanisms (standing doctrine, admission/exclusion of arguments based on institutional rules). The mountain classification is revealed as a false summit — naturalizing a strategic institutional choice that benefits institutional legal authority and harms moral constitutional claimants. Once the false summit is recognized, the constraint resolves to Tangled Rope at the institutional level: genuine coordination function (clarity, procedure) with asymmetric extraction (suppression of moral arguments). The mandatrophy is resolved by showing that the 'necessity' framing is one reading among multiple live readings (positivist vs. originalist vs. living constitutionalist), and that the reading choice itself is contestable and contested. The constraint does not dissolve the tension; it structures it in a way that privileges institutional clarity over substantive rights discourse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedural_vs_substantive_boundary,
    'Is the boundary between procedural validity and substantive moral content analytically drawn (logical necessity) or institutionally constructed (contingent choice)?',
    'Comparative jurisprudence: examine constitutional courts that reject the positivist reading (e.g., South African Constitutional Court''s dignity jurisprudence, German Constitutional Court''s natural law moorings) and assess whether they have abandoned logical coherence or simply chosen a different institutional framework. If coherent, the boundary is contingent.',
    'If analytically necessary: mountain classification holds; positivism is a true natural law. If institutionally contingent: false summit confirmed; the ''necessity'' naturalizes an institutional choice that benefits specific actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(procedural_vs_substantive_boundary, conceptual, 'Whether the law/morality boundary is logical or institutional').

omega_variable(
    amendment_procedure_exhaustiveness,
    'Does the positivist reading''s requirement for formal amendment completely suppress informal constitutional development, or do informal developments occur and eventually get formalized?',
    'Historical analysis of constitutional change pathways: constitutional amendment history vs. informal doctrinal shifts that were later formally adopted. Measure proportion of constitutional development occurring through formal amendment vs. judicial interpretation vs. practice drift.',
    'If formal amendment is the only valid pathway: extraction is severe (suppression gate is high). If informal development occurs and is eventually validated: extraction is moderate (suppression gate is moderate) — the reading constrains but does not completely block substantive development.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_procedure_exhaustiveness, empirical, 'Whether informal constitutional development is completely suppressed or constrained').

omega_variable(
    moral_reasoning_concealment_mechanism,
    'Does judicial application of the positivist reading actually prevent moral reasoning from constitutional law, or does it require moral reasoning to be performed covertly within procedural/textual framing?',
    'Jurisprudential analysis: compare stated positivist reasoning in constitutional opinions with actual decision outcomes. Assess whether outcomes can be explained by text/procedure alone or require implicit moral premises.',
    'If moral reasoning is actually suppressed: positivism works as designed (theater ratio should be lower). If moral reasoning is required but concealed: the positivist frame creates performative theater (high theater ratio justified). Theater ratio 0.65 suggests the latter — moral reasoning occurs but is hidden behind positivist vocabulary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_reasoning_concealment_mechanism, empirical, 'Whether moral reasoning is suppressed or concealed in positivist legal application').

omega_variable(
    kernel_contest_reading_selection,
    'Within the constitutional_text_authority kernel, does the positivist reading''s core claim — that validity derives purely from formal enactment and institutional source, excluding moral content — remain a live and coherent position, or has it been substantially overridden by competing readings?',
    'Jurisprudential consensus mapping: track institutional adoption of positivism vs. originalism vs. living constitutionalism across US federal judiciary, state courts, and comparative constitutional courts over the 2010-2026 period. Measure which reading has binding authority in the plurality of courts.',
    'If positivism remains live and organized: the reading relations (coexists_with siblings) are accurate. If positivism has been substantially displaced: the reading may be overridden or foreclosed within its own institutional tradition, and the drift_state should reflect authority_erosion at substantial or severe magnitude.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_reading_selection, empirical, 'Whether the positivist reading remains a live institutional position').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(positivist_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(posi_tr_t0, positivist_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(posi_tr_t5, positivist_reading, theater_ratio, 5, 0.58).
narrative_ontology:measurement(posi_tr_t10, positivist_reading, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(posi_be_t0, positivist_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(posi_be_t5, positivist_reading, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(posi_be_t10, positivist_reading, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(positivist_reading, originalist_reading).
narrative_ontology:affects_constraint(positivist_reading, living_constitutionalist_reading).
narrative_ontology:affects_constraint(positivist_reading, substantive_due_process_doctrine).
narrative_ontology:affects_constraint(positivist_reading, equal_protection_moral_content).

% DUAL FORMULATION NOTE:
% The positivist_reading is one instantiation of the constitutional_text_authority kernel. It is linked to originalist_reading and living_constitutionalist_reading as sibling readings of the same kernel. It also affects downstream constraints in constitutional doctrine (substantive due process, equal protection interpretation) because the positivist reading's core claim (morality is irrelevant to validity) creates structural pressure on doctrines that explicitly invoke moral principles (dignity, equal personhood, fundamental rights). As the positivist reading's institutional authority erodes, the scope for moral reasoning in constitutional doctrine expands.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(positivist_reading, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
