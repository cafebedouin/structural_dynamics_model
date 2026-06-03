% ============================================================================
% CONSTRAINT STORY: us_constitution_text__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__positivist_reading, []).

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
 *   constraint_id: us_constitution_text__positivist_reading
 *   human_readable: U.S. Constitution Positivist Reading: Procedural Validity and Formal Enactment
 *   domain: constitutional_law/legal_philosophy
 *
 * SUMMARY:
 *   The positivist reading of constitutional authority treats the
 *   Constitution as valid law because it was formally ratified through
 *   Article V procedures and because changes to it must follow the same
 *   procedures. This reading occupies a central position in American legal
 *   philosophy and jurisprudence: it grounds the idea that constitutional
 *   interpretation should be constrained by source-validity (did it come
 *   through legitimate procedures?) rather than outcome-validity (does it
 *   reach morally correct results?). The constraint operates at multiple
 *   levels simultaneously: (1) As a philosophical claim about what makes law
 *   valid (procedures ground validity in all legal systems); (2) As a
 *   jurisprudential doctrine about how courts should interpret the
 *   Constitution (bound by text and formal amendments, not by moral principle
 *   or living tradition); (3) As an institutional mechanism that redirects
 *   power to those capable of mobilizing formal amendment (legislatures,
 *   amendment coalitions) and away from those who lack procedural access
 *   (marginalized groups, low-power constituencies). The extractiveness
 *   emerges from the tension between the coordination function (formal
 *   procedures provide certainty and stability) and the asymmetric
 *   cost-bearing (groups locked out of amendment process must either accept
 *   the Constitution as written or mobilize massive political power). The
 *   suppression is high because alternative pathways to constitutional
 *   meaning (moral philosophy, historical recovery, adaptive interpretation)
 *   are formally delegitimized within the positivist framework, even though
 *   courts demonstrably apply them. The theater ratio reflects that
 *   positivism describes what courts SHOULD do (formal procedure + text only)
 *   while courts actually do (formal procedure + history + adaptation). This
 *   gap between prescriptive formalism and descriptive pluralism creates the
 *   piton perspective: the positivist ritual is maintained through
 *   institutional inertia despite its degraded fit to actual practice.
 *
 * KEY AGENTS:
 *   - Institutional Stability Apparatus (Courts, Legislatures, Executive): Primary beneficiary (institutional/arbitrage) — benefits from procedural formalism as a coordination mechanism; predictability and rule-of-law stability flow toward institutional actors
 *   - Substantive Justice Claims Without Formal Enactment: Primary victim (powerless/trapped) — groups seeking constitutional protection for rights not formally achievable through Article V; no exit within positivist framework
 *   - Marginalized Groups Excluded from Amendment Process: Secondary victim (powerless/trapped to organized/constrained) — lower-power constituencies face high barriers to mobilizing formal amendment; experience constraint as suppression of alternative legitimacy sources
 *   - Reform-Minded Legal Community: Mixed actor (moderate/constrained) — law professors, progressive advocates see positivism as constraining (high barrier to change) but also enabling (formal amendment IS a real path forward)
 *   - Constitutional Amendment Coalition: Organized actor (organized/constrained) — civil rights movements, suffrage coalitions experience extraction through required mobilization but also benefit from procedural certainty (if you win politically, you win constitutionally)
 *   - Positivist Legal Formalism as Institutional Practice: Degraded mechanism (institutional/arbitrage long-term view, piton) — judicial doctrine applies positivist language while actually practicing pluralist interpretation; maintained through inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risk of naturalizing contingent procedural requirements as logical necessities of law itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__positivist_reading, 0.52).
domain_priors:suppression_score(us_constitution_text__positivist_reading, 0.62).
domain_priors:theater_ratio(us_constitution_text__positivist_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__positivist_reading, "U.S. Constitution Positivist Reading: Procedural Validity and Formal Enactment").
narrative_ontology:topic_domain(us_constitution_text__positivist_reading, "constitutional_law/legal_philosophy").

domain_priors:requires_active_enforcement(us_constitution_text__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__positivist_reading, 'c45b199b-9827-43bf-8960-c5fcbe4065e2').
narrative_ontology:cs_kernel_codification('c45b199b-9827-43bf-8960-c5fcbe4065e2', formalized).
narrative_ontology:cs_authority_grounding('c45b199b-9827-43bf-8960-c5fcbe4065e2', extraction).
narrative_ontology:cs_interpretation_layer_present('c45b199b-9827-43bf-8960-c5fcbe4065e2').
narrative_ontology:cs_reading_relation('c45b199b-9827-43bf-8960-c5fcbe4065e2', us_constitution_text__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c45b199b-9827-43bf-8960-c5fcbe4065e2', us_constitution_text__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_axiom('c45b199b-9827-43bf-8960-c5fcbe4065e2', foundational, procedure_exhausts_validity).
narrative_ontology:cs_axiom_status(procedure_exhausts_validity, holdable).
narrative_ontology:cs_axiom_grounding('c45b199b-9827-43bf-8960-c5fcbe4065e2', procedure_exhausts_validity, conventional).
narrative_ontology:cs_axiom('c45b199b-9827-43bf-8960-c5fcbe4065e2', secondary, article_v_monopoly_on_change).
narrative_ontology:cs_axiom_status(article_v_monopoly_on_change, holdable).
narrative_ontology:cs_axiom_grounding('c45b199b-9827-43bf-8960-c5fcbe4065e2', article_v_monopoly_on_change, conventional).
narrative_ontology:cs_reference_frame('c45b199b-9827-43bf-8960-c5fcbe4065e2', procedural_validity_through_formal_enactment).
narrative_ontology:cs_drift_state('c45b199b-9827-43bf-8960-c5fcbe4065e2', contemporary_pluralist_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c45b199b-9827-43bf-8960-c5fcbe4065e2', '').
narrative_ontology:cs_kernel_id(us_constitution_text__positivist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, institutional_stability_apparatus).
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, judicial_hierarchy).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, substantive_justice_claims_without_formal_enactment).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, marginalized_groups_excluded_from_amendment_process).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POWERLESS/TRAPPED — Groups seeking constitutional protection for rights not formally enactable through Article V (too costly, entrenched opposition prevents amendment). The positivist reading forecloses substantive moral claims entirely: if you cannot route your claim through formal procedures, you have no valid constitutional argument. Maximum extraction — no exit option exists within the positivist framework itself.
constraint_indexing:constraint_classification(us_constitution_text__positivist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MODERATE/CONSTRAINED — Law professors, progressive advocates, and lower-court judges see the positivist reading as both constraining and enabling. It constrains through formalism (cannot bypass Article V), but it also provides coordination: formal amendment IS a path forward (Nineteenth Amendment, Civil Rights Act of 1964 via Fourteenth Amendment interpretation). Constrained by the difficulty of amendment, but not trapped — alternative procedural routes exist. Mixed experience: genuine coordination function (amendment process works) alongside extraction (high barrier to entry).
constraint_indexing:constraint_classification(us_constitution_text__positivist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL/ARBITRAGE — Federal courts, executive branch agencies, and legislatures benefit from the positivist reading as a coordination mechanism. Procedural formalism reduces inter-branch conflict: what the Constitution allows is determined by formal enactment (text + Article V process), not by shifting moral judgments. This apparatus experiences the constraint as pure coordination — it solves the collective action problem of constitutional interpretation. Net beneficiary — institutional stability and rule-of-law predictability flow toward this actor.
constraint_indexing:constraint_classification(us_constitution_text__positivist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ORGANIZED/CONSTRAINED — Organized movements (civil rights coalitions, women's suffrage activists, etc.) see the positivist reading as requiring their mobilization to amend, which is both extractive and enabling. Extraction: the constitutional system requires you to organize mass political power to change meaning (high barrier). Enabling: the formal process is guaranteed (Article V is law). The extraction mechanism is the suppression of non-procedural pathways; the coordination function is the certainty that if you succeed politically, the amendment succeeds constitutionally.
constraint_indexing:constraint_classification(us_constitution_text__positivist_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL/ARBITRAGE LONG-TERM (PITON) — At the civilizational time horizon, the positivist reading's performative content becomes visible. The claim that constitutional meaning is exhausted by formal enactment procedures is itself a theoretical choice, not a discovered fact. Judges continuously interpret within and around the text's indeterminacies (living constitutionalism and originalism both happen in practice). The positivist framework persists through institutional inertia — it provides a convenient vocabulary for judicial restraint — but its actual function is degraded. Courts apply all three readings (positivist procedure + originalist history + living-constitutionalist adaptation) simultaneously, and the positivist label obscures this mixture. Theater ratio: 0.58 reflects that procedural talk is partly performative while also providing real guidance for appellate review standards.
constraint_indexing:constraint_classification(us_constitution_text__positivist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL/ANALYTICAL (MOUNTAIN) — From a pure logical perspective, ANY valid law, constitutional or not, must derive from SOME enactment procedure. Procedure is logically prior to substance: you cannot have a valid legal rule without a source of validity (custom, legislation, ratification). The positivist reading appears as a necessary truth about law itself — procedures ground validity in all legal systems. However, the structural data reveals this as a false summit: the benefit to institutional stability and the harm to substantive justice claims are contingent facts about the U.S. political economy, not logical universals.
constraint_indexing:constraint_classification(us_constitution_text__positivist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__positivist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_constitution_text__positivist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_constitution_text__positivist_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_constitution_text__positivist_reading, TR),
    TR >= 0.70.

:- end_tests(us_constitution_text__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-to-high. The positivist reading extracts through procedural gatekeeping: substantive claims must route through Article V amendment, which is extremely costly (requires super-majority consensus, decades of mobilization, faces entrenched opposition). The constraint channels power toward institutional actors with procedural access and away from groups without such access. The value is moderate rather than severe because the amendment process genuinely works when groups succeed in mobilizing (Thirteenth, Nineteenth, Twenty-Sixth Amendments succeeded; the path is open but costly). Suppression (0.62): High. The positivist reading suppresses alternative legitimacy claims by delegitimizing moral reasoning, historical meaning-recovery, and adaptive interpretation as sources of constitutional validity. These alternatives are presented as 'non-constitutional' even though courts visibly apply them. The suppression is structural: the framework forecloses certain types of arguments in formal constitutional discourse. However, it is not total suppression (organized movements CAN mobilize amendment) — it is high but not maximally suppressive. Theater ratio (0.58): Moderate-high. Positivism prescribes that constitutional interpretation should be purely procedural and textual, but actual constitutional jurisprudence is observably pluralist (courts apply originalism, living constitutionalism, structural reasoning, precedent, prudential deference simultaneously). The positivist label provides cover for this eclectic practice but does not accurately describe it. The theater has increased over the measurement interval as the gap between positivist prescription and pluralist practice has widened (courts have become more explicitly adaptive and historically informed while positivism remains the official rhetoric).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exemplifies how a single structural arrangement produces six incompatible classifications. Powerless agents trapped without formal enactment access see pure extraction (Snare). Moderate reformers see mixed coordination and extraction (Tangled Rope) — the amendment process works but at prohibitive cost. Institutional beneficiaries see coordination (Rope) — procedural formalism solves inter-branch conflict and provides stability. Organized coalitions see mixed coordination and extraction (Tangled Rope) — they must mobilize massive political power but the formal path is guaranteed. The long-term institutional perspective reveals degradation (Piton) — positivist formalism is maintained through inertia while courts practice pluralist interpretation. The analytical observer risks seeing logical necessity (Mountain) — all legal systems require procedures to ground validity — but the structural data reveal this as false summit: the actual constraint is the asymmetric cost-bearing, which is a contingent feature of U.S. constitutional politics, not a universal law of legal systems. The perspectival gap reveals that 'constitutional validity derives from formal procedures' is not a neutral description but a claim that benefits institutional actors by suppressing alternative legitimacy sources.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is computed from the agent's structural position relative to the constraint. Powerless agents locked out of amendment procedures have d ≈ 0.95 (full targets of extraction). Institutional beneficiaries with procedural access have d ≈ 0.05 (net beneficiaries, negative effective extraction). Reform movements have d ≈ 0.55 (must mobilize political power but can succeed). The high suppression (0.62) applies uniformly across all positions — the positivist framework suppresses alternative legitimacy claims for everyone, not just the powerless. But the effective extractiveness (χ = ε × f(d) × σ(S)) varies dramatically by position: institutional actors experience coordination benefits (χ < 0), while powerless actors experience high extraction (χ > 0.70). The motor of the constraint is procedural: who has access to amendment mechanisms? This determines d. The suppression is ideological: who has standing to make constitutional arguments? This is constrained by what counts as 'constitutional' under positivism. Together, these mechanisms produce the perspectival gap: the same constraint appears as coordination (rope), gatekeeping (snare), and mixed extraction (tangled rope) depending on whether you have procedural access.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this constraint is resolved by recognizing that BOTH the coordination function AND the asymmetric extraction are real. Positivism genuinely coordinates constitutional interpretation (procedures provide certainty and stability). It genuinely extracts from substantive justice claims without formal enactment (suppresses alternative legitimacy sources, forces costly mobilization). The constraint is tangled rope because it does both. The false summit (mountain from analytical perspective) arises from treating procedures as logical necessities rather than contingent institutional arrangements. Legal systems exist without Article V-style procedures (parliamentary supremacy, customary law, informal constitutional evolution), which proves that formal amendment is not logically necessary — it is a choice embedded in U.S. institutional design. The choice benefits institutional stability and rule-of-law predictability; it harms groups locked out of amendment access. This is extractive asymmetry, not natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    living_constitutionalism_coexistence,
    'Does the positivist reading''s commitment to formal enactment procedures logically foreclose living constitutionalism, or can both readings coexist within U.S. jurisprudence?',
    'Empirical analysis of how courts actually decide cases: do they apply amendment-only logic (pure positivism) or do they interpret Constitution to address contemporary circumstances without formal amendment (living constitutionalism)? Historical survey of constitutional doctrine development.',
    'If foreclosed: the positivist reading is incompatible with 150+ years of constitutional practice (Eighth Amendment evolving standard of decency, Fourth Amendment adapting to digital surveillance, etc.). If coexists: the positivist reading is one component of a pluralist jurisprudence rather than an exclusive interpretive method.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(living_constitutionalism_coexistence, empirical, 'Whether positivist procedure forecloses or coexists with adaptive interpretation').

omega_variable(
    amendment_process_closure_mechanism,
    'Is Article V the ONLY legitimate route for constitutional change, or are there alternative formal procedures (custom, practice, institutional evolution) that alter constitutional meaning without formal amendment?',
    'Historical and doctrinal analysis: trace how constitutional meaning changed before and after formal amendments. Identify cases where constitutional doctrine shifted without Article V amendment. Compare U.S. practice to other common-law systems (UK, Canada) where constitutional change occurs through practice evolution rather than formal enactment.',
    'If Article V exclusive: positivist reading is structurally correct — formal procedure IS the sole source of validity. If alternatives exist: the reading is restrictive — it ignores custom and practice as valid enactment mechanisms, favoring statutory/amendment-based procedures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_process_closure_mechanism, empirical, 'Whether Article V monopolizes constitutional change or alternative procedures are valid').

omega_variable(
    originalism_incompatibility,
    'Does the positivist reading''s emphasis on formal enactment procedures (Article V amendment as source of validity) logically foreclose originalism''s emphasis on historical meaning at ratification?',
    'Philosophical analysis: examine whether originalism (recovering original public meaning) and positivism (procedures ground validity) are compatible frameworks. Both occupy Supreme Court jurisprudence simultaneously — analyze whether they conflict or complement each other.',
    'If foreclosed: originalism violates positivist principles by importing historical context beyond what text + formal procedures warrant. If coexists: originalism is a hermeneutic method within the positivist framework (text WAS formally enacted; recover its meaning). Current jurisprudence suggests coexistence, indicating positivism is not foreclosing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(originalism_incompatibility, conceptual, 'Whether positivism forecloses originalism or the readings coexist').

omega_variable(
    substantive_justice_exclusion_permanence,
    'Are substantive justice claims that lack formal enactment PERMANENTLY excluded from constitutional validity under positivism, or can they become valid through eventual amendment?',
    'Definitional analysis: positivism says procedural validity is the source. If a claim achieves formal enactment (through amendment), does it retroactively become ''always valid'' or does positivism distinguish between valid-at-ratification and valid-after-amendment? Examine how legal reasoning treats retroactive application of constitutional amendments.',
    'If permanent exclusion: positivism denies legitimacy to any unamended claim, making it a victim-producing mechanism. If temporal: positivism is a procedural gate that substantive justice claims must pass; it is not an eternal exclusion. This affects whether the ''victims'' are permanently trapped or constrained with a path forward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substantive_justice_exclusion_permanence, conceptual, 'Whether positivism permanently excludes non-enacted claims or temporal validity is possible').

omega_variable(
    kernel_contest_reading_ambiguity,
    'Is this constraint a reading of the US Constitution kernel (positivist legal philosophy) or is it a constraint on how judicial review operates (institutional mechanism)? The kernel contest treats it as the former; the structural data suggest it might be the latter.',
    'Distinguish the kernel (what the Constitution IS as a legal object) from the constraint (how constitutional review WORKS). The positivist reading addresses the kernel: constitutional validity derives from procedures. But the extractiveness and victim set suggest the operative constraint is about institutional gatekeeping. Survey constitutional theorists: is positivism presented as metaphysics (what makes law valid) or as a practical jurisprudential method (how courts should decide)?',
    'If kernel-level: this story is correctly positioned in the kernel contest. If institutional-mechanism-level: the story should be repositioned as a constraint on judicial power, with the kernel contest as background context. The classification might shift (currently tangled_rope; if mechanism-focused, could be snare or scaffold depending on view of Article V amendment).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_reading_ambiguity, conceptual, 'Whether positivism addresses the kernel (constitution''s metaphysics) or the mechanism (judicial review practice)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__positivist_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usconst_posit_tr_t0, us_constitution_text__positivist_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(usconst_posit_tr_t3, us_constitution_text__positivist_reading, theater_ratio, 3, 0.51).
narrative_ontology:measurement(usconst_posit_tr_t6, us_constitution_text__positivist_reading, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(usconst_posit_be_t0, us_constitution_text__positivist_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(usconst_posit_be_t3, us_constitution_text__positivist_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(usconst_posit_be_t6, us_constitution_text__positivist_reading, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_text__positivist_reading, us_constitution_text__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__positivist_reading, us_constitution_text__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% The us_constitution_text kernel contains three structurally distinct constraints, one for each reading. The positivist reading (this file) focuses on procedural validity and formal enactment as the source of constitutional authority. The originalist reading (separate constraint) focuses on recovering historical meaning fixed at ratification. The living constitutionalist reading (separate constraint) focuses on adaptive interpretation to contemporary circumstances. Each reading has its own ε value, beneficiary/victim structure, and temporal dynamics. They are linked via network.affects_constraints because all three readings operate within U.S. jurisprudence simultaneously, creating interference patterns: courts apply all three methods in a single opinion, creating the pluralist practice that the piton perspective captures. The positivist reading's suppression (0.62) reflects that it delegitimizes the other two readings as 'non-constitutional' even though they visibly operate. The omegas document the structural ambiguity: do the readings coexist or foreclose each other? The empirical evidence (court decisions applying all three simultaneously) suggests coexistence despite positivism's claim to exclusive legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_text__positivist_reading, institutional, 0.08).
constraint_indexing:directionality_override(us_constitution_text__positivist_reading, powerless, 0.94).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
