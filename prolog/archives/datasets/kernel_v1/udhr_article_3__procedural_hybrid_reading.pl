% ============================================================================
% CONSTRAINT STORY: udhr_article_3__procedural_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__procedural_hybrid_reading, []).

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
 *   constraint_id: udhr_article_3__procedural_hybrid_reading
 *   human_readable: Article 3 Procedural Hybrid: Due Process Without Substantive Liberty/Welfare Resolution
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   Article 3 of the Universal Declaration of Human Rights guarantees the
 *   right to life, liberty, and security of person without defining what
 *   these substantive rights entail. The Article has been widely interpreted
 *   as providing procedural protections (habeas corpus, prohibition on
 *   torture, right to judicial review) without resolving the underlying
 *   contest between negative liberty reading (freedom from state
 *   interference), positive entitlement reading (state obligation to provide
 *   welfare and substantive freedom), and the procedural hybrid reading
 *   instantiated in this story. The procedural hybrid reading decouples
 *   procedure from substance: it guarantees that detention processes will
 *   include certain protections (no torture, right to petition, judicial
 *   review) while remaining silent on whether individuals have a substantive
 *   entitlement to liberty or welfare. This generates a structural asymmetry:
 *   detainees gain access to procedure without gaining substantive rights.
 *   States benefit from legitimacy through procedural appearance while
 *   retaining substantive discretion. The constraint exhibits Tangled Rope
 *   characteristics: genuine coordination function (international
 *   norm-setting, procedural harmonization) coupled with asymmetric
 *   extraction (state authority retains substantive control while procedure
 *   masks the control). The extractiveness trajectory shows gradual increase
 *   over 30 years as more sophisticated detention regimes (counterterrorism,
 *   migration control, public health emergency powers) have learned to
 *   maintain procedural compliance while expanding substantive detention
 *   authority.
 *
 * KEY AGENTS:
 *   - Detainees: Primary victims (powerless/trapped) — procedural protections exist without underlying right to freedom; bear full cost of detention while procedure creates appearance of protection
 *   - State Security Apparatus: Primary beneficiary (institutional/arbitrage) — benefits from decoupling of procedure and substance; procedure provides legitimacy while leaving substantive detention authority unconstrained
 *   - Civil Society & Legal Aid Organizations: Secondary actors (moderate/constrained) — benefit from procedural framework (habeas corpus, judicial review) but constrained by absence of substantive rights to advocate for; sustained resource burden with limited outcomes
 *   - International Human Rights Bodies: Institutional monitors (institutional/constrained) — coordinate procedural norm-setting globally but experience extraction from inability to mandate substantive entitlements; limited by state sovereignty
 *   - National Courts: Institutional interpreters (institutional/constrained) — can review detention procedure but lack authority to enforce substantive liberty/welfare rights; interpret Article 3 within boundaries set by political branches
 *   - UDHR as Institutional Text: Carries institutional inertia (piton function) — performative affirmation of dignity without substantive implementation; ratification provides legitimacy while leaving substantive content to state discretion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__procedural_hybrid_reading, 0.38).
domain_priors:suppression_score(udhr_article_3__procedural_hybrid_reading, 0.52).
domain_priors:theater_ratio(udhr_article_3__procedural_hybrid_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__procedural_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__procedural_hybrid_reading, "Article 3 Procedural Hybrid: Due Process Without Substantive Liberty/Welfare Resolution").
narrative_ontology:topic_domain(udhr_article_3__procedural_hybrid_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__procedural_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__procedural_hybrid_reading, 'ee898b83-fd17-4f9d-8c35-6c37c8956fde').
narrative_ontology:cs_kernel_codification('ee898b83-fd17-4f9d-8c35-6c37c8956fde', formalized).
narrative_ontology:cs_authority_grounding('ee898b83-fd17-4f9d-8c35-6c37c8956fde', lineage).
narrative_ontology:cs_interpretation_layer_present('ee898b83-fd17-4f9d-8c35-6c37c8956fde').
narrative_ontology:cs_reading_relation('ee898b83-fd17-4f9d-8c35-6c37c8956fde', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('ee898b83-fd17-4f9d-8c35-6c37c8956fde', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_axiom('ee898b83-fd17-4f9d-8c35-6c37c8956fde', foundational, procedure_universalizable_substance_local).
narrative_ontology:cs_axiom_status(procedure_universalizable_substance_local, holdable).
narrative_ontology:cs_axiom_grounding('ee898b83-fd17-4f9d-8c35-6c37c8956fde', procedure_universalizable_substance_local, conventional).
narrative_ontology:cs_axiom('ee898b83-fd17-4f9d-8c35-6c37c8956fde', foundational, form_function_decoupling_enables_implementation).
narrative_ontology:cs_axiom_status(form_function_decoupling_enables_implementation, holdable).
narrative_ontology:cs_axiom_grounding('ee898b83-fd17-4f9d-8c35-6c37c8956fde', form_function_decoupling_enables_implementation, instrumental).
narrative_ontology:cs_reference_frame('ee898b83-fd17-4f9d-8c35-6c37c8956fde', universal_procedure_local_substance).
narrative_ontology:cs_drift_state('ee898b83-fd17-4f9d-8c35-6c37c8956fde', contemporary_security_expansion_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ee898b83-fd17-4f9d-8c35-6c37c8956fde', '').
narrative_ontology:cs_kernel_id(udhr_article_3__procedural_hybrid_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, state_security_apparatus).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, executive_detention_authority).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, detainees_without_substantial_rights).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, marginalized_populations_disproportionately_detained).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DETAINEE (SNARE) — Procedural protections (habeas corpus, torture prohibition) exist on paper but lack substantive entitlement to liberty or welfare. A detainee can petition for release but no underlying right to freedom exists — the constraint provides theater (right to petition) without substance (entitlement to freedom). Maximum extraction: the detainee bears full cost of detention while procedure masks the absence of substantive protection.
constraint_indexing:constraint_classification(udhr_article_3__procedural_hybrid_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL SOCIETY / LEGAL AID (TANGLED ROPE) — Benefits from the procedural framework (habeas corpus, judicial review mechanisms) which provide advocacy pathways, but constrained by the absence of substantive rights (judges cannot overturn detention based on liberty entitlement, only procedural violation). Mixed extraction: the constraint both enables (provides legal mechanism) and constrains (mechanism lacks substantive authority). Significant resource burden of sustained litigation with limited substantive outcomes.
constraint_indexing:constraint_classification(udhr_article_3__procedural_hybrid_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATE SECURITY APPARATUS (ROPE) — Benefits from the procedural framework's legitimacy gap. Procedural protections (torture prohibition, habeas review) create appearance of constraint without substantive limits on detention authority. The executive experiences this constraint as coordination: procedure enables sustained detention while maintaining rule-of-law appearance. Net beneficiary through the decoupling of procedure from substance.
constraint_indexing:constraint_classification(udhr_article_3__procedural_hybrid_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL HUMAN RIGHTS BODY (TANGLED ROPE) — Coordinates global norm-setting around procedural protections (habeas corpus, torture prohibition) while experiencing extraction from the absence of enforcement teeth for substantive rights. Can document procedural violations (torture, denial of review) but cannot mandate substantive entitlements (right to freedom, right to minimum welfare). Constrained by state sovereignty and limited enforcement capacity.
constraint_indexing:constraint_classification(udhr_article_3__procedural_hybrid_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: UDHR AS PITON (PROCEDURAL THEATER) — The Article 3 guarantee functions as a performative affirmation of human dignity without substantive implementation mechanism. Ratifying states gain legitimacy from the procedural commitment (habeas corpus, torture prohibition) while remaining free to define substantive rights or their absence. The constraint persists through institutional inertia and theater: states perform compliance (judicial review procedures) while substantive protections remain undefined.
constraint_indexing:constraint_classification(udhr_article_3__procedural_hybrid_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the boundary between procedural and substantive rights may appear as an immutable feature of legal reasoning itself: procedure is what can be universally guaranteed; substance requires cultural negotiation. This perspective risks naturalizing a contingent institutional choice (the UDHR's deliberate silence on substantive liberty/welfare content) as a structural necessity. The engine's false summit detector will identify this as naturalization of constructed doctrine.
constraint_indexing:constraint_classification(udhr_article_3__procedural_hybrid_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__procedural_hybrid_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(udhr_article_3__procedural_hybrid_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(udhr_article_3__procedural_hybrid_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(udhr_article_3__procedural_hybrid_reading, TR),
    TR >= 0.70.

:- end_tests(udhr_article_3__procedural_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint exhibits genuine coordination function (procedural harmonization across jurisdictions enables international cooperation on detention standards) coupled with asymmetric extraction (states benefit from legitimacy through procedure while retaining substantive control). The moderate value reflects that the extraction is real but not maximal — procedure does impose costs (judicial review burden, torture prohibition enforcement infrastructure) and occasionally produces outcomes that constrain detention. The trajectory from 0.22 to 0.38 reflects expanding sophistication in detention regimes that maintain procedural compliance while increasing substantive authority. Suppression (0.52): Moderate-high. Detainees face significant barriers to meaningful remedy: habeas petitions are frequently dismissed on technical procedural grounds; torture findings may occur without release; judicial review processes are resource-intensive and often ineffective. However, suppression is not total because procedure creates some avenue for challenge and occasional successful cases exist. Theater ratio (0.58): Moderate-high. The constraint exhibits substantial theater: habeas corpus procedures exist and are performed but with low substantive success rates; torture prohibition is formally comprehensive but enforcement is selective; the constraint's legitimacy derives partly from the appearance of protection rather than actual protection delivery. The theater has increased over time as detention regimes have become more sophisticated at maintaining procedural form while minimizing substantive constraint.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single institutional text can support contradictory classifications depending on observer position. The detainee sees pure extraction (Snare) — procedure without substance. The state security apparatus sees coordination benefit (Rope) — procedure provides legitimacy while leaving substantive authority unconstrained. Civil society sees mixed coordination and extraction (Tangled Rope) — the procedure enables advocacy but substantive rights are absent. International monitoring bodies see similar mixed dynamics but at institutional level (Tangled Rope). Courts treating Article 3 as performative text see institutional inertia (Piton) — procedure persists because it provides legitimacy without substantive cost. The analytical observer risks seeing procedure-substance boundary as natural law (Mountain) — fundamental feature of human rights reasoning — but the structural data reveals this as a contingent institutional choice, not a necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from agent structural position relative to this specific constraint. Detainees classified as victims (no substantive entitlement) with trapped exit (cannot access meaningful remedy) produce high d → high χ (maximum extraction). State apparatus classified as beneficiaries (gains legitimacy and substantive authority retention) with arbitrage exit (can satisfy procedure while maintaining substantive control) produce low d → negative or minimal χ. Civil society classified as partial victims (lacks substantive rights to advocate) with constrained exit (can use procedure but cannot mandate substantive remedy) produce moderate d. International bodies classified as institutional actors with constrained exit (coordinating norms globally but limited enforcement) produce moderate-high d. The perspectival gap between beneficiary (state, d ≈ 0.15) and victim (detainee, d ≈ 0.92) is large and structurally stable — procedure does not substantially reduce this gap.
 *
 * MANDATROPHY ANALYSIS:
 *   The procedural hybrid reading resolves mandatrophy by clarifying that Article 3's coordination function (procedural harmonization for international detention standards) is genuine but coupled with asymmetric extraction (substantive discretion retained by states). This is the defining signature of Tangled Rope: both coordination AND extraction are real. The alternative readings (negative liberty reading, positive entitlement reading) would change the mandatrophy resolution. A pure negative liberty reading would classify as Rope (coordination only, no extraction). A positive entitlement reading would classify as Snare or Mountain (extraction without coordination, or naturalization of extraction). The procedural hybrid reading holds that both elements — coordination and extraction — are essential to understanding Article 3's structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedure_substance_boundary_definition,
    'Is the procedure-substance boundary in Article 3 a structural feature of human rights law or a contingent institutional choice?',
    'Comparative analysis of regional human rights instruments (ECHR, ACHR, ACHPR) examining how they couple procedural and substantive guarantees. Historical analysis of UDHR drafting: why did framers decouple procedure from substance?',
    'If boundary is structural necessity: the constraint is a genuine Mountain — procedure cannot meaningfully guarantee substantive rights. If boundary is contingent institutional choice: the constraint is a false summit — the decoupling naturalizes what could be integrated, enabling extraction through legitimacy theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedure_substance_boundary_definition, conceptual, 'Whether procedure-substance boundary is structural or contingent').

omega_variable(
    habeas_corpus_effectiveness_gap,
    'What proportion of habeas petitions in signatory states succeed in obtaining release (or substantive remedy) versus being dismissed on technical procedural grounds without addressing detention legality?',
    'Systematic analysis of habeas petition outcomes across jurisdiction sample (10-15 major signatory states) over 10-year period. Categorize outcomes: successful release, procedural dismissal without merits review, torture finding without release, detention affirmed after review.',
    'If success rate < 15%: habeas procedure functions as extraction mechanism (theater without substance). If success rate > 35%: procedure provides meaningful constraint on detention authority. Results below 25% confirm piton classification (institutional inertia without functional verification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(habeas_corpus_effectiveness_gap, empirical, 'Habeas petition success rates and outcome distribution').

omega_variable(
    torture_prohibition_enforcement_asymmetry,
    'Does torture prohibition enforcement vary systematically by detainee characteristics (citizenship, political alignment, marginalization status)? Is the constraint enforced symmetrically or does enforcement track extraction patterns?',
    'Comparative analysis of torture allegations, investigations, and prosecutions across detainee populations within single jurisdiction (or cross-jurisdiction comparison controlling for institutional framework). Identify disparities in investigation initiation, torture finding rates, and prosecution outcomes by detainee status.',
    'If enforcement is symmetric: torture prohibition provides genuine constraint on extraction. If enforcement is asymmetric: the constraint masks differential application — procedure theater while substantive protection is selective. Strongly asymmetric patterns confirm snare classification (procedure without substance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(torture_prohibition_enforcement_asymmetry, empirical, 'Whether torture prohibition enforcement is symmetric or selective').

omega_variable(
    substantive_right_emergence_mechanisms,
    'In which jurisdictions and under what conditions have substantive liberty/welfare rights been grafted onto Article 3 procedural framework? What institutional mechanisms enabled this emergence?',
    'Case law analysis and constitutional interpretation survey. Identify jurisdictions where courts have read substantive entitlements into Article 3 (e.g., minimum welfare as precondition for detention legality, presumption of liberty as substantive right rather than presumption of innocence). Document the doctrinal moves and institutional conditions that enabled this reading.',
    'If substantive emergence is rare and unstable: the procedural-substance decoupling is highly resistant to reinterpretation. If substantive emergence has occurred in multiple jurisdictions: the boundary is permeable and contingent. Documentation of successful substantive emergence routes informs the false summit assessment — the constraint is a choice, not a limit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substantive_right_emergence_mechanisms, empirical, 'Mechanisms and conditions for emergence of substantive rights within Article 3 framework').

omega_variable(
    reading_coexistence_vs_foreclosure,
    'Can a single legal framework simultaneously maintain all three readings (procedural hybrid, negative liberty, positive entitlement) or does adoption of one reading foreclose the others?',
    'Analysis of constitutional jurisprudence in federalist systems (e.g., US constitutional law, EU multiple-jurisdiction framework) where different readings coexist within a formal unity. Identify whether apparent coexistence masks hierarchy or genuine pluralism. Test through counterfactual: could a jurisdiction adopt positive entitlement reading without abandoning negative liberty reading?',
    'If readings genuinely coexist: the constraint supports all three (classified as coexists_with in reading_relations). If one reading logically forecloses another within a single framework: classify as forecloses. If coexistence is asymmetric (one reading is canonical and others are alternatives): classify as influences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_vs_foreclosure, conceptual, 'Whether alternative readings of Article 3 coexist or foreclose each other').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__procedural_hybrid_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(measure_theater_t0, udhr_article_3__procedural_hybrid_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(measure_theater_t15, udhr_article_3__procedural_hybrid_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement(measure_theater_t30, udhr_article_3__procedural_hybrid_reading, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(measure_extract_t0, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(measure_extract_t15, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 15, 0.3).
narrative_ontology:measurement(measure_extract_t30, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(measure_suppress_t0, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(measure_suppress_t15, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(measure_suppress_t30, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__procedural_hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(udhr_article_3__procedural_hybrid_reading, 0.12).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__positive_entitlement_reading).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, international_habeas_corpus_framework).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, torture_prohibition_global_norm).

% DUAL FORMULATION NOTE:
% Article 3 UDHR decomposes into three constraint stories representing distinct readings of the same kernel text. This story (procedural_hybrid_reading, ε=0.38) represents the reading that decouples procedure from substance. The negative_liberty_reading (ε≈0.25, rope) interprets Article 3 as freedom from interference. The positive_entitlement_reading (ε≈0.52, snare or tangled_rope) interprets Article 3 as obligation to provide. The three stories are linked via network.affects_constraints because adoption of one reading influences but does not foreclose the others. All three readings coexist in global practice, supporting different jurisdictional implementations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(udhr_article_3__procedural_hybrid_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
