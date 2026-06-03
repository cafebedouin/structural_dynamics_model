% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__judicial_supremacy_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__judicial_supremacy_reading
 *   human_readable: Basic Law Interpretive Boundary (Judicial Supremacy Reading)
 *   domain: constitutional_law/comparative_constitutionalism/judicial_review_theory
 *
 * SUMMARY:
 *   This constraint instantiates the judicial_supremacy_reading of the
 *   contested basic_law_interpretive_boundary kernel. In this reading, the
 *   Basic Laws constitute a higher-order legal framework that the Supreme
 *   Court must interpret and enforce, with judicial invalidation of
 *   contradictory legislation binding on the Knesset. The Supreme Court
 *   enters the system as the primary constraint-enforcer, subordinating
 *   ordinary legislation to its interpretation of constitutional principles.
 *   Knesset legislation becomes subject to judicial nullification;
 *   rights-claimants gain a veto mechanism via litigation; and any
 *   legislation threatening court-protected liberties faces high
 *   extractiveness (legislative authority is extracted via judicial review).
 *   The constraint exhibits tangled_rope structure: genuine coordination
 *   function (preventing majority tyranny, enforcing constitutional fidelity)
 *   coupled with asymmetric extraction (legislative authority is
 *   subordinated; the Knesset lacks final say over its own constitutional
 *   interpretation). The theater_ratio (0.55) reflects that judicial review
 *   in this reading mixes genuine constitutional enforcement with
 *   performative legitimacy claims — the Court must justify each invalidation
 *   by reference to the Basic Laws, creating some theatrical element even as
 *   the substantive power transfer is real.
 *
 * KEY AGENTS:
 *   - Supreme Court: Primary beneficiary (institutional/arbitrage) — gains constitutional supremacy and final interpretive authority; can defer or enforce selectively
 *   - Knesset as Institution: Primary victim (organized/constrained) — legislative authority is subordinated; can amend Basic Laws only via supermajority (functionally difficult)
 *   - Rights Claimants: Secondary beneficiary (moderate/mobile) — gain litigation veto but face procedural barriers and transaction costs
 *   - Legislator (individual member): Powerless/trapped — faces total suppression; legislation can be nullified regardless of parliamentary majority
 *   - Executive Branch: Secondary victim (powerful/mobile) — must implement laws consistent with Court interpretation; faces policy flexibility constraints
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing this specific institutional arrangement as an immutable law of democratic governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.58).
domain_priors:suppression_score(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.62).
domain_priors:theater_ratio(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__judicial_supremacy_reading, "Basic Law Interpretive Boundary (Judicial Supremacy Reading)").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__judicial_supremacy_reading, "constitutional_law/comparative_constitutionalism/judicial_review_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__judicial_supremacy_reading, '339374d3-97f1-4cc5-afee-a715463d990e').
narrative_ontology:cs_kernel_codification('339374d3-97f1-4cc5-afee-a715463d990e', fixed_text).
narrative_ontology:cs_authority_grounding('339374d3-97f1-4cc5-afee-a715463d990e', extraction).
narrative_ontology:cs_interpretation_layer_present('339374d3-97f1-4cc5-afee-a715463d990e').
narrative_ontology:cs_reading_relation('339374d3-97f1-4cc5-afee-a715463d990e', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('339374d3-97f1-4cc5-afee-a715463d990e', basic_law_interpretive_boundary__balanced_contestation_reading, influences).
narrative_ontology:cs_axiom('339374d3-97f1-4cc5-afee-a715463d990e', foundational, supreme_court_final_basic_law_interpretation).
narrative_ontology:cs_axiom_status(supreme_court_final_basic_law_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('339374d3-97f1-4cc5-afee-a715463d990e', supreme_court_final_basic_law_interpretation, deontological).
narrative_ontology:cs_axiom('339374d3-97f1-4cc5-afee-a715463d990e', secondary, judicial_nullification_supremacy).
narrative_ontology:cs_axiom_status(judicial_nullification_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('339374d3-97f1-4cc5-afee-a715463d990e', judicial_nullification_supremacy, instrumental).
narrative_ontology:cs_reference_frame('339374d3-97f1-4cc5-afee-a715463d990e', judicial_review_as_constitutional_necessity).
narrative_ontology:cs_drift_state('339374d3-97f1-4cc5-afee-a715463d990e', contemporary_21st_century_israeli_constitutional_politics, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('339374d3-97f1-4cc5-afee-a715463d990e', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimants_via_litigation).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_legislative_authority).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, executive_implementation_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE LEGISLATOR BOUND BY JUDICIAL VETO (SNARE) — The Knesset member faces structural powerlessness: legislation can be nullified by the Court regardless of parliamentary majority, and the legislator has no exit mechanism short of constitutional amendment (politically insurmountable). The suppression is total — the Court's interpretation of the Basic Laws forecloses legislative pathways. High experienced extraction: the legislative authority is stripped via judicial review.
constraint_indexing:constraint_classification(basic_law_interpretive_boundary__judicial_supremacy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE KNESSET AS INSTITUTION (TANGLED ROPE) — The Knesset retains genuine legislative function (coordination: it must pass laws, debate priorities, allocate budgets) but subject to judicial override. The constraint both enables and disables: it prevents majority tyranny (coordination benefit) while subordinating the legislature's final authority (extraction cost). The institution has constrained agency — it can veto certain judicial interpretations through super-majority amendment, but the cost is prohibitive.
constraint_indexing:constraint_classification(basic_law_interpretive_boundary__judicial_supremacy_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE SUPREME COURT (ROPE) — The Court experiences the constraint as coordination: it interprets the Basic Laws to prevent legislative encroachment on constitutional rights, enabling the rule of law. The Court has full arbitrage options — it can defer to the legislature, strike down legislation, or craft narrow holdings. The constraint is a pure coordination mechanism from the Court's position: it solves the collective action problem of constitutional fidelity.
constraint_indexing:constraint_classification(basic_law_interpretive_boundary__judicial_supremacy_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE RIGHTS CLAIMANT (TANGLED ROPE) — The individual whose rights are threatened gains a veto mechanism via litigation but must navigate the Court's doctrinal thresholds and bear legal costs. They benefit from judicial protection (coordination: the Court enforces rights) but bear the cost of litigation delay and doctrinal unpredictability. Moderate extraction: the rights claimant has agency but faces high transaction costs and procedural barriers to vindication.
constraint_indexing:constraint_classification(basic_law_interpretive_boundary__judicial_supremacy_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: THE EXECUTIVE BRANCH (SCAFFOLD) — The executive must implement laws consistent with the Court's constitutional interpretation, constraining policy flexibility. But the constraint has a sunset: a sufficiently broad legislative supermajority can amend the Basic Laws and redefine the executive's authority. The executive experiences the constraint as temporary coordination burden — high in the short term (immediate implementation constraints) but revisable over generational timescales.
constraint_indexing:constraint_classification(basic_law_interpretive_boundary__judicial_supremacy_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some degree of judicial independence from legislative override is an immutable requirement for the rule of law: if courts cannot invalidate unconstitutional legislation, the concept of higher-order law dissolves. This perspective sees judicial review as a structural necessity, not a contingent institutional choice. However, the base properties show genuine extraction and active enforcement — this perspective risks naturalizing the SPECIFIC institutional arrangement (Israeli Supreme Court authority to invalidate Knesset legislation) as a universal law rather than one possible implementation of the broader principle of judicial independence.
constraint_indexing:constraint_classification(basic_law_interpretive_boundary__judicial_supremacy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(basic_law_interpretive_boundary__judicial_supremacy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(basic_law_interpretive_boundary__judicial_supremacy_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The judicial supremacy reading concentrates interpretive authority in the Court, which extracts significant legislative capacity. However, the extraction is not maximal (0.72+) because the Knesset retains legislative initiative for ordinary legislation (only those contradicting the Court's Basic Law interpretation are nullified) and the supermajority amendment pathway preserves some legislative control over the constitutional framework itself. The extraction trajectory rises from 0.42 to 0.62 over the 20-year interval, reflecting institutional entrenchment: as the Court accumulates precedent and the Knesset adapts to the constraint, the Supreme Court's interpretive authority becomes normalized and harder to challenge. Suppression (0.62): High. The Knesset faces substantial barriers to escaping the constraint: (1) supermajority amendment is politically difficult; (2) ordinary legislation within Court-approved bounds is possible but subject to invalidation risk; (3) legislative exit requires constitutional override, not statutory revision. The suppression is active enforcement — the Court must continually invalidate legislation, and the Knesset must continually anticipate judicial response. Theater ratio (0.55): Moderate. The constraint mixes genuine constitutional enforcement with performative elements. The Court must justify each invalidation by textual reference to the Basic Laws, which creates an interpretive theater — the Court presents each decision as constrained by text when significant judicial creativity is often present. As precedent accumulates, theater may increase because the Court's prior decisions become de facto constitutional law that obscures the underlying interpretive choices. The rising trajectory (0.48 → 0.55) reflects growing performativity as the system matures.
 *
 * PERSPECTIVAL GAP:
 *   The judicial supremacy reading generates maximal perspectival divergence. The Supreme Court (institutional/arbitrage) sees pure coordination: enforcing constitutional limits prevents majority tyranny. The Knesset as an institution (organized/constrained) sees mixed coordination and extraction: it must coordinate on legislative goals within a shrinking feasible set. The legislator as an individual (powerless/trapped) sees pure extraction: the veto power is total and inescapable. Rights claimants (moderate/mobile) see mixed benefit and cost: they gain a veto mechanism but face litigation barriers. The executive (powerful/mobile) sees temporary constraints: policy flexibility is reduced but not eliminated, and supermajority amendment pathways exist. The analytical observer (civilizational/analytical) risks seeing natural law: judicial independence seems immutable to democratic governance. The perspectival gap reveals the constraint's true structure — it is not a universal requirement but a specific institutional choice that different actors experience very differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position: the Court, as beneficiary with arbitrage options, experiences low/negative d (around 0.10–0.20) because it can choose when to enforce and when to defer; d=0.10 produces f(d)≈-0.12, mapping to negative chi and rope classification. The Knesset as victim/constrained faces high d (around 0.70–0.75) because it cannot exit the constraint without constitutional amendment; d=0.75 produces f(d)≈1.10, mapping to high chi and snare classification for individual legislators. Rights claimants as beneficiaries/mobile face moderate d (around 0.35–0.40) because they benefit from the veto but face procedural costs; d=0.38 produces f(d)≈0.30, mapping to moderate chi and tangled rope. The legislator individually (powerless/trapped) faces maximal d (around 0.90–0.95) because they are a pure target with no exit; d=0.92 produces f(d)≈1.35, mapping to very high chi and snare. The executive (powerful/mobile) faces low d (around 0.40–0.50) because it retains policy flexibility within constitutional bounds and can influence judicial interpretation through implementation choices; d=0.45 produces f(d)≈0.55, mapping to moderate chi and scaffold/tangled rope.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED VIA READING SPECIFICATION. The mandatrophy in naive analysis would be: 'Is judicial review a coordination mechanism (rope) or extraction (snare)?' This constraint resolves the ambiguity by specifying the judicial_supremacy reading explicitly. Within this reading, the answer is tangled_rope: genuine coordination (preventing majority tyranny) coupled with extraction (legislative authority subordinated). The analytical observer's mountain classification is a false summit — it naturalizes the specific institutional choice of judicial supremacy as an immutable law of governance, when the sibling readings show the same kernel admits other institutional arrangements. The mandatrophy is dissolved by noting that 'judicial review' is not a single constraint but a family of readings, each with different ε values and classifications.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_restraint_threshold,
    'What standard of judicial deference to the legislature constitutes the legitimate boundary between constitutional interpretation and legislative override?',
    'Comparative analysis: examination of how other constitutional courts (Canada, Germany, Australia) define and operationalize the justiciability threshold; tracking Israeli Court doctrine evolution on political questions and discretionary decisions',
    'If threshold is strict (high deference): the Knesset''s legislative authority is preserved; constraint shifts toward Balanced Contestation reading. If threshold is minimal (low deference): Court authority expands; Judicial Supremacy reading hardens into something closer to pure Rope for the Court.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_restraint_threshold, conceptual, 'Judicial deference standard and the boundary of interpretive authority').

omega_variable(
    basic_law_amendment_tractability,
    'Is the supermajority requirement for amending Basic Laws a genuine outlet for legislative sovereignty or a functional ceiling that makes amendment politically impossible?',
    'Historical analysis: success rate of Basic Law amendment attempts, vote margins in successful amendments, political coalitions required; comparative analysis with other constitutions requiring supermajority amendment',
    'If amendments are tractable (recent successes, achievable coalitions): Legislative authority is preserved in principle; Balanced Contestation reading gains structural support. If amendments are functionally impossible: Judicial Supremacy reading hardens into permanent constraint without legislative exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(basic_law_amendment_tractability, empirical, 'Whether Basic Law amendment is a viable legislative exit mechanism').

omega_variable(
    unwritten_constitutional_principles_authority,
    'Can the Supreme Court invalidate legislation based on unwritten constitutional principles (implied rights, fundamental values) beyond the text of the Basic Laws, or is Court authority limited to textual interpretation?',
    'Doctrinal analysis: examination of landmark Israeli Court decisions (Bank Mizrahi v Migdal, HCJ 6000/90 on human dignity); comparison with foreign jurisprudence on implied constitutional rights; assessment of whether unwritten principles function as extraconstitutional super-law',
    'If unwritten principles are binding: Court authority extends beyond the Basic Laws themselves; Judicial Supremacy reading encompasses judge-made constitutional law. If Court is textually bound: Legislative authority is preserved for constitutional amendment; Balanced Contestation reading gains structural support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unwritten_constitutional_principles_authority, conceptual, 'Scope of judicial authority beyond written Basic Law text').

omega_variable(
    reading_kernel_identity,
    'Which reading of the basic_law_interpretive_boundary kernel does this constraint instantiate, and what structural assumptions distinguish it from the parliamentary_sovereignty_reading and balanced_contestation_reading?',
    'This constraint instantiates the judicial_supremacy_reading: the Basic Laws constitute a higher-order legal framework that the Supreme Court must interpret and enforce, with judicial invalidation of contradictory legislation binding on the Knesset. The sibling readings contest whether (1) the Knesset retains ultimate sovereign authority to override or redefine the Court''s jurisdiction (parliamentary_sovereignty_reading) or (2) both institutions hold bounded but legitimate authority within their respective domains (balanced_contestation_reading). This reading forecloses the parliamentary_sovereignty reading''s core premise (legislative ultimate authority) while coexisting with the balanced_contestation reading as an alternative institutional balance.',
    'The reading''s classification (tangled_rope) reflects the constraint''s structure: genuine coordination function (preventing majority encroachment on rights) coupled with asymmetric extraction (legislative authority subordinated to judicial interpretation). Alternative readings would produce different ε values and classifications: the parliamentary_sovereignty reading would likely show ε closer to rope (no extraction, pure legislative coordination); the balanced_contestation_reading (already generated as ε=0.38) shows lower extraction because both institutions retain stronger authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_identity, conceptual, 'This constraint as a specific reading of the contested judicial authority kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__judicial_supremacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(blisj_tr_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(blisj_tr_t10, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 10, 0.52).
narrative_ontology:measurement(blisj_tr_t20, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(blisj_be_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(blisj_be_t10, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(blisj_be_t20, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__balanced_contestation_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% The basic_law_interpretive_boundary kernel admits multiple constraint readings. This file generates the judicial_supremacy_reading (ε=0.58, tangled rope). The sibling reading basic_law_interpretive_boundary__balanced_contestation_reading (ε=0.38, tangled rope) shows lower extraction because both institutions retain stronger bounded authority. The third reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, remains to be generated and would likely show ε≤0.45 (rope, with no extraction because the legislature is supreme). All three readings are live institutional positions in Israeli constitutional discourse — they are not historical stages but coexisting frameworks held by different constitutional theorists and political actors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
