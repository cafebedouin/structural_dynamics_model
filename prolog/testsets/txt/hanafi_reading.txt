% ============================================================================
% CONSTRAINT STORY: hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hanafi_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hanafi_reading
 *   human_readable: Hanafi Juristic Discretion via Qiyas and Ra'y
 *   domain: islamic_jurisprudence/legal_theory/commitment_systems
 *
 * SUMMARY:
 *   The Hanafi juristic method, particularly the expansive application of
 *   qiyas (analogical reasoning) and ra'y (juristic preference), represents a
 *   reading of the broader usul_al_fiqh_method kernel that prioritizes
 *   adaptive capacity and juristic discretion when texts are silent or
 *   ambiguous. This reading differs structurally from the Maliki, Shafi'i,
 *   and Hanbali approaches, which employ different methodological balances.
 *   The Hanafi reading exhibits the properties of a Tangled Rope: it provides
 *   genuine coordination (legal adaptation without constant textual
 *   re-examination) while simultaneously concentrating authority in juristic
 *   hands, extracting predictability from lay petitioners and textual
 *   certainty from the epistemic commons. The constraint's theater_ratio
 *   (0.35) reflects that Hanafi juristic reasoning is substantially
 *   functional — qiyas and ra'y are legitimate tools for resolving ambiguity
 *   — but increasingly incorporates performative elements as the juristic
 *   tradition develops and institutionalizes. The measurement trajectory (ε:
 *   0.35 → 0.52 over 600 time units) shows extractiveness accumulation as
 *   Hanafi jurisprudence expands ra'y applications and the interpretive
 *   tradition grows thicker, creating more layers of inference between text
 *   and decision.
 *
 * KEY AGENTS:
 *   - Hanafi Jurists (Institutional/Arbitrage): Primary beneficiaries — gain interpretive authority and adaptive capacity through expanded qiyas/ra'y; can choose the inference depth that solves cases.
 *   - Lay Petitioners (Powerless/Trapped): Primary victims — cannot predict legal outcomes from text alone; must rely on expert judgment; face cognitive and economic overhead.
 *   - Textual Certainty (Abstract Collective/Trapped): Epistemic victim — the ideal of law grounded in explicit text is subordinated to inference; no self-advocacy mechanism.
 *   - Textualist Jurists (Moderate/Constrained): Secondary agents — benefit from interpretive frameworks but face professional cost for rejecting ra'y innovations; constrained by school consensus.
 *   - Competing Juristic Schools (Institutional/Constrained): Institutional victims — constrained by Hanafi method's persuasive power; lose adherents to Hanafi flexibility; must actively defend restrictive readings.
 *   - Analytical Observer (Analytical/Analytical): Risks naturalizing contingent institutional arrangements as inherent to jurisprudence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hanafi_reading, 0.52).
domain_priors:suppression_score(hanafi_reading, 0.48).
domain_priors:theater_ratio(hanafi_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hanafi_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(hanafi_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(hanafi_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hanafi_reading, tangled_rope).
narrative_ontology:human_readable(hanafi_reading, "Hanafi Juristic Discretion via Qiyas and Ra'y").
narrative_ontology:topic_domain(hanafi_reading, "islamic_jurisprudence/legal_theory/commitment_systems").

domain_priors:requires_active_enforcement(hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(hanafi_reading, fixed_text).
narrative_ontology:cs_authority_grounding(hanafi_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(hanafi_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hanafi_reading, hanafi_jurists).
narrative_ontology:constraint_beneficiary(hanafi_reading, legal_adaptatability).
narrative_ontology:constraint_victim(hanafi_reading, textual_certainty).
narrative_ontology:constraint_victim(hanafi_reading, lay_predictability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LAY PETITIONER (SNARE) — Cannot predict legal outcomes from text alone. Trapped within juristic discretion; the answer depends on which jurist interprets the ambiguous case. Maximum extraction of cognitive and social overhead. No exit from reliance on expert judgment.
constraint_indexing:constraint_classification(hanafi_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: TEXTUAL CERTAINTY AS EPISTEMIC COLLECTIVE (SNARE) — The ideal of law grounded in explicit Qur'anic/Hadith text cannot exit the system. Expansive qiyas and ra'y subordinate text-based certainty to jurist inference. No coordination function protects textual foundations — only extraction of epistemological authority into juristic hands.
constraint_indexing:constraint_classification(hanafi_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: TEXTUALIST JURIST (TANGLED ROPE) — Constrained by Hanafi consensus but also benefits from access to interpretive frameworks and scholarly tradition. Can apply strict qiyas (analogical reasoning) but faces social/professional cost for rejecting ra'y (juristic preference) innovations. Mixed: genuine coordination function (coherent juristic method) but also extraction of discretionary authority.
constraint_indexing:constraint_classification(hanafi_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: HANAFI JURISTIC SCHOOL (ROPE) — Experiences expansive qiyas and ra'y as pure coordination: these methods enable legal adaptation to novel cases without constant textual re-examination. The school gains institutional coherence and adaptive capacity. Beneficiary through arbitrage — can choose the interpretive depth that solves immediate problems.
constraint_indexing:constraint_classification(hanafi_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: COMPETING JURISTIC SCHOOLS (TANGLED ROPE) — Constrained by the Hanafi method's persuasive power (ra'y innovations attract followers). Also benefit from the boundary maintained by method (Hanafi identity persists). Asymmetric extraction: schools with more restrictive qiyas lose adherents to Hanafi flexibility. Active enforcement required: school leadership must defend restrictive readings against Hanafi reinterpretation.
constraint_indexing:constraint_classification(hanafi_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational horizon, textual silence and ambiguity are inherent to any fixed text applied to novel cases. Qiyas and ra'y respond to an immutable structural feature of jurisprudence: no text can pre-specify all cases. This perspective risks naturalizing what is actually a contested methodological choice between schools. Engine false-summit detection applies.
constraint_indexing:constraint_classification(hanafi_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hanafi_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hanafi_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hanafi_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hanafi_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The Hanafi method's expansion of qiyas and ra'y extracts predictability from lay agents and textual certainty from the epistemic commons. However, the extraction is not absolute — the methods are genuine tools for resolving real ambiguities, and their use is bounded by juristic tradition and scholarly consensus. The value reflects that most extraction is justified (coordination overhead) but some is preferential (authority accumulation). Suppression (0.48): Moderate. Barriers to exit include institutional dependence on juristic expertise (no alternative source of legal authority), social integration of juristic judgment into community decision-making, and the high cognitive cost of learning alternative methodologies. However, suppression is not absolute — textualist alternatives exist (Hanbali strictness), and lay agents can theoretically organize around textual simplicity. Theater ratio (0.35): Low-moderate. Hanafi juristic reasoning is substantially functional — the methods genuinely solve the coordination problem of applying fixed text to novel cases. Theater increases over time as the juristic tradition becomes more elaborate and stratified, creating more performative layers of commentary on commentary. The 0.35 endpoint reflects that the tradition has begun to substitute procedural elaboration for problem-solving.
 *
 * PERSPECTIVAL GAP:
 *   The Hanafi juristic school experiences expansive qiyas and ra'y as a solution to a genuine coordination problem: how to apply fixed text to cases the text does not anticipate. This perspective sees the methods as Rope — pure coordination with minimal coercive overhead. Lay petitioners experience the same methods as pure extraction — they cannot predict outcomes from text, incurring high cognitive cost and dependence on expert judgment. Textualist jurists see mixed extraction and coordination — the methods enable problem-solving but also extract authority they would prefer to see remain in the text. The epistemic commons (textual certainty as abstract victim) sees pure extraction — its ideal is subordinated to inference with no protective mechanism. Competing juristic schools see the Hanafi methods as enforced extraction — they are constrained to defend restrictive readings against Hanafi reinterpretation, incurring institutional cost. The analytical observer risks seeing Mountain (the necessity of interpretation is a natural law of jurisprudence) but this risks naturalizing what is actually a contested institutional choice. The false-summit detector identifies the naturalization: competing schools exist and prove that alternative methodologies are structurally possible, even if institutionally less successful.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) for each perspective are derived from structural position: lay petitioners are trapped victims (high d, high f(d), high experienced extraction); Hanafi jurists are beneficiaries with arbitrage options (low d, low/negative f(d), minimal experienced extraction); textualist jurists are moderate victims with constrained options (moderate d, moderate f(d), moderate experienced extraction); competing schools are constrained institutional actors with partial extraction asymmetry (moderate-high d). The Hanafi juristic school itself, as beneficiary, derives d ≈ 0.10 (arbitrage exit, beneficiary status), yielding negative χ relative to base ε — they experience the constraint as coordination benefit, not extraction. The analytical observer derives d ≈ 0.72 from (analytical, analytical) canonical values, experiencing the constraint as extraction of epistemic authority from lay agents. Perspective gaps emerge: the beneficiary sees Rope (pure coordination), the victims see Snare (pure extraction), the moderate actor sees Tangled Rope (genuine coordination + asymmetric extraction), the competitor sees Tangled Rope with enforcement (active defense required), the analytical observer risks seeing Mountain (natural law) but engine false-summit detection flags the naturalization.
 *
 * MANDATROPHY ANALYSIS:
 *   The Hanafi constraint resolves mandatrophy by showing that 'genuine coordination function + asymmetric extraction' is precisely the definition of Tangled Rope. The Hanafi jurists are solving a real problem (legal adaptation when text is silent) — this is the coordination function. But the solution concentrates authority in their hands and extracts predictability from lay petitioners — this is the asymmetric extraction. Both are structurally true. No type misapplication occurs. The false-summit risk (Mountain from analytical view) is the actual analytical error to flag: naturalizing institutional choice as law of nature. The constraint's measurement trajectory shows extractiveness accumulation (0.35 → 0.52) without corresponding theater growth (0.20 → 0.35), indicating that the core extraction mechanism is strengthening while the functional problem-solving capacity remains robust. This pattern suggests the coordination function is real and stable, but the institutional authority layer is accumulating — a healthy Tangled Rope gradually sliding toward higher asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discretion_scope_boundary,
    'Where does legitimate juristic discretion (ra''y) end and arbitrary preference-imposition begin?',
    'Historical analysis of Hanafi juristic decisions: identify cases where ra''y produced outcomes contradicting explicit textual preference. Measure divergence between Hanafi conclusions and those of stricter schools on identical cases. Assess whether divergence is explainable by method difference or by hidden preferential extraction.',
    'If boundary is stable and defensible: Hanafi constraint is primarily coordination (Rope with legitimate overhead). If boundary is permeable and preference-driven: constraint is primarily extraction (Snare features dominate). If boundary shifts over time: theater_ratio rises, suggesting Piton degradation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretion_scope_boundary, empirical, 'Boundary between legitimate discretion and arbitrary extraction in juristic preference').

omega_variable(
    reading_contingency,
    'Is this constraint a reading-specific feature of Hanafi jurisprudence, or does it apply equally to all four schools?',
    'Comparative analysis of qiyas and ra''y across Hanafi, Maliki, Shafi''i, and Hanbali schools. Measure extractiveness of each school''s method by: (a) frequency of divergence from text, (b) breadth of discretionary latitude, (c) institutional power concentration in jurist hands. Compare beneficiary/victim structures.',
    'If Hanafi is highest in both extractiveness and juristic empowerment: reading-specific constraint confirmed. If all schools show similar structure: constraint is method-invariant (belongs to usul_al_fiqh_method kernel, not hanafi_reading). If Hanafi is lowest: reading challenges the hypothesis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contingency, conceptual, 'Whether expansive discretion is Hanafi-specific or applicable to all schools').

omega_variable(
    historical_evolution_tea_leaf,
    'Did Hanafi ra''y expansion occur to solve genuine coordination problems, or to accumulate juristic authority?',
    'Genealogical analysis of specific innovations in Hanafi jurisprudence: identify the case that prompted each major ra''y extension. Distinguish between: (A) cases where text was genuinely silent/ambiguous and expansion solved a real legal problem; (B) cases where text was explicit but ra''y overrode it for preferential reasons; (C) cases where ra''y was used to harmonize conflicting rules (genuine coordination). Measure proportion of each type.',
    'If majority is Type A/C: constraint is hybrid (Tangled Rope confirmed). If majority is Type B: constraint is pure extraction (Snare). If Type A/C ratio declined over time: Piton degradation (theater_ratio increases as original coordination function atrophies).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_evolution_tea_leaf, empirical, 'Whether Hanafi ra''y expansion responded to coordination problems or authority accumulation').

omega_variable(
    false_summit_natural_law,
    'Is the perceived necessity of qiyas and ra''y a genuine immutable feature of textual jurisprudence, or a contingent institutional feature of how Islamic law developed?',
    'Counterfactual analysis: (1) Can a textualist jurisprudence function without qiyas/ra''y? (2) What would it require institutionally (e.g., continuous legislative amendment, different scholarly roles, different authority structures)? (3) Which features of textualism are actually constraining vs. assumed to be constraining? Historical comparison with non-Islamic legal traditions that handle textual silence differently (Talmudic reasoning, Roman law, common law development).',
    'If textualism is fundamentally impossible: Mountain classification sustained. If textualism is possible but institutionally expensive: constraint is Tangled Rope (coordination at high cost) not Mountain. If textualism is possible and cost-competitive: constraint is extraction under false naturalization (False Summit — reclassify to Tangled Rope or Snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_natural_law, conceptual, 'Whether interpretive necessity is natural law or contingent institutional design').

omega_variable(
    kernel_reading_committer,
    'This constraint is ONE reading of the usul_al_fiqh_method kernel. How does the Hanafi reading differ structurally from Maliki, Shafi''i, and Hanbali readings?',
    'Comparative constraint authoring: generate parallel constraint stories for maliki_reading, shafii_reading, hanbali_reading with identical base structure but measured differences in: (1) extractiveness (qiyas/ra''y latitude), (2) beneficiaries (which jurists gain authority), (3) theater_ratio (performative vs. functional content). Network link all four stories via affects_constraints. Identify which reading has highest ε and which has highest institutional capture.',
    'Different readings will have different ε values. The reading with highest ε represents the most expansive discretion and thus the strongest extraction mechanism. Structural data will show whether ''expansion'' is coordination-driven or preference-driven by comparing beneficiary power concentration and victim constraints across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer, conceptual, 'Comparative structure of Hanafi vs. other school readings of the usul_al_fiqh_method kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hanafi_reading, 0, 600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hana_tr_t0, hanafi_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hana_tr_t300, hanafi_reading, theater_ratio, 300, 0.28).
narrative_ontology:measurement(hana_tr_t600, hanafi_reading, theater_ratio, 600, 0.35).

% Extraction over time
narrative_ontology:measurement(hana_be_t0, hanafi_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hana_be_t300, hanafi_reading, base_extractiveness, 300, 0.45).
narrative_ontology:measurement(hana_be_t600, hanafi_reading, base_extractiveness, 600, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hanafi_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(hanafi_reading, maliki_reading).
narrative_ontology:affects_constraint(hanafi_reading, shafii_reading).
narrative_ontology:affects_constraint(hanafi_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% The Hanafi reading is one constraint within the usul_al_fiqh_method kernel family. Each school reading has its own ε value reflecting the scope of juristic discretion permitted. The Hanafi reading exhibits ε=0.52 (moderate extractiveness) due to expansive qiyas/ra'y. Sibling readings will show different ε values: Maliki (ε≈0.45, moderate constraint on discretion via community practice), Shafi'i (ε≈0.35, stricter qiyas limitation), Hanbali (ε≈0.25, minimal ra'y). The network structure represents the methodological ecosystem in Islamic jurisprudence: all schools address the same kernel (textual silence/ambiguity) but with different extractiveness profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hanafi_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
