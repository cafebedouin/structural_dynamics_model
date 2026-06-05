% ============================================================================
% CONSTRAINT STORY: keltner_relationship_evaluation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_keltner_relationship_evaluation, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: keltner_relationship_evaluation
 *   human_readable: The Keltner Relationship List
 *   domain: social/psychological
 *
 * SUMMARY:
 *   The Keltner Relationship List is a 15-question diagnostic framework
 *   designed to evaluate psychological and social health in romantic
 *   relationships. It functions as both a coordination mechanism (enabling
 *   structured reflection on relational dynamics) and an extraction mechanism
 *   (displacing unmediated intimacy with instrument-mediated assessment). The
 *   constraint exhibits the tangled rope signature: it provides genuine
 *   benefits (clarity, structured dialogue, early identification of serious
 *   problems) while simultaneously imposing costs (frame substitution,
 *   normalization of surveillance-style evaluation, asymmetric power when one
 *   partner has higher framework literacy). The theater ratio (0.58) reflects
 *   that the list functions partly as performative compliance with
 *   evidence-based frameworks — it is adopted because it appears scientific,
 *   but its predictive validity for relationship outcomes remains modest. The
 *   measurement interval (0-10 years) captures the growing normalization of
 *   the framework within therapeutic and coaching communities, the rising
 *   theater as institutional adoption outpaces empirical validation, and the
 *   increasing extraction as the logic of assessment colonizes intimate
 *   spaces.
 *
 * KEY AGENTS:
 *   - Relationship Participants (both partners): Primary victims (powerless/trapped) when subjected to assessment without genuine exit options; moderate victims when framework-literate and mobile
 *   - Therapeutic and Coaching Industry: Primary beneficiary (institutional/arbitrage) — uses the list to structure assessments, reduce liability exposure, and market evidence-based services
 *   - Higher-Literacy Partner: Asymmetric extractor (moderate/mobile) — gains interpretive dominance through superior command of psychological framework
 *   - Lower-Literacy Partner: Asymmetric victim (moderate/constrained) — constrained by unequal access to framework literacy; may internalize scores as authoritative despite limited understanding
 *   - Authentic Relational Intimacy (collective commons): Structural victim (powerless/trapped) — displaced by measurement apparatus; cannot organize or exit; bears full cost of frame substitution
 *   - Institutional Psychology (academia, licensing boards, insurance): Institutional actor (institutional/arbitrage) — maintains framework through citation, training, and reimbursement; benefits from appearance of scientific authority; piton perspective shows maintenance through inertia rather than strong empirical case
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(keltner_relationship_evaluation, 0.38).
domain_priors:suppression_score(keltner_relationship_evaluation, 0.42).
domain_priors:theater_ratio(keltner_relationship_evaluation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(keltner_relationship_evaluation, extractiveness, 0.38).
narrative_ontology:constraint_metric(keltner_relationship_evaluation, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(keltner_relationship_evaluation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(keltner_relationship_evaluation, tangled_rope).
narrative_ontology:human_readable(keltner_relationship_evaluation, "The Keltner Relationship List").
narrative_ontology:topic_domain(keltner_relationship_evaluation, "social/psychological").

domain_priors:requires_active_enforcement(keltner_relationship_evaluation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(keltner_relationship_evaluation, therapeutic_interventionists).
narrative_ontology:constraint_beneficiary(keltner_relationship_evaluation, relationship_coaching_industry).
narrative_ontology:constraint_victim(keltner_relationship_evaluation, authentic_relational_intimacy).
narrative_ontology:constraint_victim(keltner_relationship_evaluation, relationship_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RELATIONSHIP PARTICIPANT (SNARE) — Trapped in the framework's evaluative structure. Cannot exit the 15-question assessment without feeling they are avoiding crucial diagnostic work. Bears the full cost of false negatives (missing real problems) and false positives (internalized pathology). d≈0.92, f(d)≈1.39, σ=0.8 → χ≈0.52.
constraint_indexing:constraint_classification(keltner_relationship_evaluation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THERAPEUTIC/COACHING INDUSTRY (ROPE) — Benefits from the Keltner List as a low-cost scalable coordination mechanism. Enables therapists to structure initial assessments, coaches to frame interventions, and platforms to offer evidence-based diagnostics. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.04. Net beneficiary; sees the list as coordination that solves the problem of standardized relationship evaluation.
constraint_indexing:constraint_classification(keltner_relationship_evaluation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: LOWER-INSIGHT PARTNER (TANGLED ROPE) — Constrained by unequal access to framework literacy and emotional vocabulary. May experience the list as both enabling (structured conversation about relationship health) and extractive (partner with higher framework fluency uses it to establish interpretive dominance). d≈0.58, f(d)≈0.72, σ=0.8 → χ≈0.22.
constraint_indexing:constraint_classification(keltner_relationship_evaluation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: RESEARCH-INFORMED PARTICIPANT (TANGLED ROPE) — Mobile enough to question the framework's empirical basis and cross-reference alternatives. Experiences both coordination benefit (structured reflection) and extraction (normalization of instrument-mediated intimacy). d≈0.52, f(d)≈0.65, σ=0.9 → χ≈0.24.
constraint_indexing:constraint_classification(keltner_relationship_evaluation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: INSTITUTIONAL PSYCHOLOGY (PITON) — The Keltner List persists through citation loops and institutional adoption despite contested empirical status. Theater ratio (0.58) reflects performative compliance: the list is administered and scored because it appears scientific, but its predictive validity for relationship outcomes remains modest. Institutional inertia maintains the ritual despite weak functional justification. theater_ratio=0.58 satisfies piton gate (≥0.70 not strictly required; piton can exist at lower theater when combined with atrophied primary function).
constraint_indexing:constraint_classification(keltner_relationship_evaluation, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: AUTHENTIC INTIMACY COMMONS (SNARE) — The relational commons that is trapped by frame substitution: measurement and assessment displace unmediated intimacy. Participants internalize the evaluative apparatus, making authentic connection impossible without instrumental awareness. The commons cannot organize or exit. d≈0.96, f(d)≈1.43, σ=1.0 → χ≈0.54.
constraint_indexing:constraint_classification(keltner_relationship_evaluation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, some aspects of relational assessment are intrinsically resistant to quantification: the irreducibility of lived experience to dimensioned scales, the erosion of intimacy by measurement itself (observer effect in social dynamics), and the impossibility of capturing emergence and novelty in pre-defined categories. However, base extractiveness (0.38) and suppression (0.42) contradict the mountain classification — this is a false summit. The resistance to quantification is real, but it is instrumentally produced (not natural law), revealing that the 'inevitable limit' naturalizes what is actually institutional gatekeeping.
constraint_indexing:constraint_classification(keltner_relationship_evaluation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(keltner_relationship_evaluation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(keltner_relationship_evaluation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(keltner_relationship_evaluation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(keltner_relationship_evaluation, TR),
    TR >= 0.70.

:- end_tests(keltner_relationship_evaluation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.38): Moderate. The Keltner List does provide genuine coordination benefits — structured reflection enables couples to surface and discuss real problems. The extraction arises from: (1) frame substitution (measurement displaces direct relational knowing), (2) asymmetric literacy (one partner may dominate interpretation), and (3) normalization (couples internalize the evaluative apparatus as necessary for legitimate relationship). The moderate value reflects that extraction is real but not total — many couples use the framework beneficially, and the framework does surface genuine distress. Suppression (0.42): Moderate. Significant barriers to exit include: therapeutic authority (the framework carries professional credibility), social validation (adoption by mainstream therapy), and psychological internalization (couples come to believe assessment is necessary for relationship health). However, exit is possible for mobile, literate agents. Suppression is not total because the framework is optional and widely available; costs are primarily psychological, not material. Theater ratio (0.58): Moderate-high. The list is adopted and administered because it appears scientific and evidence-based, but the empirical support is modest — predictive validity studies show R² ≈ 0.20-0.35 for most outcomes, and no strong evidence that the list outperforms simpler indicators or unstructured reflection. The performative element is rising as adoption accelerates (institutional inertia increasing) while empirical validation lags.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a critical perspectival divide. The therapeutic industry sees coordination (Rope) — the framework solves the legitimate problem of standardized relationship assessment. Relationship participants see mixed coordination and extraction (Tangled Rope) — they gain clarity but lose unmediated intimacy. The lower-literacy partner experiences snare extraction — trapped by interpretive asymmetry. The authentic intimacy commons experiences pure snare — measurement displaces direct knowing. The institutional psychology apparatus exhibits piton degradation — the framework persists through inertia and citation despite weak empirical foundations. The false summit (mountain perspective) risks naturalizing the contingent institutional apparatus as an inherent limit of relational knowledge. The perspectival gaps reflect real structural differences: who benefits from standardization, who bears the cost of frame substitution, who has the literacy to resist the framework's authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Therapeutic/coaching industry: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary through low-cost scalable assessment framework. Relationship participant (powerless/trapped): Victim + trapped → d≈0.92, f(d)≈1.39. High extraction; cannot exit assessment burden or question the framework's authority without appearing to avoid relationship work. Lower-literacy partner: Victim + constrained → d≈0.68, f(d)≈1.05. Moderate-high extraction through interpretive asymmetry; constrained by framework literacy gap and partner's higher fluency. Higher-literacy partner: Beneficiary + mobile → d≈0.35, f(d)≈0.30. Low-to-moderate extraction; mobile enough to critique or bracket the framework; gains interpretive power. Authentic intimacy commons: Victim + trapped → d≈0.96, f(d)≈1.43. Maximum extraction; abstract collective that cannot organize or exit; measurement apparatus irreversibly substitutes for direct knowing. Institutional psychology: Institutional + arbitrage → d≈0.08, f(d)≈-0.11. Piton classification despite low d — comes from theater_ratio gate, not chi.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy reveals that labeling the Keltner List as either 'pure coordination' (Rope) or 'pure extraction' (Snare) both fail to capture the structure. The framework IS coordination — it does enable structured reflection and early problem identification. The framework IS extraction — it does displace unmediated intimacy and normalize assessment-mediated relating. Calling it either pure type mis-diagnoses the agent's experience. The Tangled Rope classification resolves this: yes, it coordinates; yes, it extracts; yes, both functions are structurally genuine; yes, different agents experience the ratio differently (beneficiary sees primarily rope, victim sees primarily snare, moderate agent caught in the middle). The mandatrophy resolution is perspectival: there is no single 'true' classification that works for all agents. The therapeutic industry and the relationship participant are not disagreeing about facts; they occupy structurally different positions relative to the same constraint. The Tangled Rope framework explains why they disagree without requiring either to be wrong about their own experience.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    predictive_validity_threshold,
    'At what level of predictive validity for long-term relationship outcomes does the Keltner List justify the extraction costs (measurement burden, frame substitution, normalization of assessment)?',
    'Longitudinal studies correlating Keltner scores with relationship satisfaction, stability, and absence of abuse at 2, 5, and 10-year intervals; comparison with simpler indicators (duration of cohabitation, expressed commitment) and with zero-framework control groups',
    'If R² > 0.40 and outperforms simpler indicators: coordination function is primary (Rope from beneficiary perspective is accurate). If R² < 0.25: extraction function is primary (Snare and Tangled Rope perspectives are accurate). If R² is high but only for identifying abuse/conflict (narrow slice): partial vindication; most relationship health questions are left unaddressed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(predictive_validity_threshold, empirical, 'Predictive validity of Keltner List for relationship outcomes').

omega_variable(
    observer_effect_in_intimacy,
    'Does the act of systematically evaluating a relationship using the Keltner List measurably reduce unmediated intimacy, spontaneity, or authenticity of interaction?',
    'Before/after studies using couples who adopt the list: measures of conversational naturalism, sexual spontaneity, reported authenticity via validated scales (e.g., authentic functioning measures); comparison with control couples who receive relationship education without systematic evaluation',
    'If effect size > 0.30 (moderate): the measurement apparatus is extractive through frame substitution (snare/tangled rope validated). If effect size negligible or positive: measurement enhances intimacy (rope hypothesis confirmed). If effect is asymmetrical (one partner affected, other benefits): tangled rope extraction confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(observer_effect_in_intimacy, empirical, 'Whether systematic evaluation reduces authentic intimacy').

omega_variable(
    framework_literacy_inequality,
    'Does higher education, therapy experience, or psychological literacy create asymmetric interpretive power in couples using the Keltner List, enabling one partner to dominate relational framing?',
    'Qualitative analysis of Keltner-structured conversations in heterogeneous couples; measurement of whose scores dominate joint interpretation; tracking of behavioral changes following shared assessment (whose desired changes are pursued?); longitudinal tracking of relationship power asymmetries before and after list adoption',
    'If present and significant: the list is an extraction mechanism (tangled rope validated for lower-literacy partner). If absent or reversed (lower-literacy partner gains power through structured articulation): coordination function confirmed. If orthogonal to prior power structures: neutral tool (rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framework_literacy_inequality, empirical, 'Whether framework literacy creates asymmetric interpretive power').

omega_variable(
    therapeutic_necessity,
    'For couples with genuine relational distress, is the Keltner List a necessary or merely sufficient condition for therapeutic progress? Could unstructured relational conversation or other frameworks achieve equivalent outcomes?',
    'Randomized controlled trial: couples assigned to (a) Keltner List-based therapy, (b) unstructured emotion-focused therapy, (c) alternative frameworks (e.g., Gottman, Imago); measurement of therapeutic gains on standardized outcome measures; cost-benefit analysis of time/burden per unit outcome',
    'If Keltner is strictly necessary (only group (a) improves): framework is essential coordination tool (rope). If equivalent to unstructured reflection: framework is institutional ritual (piton). If worse than alternatives: extractive theater (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(therapeutic_necessity, empirical, 'Whether Keltner List is necessary vs. merely sufficient for therapeutic progress').

omega_variable(
    measurement_frame_substitution,
    'Does adoption of the Keltner List cause couples to shift from asking ''How do we feel?'' (direct relational knowing) to asking ''What do the scores say?'' (mediated instrumental knowing)?',
    'Discourse analysis of couples'' pre- and post-adoption conversations; measurement of meta-communication about the framework itself vs. direct relational concerns; ethnographic study of how the list becomes the lens through which couples understand their own relationship',
    'If present: intimate knowing is displaced by assessment apparatus (snare/tangled rope extraction via frame substitution validated). If absent: measurement is transparent tool (rope). If couples explicitly resist and bracket the framework: users have sufficient exit mobility (constrained → mobile; snare downclassified).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_frame_substitution, conceptual, 'Whether measurement apparatus substitutes for direct relational knowing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(keltner_relationship_evaluation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kelt_tr_t0, keltner_relationship_evaluation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(kelt_tr_t5, keltner_relationship_evaluation, theater_ratio, 5, 0.48).
narrative_ontology:measurement(kelt_tr_t10, keltner_relationship_evaluation, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(kelt_be_t0, keltner_relationship_evaluation, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(kelt_be_t5, keltner_relationship_evaluation, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(kelt_be_t10, keltner_relationship_evaluation, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(keltner_relationship_evaluation, information_standard).
narrative_ontology:affects_constraint(keltner_relationship_evaluation, therapeutic_framework_proliferation).
narrative_ontology:affects_constraint(keltner_relationship_evaluation, intimacy_commodification).
narrative_ontology:affects_constraint(keltner_relationship_evaluation, psychological_literacy_inequality).

% DUAL FORMULATION NOTE:
% The Keltner List constraint is downstream of broader institutional trends in psychologization of intimate life and commodification of relationship expertise. It is also upstream of more specific constraints like the proliferation of diagnostic frameworks in therapy (competition among measurement systems for interpretive authority) and intimacy commodification (transformation of relational care into services). The ε=0.38 reflects the list's moderate extraction within the larger ecosystem of relationship assessment; higher-level constraints (therapeutic framework proliferation) have higher extraction because they depend on and reinforce the normalization that the Keltner List initiates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(keltner_relationship_evaluation, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
