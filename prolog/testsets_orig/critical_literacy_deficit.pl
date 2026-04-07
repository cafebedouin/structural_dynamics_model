% ============================================================================
% CONSTRAINT STORY: critical_literacy_deficit
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_critical_literacy_deficit, []).

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
 *   constraint_id: critical_literacy_deficit
 *   human_readable: Critical Literacy Deficit as a Structural Constraint on Democratic Participation
 *   domain: social/epistemic/political
 *
 * SUMMARY:
 *   Critical literacy — the ability to evaluate sources, detect manipulation,
 *   identify assumptions, and synthesize contradictory information — is a
 *   structural constraint on democratic participation and epistemic
 *   reliability. The deficit operates as both institutional trap and
 *   distributed cognitive burden. Underfunded educational systems cannot
 *   teach these skills; mass readers cannot acquire them without substantial
 *   individual effort; algorithm-driven platforms profit from their absence.
 *   The constraint is not uniform: wealthy households access critical
 *   literacy education through private schools and parental modeling;
 *   organized communities build it through mutual aid; institutional actors
 *   with resources exercise it routinely. The constraint concentrates
 *   extraction on powerless populations with fewest resources to acquire
 *   skills or afford the attention costs of developing them. Over the 20-year
 *   interval, extractiveness has increased as algorithmic platforms matured
 *   and as attention demands intensified. Theater ratio has risen as
 *   traditional media institutions claim quality gatekeeping they no longer
 *   functionally perform. The critical literacy deficit is a tangled rope:
 *   genuine coordination functions (education does improve reasoning;
 *   communities practicing critical analysis do make better decisions) are
 *   embedded within asymmetric extraction (attention harvesting,
 *   misinformation amplification, reduced political efficacy for low-literacy
 *   populations).
 *
 * KEY AGENTS:
 *   - Mass Population Readers: Primary victims (powerless/trapped) — lack skills to navigate information environment; bear extraction through attention harvesting, misinformation exposure, and reduced political agency
 *   - Self-Educated Readers: Secondary victims (moderate/constrained) — invest time/effort to acquire skills; benefit from literacy but at significant opportunity cost and exposure risk during learning
 *   - Educational Institutions with Critical Literacy Programs: Beneficiaries (institutional/arbitrage) — gain enrollment, reputation, and prestige from quality critical thinking curricula; have exit options and mobile positioning
 *   - Underfunded Educational Systems: Identity-locked victims (institutional/identity_locked) — structurally mobile in principle but identity constituted through productivity metrics that treat critical literacy as inefficient luxury rather than epistemic infrastructure
 *   - Attention Merchants and Platform Algorithms: Primary beneficiaries (powerful/mobile) — profit from low literacy (engagement = emotional reaction = revenue); possess coordination capacity layered with extraction
 *   - Traditional Media Institutions: Degraded (institutional/arbitrage, Piton) — once functioned as verification gatekeepers, now perform theater of editorial oversight while losing actual quality control capacity
 *   - Organized Literacy Advocacy and Fact-Checking Networks: Mixed (organized/constrained) — provide genuine coordination function but face extraction through funding scarcity and resource imbalance
 *   - Epistemic Commons: Structural victim (powerless/trapped) — abstract collective good bearing costs of misinformation contamination with no organized voice or exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(critical_literacy_deficit, 0.58).
domain_priors:suppression_score(critical_literacy_deficit, 0.65).
domain_priors:theater_ratio(critical_literacy_deficit, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(critical_literacy_deficit, extractiveness, 0.58).
narrative_ontology:constraint_metric(critical_literacy_deficit, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(critical_literacy_deficit, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(critical_literacy_deficit, tangled_rope).
narrative_ontology:human_readable(critical_literacy_deficit, "Critical Literacy Deficit as a Structural Constraint on Democratic Participation").
narrative_ontology:topic_domain(critical_literacy_deficit, "social/epistemic/political").

domain_priors:requires_active_enforcement(critical_literacy_deficit).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(critical_literacy_deficit, information_extractors).
narrative_ontology:constraint_beneficiary(critical_literacy_deficit, attention_merchants).
narrative_ontology:constraint_beneficiary(critical_literacy_deficit, political_manipulation_actors).
narrative_ontology:constraint_victim(critical_literacy_deficit, mass_population_readers).
narrative_ontology:constraint_victim(critical_literacy_deficit, epistemic_commons).
narrative_ontology:constraint_victim(critical_literacy_deficit, informed_citizenship_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MASS READER (SNARE) — Structurally trapped by lack of skills to evaluate source credibility, detect manipulation, or synthesize contradictory claims. Cannot exit without cognitive tools they were not taught. Bears extraction through attention harvesting, misinformation exposure, and reduced capacity to identify their own interests. No coordination benefit — pure extraction.
constraint_indexing:constraint_classification(critical_literacy_deficit, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SELF-EDUCATED READER (TANGLED ROPE) — Faces high learning costs and social friction (time burden, exposure to false information during learning, isolation from peer groups that dismiss critical analysis) but can invest in skills through effort. Experiences mixed coordination benefit (access to better information, community of practice) and extraction (resource scarcity in critical pedagogy, opportunity costs). Constrained but not trapped — exit is possible at significant cost.
constraint_indexing:constraint_classification(critical_literacy_deficit, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EDUCATIONAL INSTITUTIONS (ROPE) — Benefit from reputation and student demand when offering critical thinking curricula. Experience the constraint as coordination problem they are solving through pedagogy. Have exit options (arbitrage to alternative frameworks, educational specialization). Net beneficiary relative to this constraint — extraction runs toward them through increased enrollment and prestige.
constraint_indexing:constraint_classification(critical_literacy_deficit, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: UNDERFUNDED EDUCATIONAL SYSTEMS (SNARE via identity_locked) — Structurally mobile in principle (could redirect resources, reform curricula) but identity-locked by neoliberal framing that treats education as economically productive labor pipeline rather than epistemic capability builder. System's identity is constituted through standardized testing, workforce metrics, and measurable ROI. Critical literacy appears as inefficient luxury rather than structural necessity. Cannot perceive exit because their institutional identity depends on the framework that prevents critical pedagogy investment.
constraint_indexing:constraint_classification(critical_literacy_deficit, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL MEDIA SYSTEMS (PITON) — Once functioned as gatekeepers enforcing editorial standards and fact-checking (coordination role). Now largely degraded — theater ratio high as legacy reputation institutions (newspapers, broadcast news) persist but have lost functional verification and quality control capacity. Maintained through institutional inertia and audience habit, not genuine coordination function. High theater, low actual literacy promotion.
constraint_indexing:constraint_classification(critical_literacy_deficit, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ATTENTION MERCHANTS (TANGLED ROPE) — Benefit substantially from critical literacy deficit (algorithmic engagement optimizes for emotional reaction over accuracy; low literacy users more susceptible to viral misinformation; high advertising value). Possess genuine coordination capacity (platforms do coordinate information flow, create communities) layered with asymmetric extraction (attention harvesting, data extraction, cognitive manipulation). Mobile exit options (could shift business models) but choose not to — extraction is profitable.
constraint_indexing:constraint_classification(critical_literacy_deficit, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ORGANIZED LITERACY ADVOCACY (TANGLED ROPE) — Provides genuine coordination function (teaches evaluation skills, builds networks for fact-checking, creates community standards). Faces extraction through insufficient funding, low public priority, and competition with well-resourced manipulation networks. Constrained by resource scarcity and institutional resistance. Experience moderate chi — genuine coordination function plus significant extraction burden.
constraint_indexing:constraint_classification(critical_literacy_deficit, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (MOUNTAIN) — Risk perspective: treats critical literacy as naturally scarce due to cognitive limits, information complexity, and attention constraints — immutable features of human cognition and information environments. FALSE SUMMIT: The structural data reveals this is not an immutable natural law but a contingent institutional arrangement shaped by funding decisions, pedagogical priorities, algorithmic design, and incentive structures.
constraint_indexing:constraint_classification(critical_literacy_deficit, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(critical_literacy_deficit_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(critical_literacy_deficit, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(critical_literacy_deficit, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(critical_literacy_deficit, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(critical_literacy_deficit, TR),
    TR >= 0.70.

:- end_tests(critical_literacy_deficit_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over interval. Initial value 0.35 reflected educational insufficiency as primary driver. Current value 0.58 reflects intensification through algorithmic amplification and attention competition. Base extraction captures asymmetric benefit flow to information merchants and attention platforms. The rise from 0.35 to 0.58 represents platform maturation — as algorithms became more sophisticated, extraction mechanisms became more potent. Not at snare level (0.66+) because coordination functions genuinely exist: education does improve reasoning; literacy communities do enable better decision-making. But extraction component is substantial and growing. Suppression (0.65): Moderate-high, stable. Barriers to acquiring critical literacy include: lack of early pedagogical foundation (students never learn to question); time scarcity (attention is expensive); cognitive load from information overload; social friction (communities that prioritize other values); institutional resistance to funding critical thinking over workforce metrics; algorithmic designs that optimize for emotional engagement over accuracy. These are structural barriers, not immutable limits — they can be reduced through institutional investment. Theater ratio (0.68): High, increasing. Traditional media claim quality gatekeeping while algorithmic systems claim neutrality — both are performative. Traditional media's reputation lags its actual capacity; platforms claim values-neutrality while optimizing for engagement metrics that amplify misinformation. The theater serves beneficiaries (maintains perceived legitimacy of institutions that profit from low literacy) while reducing coordination function (users cannot trust institutions claiming verification capacity). Rise from 0.42 to 0.68 reflects platform theater maturation — claims of community standards, fact-checking labeling, and content moderation mask algorithmic optimization for engagement.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates extreme perspectival divergence. The powerless reader trapped without skills experiences this as pure extraction (Snare) — misinformation damages their interests with no benefit. The self-educated reader who invests effort sees mixed coordination and extraction (Tangled Rope) — literacy does improve reasoning but at high cost. The well-resourced institution sees rope (Rope) — critical thinking programs enhance reputation and student demand, pure coordination. The underfunded educational system sees itself as pragmatically managing scarcity (Rope framing) but is actually identity-locked (Snare reality) — cannot perceive that reallocation toward critical literacy is possible because economic productivity framing constitutes the institution's identity. The attention merchant sees genuine coordination opportunity with profitable extraction overlay (Tangled Rope for them; Snare for users). The traditional media institution sees itself preserving valuable gatekeeping function (Piton rationalizes degraded role as institutional necessity) while losing actual verification capacity. The analytical observer risks false summit (Mountain) — treating literacy deficit as inherent human limitation rather than institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) reflects structural position relative to extraction flow. Mass readers facing skill trap without resources: d ≈ 0.95 (near-total target), f(d) ≈ 1.42, high chi. Self-educated readers with effort investment: d ≈ 0.70 (high target, but with agency), f(d) ≈ 1.05, moderate chi. Beneficiary institutions with arbitrage options: d ≈ 0.10 (beneficiary), f(d) ≈ -0.01, near-zero or negative chi. Underfunded systems with identity lock: d ≈ 0.80 (victim through institutional capture), f(d) ≈ 1.25, high chi even though institutional power level. Attention merchants: d ≈ 0.15 (beneficiary with agency to change and choose not to), f(d) ≈ -0.01, negative chi (they extract rather than bear extraction). The directionality structure reveals that chi ≠ power: an institutional actor can have high chi if structurally victimized (underfunded system) or near-zero chi if beneficiary (well-resourced institution). Power atom describes structural position; exit options describe capacity; directionality synthesis determines experienced extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates the mandatrophy by showing how coordination and extraction are simultaneously present. The claim 'critical literacy is just coordination' (pure Rope) would ignore that educational systems intentionally underinvest in skills that would reduce attention merchant profits. The claim 'critical literacy deficit is pure extraction' (pure Snare) would ignore that education genuinely does improve reasoning and that literate communities do coordinate better. The reality is Tangled Rope: the constraint solves a genuine coordination problem (how to transmit epistemic standards across populations) while simultaneously enabling asymmetric extraction (beneficiaries profit from low literacy while bearing minimal coordination cost). The mandatrophy resolution requires naming both mechanisms and the directionality flow: who benefits (attention merchants, economic models that profit from reduced political efficacy), who bears costs (trapped populations, epistemic commons), what coordination is genuine (education does work when funded), and what extraction is real (algorithmic engagement amplification, pedagogical neglect). The constraint cannot be solved by removing extraction while preserving coordination OR by removing coordination costs while preserving extraction — both are structurally necessary to the current institutional arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literacy_skill_ceiling,
    'Is the critical literacy deficit a genuine skill distribution problem or a coordination failure in access to epistemic resources?',
    'Comparison of literacy outcomes in high-resource vs low-resource educational environments with identical pedagogical intensity; analysis of whether skill gaps close with resource parity or persist due to cognitive limits',
    'If skill distribution: classify as Mountain (immutable cognitive constraint). If coordination failure: classify as Tangled Rope or Snare (institutional extraction). Current evidence suggests coordination failure — suggests reclassification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(literacy_skill_ceiling, empirical, 'Whether literacy deficit is skill distribution or resource coordination failure').

omega_variable(
    manipulation_platform_interplay,
    'What fraction of observed critical literacy deficit is caused by algorithmic amplification of misinformation vs. by genuine pedagogical insufficiency?',
    'Comparative analysis: literacy metrics in low-algorithmic-engagement environments (rural areas, older demographics with traditional media) vs high-algorithmic-engagement; intervention studies adding fact-checking overlays without changing algorithms',
    'If algorithms dominant cause: constraint is primarily Snare (platform extraction) with secondary Tangled Rope (educational coordination). If pedagogy dominant: constraint is primarily educational Tangled Rope with platform as secondary factor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manipulation_platform_interplay, empirical, 'Relative causal weight of algorithms vs pedagogy in literacy deficit').

omega_variable(
    identity_locked_institutional_capture,
    'Are educational systems genuinely unable to fund critical literacy or identity-locked by economic productivity framing that prevents perceiving it as essential?',
    'Policy case studies: jurisdictions that reallocated resources TO critical literacy AND observed outcomes; interviews with educational administrators about perceived constraints vs actual constraints; counterfactual resource analysis',
    'If genuinely constrained (resource scarcity is real): Snare classification for underfunded systems. If identity-locked: institutions could reallocate but don''t because their identity depends on productivity metrics — different causal mechanism, same outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_institutional_capture, conceptual, 'Whether educational capture is resource constraint or identity lock').

omega_variable(
    intergenerational_feedback_loop,
    'Does low critical literacy in parents reduce literacy investment in children, creating self-perpetuating extraction cycle?',
    'Longitudinal educational attainment tracking; comparative analysis of intergenerational mobility in literacy vs other skill domains; intervention effect sizes in different age cohorts',
    'If strong feedback: constraint becomes self-perpetuating (snare from trapped populations). If weak: constraint is primarily environmental/institutional. Strong feedback suggests different intervention model.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_feedback_loop, empirical, 'Strength of intergenerational literacy feedback mechanism').

omega_variable(
    attention_scarcity_authenticity,
    'Is the claimed ''attention scarcity'' that justifies low critical literacy effort a genuine constraint or a failure of will to prioritize cognition development?',
    'Time-use studies of populations by literacy level; analysis of attention allocation when incentives change; comparison with other learned skill acquisition (sports, gaming, hobbies) that competes for same attention',
    'If attention genuinely scarce: literacy deficit is unsolvable without reducing other demands. If scarcity is choice/priority: constraint is maintained by social structure, not cognitive limit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attention_scarcity_authenticity, empirical, 'Whether attention scarcity justifying low literacy is genuine or volitional').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(critical_literacy_deficit, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clit_tr_t0, critical_literacy_deficit, theater_ratio, 0, 0.42).
narrative_ontology:measurement(clit_tr_t10, critical_literacy_deficit, theater_ratio, 10, 0.55).
narrative_ontology:measurement(clit_tr_t20, critical_literacy_deficit, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(clit_be_t0, critical_literacy_deficit, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clit_be_t10, critical_literacy_deficit, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(clit_be_t20, critical_literacy_deficit, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(critical_literacy_deficit, information_standard).
narrative_ontology:affects_constraint(critical_literacy_deficit, epistemic_commons_degradation).
narrative_ontology:affects_constraint(critical_literacy_deficit, algorithmic_attention_extraction).
narrative_ontology:affects_constraint(critical_literacy_deficit, democratic_participation_asymmetry).

% DUAL FORMULATION NOTE:
% Critical literacy deficit decomposes into three linked constraints: (1) pedagogical/institutional (education system underinvestment, theater ratio rise in quality signaling), (2) algorithmic/attentional (platform design optimizing for engagement over accuracy), and (3) epistemic commons (misinformation contamination with no self-correction mechanism). Each has different ε values and different dominant agent positions. This story addresses the integrated constraint; downstream stories address pedagogical degradation and algorithmic extraction separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
