% ============================================================================
% CONSTRAINT STORY: naskh_principle__contextual_harmonization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__contextual_harmonization, []).

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
 *   constraint_id: naskh_principle__contextual_harmonization
 *   human_readable: The Contextual Harmonization Reading of the Naskh Principle
 *   domain: islamic_jurisprudence/quranic_hermeneutics
 *
 * SUMMARY:
 *   The contextual harmonization reading of the naskh (abrogation) principle
 *   represents one competing interpretation of how Islamic jurisprudence
 *   handles apparent contradictions in the Quranic text. This reading claims
 *   that all Quranic verses remain valid within their specific revelatory and
 *   situational contexts, and that apparent contradictions are resolved
 *   through contextual specification rather than through chronological
 *   supersession (classical abrogation) or progressive restriction of
 *   applicability. This constraint is one reading of the contested kernel
 *   'naskh_principle' — it coexists with and influences two sibling readings:
 *   the classical abrogation reading (which treats some verses as permanently
 *   superseded by later revelation) and the progressive restriction reading
 *   (which treats verses as having narrowed applicability over time). The
 *   contextual harmonization reading benefits institutions committed to
 *   theological coherence and adaptive jurisprudence (maqasid al-shariah
 *   frameworks, progressive reform movements), but it imposes costs on those
 *   seeking definitive legal closure and institutional authority to end
 *   interpretive dispute. The constraint exhibits tangled coordination: legal
 *   systems genuinely need ways to adapt rulings to changing contexts
 *   (coordination function), yet the perpetual recontextualization
 *   requirement extracts interpretive labor from jurists and denies them
 *   canonical finality (extraction mechanism).
 *
 * KEY AGENTS:
 *   - Theological Coherence Institutions (Maqasid Schools): Primary beneficiary (institutional/arbitrage) — contextual harmonization enables them to maintain theological unity across diverse legal contexts
 *   - Adaptive Jurisprudence Schools (Progressive Reform): Secondary beneficiary (organized/constrained) — use contextual harmonization to modernize Islamic law while maintaining doctrinal legitimacy
 *   - Community Jurists (Muftis/Qadis): Dual-position (moderate/constrained) — benefit from flexibility to adapt rulings to local circumstance, but bear burden of perpetual recontextualization
 *   - Definitive Closure Seekers (Traditionalist Schools): Primary victim (powerless/trapped) — denied ability to use chronological abrogation to definitively close questions; forced into endless contextual analysis
 *   - Legal Predictability (Abstract): Victim (powerless/trapped) — extractive burden falls on the epistemic commons; uncertainty accumulates as each case potentially generates new contextual specifications
 *   - Classical Abrogation Scholars: Institutional actor (institutional/arbitrage) — their framework is constrained (not foreclosed) by this reading; abrogation doctrine retains some authority but loses primacy
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contingent institutional choice (maintaining theological coherence) as an immutable feature of textual interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__contextual_harmonization, 0.52).
domain_priors:suppression_score(naskh_principle__contextual_harmonization, 0.48).
domain_priors:theater_ratio(naskh_principle__contextual_harmonization, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, extractiveness, 0.52).
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__contextual_harmonization, tangled_rope).
narrative_ontology:human_readable(naskh_principle__contextual_harmonization, "The Contextual Harmonization Reading of the Naskh Principle").
narrative_ontology:topic_domain(naskh_principle__contextual_harmonization, "islamic_jurisprudence/quranic_hermeneutics").

domain_priors:requires_active_enforcement(naskh_principle__contextual_harmonization).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__contextual_harmonization, 'c7701a10-5b42-43b2-9103-6fd0c9b3d1bf').
narrative_ontology:cs_kernel_codification('c7701a10-5b42-43b2-9103-6fd0c9b3d1bf', fixed_text).
narrative_ontology:cs_authority_grounding('c7701a10-5b42-43b2-9103-6fd0c9b3d1bf', lineage).
narrative_ontology:cs_interpretation_layer_present('c7701a10-5b42-43b2-9103-6fd0c9b3d1bf').
narrative_ontology:cs_reading_relation('c7701a10-5b42-43b2-9103-6fd0c9b3d1bf', naskh_principle__classical_abrogation, coexists_with).
narrative_ontology:cs_reading_relation('c7701a10-5b42-43b2-9103-6fd0c9b3d1bf', naskh_principle__progressive_restriction, influences).
narrative_ontology:cs_axiom('c7701a10-5b42-43b2-9103-6fd0c9b3d1bf', foundational, all_verses_contextually_coordinate).
narrative_ontology:cs_axiom_status(all_verses_contextually_coordinate, holdable).
narrative_ontology:cs_axiom_grounding('c7701a10-5b42-43b2-9103-6fd0c9b3d1bf', all_verses_contextually_coordinate, deontological).
narrative_ontology:cs_axiom('c7701a10-5b42-43b2-9103-6fd0c9b3d1bf', secondary, interpretive_precision_closure).
narrative_ontology:cs_axiom_status(interpretive_precision_closure, holdable).
narrative_ontology:cs_axiom_grounding('c7701a10-5b42-43b2-9103-6fd0c9b3d1bf', interpretive_precision_closure, instrumental).
narrative_ontology:cs_reference_frame('c7701a10-5b42-43b2-9103-6fd0c9b3d1bf', integrated_quranic_law).
narrative_ontology:cs_drift_state('c7701a10-5b42-43b2-9103-6fd0c9b3d1bf', contemporary_islamic_jurisprudence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c7701a10-5b42-43b2-9103-6fd0c9b3d1bf', '').
narrative_ontology:cs_kernel_id(naskh_principle__contextual_harmonization, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__contextual_harmonization, theological_coherence_seekers).
narrative_ontology:constraint_beneficiary(naskh_principle__contextual_harmonization, adaptive_jurisprudence_schools).
narrative_ontology:constraint_victim(naskh_principle__contextual_harmonization, legal_predictability).
narrative_ontology:constraint_victim(naskh_principle__contextual_harmonization, definitive_closure_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JURIST SEEKING DEFINITIVE CLOSURE (SNARE) — This reading denies the jurist the ability to definitively resolve conflicting verses through clear chronological supersession rules. The jurist remains perpetually trapped in contextual analysis without final authority to close interpretive questions. Each application requires recontextualization; no decision can be final.
constraint_indexing:constraint_classification(naskh_principle__contextual_harmonization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMMUNITY JURIST / MUFTI (TANGLED ROPE) — This reading enables judges and muftis to adapt rulings to local context and contemporary circumstance (genuine coordination benefit), but simultaneously constrains them: they must justify each contextual application through hermeneutical labor and cannot appeal to canonical closure. High interpretive burden; moderate authority. Benefits from flexibility; victim of perpetual responsibility.
constraint_indexing:constraint_classification(naskh_principle__contextual_harmonization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THEOLOGICAL SYSTEM / MAQASID FRAMEWORKS (ROPE) — Institutions built on purposive jurisprudence (maqasid al-shariah, maslaha) experience this reading as pure coordination: contextual harmonization enables the legal system to maintain coherence across diverse circumstances while protecting core purposes. The theological edifice benefits from the flexibility to harmonize apparent contradictions. Arbitrage-level exit: institutional jurisprudence can reprioritize purposes if needed.
constraint_indexing:constraint_classification(naskh_principle__contextual_harmonization, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROGRESSIVE REFORM MOVEMENT (SCAFFOLD) — Reform-oriented Islamic scholars see contextual harmonization as a temporary interpretive bridge enabling gradual legal modernization without doctrinal rupture. As Islamic law develops new contextual readings of classical verses (women's rights, governance, bioethics), this framework allows adaptation with theological legitimacy. Sunset logic: once new jurisprudential consensus emerges around contemporary contexts, the need for complex harmonization decreases. Organized but facing clerical resistance.
constraint_indexing:constraint_classification(naskh_principle__contextual_harmonization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL MADRASA SYSTEM (PITON) — Classical Islamic seminaries maintain contextual harmonization as a performative ritual: they teach students the elaborate techniques of reconciling verses through contextual specification while in practice defaulting to received doctrine from established madhabs (schools). The hermeneutical machinery is sustained through institutional inertia and pedagogical tradition, but much of its function has atrophied as formal doctrine has solidified. Theater ratio high: students learn complex techniques that rarely alter actual rulings.
constraint_indexing:constraint_classification(naskh_principle__contextual_harmonization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, contextual harmonization reflects an immutable feature of how texts with multiple interpretations must be reconciled: any large corpus of authoritative text containing prima facie contradictions requires some principle of integration. This appears as a natural law of hermeneutics. However, the structural data contradicts this: specific jurists benefit from indefiniteness (theological coherence institutions), while others bear costs (those seeking closure). The false summit reveals that this 'law of interpretation' actually naturalizes a contingent institutional choice.
constraint_indexing:constraint_classification(naskh_principle__contextual_harmonization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__contextual_harmonization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(naskh_principle__contextual_harmonization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(naskh_principle__contextual_harmonization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__contextual_harmonization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(naskh_principle__contextual_harmonization, TR),
    TR >= 0.70.

:- end_tests(naskh_principle__contextual_harmonization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The reading creates persistent interpretive burden on jurists — each contextual specification must be reasoned, justified, and defended, with no canonical closure point available. The extractiveness increases over the interval (0.35 → 0.52) reflecting that as Islamic jurisprudence develops more fine-grained contextual readings (of gender law, bioethics, governance), the burden of maintaining contextual harmonization grows. The extraction is not severe enough for Snare because institutional jurisprudence genuinely benefits from the flexibility — it is real coordination, not pure coercion. Suppression (0.48): Moderate. Substantial barriers exist to alternatives: jurists committed to Islamic law cannot simply abandon the Quranic text, and classical abrogation doctrine has legitimacy challenges in modern contexts. However, suppression is not total — alternative readings (abrogation, progressive restriction) remain live options in some schools, and reformers are developing independent contextual principles. Theater ratio (0.58): Moderate-high, increasing over interval. Traditional Islamic seminaries teach elaborate contextual harmonization techniques (usul al-fiqh), but in practice much jurisprudence defaults to received madhab doctrine. As schools commit more explicitly to adaptive jurisprudence (progressive reform movements), the theater decreases — contextual analysis becomes more functionally central. The rise in theater from 0.42 to 0.58 reflects the gap between the hermeneutical sophistication taught and the actual frequency with which contextual reinterpretation drives novel rulings.
 *
 * PERSPECTIVAL GAP:
 *   The classic perspectival collapse occurs between the institutional theological frameworks (seeing Rope — pure coordination) and the powerless justice-seeker (seeing Snare — perpetual burden with no exit). The mufti inhabits the middle, seeing both sides: the coordination benefit of contextual flexibility, the extraction cost of perpetual recontextualization. The progressive reform movement sees a temporary tool (Scaffold) — as Islamic jurisprudence develops sufficient new contextual reasoning around contemporary issues, the need for harmonization-of-apparent-contradictions work decreases, replaced by established contextual precedent. The traditional madrasa sees its own practice as degraded (Piton) — the teaching of contextual harmonization techniques persists through institutional momentum even as actual jurisprudential progress relies more on madhab authority. The analytical observer risks seeing natural law (Mountain) — 'any text with contradictions requires some harmonization principle' — but the structural beneficiary/victim pattern reveals this as a naturalization: specific institutions benefit, specific groups bear costs, and the 'natural law' framing obscures a contingent choice among three competing frameworks.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary institutions (theological coherence seekers, adaptive jurisprudence schools) derive clear extraction benefit from the flexibility this reading provides — they can maintain doctrinal coherence while adapting to new circumstances without the awkwardness of declaring abrogation. Their exit options (arbitrage for institutional, constrained for organized reformers) produce low directionality. Victims of this reading experience it as perpetual burden: jurists seeking to close questions definitively face the constraint's refusal to permit chronological supersession (trapped), while legal predictability as an abstract good cannot organize to resist (powerless). These high-d positions produce high experienced chi. Community jurists occupy the middle: they benefit from flexibility (low-d benefit) but bear the hermeneutical burden (high-d cost), making them genuinely tangled rope. The composition of these beneficiary/victim relationships, plus the institutional power distribution favoring those who can afford hermeneutical complexity, drives the tangled-rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading escapes the mandatrophy (the paradox of coordination vs extraction) by showing that the same principle (contextual harmonization) simultaneously enables institutional jurisprudence AND constrains justice-seekers. The constraint is genuinely tangled rope: maqasid frameworks benefit from a principle that keeps law adaptive (coordination), while those seeking interpretive finality bear the cost of perpetual recontextualization (extraction). There is no single type that captures both; the mixture is intrinsic. The false summit mountain perspective reveals the mandatrophy trap: claiming this is a 'natural law of interpretation' naturalizes what is actually a institutional choice that benefits some and burdens others. Resolution comes from acknowledging that interpretive principles are choice-points where beneficiaries and victims differ, not universal features of text.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    context_specification_boundaries,
    'What principles determine when contextual specification has reached adequate precision versus remaining indeterminate? When does ''context matters'' become a cover for indefiniteness?',
    'Comparative analysis of successful contextual closures across Islamic jurisprudential schools; identification of patterns in what contextual factors count as determinative; examination of cases where contextual analysis produces convergent vs divergent rulings',
    'If boundaries are determinate: contextual harmonization is a legitimate interpretive principle reducing to something rule-like (moves toward Rope). If boundaries remain indeterminate: the constraint is perpetually extractive (stays or moves toward Snare). Classification depends on whether context-specification can actually close questions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(context_specification_boundaries, empirical, 'Whether contextual specification boundaries are actually determinate').

omega_variable(
    theological_coherence_necessity,
    'Does Islamic jurisprudence structurally require all Quranic verses to be compatible in meaning, or is coherentism itself an imported framework that classical jurisprudence did not assume?',
    'Historical analysis of early Islamic jurisprudence: did early jurists assume all verses must be harmonized, or did they accept genuine abrogation (naskh) as a valid solution? Textual evidence from classical legal theory; comparison with Jewish Talmudic approaches to apparent scriptural contradictions',
    'If coherence is intrinsic: contextual harmonization is mandated by the kernel itself (strengthens Mountain classification from theological perspective). If coherence is contingent (Quranic law permits abrogation as solution): this reading is a choice among alternatives, not an inherent requirement (remains Tangled Rope from beneficiary perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theological_coherence_necessity, conceptual, 'Whether theological coherence is intrinsic to Islamic jurisprudence or an optional framework').

omega_variable(
    classical_abrogation_foreclosure,
    'Does this reading''s commitment to contextual harmonization logically foreclose the classical abrogation reading (naskh as chronological supersession), or do they coexist as alternative interpretive tools?',
    'Formal logical analysis: can a single framework (Islamic jurisprudence as a commitment system) simultaneously hold that (a) no verse is superseded, all remain contextually valid, and (b) some verses are abrogated chronologically? Or does accepting (a) require rejecting (b)? Historical analysis: have any classical or contemporary Islamic schools held both principles?',
    'If foreclosure: this reading and classical abrogation are incompatible frames; they coexist only across different schools. If coexistence: the two readings operate on different registers (contextual for interpretation, abrogation for historical development) and are genuinely complementary. Classification consequence: foreclosure strengthens the omega around whether readings are alternatives or layers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classical_abrogation_foreclosure, conceptual, 'Logical relationship between contextual harmonization and classical abrogation').

omega_variable(
    reading_kernel_specification,
    'Which aspects of the Quranic text constitute the kernel this reading harmonizes, and which aspects are interpretive additions? Is the kernel a set of substantive legal rules, or a meta-principle about how to treat apparent contradictions?',
    'Doctrinal analysis: map which specific Quranic verses and passages the contextual harmonization reading claims are mutually valid. Identify which verses are grouped as coordinate (all valid in their contexts) and which are treated as contextually superseded. Compare across Islamic schools'' actual harmonization practices.',
    'If kernel is substantive rules: the reading''s coherence depends on getting contextual specifications right (extractiveness increases with interpretive burden). If kernel is meta-principle: the reading is more abstract and less dependent on specific contextual correctness (extractiveness decreases). Classification moves along Tangled Rope ↔ Rope spectrum depending on kernel scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_specification, empirical, 'What textual and doctrinal content constitutes the kernel being harmonized').

omega_variable(
    knowledge_gap_at_reading_boundary,
    'Is this reading one of three incompatible alternatives (classical abrogation, progressive restriction, contextual harmonization), or are these three readings actually three layers of a single hermeneutical process that can coexist?',
    'Detailed examination of contemporary Islamic legal scholarship: do jurists invoke all three interpretive moves within a single fatwa or legal reasoning chain? Or do they choose one canonical framework and exclude the others? Corpus analysis of fatwa collections and legal opinions.',
    'If incompatible alternatives (mutual foreclosure): each reading constrains the structural design differently, and the engine can distinguish them by which beneficiary/victim structure each produces. If coexisting layers: the three readings may not be three constraints but three analytical frames for one constraint, requiring decomposition into separate stories (ε-invariance principle).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(knowledge_gap_at_reading_boundary, conceptual, 'Whether the three naskh readings are incompatible alternatives or compatible layers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__contextual_harmonization, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naskh_ctx_tr_t0, naskh_principle__contextual_harmonization, theater_ratio, 0, 0.42).
narrative_ontology:measurement(naskh_ctx_tr_t4, naskh_principle__contextual_harmonization, theater_ratio, 4, 0.55).
narrative_ontology:measurement(naskh_ctx_tr_t8, naskh_principle__contextual_harmonization, theater_ratio, 8, 0.58).

% Extraction over time
narrative_ontology:measurement(naskh_ctx_be_t0, naskh_principle__contextual_harmonization, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(naskh_ctx_be_t4, naskh_principle__contextual_harmonization, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(naskh_ctx_be_t8, naskh_principle__contextual_harmonization, base_extractiveness, 8, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(naskh_ctx_su_t0, naskh_principle__contextual_harmonization, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(naskh_ctx_su_t4, naskh_principle__contextual_harmonization, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(naskh_ctx_su_t8, naskh_principle__contextual_harmonization, suppression_requirement, 8, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__contextual_harmonization, identity_coordination).
narrative_ontology:affects_constraint(naskh_principle__contextual_harmonization, naskh_principle__classical_abrogation).
narrative_ontology:affects_constraint(naskh_principle__contextual_harmonization, naskh_principle__progressive_restriction).
narrative_ontology:affects_constraint(naskh_principle__contextual_harmonization, quranic_legal_uniformity_presupposition).

% DUAL FORMULATION NOTE:
% The naskh principle contains three structurally distinct constraint readings with different ε values and beneficiary/victim structures. The contextual harmonization reading (this file) treats all verses as valid in their contexts (moderate extractiveness, tangled rope structure). Classical abrogation treats later verses as superseding earlier ones (lower extractiveness in some perspectives, different institutional beneficiaries). Progressive restriction treats verses as having progressively narrowed scope (different ε profile). These are not three perspectives on one constraint but three distinct constraints linked through the contested kernel 'naskh_principle'. Each should be generated as a separate story. This story is the contextual harmonization reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(naskh_principle__contextual_harmonization, institutional, 0.22).
constraint_indexing:directionality_override(naskh_principle__contextual_harmonization, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
