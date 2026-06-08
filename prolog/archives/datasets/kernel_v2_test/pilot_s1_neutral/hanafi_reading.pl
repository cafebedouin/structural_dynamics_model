% ============================================================================
% CONSTRAINT STORY: hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   constraint_id: hanafi_reading
 *   human_readable: Hanafi Jurisprudential Method: Qiyas and Istihsan as Valid Sources
 *   domain: islamic_jurisprudence/legal_theory/usul_al_fiqh
 *
 * SUMMARY:
 *   The Hanafi reading of Islamic jurisprudential method legitimizes qiyas
 *   (analogical reasoning) and istihsan (juristic preference) as valid
 *   sources of law alongside the Qur'an and Sunnah. This constraint
 *   represents one reading of the contested usul al-fiqh (jurisprudential
 *   foundations) kernel — a stabilized commitment about what sources are
 *   legitimate in deriving Islamic law. The Hanafi reading is a rationalist
 *   methodological choice that benefits the rationalist jurist class and the
 *   Hanafi institutional authority by expanding their juristic discretion and
 *   adaptability, while imposing constraints on strict textualist jurists
 *   whose interpretive framework is implicitly delegitimized. The
 *   extractiveness is moderate (0.35) because the constraint involves genuine
 *   coordination of textual authority with practical judgment, not pure
 *   extraction — judges need a framework for deciding novel cases. The
 *   suppression (0.28) reflects the institutional cost textualists bear in
 *   defending their position against the rationalist mainstream, but
 *   textualists retain institutional footholds in some contexts. Theater
 *   ratio (0.15) is low, indicating that the constraint's operation is
 *   substantially functional (qiyas and istihsan do real juristic work)
 *   rather than performative. Measurements show extractiveness and
 *   suppression rising slightly over the interval as Hanafi dominance
 *   solidifies institutionally, with theater remaining stable (the core
 *   function persists even as institutional power concentrates).
 *
 * KEY AGENTS:
 *   - Rationalist Jurists (faqih mujaddids): Organized beneficiaries (organized/constrained) — gain methodological authority and juristic discretion through qiyas/istihsan legitimation
 *   - Hanafi Institutional Authority (madhhab leadership, teaching networks): Institutional beneficiary (institutional/arbitrage) — benefits from institutional legitimacy and jurisdictional scope expansion
 *   - Strict Textualist Jurists (akhbaris, literal-text adherents): Primary victims (powerless/identity_locked) — identity constituted through textual literalism; suppressed without structural exit path
 *   - Empirically-Minded Judges (qadis, practical adjudicators): Secondary beneficiary (moderate/constrained) — benefit from methodological flexibility but constrained by formal doctrine
 *   - Hanbali/Literalist Schools: Secondary victims (organized/constrained) — attempt to maintain text-only positions but face institutional pressure from Hanafi dominance
 *   - Medieval Islamic Legal Ecosystem: Analytical observer (analytical/analytical) — cross-madhhab learning infrastructure that absorbs methodological diversity but is structured by Hanafi institutional dominance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hanafi_reading, 0.35).
domain_priors:suppression_score(hanafi_reading, 0.28).
domain_priors:theater_ratio(hanafi_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hanafi_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(hanafi_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(hanafi_reading, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hanafi_reading, tangled_rope).
narrative_ontology:human_readable(hanafi_reading, "Hanafi Jurisprudential Method: Qiyas and Istihsan as Valid Sources").
narrative_ontology:topic_domain(hanafi_reading, "islamic_jurisprudence/legal_theory/usul_al_fiqh").

domain_priors:requires_active_enforcement(hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hanafi_reading, 'e23995dc-3c9a-49a6-b15d-eab4c7faaf7b').
narrative_ontology:cs_kernel_codification('e23995dc-3c9a-49a6-b15d-eab4c7faaf7b', formalized).
narrative_ontology:cs_authority_grounding('e23995dc-3c9a-49a6-b15d-eab4c7faaf7b', lineage).
narrative_ontology:cs_interpretation_layer_present('e23995dc-3c9a-49a6-b15d-eab4c7faaf7b').
narrative_ontology:cs_reading_relation('e23995dc-3c9a-49a6-b15d-eab4c7faaf7b', hanafi_reading__hanbali_reading, influences).
narrative_ontology:cs_reading_relation('e23995dc-3c9a-49a6-b15d-eab4c7faaf7b', hanafi_reading__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('e23995dc-3c9a-49a6-b15d-eab4c7faaf7b', hanafi_reading__maliki_reading, coexists_with).
narrative_ontology:cs_axiom('e23995dc-3c9a-49a6-b15d-eab4c7faaf7b', foundational, qiyas_validity_foundational).
narrative_ontology:cs_axiom_status(qiyas_validity_foundational, holdable).
narrative_ontology:cs_axiom_grounding('e23995dc-3c9a-49a6-b15d-eab4c7faaf7b', qiyas_validity_foundational, deontological).
narrative_ontology:cs_axiom('e23995dc-3c9a-49a6-b15d-eab4c7faaf7b', foundational, istihsan_permissible_juristic_preference).
narrative_ontology:cs_axiom_status(istihsan_permissible_juristic_preference, holdable).
narrative_ontology:cs_axiom_grounding('e23995dc-3c9a-49a6-b15d-eab4c7faaf7b', istihsan_permissible_juristic_preference, deontological).
narrative_ontology:cs_reference_frame('e23995dc-3c9a-49a6-b15d-eab4c7faaf7b', quranic_sunnaic_sufficiency_with_analogical_extension).
narrative_ontology:cs_drift_state('e23995dc-3c9a-49a6-b15d-eab4c7faaf7b', medieval_juristic_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e23995dc-3c9a-49a6-b15d-eab4c7faaf7b', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(hanafi_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hanafi_reading, rationalist_jurists).
narrative_ontology:constraint_beneficiary(hanafi_reading, hanafi_school_institutional_authority).
narrative_ontology:constraint_victim(hanafi_reading, strict_textualist_jurists).
narrative_ontology:constraint_victim(hanafi_reading, literal_textual_closure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STRICT TEXTUALIST (SNARE) — Structurally mobile (could adopt rationalist methods) but identity-locked through centuries of textual-literalist tradition and professional identity constituted through strict adherence to apparent text (zahir). Experiences the Hanafi expansion of source materials as an erosion of textual closure and a loss of authoritative ground. Cannot exit without abandoning juristic identity. Extraction is severe because the methodological expansion delegitimizes their entire interpretive framework without offering structural compensation.
constraint_indexing:constraint_classification(hanafi_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: EMPIRICALLY-MINDED JUDGE (ROPE) — Faces concrete cases requiring judgment beyond textual letter; constrained by formal jurisprudential doctrine but benefits from the Hanafi permission to deploy qiyas and istihsan in resolving novel disputes. Experiences the constraint as genuine coordination of text-based authority with circumstantial judgment. Moderate power and constrained exit (cannot overtly ignore doctrine) but real coordination function and real benefit.
constraint_indexing:constraint_classification(hanafi_reading, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RATIONALIST JURISTIC SCHOOL (TANGLED ROPE) — Organized institutional actors (Hanafi madhhab leadership, teaching scholars) benefit from the legitimation of qiyas and istihsan, which expands their juristic authority and adaptability. But they also bear the cost of enforcement: constantly defending the validity of analogy and juristic preference against strict-text challengers, managing internal coherence of expanded method, bearing epistemic risk that rationalist reasoning might diverge from textual intent. Genuine coordination (need to judge real cases) AND asymmetric benefit (methodological legitimacy accumulates to rationalist jurists). Requires active enforcement of methodological boundaries.
constraint_indexing:constraint_classification(hanafi_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: MEDIEVAL INSTITUTIONAL HANAFI AUTHORITY (PITON) — From the vantage of institutional authority wielding canonical texts (Quduri, Marghinani), the method is substantially performative: the authority claims rationalist latitude (qiyas, istihsan) for its own juristic class while restricting it for subordinate judges, maintaining theatrical coherence between textual authority and juristic flexibility. Theater ratio high: the method is defended with textual-proof rhetoric even when the rationale for deployment is circumstantial. The institutional inertia of the Hanafi madhhab persists partly through this performance — it survived alternative methodologies by claiming both textual fidelity and practical adaptability.
constraint_indexing:constraint_classification(hanafi_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: COMPARATIVE LEGAL SCHOLAR (SCAFFOLD) — Sees Hanafi qiyas/istihsan as a temporary institutional solution to the problem of bridging textual authority and practical judgment — useful in pre-modern contexts but transitional toward codified law and formal reasoning. Views the methodological latitude as scaffold: it solves the coordination problem until explicit legislation and written codes take over. Mobile exit (can move to secular comparative jurisprudence). Sunset implicit: as written law matures, the rationalist discretion encoded in qiyas/istihsan becomes unnecessary. Low experienced extraction because the scholar perceives the method as structurally temporary.
constraint_indexing:constraint_classification(hanafi_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: EPISTEMOLOGICAL ANALYST / NATURAL LAW VIEW (MOUNTAIN) — From a universalized epistemological stance, the recognition of analogy (qiyas) and juristic preference (istihsan) as valid interpretive methods is an immutable feature of ANY sophisticated legal reasoning: you cannot apply text to novel cases without either analogy or discretionary judgment. The constraint appears as a discovery of natural law in reasoning itself, not a contingent methodological choice. However, this perspective risks naturalizing what is actually a contestable institutional commitment — the engine's false summit detector will flag this if structural data reveals that rejecting qiyas/istihsan is coherently sustainable within Islamic jurisprudence.
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

test(piton_threshold) :-
    domain_priors:theater_ratio(hanafi_reading, TR),
    TR >= 0.70.

:- end_tests(hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The rationalist jurist class gains real authority and discretion from qiyas legitimation, but the extraction is limited because (a) the constraint solves a genuine coordination problem (novel cases require judgment beyond text), (b) the benefit is not pure extraction but authorization to deploy reasoning already in implicit use, and (c) non-rationalist approaches retain institutional footholds. The value increased slightly over the interval (0.20 → 0.35) as Hanafi institutional dominance solidified and qiyas became the default framework, concentrating benefit to the rationalist faction. Suppression (0.28): Moderate. Strict textualists face institutional barriers (textual authorities privilege rationalist readings, teaching institutions emphasize qiyas-validity, judges expect rationalist reasoning) but retain agency — they can and do defend literalist positions, and some contexts (hadith scholarship, certain fatwa traditions) remain partially textualist. The upward trajectory reflects increasing institutional pressure as Hanafi methods became canonical. Theater ratio (0.15): Low. The constraint is substantially functional — qiyas and istihsan do real juristic work resolving cases, enabling judges to navigate novel disputes without pure textual literalism. The theatrical element is present (defending the method with proof-texts, presenting juristic preference as textual derivation) but secondary to function. Theater remained stable across the interval, indicating that the functional core persisted even as institutional power dynamics shifted.
 *
 * PERSPECTIVAL GAP:
 *   The Hanafi reading produces a strong perspectival gap across indexical positions. The rationalist jurist (moderate/constrained, biographical, national scope) sees the constraint as rope: it solves the coordination problem of extending text to novel cases while preserving judicial authority. The strict textualist (powerless/identity_locked, biographical, regional scope) sees the constraint as snare: it eliminates their interpretive framework without structural exit. The Hanafi institutional authority (institutional/arbitrage, immediate, continental scope) sees tangled rope: they benefit from methodological legitimacy but must defend against challenge and manage internal coherence. The empirically-minded judge (moderate/constrained, generational, regional scope) sees rope: the framework solves practical judgment needs. The comparative legal scholar (organized/mobile, generational, continental scope) sees scaffold: the method is transitory, useful until written law matures. The epistemological analyst (analytical/analytical, civilizational, universal scope) risks seeing mountain: the recognition of analogy in legal reasoning appears as a discovery of reasoning's natural structure. This full spectrum from snare (powerless victim, identity-locked) to rope (moderate beneficiary, constrained) to mountain (analytical, universal) demonstrates how the same structural constraint manifests differently depending on agent position. The gap is maximized between textualist and rationalist perspectives — their classifications are nearly opposite, revealing the reading as fundamentally contested.
 *
 * DIRECTIONALITY LOGIC:
 *   The Hanafi reading's directionality (d) derives from the structural relationship of each agent to the constraint. For beneficiaries (rationalist jurists, Hanafi authority), the constraint flows benefit without extraction cost — they gain methodological legitimacy and discretion. The engine computes low d (beneficiary → d ~0.1–0.3) and negative effective extraction chi (benefit). For victims (strict textualists), the constraint extracts by delegitimizing their interpretive framework — they bear institutional cost without compensatory benefit. The engine computes high d (victim + identity_locked exit → d ~0.7–0.9) and high chi (extraction). For secondary agents (judges, Hanbali schools), d is intermediate — they benefit from methodological flexibility but bear enforcement costs. The rationalist jurists' arbitrage-level exit capacity is high (they can adopt pure textualism if preferred, but lack incentive to do so) → low d. Textualists' identity-locked exit capacity is near-zero (exit requires abandoning juristic identity) → high d. This differential drives the perspectival gap: beneficiaries see rope (low chi), victims see snare (high chi), the organized Hanafi network sees tangled rope (balanced benefit and enforcement cost). Note: The analytical observer's mountain perspective risks naturalizing a contingent methodological choice as immutable law — the false summit detector will flag this if empirical data shows that pure-text jurisprudence is logically sustainable (contradicting the mountain claim).
 *
 * MANDATROPHY ANALYSIS:
 *   The Hanafi reading involves a mandatrophy dynamic: the original mandate (derive law from established sources) persists, but the means of fulfilling it has expanded (adding qiyas and istihsan to Qur'an/Sunnah), and the institutional structures that govern method application have ossified around the expanded interpretation. UNRESOLVED: Whether the constraint's mandate (justifying legal derivation and judicial reasoning) has been successfully adapted to novel cases (justified expansion of sources) or whether the constraint represents institutional expansion that has outgrown its original justification (mandate degradation). The measurement trajectory suggests mandate persistence — extractiveness rises but stays moderate (0.35), and theater remains low (0.15), indicating the constraint retains substantive function rather than becoming purely performative. However, the identity-locking of textualists suggests mandate drift: if the original mandate could be satisfied without qiyas/istihsan (pure-text jurisprudence), then the expansion is not mandated but represents institutional power consolidation. The omega on whether text-only law is logically sustainable will clarify this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rationalism_vs_textualism_foreclosure,
    'Does the Hanafi acceptance of qiyas and istihsan logically foreclose the strict textualist position, or do these coexist as live methodological options?',
    'Examine whether a Hanafi jurist can coherently hold both: (1) qiyas is a valid jurisprudential source, and (2) a specific case must be decided by text alone. If (2) is possible within Hanafi doctrine (via declaring qiyas inapplicable to this case), the readings coexist. If (2) contradicts foundational Hanafi methodology, they foreclose.',
    'If foreclosure: the relationship is binary (one reading eliminates the other in unified framework). If coexistence: both readings persist as live options held by different parties (typical of Islamic jurisprudence pluralism). Classification of reading_relations changes from forecloses to coexists_with.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rationalism_vs_textualism_foreclosure, conceptual, 'Whether Hanafi rationalism forecloses strict textualism or permits coexistence').

omega_variable(
    beneficiary_identification_stability,
    'Are rationalist jurists and the Hanafi institutional authority genuinely the primary beneficiaries, or does the beneficiary set shift with time horizon and scope?',
    'Track who collects from qiyas/istihsan legitimation across scales: (a) individual rationalist jurist — career advancement and methodological authority; (b) Hanafi madhhab — institutional legitimacy and jurisdictional scope; (c) broader Islamic jurisprudential ecosystem — standardization and cross-madhhab learning. Do all three benefit, or do benefits concentrate at one scale?',
    'If beneficiaries shift across scope: the directionality derivation must account for scale-dependent beneficiary identity, potentially requiring directionality overrides. If stable: the story''s beneficiary declaration holds across perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification_stability, empirical, 'Stability of beneficiary identity across time and scope scales').

omega_variable(
    natural_law_vs_contingent_method,
    'Is the acceptance of qiyas and istihsan a discovery of natural law (immutable feature of legal reasoning), or a contingent institutional commitment (one defensible choice among alternatives)?',
    'Test whether strict-text-only jurisprudence is logically sustainable: Can a coherent legal system function entirely on text without analogy or discretion? If yes (logical sustainability), the Hanafi method is contingent. If no (analogy is unavoidable), the method is closer to natural law. Cross-check against Hanbali attempts at text-only reasoning — did they succeed or require hidden qiyas?',
    'If natural law: the mountain perspective is legitimate, and the constraint emerges naturally from reasoning structure. If contingent: the mountain perspective is a false summit, and the constraint is a political/institutional choice that benefits the rationalist faction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_contingent_method, conceptual, 'Whether qiyas/istihsan is natural law or contingent institutional choice').

omega_variable(
    suppression_mechanism_identity_vs_structural,
    'Is the suppression of strict textualism primarily structural (institutional barriers to adoption) or internalized (identity-locked jurists cannot see alternative methods)?',
    'Historical analysis: When a strict textualist is offered institutional support and resources to maintain their position despite Hanafi dominance, do they persist or shift? If they persist despite structural opportunity, suppression is partly internalized (identity lock). If they shift when barriers lower, suppression is primarily structural.',
    'If internalized: the identity_locked exit option is appropriate for textualist victims, meaning their suppression is cognitive. If structural: the trapped or constrained exit option is appropriate, meaning barriers are material. The story''s exit-option declarations depend on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_identity_vs_structural, empirical, 'Whether suppression of textualism is structural or identity-locked').

omega_variable(
    qiyas_scope_clarity_boundary,
    'What is the boundary of valid qiyas application in Hanafi doctrine? Is the boundary clearly specified or interpretively fluid?',
    'Review foundational Hanafi texts (Usul al-Fiqh treatises, major fatwa collections) to determine: Are there explicit conditions limiting qiyas to specific case types? Or is the validity of qiyas itself open to juristic reasoning? If boundaries are clear and enforceable, the method has definite scope. If boundaries are subject to juristic preference (ijtihad), the method is more fluid.',
    'If boundaries are fluid: extractiveness may be higher (rationalist jurists have discretion to expand qiyas as needed). If boundaries are clear: extractiveness is lower (method is standardized). Theater_ratio implications: fluid boundaries require more rhetorical defense.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(qiyas_scope_clarity_boundary, empirical, 'Clarity and enforceability of qiyas scope boundaries in Hanafi doctrine').

omega_variable(
    kernel_reading_interpretation_ambiguity,
    'Is the Hanafi reading genuinely distinct from the Maliki reading, or does it differ only in degree (more qiyas, more istihsan) rather than kind (different ground-source categories)?',
    'Compare the foundational axioms: Hanafi qiyas as valid source. Maliki maslahah mursalah as valid source. Shafii qiyas with strict conditions. Hanbali text-primacy with limited qiyas. Do these represent different SOURCE CATEGORIES or different SCOPES of the same category? If different categories, readings foreclose. If different scopes, readings influence.',
    'If different categories: reading_relations are forecloses (stronger difference). If different scopes: reading_relations are influences (weaker, allows coexistence). Affects the cs_structure.reading_relations declarations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_interpretation_ambiguity, conceptual, 'Whether Hanafi reading differs from siblings in kind or degree').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hanafi_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hanafi_theater_t0, hanafi_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(hanafi_theater_t3, hanafi_reading, theater_ratio, 3, 0.14).
narrative_ontology:measurement(hanafi_theater_t6, hanafi_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(hanafi_theater_t10, hanafi_reading, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(hanafi_extractiveness_t0, hanafi_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(hanafi_extractiveness_t3, hanafi_reading, base_extractiveness, 3, 0.28).
narrative_ontology:measurement(hanafi_extractiveness_t6, hanafi_reading, base_extractiveness, 6, 0.35).
narrative_ontology:measurement(hanafi_extractiveness_t10, hanafi_reading, base_extractiveness, 10, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(hanafi_suppression_t0, hanafi_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(hanafi_suppression_t3, hanafi_reading, suppression_requirement, 3, 0.24).
narrative_ontology:measurement(hanafi_suppression_t6, hanafi_reading, suppression_requirement, 6, 0.28).
narrative_ontology:measurement(hanafi_suppression_t10, hanafi_reading, suppression_requirement, 10, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hanafi_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hanafi_reading, 0.12).
narrative_ontology:affects_constraint(hanafi_reading, maliki_reading).
narrative_ontology:affects_constraint(hanafi_reading, shafii_reading).
narrative_ontology:affects_constraint(hanafi_reading, hanbali_reading).
narrative_ontology:affects_constraint(hanafi_reading, istihsan_institutional_authority).
narrative_ontology:affects_constraint(hanafi_reading, qiyas_scope_limitation).

% DUAL FORMULATION NOTE:
% The Hanafi reading is part of a constraint family decomposing the usul_al_fiqh_method kernel into structurally distinct stories. Each reading (Hanafi, Maliki, Shafii, Hanbali) represents a different ε value for the same underlying kernel. The ε-invariance principle applies: the extractiveness of qiyas/istihsan legitimation differs depending on which reading's methodological scope is at stake. Hanafi qiyas (broad scope, high legitimacy) has ε ~0.35. Hanbali qiyas (narrow scope, low legitimacy, mostly text) has ε ~0.10. These are different constraints sharing a kernel. The Hanafi reading influences but does not foreclose the Hanbali reading — they coexist institutionally. This file models the Hanafi reading as a tangled-rope constraint; sibling readings will model their own claimed types (Hanbali likely mountain or rope, Maliki likely tangled-rope).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hanafi_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
