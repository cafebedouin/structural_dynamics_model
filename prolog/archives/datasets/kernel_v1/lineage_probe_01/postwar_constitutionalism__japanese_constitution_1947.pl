% ============================================================================
% CONSTRAINT STORY: postwar_constitutionalism__japanese_constitution_1947
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_postwar_constitutionalism__japanese_constitution_1947, []).

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
 *   constraint_id: postwar_constitutionalism__japanese_constitution_1947
 *   human_readable: The 1947 Japanese Constitution: Imposed Postwar Constitutionalism and Domestication
 *   domain: political/legal
 *
 * SUMMARY:
 *   The 1947 Japanese Constitution represents a singular case in postwar
 *   constitutionalism: a victor-drafted charter imposed during military
 *   occupation that the polity nonetheless made its own through sustained
 *   practice, generational internalization, and constitutional culture. The
 *   constraint instantiates the domestication paradox: how an externally
 *   imposed text becomes constitutionally authentic not through formal
 *   amendment but through consistent use, judicial validation, political
 *   entrenchment, and citizen attachment. Article 1 strips the Emperor of
 *   sovereignty while preserving the institution; Article 9 renounces war and
 *   military force in perpetuity. These core provisions suppress the prewar
 *   imperial-sovereignty doctrine and constrain Japan's capacity to redefine
 *   itself along nationalist-traditional lines. Yet after 75 years, the
 *   constitution retains legitimacy across most of the political spectrum,
 *   and attempts at revision have failed despite sustained conservative
 *   effort. The constraint exhibits Tangled Rope dynamics: genuine democratic
 *   governance coordination (beneficiary: postwar citizens, occupying power
 *   settlement) combined with structural suppression of alternative
 *   sovereignty frameworks (victims: displaced imperial institution,
 *   constitutional revisionists). The theater ratio has declined from 0.62
 *   (occupation-era compulsion) to 0.38 (internalized culture), suggesting
 *   domestication. Extractiveness remains moderate (0.28) because suppression
 *   has softened from external imposition to self-maintaining constitutional
 *   culture. This reading instantiates one approach to postwar
 *   constitutionalism: the imposed-but-domesticated charter, distinct from
 *   the German militant-democracy approach (entrenched human dignity against
 *   totalitarianism) and the Indian exhaustive-text approach (social
 *   revolution via constitutional design). The kernel contest is whether
 *   these three readings coexist as alternatives or converge toward a single
 *   postwar constitutionalism logic.
 *
 * KEY AGENTS:
 *   - General Douglas MacArthur / Occupation Authority (1945-1952): Primary architect of imposed constitutionalism; enforcer of Article 9 pacifism and sovereignty suppression. Institutional power, arbitrage exit (could withdraw and let Japan revert). Perspective: Rope (achieves regional settlement coordination).
 *   - The Japanese Polity (Citizens, Politicians, Courts): Receiver and domesticator of the imposed text. Moderate power initially, constrained exit (cannot unilaterally amend). Gradually internalized Article 9 and democratic governance as constitutional culture. Perspective: Tangled Rope (coordinates democracy while suppressed on sovereignty).
 *   - Displaced Imperial Institution (Emperor, Imperial Ideology, Nationalist Intellectuals): Victim of constitutional suppression. Article 1 (Emperor as symbol) and Article 9 (renunciation of war) foreclose the prewar sovereignty theology. Powerless to reverse without supermajority consensus. Perspective: Snare (structurally trapped; no exit).
 *   - Constitutional Revisionists (Conservative LDP, Nationalist Coalition, Legal Scholars): Organized opposition to Article 9 and imperial suppression. Institutional power, constrained exit (can argue for revision but face supermajority gate). 75-year failure to amend reveals structural barrier. Perspective: Tangled Rope (benefit from political freedom to organize; extracted by amendment difficulty).
 *   - Progressive Coalition (Left Parties, Peace Movement, Court-Protective Scholars): Beneficiaries of Article 9 entrenchment and supermajority gate. Institutional power, constrained exit (depend on maintaining consensus; cannot unilaterally strengthen Article 9). Perspective: Tangled Rope (benefit from pacifism lock; constrained by democratic dependence).
 *   - US-Japan Alliance (Post-1952 Security Treaty): Stabilizes the constitutional order through security guarantee. US achieves containment of Soviet/Chinese expansion; Japan achieves security without remilitarization. Institutional power, arbitrage (can withdraw). Perspective: Rope (coordination of regional order).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(postwar_constitutionalism__japanese_constitution_1947, 0.28).
domain_priors:suppression_score(postwar_constitutionalism__japanese_constitution_1947, 0.45).
domain_priors:theater_ratio(postwar_constitutionalism__japanese_constitution_1947, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(postwar_constitutionalism__japanese_constitution_1947, extractiveness, 0.28).
narrative_ontology:constraint_metric(postwar_constitutionalism__japanese_constitution_1947, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(postwar_constitutionalism__japanese_constitution_1947, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(postwar_constitutionalism__japanese_constitution_1947, tangled_rope).
narrative_ontology:human_readable(postwar_constitutionalism__japanese_constitution_1947, "The 1947 Japanese Constitution: Imposed Postwar Constitutionalism and Domestication").
narrative_ontology:topic_domain(postwar_constitutionalism__japanese_constitution_1947, "political/legal").

domain_priors:requires_active_enforcement(postwar_constitutionalism__japanese_constitution_1947).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(postwar_constitutionalism__japanese_constitution_1947, 'f3b6c6f9-3da8-4e95-9e4c-d8de9ea2ebc4').
narrative_ontology:cs_kernel_codification('f3b6c6f9-3da8-4e95-9e4c-d8de9ea2ebc4', formalized).
narrative_ontology:cs_authority_grounding('f3b6c6f9-3da8-4e95-9e4c-d8de9ea2ebc4', extraction).
narrative_ontology:cs_interpretation_layer_present('f3b6c6f9-3da8-4e95-9e4c-d8de9ea2ebc4').
narrative_ontology:cs_reading_relation('f3b6c6f9-3da8-4e95-9e4c-d8de9ea2ebc4', postwar_constitutionalism__german_basic_law, coexists_with).
narrative_ontology:cs_reading_relation('f3b6c6f9-3da8-4e95-9e4c-d8de9ea2ebc4', postwar_constitutionalism__indian_constitution_1950, coexists_with).
narrative_ontology:cs_axiom('f3b6c6f9-3da8-4e95-9e4c-d8de9ea2ebc4', foundational, constitutional_legitimacy_through_practice).
narrative_ontology:cs_axiom_status(constitutional_legitimacy_through_practice, holdable).
narrative_ontology:cs_axiom_grounding('f3b6c6f9-3da8-4e95-9e4c-d8de9ea2ebc4', constitutional_legitimacy_through_practice, conventional).
narrative_ontology:cs_axiom('f3b6c6f9-3da8-4e95-9e4c-d8de9ea2ebc4', foundational, pacifism_as_postwar_identity).
narrative_ontology:cs_axiom_status(pacifism_as_postwar_identity, holdable).
narrative_ontology:cs_axiom_grounding('f3b6c6f9-3da8-4e95-9e4c-d8de9ea2ebc4', pacifism_as_postwar_identity, deontological).
narrative_ontology:cs_reference_frame('f3b6c6f9-3da8-4e95-9e4c-d8de9ea2ebc4', occupation_imposed_charter).
narrative_ontology:cs_drift_state('f3b6c6f9-3da8-4e95-9e4c-d8de9ea2ebc4', contemporary_constitutional_culture, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('f3b6c6f9-3da8-4e95-9e4c-d8de9ea2ebc4', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(postwar_constitutionalism__japanese_constitution_1947, postwar_constitutionalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(postwar_constitutionalism__japanese_constitution_1947, postwar_citizen_democratic_rights).
narrative_ontology:constraint_beneficiary(postwar_constitutionalism__japanese_constitution_1947, occupying_power_regional_settlement).
narrative_ontology:constraint_victim(postwar_constitutionalism__japanese_constitution_1947, displaced_imperial_sovereignty_doctrine).
narrative_ontology:constraint_victim(postwar_constitutionalism__japanese_constitution_1947, constitutional_revisionists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED IMPERIAL INSTITUTION (SNARE) — The Emperor's sovereignty doctrine and the prewar imperial-national mythology are structurally foreclosed by Article 1 (Emperor as symbol of state) and Article 9 (renunciation of war). Imperial revisionists face maximum suppression: constitutional amendment requires supermajority consensus that has not materialized in 75 years. The institution is trapped within a text designed to prevent its restoration. Zero exit options; bears full cost of constitutional suppression.
constraint_indexing:constraint_classification(postwar_constitutionalism__japanese_constitution_1947, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: OCCUPYING POWER SETTLEMENT (ROPE) — The United States achieves regional stability through constitutional entrenchment of pacifism and demilitarization. Article 9 binds Japan's future military commitments; the constitution's permanence (via supermajority amendment threshold) locks in the postwar regional order. The occupying power experiences this as pure coordination: Japan cannot rearm or revisit sovereignty theology without destabilizing the settlement. Net beneficiary with high arbitrage (can withdraw occupation, can ratify the settlement formally); extraction flows toward this agent in the form of structural stability.
constraint_indexing:constraint_classification(postwar_constitutionalism__japanese_constitution_1947, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: POSTWAR JAPANESE CITIZEN (TANGLED ROPE) — The constitution grants genuine democratic rights (Articles 3–40) and popular sovereignty (Article 1: Emperor derives legitimacy from the people). But it imposes pacifism and constrains sovereignty theology through a text many Japanese experience as externally imposed. Citizens benefit from the democratic framework but constrained by Article 9's renunciation — they cannot amend it without consensus they do not possess. The constraint coordinates democratic governance while extracting the capacity to redefine national identity along traditional lines. Moderate power; constrained exit (could theoretically organize for supermajority amendment, but 75 years of failed efforts shows the structural barrier is real).
constraint_indexing:constraint_classification(postwar_constitutionalism__japanese_constitution_1947, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL REVISIONISTS (TANGLED ROPE) — Conservative parties and nationalist intellectuals seek to restore imperial sovereignty doctrine and rearm Japan. They benefit from constitutional procedure (free speech to argue for revision, electoral platform for amendment campaigns) but are structurally suppressed by the supermajority gate. The revision movement coordinates legitimate political contestation while being extracted from by a gate designed to make their project nearly impossible without transforming Japanese politics entirely. Institutional power; constrained exit (the political process remains open, but the mathematical barrier to amendment is insurmountable under current consensus).
constraint_indexing:constraint_classification(postwar_constitutionalism__japanese_constitution_1947, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PROGRESSIVE COALITION (TANGLED ROPE) — The left-wing political parties, constitutional scholars, and pacifist movements benefit from Article 9's entrenchment as a coordination mechanism for preventing remilitarization. But they are also constrained by the supermajority gate — if public opinion shifted decisively, they could not block amendment indefinitely. The constraint both serves their interests (pacifism is locked in) and extracts from them (they have no absolute veto; constitutional permanence depends on sustained political consensus). The supermajority gate is a tangled mechanism: it protects pacifism from simple-majority reversal but also prevents the left from unilaterally strengthening Article 9 without broader consensus.
constraint_indexing:constraint_classification(postwar_constitutionalism__japanese_constitution_1947, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / DOMESTICATION THESIS (ROPE) — From a generational/global perspective, the Japanese Constitution exemplifies the domestication of imposed constitutionalism: an externally drafted text that the polity internalized through 75 years of consistent practice, court validation, political entrenchment, and citizen attachment. The 'imposition' becomes 'making one's own' through sustained use. The constraint functions primarily as coordination (democratic governance, pacifism as national identity) with low extraction (beneficiaries are distributed across citizen and state interests). The analytical observer sees the paradox resolved: Japan accepted the constitution not because coerced, but because the constitution proved functional and became constitutional culture.
constraint_indexing:constraint_classification(postwar_constitutionalism__japanese_constitution_1947, rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(postwar_constitutionalism__japanese_constitution_1947_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(postwar_constitutionalism__japanese_constitution_1947, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(postwar_constitutionalism__japanese_constitution_1947, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(postwar_constitutionalism__japanese_constitution_1947_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate, declining over the interval from 0.15 to 0.28 (measurement direction is historical, t=0 is 1947 occupation, t=30 is late Cold War, 1977). The low initial value reflects that immediate imposition involves coercion but not sustained extraction — the occupation was extractive in imposing terms, but the constitution itself transfers wealth/power minimally (it redistributes sovereignty, not resources). The slight rise from 0.15 to 0.28 reflects the accretion of extraction mechanisms as the constraint become self-maintaining: revision attempts cost political capital; suppression of alternative sovereignty frameworks requires sustained argument against nationalist sentiment; the compensation mechanism (US security guarantee for constrained autonomy) becomes subtly extractive as Japan assumes client-state posture. Suppression (0.45): Moderate, declining sharply from 0.72 at occupation to 0.45 at stabilization. The high initial suppression reflects military occupation's coercive power (can enforce constitutional adoption). The decline reflects domestication: as Japanese citizens internalize the constitution, external coercion becomes unnecessary. Suppression persists at 0.45 because the supermajority amendment gate is a structural barrier (not easily overcome), and the internalized stigma against imperialism/militarism constrains even willing revisionists. Theater ratio (0.38): Low-moderate, declining from 0.62 to 0.38. The high initial ratio reflects that occupation-era acceptance was performative (compliance under coercion, not internalized commitment). The sharp decline reflects genuine domestication: the constitution became a real organizing principle of political life, not a theatrical performance. The remaining 0.38 reflects that some constitutional discourse is performative — rhetorical attachment to pacifism mixed with creeping reinterpretations of Article 9 for security purposes, formal commitment to constitutional supremacy alongside practical evasion.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence. The displaced imperial institution sees a Snare (trapped, no exit, full suppression). Constitutional revisionists see a Tangled Rope (benefit from open political process but extracted by amendment difficulty). Postwar citizens see a Tangled Rope (benefit from democracy but constrained on sovereignty). The occupying power saw a Rope initially (pure coordination of regional settlement) but the analytical observer now sees the complexity: the occupying power's interest becomes subtly extractive as it maintains the constraint through alliance structure (Japan stays client-state). The analytical observer at generational scale sees a Rope or low-extraction Tangled Rope (the constraint has become constitutive of Japanese identity and genuinely coordinates democratic governance). The core perspectival gap is between (a) those who experience the constitution as an imposed, suppressive constraint on authentic national identity (revisionists, imperial nostalgists) and (b) those who have internalized it as constitutive of authentic Japanese postwar identity (progressives, courts, majority of political establishment). The gap has narrowed dramatically: initial rejection of the constitution by conservatives (1947-1952) has given way to acceptance of the text while disputing its interpretation (Article 9 reinterpretation debate, not outright rejection). The constraint's evolution from externally imposed (high theater, high suppression) to self-maintaining (low theater, soft suppression) is the domestication story.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position — who benefits, who bears costs, what exit options they possess. The occupying power (institutional, arbitrage) has d ≈ 0.05 (full beneficiary of regional settlement): derives low effective extraction chi. The postwar citizen (moderate, constrained) has d ≈ 0.60 (mixed costs and benefits, high constraint on exit): derives moderate chi. The imperial institution (powerless, trapped) has d ≈ 0.95 (full victim, no exit): derives high chi. The constitutional revisionist (institutional, constrained) has d ≈ 0.55 (victim of amendment gate, but benefits from political platform): derives moderate chi. The progressive coalition (institutional, constrained) has d ≈ 0.40 (beneficiary of Article 9 lock, but depends on consensus): derives moderate chi. The analytical observer (analytical, analytical) derives d ≈ 0.72 per canonical fallback, but the domestication thesis lowers it to 0.50 (symmetric position: the constraint both coordinates governance and suppresses alternatives, costs and benefits distributed). The directionality gap narrows as the constraint domesticates: the initial imposed/beneficiary divide (high d disparity) softens as citizens internalize the framework and revisionists accept the text while disputing interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is avoided because the constraint's extractiveness (0.28) remains below the 0.46 threshold for high-extraction conflict. However, the constraint exhibits mandatrophy tensions that are resolved through domestication: the imposition of a foreign constitution creates a prima facie mandatrophy (external enforcement of internal governance, beneficiary-victim asymmetry) that the polity resolves by internalizing the constraint and converting it from Snare (imposed suppression) to Tangled Rope (mixed coordination-extraction). The resolution is not theoretical but historical — the Japanese political system spent 30 years converting the constraint's character through practice. The analytical observer sees the Rope classification (coordination of regional order, low extraction), while the revisionists see Snare (trapped by amendment gate). The mandatrophy is resolved not by choosing one classification but by recognizing that the constraint's type has changed: it began as imposed (high theater, high suppression, snare-like for victims) and evolved into domesticated (low theater, lower suppression, tangledRope for all participants). The timeline measurements document this evolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imperial_sovereignty_doctrine_suppression,
    'Is the suppression of imperial sovereignty doctrine (Article 1, Emperor as symbol) a genuine constitutional constraint or a contingent historical circumstance that could be revised with sufficient political consensus?',
    'Longitudinal analysis of constitutional amendment attempts; polling on public support for imperial restoration; comparative analysis of how other postwar constitutions handled sovereignty transitions (German, Italian, Austrian cases). If supermajority threshold is the only barrier and consensus could emerge, the suppression is contingent. If structural-cultural factors make consensus-building for revision psychologically impossible (identity lock on pacifism-as-Japaneseness), the suppression is harder than the text itself.',
    'If contingent: the constraint is a Rope (coordination with soft enforcement). If structural-cultural: the constraint is a Tangled Rope or Snare (extraction via internalized commitment). If revisable without constitutional amendment (via reinterpretation, as scholars have attempted for Article 9): the suppression is even softer, suggesting Theater rather than structural extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_sovereignty_doctrine_suppression, empirical, 'Whether imperial sovereignty suppression is contingent on supermajority consensus or structurally deeper').

omega_variable(
    article_9_domestication_vs_imposition,
    'Has Article 9 (renunciation of war and military force) transitioned from an imposed constraint to a genuine constitutional culture, or does it persist as suppression maintained by external political pressure and internalized obligation?',
    'Historical institutionalism: track the timeline of Article 9''s acceptance. Early postwar period (1947-1960): imposed, resisted by conservatives, protected by US security guarantee. Cold War period (1960-1990): internalized as peace culture, integrated into national identity, articulated in textbooks and diplomacy. Post-Cold War (1990-2024): tested by security pressures (Korean peninsula, China rise, terrorism); track constitutional reinterpretation attempts (collective self-defense, Security Dilemma creep). If Article 9 is now genuine constitutional culture, it should survive without external enforcement. If it requires US military guarantee and US-Japan alliance structure to survive, it is still partially imposed.',
    'If genuinely domesticated: the constraint is low-extractiveness Rope or Tangled Rope (coordination with internalized commitment). If still extraction-dependent on external power: the constraint is higher-extractiveness Snare or Tangled Rope (suppression requires active enforcement). The reading''s entire interpretation hinges on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_9_domestication_vs_imposition, empirical, 'Whether Article 9 is internalized constitutional culture or externally enforced suppression').

omega_variable(
    kernel_reading_sufficiency,
    'Does the 1947 Japanese Constitution instantiate a coherent postwar constitutionalism reading distinct from German Basic Law and Indian Constitution, or are the three readings aspects of a single global phenomenon with local variation?',
    'Structural comparison: kernel question is ''what legitimates a postwar constitution when the prior sovereignty structure is destroyed?'' German answer: militant democracy entrenching human dignity against totalitarianism''s return (anxiety about repetition). Indian answer: exhaustive constitutional text attempting social revolution in a society without prior equality (anxiety about inequality). Japanese answer: imposed charter that became culture through generational practice (anxiety about legitimacy of external imposition). If the three readings answer the same kernel question with structurally distinct approaches, they coexist as three readings of one kernel. If they answer different kernel questions, they should be three separate kernels. This omega tests whether kernel_id=''postwar_constitutionalism'' is the right grouping.',
    'If the three readings coexist within one kernel: this story is one reading among three, and reading_relations should be ''coexists_with'' for both siblings. If they are three separate kernels: this story stands alone and should not reference siblings. The axioms and cs_structure interpretation depend on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sufficiency, conceptual, 'Whether the three postwar constitutions are readings of one kernel or instances of three kernels').

omega_variable(
    collective_self_defense_reinterpretation,
    'Can Article 9''s renunciation of war be reinterpreted to permit collective self-defense (as the government has begun to argue for security treaty purposes) without formally amending the text, and if so, does such reinterpretation signal the constraint''s failure or its flexibility?',
    'Constitutional law scholarship: track government reinterpretation of Article 9 (2014 Cabinet decision permitting collective self-defense exercise under security treaties). If courts validate the reinterpretation, the constraint permits internal revision without formal amendment (interpretive flexibility). If courts reject or constrain it, Article 9 retains its original meaning (structural rigidity). The resolution determines whether suppression is via text or via interpretation gatekeeping.',
    'If reinterpretation is valid: suppression mechanism shifts from textual to interpretive (power moves to courts). If reinterpretation is invalid: suppression is textual (power remains distributed across supermajority). Either way, the constraint''s structure is revealed: is Article 9 a hard textual barrier or a soft interpretive one that powerful actors can work around?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collective_self_defense_reinterpretation, empirical, 'Whether Article 9 can be reinterpreted for collective self-defense without formal amendment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(postwar_constitutionalism__japanese_constitution_1947, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jpconst_theater_t0_occupation, postwar_constitutionalism__japanese_constitution_1947, theater_ratio, 0, 0.62).
narrative_ontology:measurement(jpconst_theater_t15_sanfrancisco_peace, postwar_constitutionalism__japanese_constitution_1947, theater_ratio, 15, 0.45).
narrative_ontology:measurement(jpconst_theater_t30_cold_war_stabilization, postwar_constitutionalism__japanese_constitution_1947, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(jpconst_extractiveness_t0_occupation, postwar_constitutionalism__japanese_constitution_1947, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(jpconst_extractiveness_t15_sanfrancisco_peace, postwar_constitutionalism__japanese_constitution_1947, base_extractiveness, 15, 0.22).
narrative_ontology:measurement(jpconst_extractiveness_t30_cold_war_stabilization, postwar_constitutionalism__japanese_constitution_1947, base_extractiveness, 30, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(jpconst_suppression_t0_occupation, postwar_constitutionalism__japanese_constitution_1947, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(jpconst_suppression_t15_sanfrancisco_peace, postwar_constitutionalism__japanese_constitution_1947, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(jpconst_suppression_t30_cold_war_stabilization, postwar_constitutionalism__japanese_constitution_1947, suppression_requirement, 30, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(postwar_constitutionalism__japanese_constitution_1947, enforcement_mechanism).
narrative_ontology:affects_constraint(postwar_constitutionalism__japanese_constitution_1947, postwar_constitutionalism__german_basic_law).
narrative_ontology:affects_constraint(postwar_constitutionalism__japanese_constitution_1947, postwar_constitutionalism__indian_constitution_1950).

% DUAL FORMULATION NOTE:
% The three postwar constitutions (Japanese 1947, German 1949, Indian 1950) are readings of one contested kernel: 'postwar constitutionalism — the legitimacy of constitutions imposed or written in the wake of colonial/imperial/totalitarian collapse.' Each reading answers the kernel question with a distinct structural approach and epsilon value. Japanese reading (this story, ε=0.28): imposed charter domesticated through generational practice. German reading (ε≈0.32): militant democracy entrenching human dignity against totalitarian return. Indian reading (ε≈0.35): exhaustive social revolutionary text in hierarchical society. All three are structurally distinct constraints linked by kernel relationships, not variations of a single constraint. Each has its own beneficiary/victim structure, suppression mechanism, and temporal trajectory. They coexist as live alternatives — no reading forecloses another within the postwar constitutional universe.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(postwar_constitutionalism__japanese_constitution_1947, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
