% ============================================================================
% CONSTRAINT STORY: john_1_1_logos__subordinationist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_john_1_1_logos__subordinationist, []).

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
 *   constraint_id: john_1_1_logos__subordinationist
 *   human_readable: Logos Subordinationism (John 1:1 Reading)
 *   domain: theology/christology/biblical_hermeneutics
 *
 * SUMMARY:
 *   The subordinationist reading of John 1:1 — that the Logos is a created
 *   being or subordinate divine agent, first and highest creation but not
 *   co-eternal or consubstantial with the Father — creates a structural
 *   constraint on Christian theology and institutional authority. This
 *   constraint is one of three contending readings of the kernel at John 1:1
 *   (the subordinationist reading, the orthodox trinitarian reading, and
 *   non-incarnational monotheist readings all claim authority over the same
 *   text). The subordinationist reading produces genuine theological
 *   coordination (a coherent Christological framework) while simultaneously
 *   enabling institutional extraction: it redistributes theological authority
 *   away from high-church structures that depend on Christ's full divinity
 *   for their sacramental exclusivity. The constraint exhibits the signature
 *   of a tangled rope — real coordination function (coherent theology)
 *   coupled with asymmetric extraction (subordinationist communities
 *   suppressed, orthodox authority elevated). The measurement trajectory
 *   shows extractiveness and suppression increasing sharply post-Council of
 *   Nicaea (t≈150 years after the reading emerges), and theater ratio rising
 *   as orthodox monopoly deepens and subordinationist exegesis becomes
 *   officially heretical rather than debatable. The subordinationist
 *   constraint operates through enforced doctrinal conformity backed by
 *   ecclesiastical authority, political power, and textual-tradition survival
 *   bias.
 *
 * KEY AGENTS:
 *   - Subordinationist communities: Primary victims (powerless/trapped) — bearers of suppression, theological identity locked into heretical category
 *   - High-church authority structures: Primary beneficiaries (institutional/arbitrage) — maintain exclusive control over Christological interpretation, defend sacramental authority based on Christ's full divinity
 *   - Arius and Arian theological schools: Secondary beneficiaries (organized/constrained) — established intellectual framework, subordinationist Christology as coherent theological option, faced institutional extirpation
 *   - Trinitarian orthodox theologians (Athanasius, Cappadocian fathers): Institutional/powerful beneficiaries — won ecclesiastical authority determination at councils, established orthodoxy as enforcement mechanism
 *   - Protestant Reformation scholars: Organized/constrained agents — recovered interpretive freedom to consider subordinationist reading as legitimate option, sunset from institutional suppression without full reversal
 *   - Modern biblical scholarship: Analytical/arbitrage observers — perform academic neutrality toward subordinationism while institutional suppression persists, high theater without functional institutional authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(john_1_1_logos__subordinationist, 0.58).
domain_priors:suppression_score(john_1_1_logos__subordinationist, 0.62).
domain_priors:theater_ratio(john_1_1_logos__subordinationist, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, extractiveness, 0.58).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(john_1_1_logos__subordinationist, tangled_rope).
narrative_ontology:human_readable(john_1_1_logos__subordinationist, "Logos Subordinationism (John 1:1 Reading)").
narrative_ontology:topic_domain(john_1_1_logos__subordinationist, "theology/christology/biblical_hermeneutics").

domain_priors:requires_active_enforcement(john_1_1_logos__subordinationist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(john_1_1_logos__subordinationist, '2b0c6751-92d3-4f73-ac88-e5f05ccd8937').
narrative_ontology:cs_kernel_codification('2b0c6751-92d3-4f73-ac88-e5f05ccd8937', fixed_text).
narrative_ontology:cs_authority_grounding('2b0c6751-92d3-4f73-ac88-e5f05ccd8937', extraction).
narrative_ontology:cs_interpretation_layer_present('2b0c6751-92d3-4f73-ac88-e5f05ccd8937').
narrative_ontology:cs_reading_relation('2b0c6751-92d3-4f73-ac88-e5f05ccd8937', john_1_1_logos__orthodox_christological, forecloses).
narrative_ontology:cs_reading_relation('2b0c6751-92d3-4f73-ac88-e5f05ccd8937', john_1_1_logos__non_incarnational_monotheist, influences).
narrative_ontology:cs_axiom('2b0c6751-92d3-4f73-ac88-e5f05ccd8937', foundational, logos_created_being).
narrative_ontology:cs_axiom_status(logos_created_being, holdable).
narrative_ontology:cs_axiom_grounding('2b0c6751-92d3-4f73-ac88-e5f05ccd8937', logos_created_being, empirically_contingent).
narrative_ontology:cs_axiom('2b0c6751-92d3-4f73-ac88-e5f05ccd8937', foundational, monotheistic_father_primacy).
narrative_ontology:cs_axiom_status(monotheistic_father_primacy, holdable).
narrative_ontology:cs_axiom_grounding('2b0c6751-92d3-4f73-ac88-e5f05ccd8937', monotheistic_father_primacy, deontological).
narrative_ontology:cs_reference_frame('2b0c6751-92d3-4f73-ac88-e5f05ccd8937', subordinationist_logos_agency).
narrative_ontology:cs_drift_state('2b0c6751-92d3-4f73-ac88-e5f05ccd8937', post_nicaea_orthodox_dominance, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('2b0c6751-92d3-4f73-ac88-e5f05ccd8937', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(john_1_1_logos__subordinationist, john_1_1_logos).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, subordinationist_communities).
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, modalist_theological_schools).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, high_church_authority_structures).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, trinitarian_orthodox_exclusivity_claim).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATIONIST FAITHFUL (SNARE) — Communities holding this reading face systematic suppression across ecclesiastical authority structures (councils, creeds, institutional orthodoxy). Exit from the reading is treated as heresy; remaining requires internalization of subordinate status within Christendom. No structural escape route; theological identity locked into suppressed position.
constraint_indexing:constraint_classification(john_1_1_logos__subordinationist, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REFORM-MINDED CLERGY (TANGLED ROPE) — Some clergy benefit from intellectual space to question Nicene orthodoxy (coordination: hermeneutical freedom, creative biblical interpretation) while also bearing costs of institutional suspicion and reduced advancement. Constrained by career dependence on ecclesiastical hierarchy; coordination and extraction coexist.
constraint_indexing:constraint_classification(john_1_1_logos__subordinationist, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SUBORDINATIONIST THEOLOGICAL SCHOOLS (ROPE) — As organized intellectual positions (Arius and his students, Eusebius of Caesarea, Arian Christianity as institutional tradition), these function as pure coordination: establishing a coherent reading system that organizes biblical texts and theological claims without requiring extraction. Beneficiaries through intellectual prestige and organizational identity.
constraint_indexing:constraint_classification(john_1_1_logos__subordinationist, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TRINITARIAN ORTHODOX AUTHORITY (SNARE) — From the perspective of the victorious orthodoxy post-Nicaea, subordinationism is an extractive threat that must be suppressed: it threatens the exclusivity of the divinity claim, the sacramental authority grounded in consubstantiality, and the institutional power structure of the high church. The constraint appears as a snare FROM THE SIDE OF THE SUPPRESSOR — the orthodox must actively extract conformity from the subordinationist tradition.
constraint_indexing:constraint_classification(john_1_1_logos__subordinationist, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 5: PROTESTANT REFORMATION HERMENEUTICS (SCAFFOLD) — Protestant scholarship (esp. 16th–18th century critical philology) treats subordinationism as a temporary interpretive option enabled by returning to Greek texts and rejecting Catholic orthodox monopoly on exegesis. The sunset is built in: as historical-critical method matures and consensus on Johannine theology develops, the interpretive freedom to read John 1:1 subordinationistically becomes less available (superseded by source criticism, Johannine redaction analysis).
constraint_indexing:constraint_classification(john_1_1_logos__subordinationist, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: MODERN SCHOLARLY CONSENSUS (PITON) — Contemporary academic biblical scholarship treats the subordinationist reading as a 'live historical option' (performs analytical neutrality) while the institutional church continues to treat it as heresy (suppressed). The scholarly consensus performs a subordinationist-friendly exegetical apparatus (documentary evidence, Arian textual traditions, Christological development models) but without corresponding institutional authority to change worship practice or dogma. High theater: the machinery of scholarly argument persists in journals and seminaries without functional impact on ecclesiastical authority structure.
constraint_indexing:constraint_classification(john_1_1_logos__subordinationist, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (FALSE SUMMIT CANDIDATE) — From a civilizational/universal perspective, textual polyvalence in ancient Greek grammar and theology is an immutable feature of the Johannine corpus: John 1:1 (ho logos) is genuinely ambiguous between full divinity and subordinate agency across multiple linguistic and theological frameworks. This appears as a natural law of interpretation — the text IS underdetermined. However, the structural data (the enforced orthodoxy, the institutional suppression, the beneficiaries of the orthodox reading, the heresy trials) reveals this as a false summit: the naturalness of the ambiguity masks a contingent institutional determination in favor of orthodoxy.
constraint_indexing:constraint_classification(john_1_1_logos__subordinationist, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(john_1_1_logos__subordinationist_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(john_1_1_logos__subordinationist, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(john_1_1_logos__subordinationist, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(john_1_1_logos__subordinationist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(john_1_1_logos__subordinationist, TR),
    TR >= 0.70.

:- end_tests(john_1_1_logos__subordinationist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The subordinationist reading creates real theological gains (internal logical coherence, scriptural parsimony, avoidance of Greek philosophical concepts like ousia) that coordinate communities around a shared interpretation. But it also enables redistribution of theological authority: subordinationism threatens the exclusive authority of high-church structures whose power derives from Christ's full divinity claim. The extraction is asymmetric — subordinationist communities gain intellectual coherence but lose institutional authority and ecclesiastical standing; orthodox authorities gain control but lose intellectual flexibility. Suppression (0.62): High. Institutional suppression is extreme: heresy trials, anathemas, doctrinal enforcement through ecclesiastical hierarchy, political penalties for subordinationist affiliation, manuscript tradition biased toward orthodox survival. Suppression increases dramatically post-Nicaea as imperial backing solidifies. Theater ratio (0.68): Moderate-high. Modern scholarship performs exegetical apparatus supporting subordinationist reading (source criticism, grammatical analysis, historical-critical method) while the constraint persists through institutional inertia — academic legitimacy coupled with ecclesiastical suppression creates a simulacrum of settled debate. The theater rises as scholarship becomes more sophisticated but less consequential for actual church authority.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence. Subordinationist communities see suppression and extraction (Snare). Orthodox authorities see a threat requiring suppression (Snare from the suppressor's side). Intellectual schools see coherent theology (Rope). Reform clergy see mixed coordination and constraint (Tangled Rope). Modern scholarship sees historical optionality (Piton — sophisticated analysis with no power to change institutional practice). The analytical observer risks seeing textual polyvalence as a natural law of grammar (Mountain — the text is genuinely ambiguous) while the structural data reveals institutional determination (false summit — the constraint's 'naturalness' masks enforced orthodoxy). The gap between scholarly consensus (subordinationism is a legitimate reading) and ecclesiastical authority (subordinationism is heresy) is the signature diagnostic signal that this is a contingent institutional arrangement, not a natural theological limit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d is derived from each agent's structural relationship to the subordinationist constraint. Subordinationist communities are victims of institutional suppression with trapped exit (d ≈ 0.95, high f(d), maximum experienced extraction). High-church authorities are beneficiaries with arbitrage options (d ≈ 0.05, low/negative f(d), experiencing the constraint as coordination of their authority). Trinitarian orthodox theologians occupy institutional positions with constrained exit if they abandoned orthodoxy (d ≈ 0.15, low f(d)). The Reformation scholars have constrained exit but increasing intellectual agency (d ≈ 0.40, moderate f(d)). Modern scholarship has analytical/observer status (d ≈ 0.73, canonical analytical value, high f(d) per observer paradox — seeing the full structure without power to act). The perspectival gaps between d values explain why the same textual phenomenon classifies as Snare from the victim perspective and Rope from the beneficiary perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is NOT a mandatrophy paradox. All seven perspectives are legitimate readings of the same structural data, and no single type is 'the correct' classification. The mandatrophy would arise if the constraint MUST be one type while evidence supports multiple types — but the framework permits indexed classification precisely to capture this multiplicity. The subordinationist constraint IS a tangled rope for its base classification: it has genuine coordination function (theological coherence) coupled with asymmetric extraction (redistributing authority away from high-church monopoly). The snare perspectives from subordinationist communities and from orthodox suppression are orthogonal views of the same extraction mechanism from opposite sides. The rope and scaffold perspectives capture intellectual and historical-critical readings of the constraint. The piton perspective captures the modern degradation of scholarly consensus into performance. The false summit perspective reveals the naturalness of orthodoxy as contingent institutional entrenchment. All types are analytically legitimate once the observation position is specified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    greek_theos_referent_scope,
    'Does ho theos in John 1:1c (''and the word was God'') logically require numerical identity with the theos in 1:1b (''the word was with God''), or does it permit distinct-but-divine agency referents?',
    'Comparative linguistic analysis across Greek theological texts (LXX, Philo, Clement); analysis of predicate nominative constructions in Johannine Christology; examination of whether the absence of the article before theos permits subordinate divine attribution.',
    'If numerical identity required: orthodox reading is textually necessary, subordinationism is grammatically impossible. If distinct-agency permitted: both readings are legitimate grammatical options; orthodoxy is an interpretive choice, not a textual constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(greek_theos_referent_scope, empirical, 'Linguistic scope of theos predication in John 1:1c').

omega_variable(
    institutional_determination_vs_textual_underdetermination,
    'Is the victory of Nicene orthodoxy (the subordinationist constraint''s suppression) a consequence of better exegetical argument and superior textual evidence, or a consequence of institutional power consolidation and political-ecclesiastical authority?',
    'Historical analysis of council voting patterns, imperial political influence, heresy trial records, and survival bias in textual traditions; comparison of exegetical strength of Arian vs Nicene theological arguments by modern standards; examination of how non-theological factors (political alliance, charisma of advocates, imperial favor) influenced outcomes.',
    'If exegetical: the mountain perspective is justified — orthodoxy won because it was textually correct. If political: the false summit is confirmed — the constraint is a contingent institutional determination masquerading as natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_determination_vs_textual_underdetermination, empirical, 'Historical causation: exegesis vs ecclesiastical power in Nicene victory').

omega_variable(
    kernel_underdetermination_reading_coexistence,
    'Can a single Christian theological framework coherently hold both subordinationist and orthodox readings of John 1:1 without logical contradiction, or do they foreclose one another?',
    'Formal logical analysis of axiom compatibility; examination of historical periods where both readings coexisted within Christian communities (Arian Christianity, semi-Arian compromise positions, late Reformation debates); analysis of whether coexistence required explicit framework separation (different canons, different hermeneutical principles) or genuine compatibility.',
    'If foreclosed: one reading logically rules out the other; the relationship is forecloses, not coexists_with. If compatible: both can be held simultaneously within different communities or with different theological commitments; the relationship is coexists_with. If requires framework separation: influences relation holds — adopting one reading constrains but does not eliminate the other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_underdetermination_reading_coexistence, conceptual, 'Logical compatibility of subordinationist and orthodox readings within one framework').

omega_variable(
    textual_survival_bias_in_manuscript_tradition,
    'Does the dominance of Nicene-orthodox manuscripts in the surviving textual tradition (post-4th century) reflect genuine textual stability, or reflect institutional suppression of subordinationist scriptural variants?',
    'Analysis of manuscript dating, provenance, and institutional copying patterns; examination of subordinationist biblical quotations in surviving texts (Arian fragmentary quotations, Eusebius citations); comparison of textual variants in John 1:1 across available manuscript families.',
    'If dominant tradition reflects genuinely superior textual evidence: subordinationist reading loses evidentiary support. If survival bias reflects institutional suppression: textual tradition is contaminated by orthodoxy''s enforced consensus; textual evidence for subordinationism may be underrepresented.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_survival_bias_in_manuscript_tradition, empirical, 'Manuscript survival bias toward Nicene-orthodox textual traditions').

omega_variable(
    subordinationism_as_identity_locked_exit_option,
    'For contemporary subordinationist communities (e.g., certain Arian-heritage churches, biblical unitarian movements), is exit from subordinationism prevented primarily by structural barriers (institutional suppression, no alternative institutional home) or by identity-locked commitment (theological identity fused with subordinationist reading)?',
    'Ethnographic study of contemporary subordinationist groups; analysis of membership retention patterns when institutional barriers are removed; examination of how adherents justify the reading post-suppression (still referentially tied to orthodoxy''s negation, or independently grounded?).',
    'If structural: the trapped classification is correct; removing suppression would enable exit. If identity-locked: members perceive subordinationism as essential to their theological identity; removing suppression would not enable exit; the constraint operates cognitively rather than structurally.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subordinationism_as_identity_locked_exit_option, empirical, 'Whether subordinationist commitment is structurally trapped or identity-locked').

omega_variable(
    orthodoxy_as_false_summit_natural_law,
    'Is the apparent immutability of Nicene orthodoxy a consequence of its being the correct theological interpretation (natural law), or a consequence of institutional entrenchment and doctrinal enforcement?',
    'Historical counterfactual analysis: if the Council of Nicaea had voted differently or if imperial support had favored Arius, would we see 1700+ years of enforced subordinationism instead? Examination of whether orthodoxy survives empirical challenge or whether its authority is maintained through institutional means independent of evidence.',
    'If correct interpretation: mountain classification is appropriate; the constraint emerges naturally from theological truth. If institutional entrenchment: false summit is confirmed; the naturalness of orthodoxy masks a contingent institutional arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(orthodoxy_as_false_summit_natural_law, conceptual, 'Whether Nicene orthodoxy is theological natural law or institutionally contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(john_1_1_logos__subordinationist, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(logos_sub_theater_t0, john_1_1_logos__subordinationist, theater_ratio, 0, 0.42).
narrative_ontology:measurement(logos_sub_theater_t150, john_1_1_logos__subordinationist, theater_ratio, 150, 0.58).
narrative_ontology:measurement(logos_sub_theater_t300, john_1_1_logos__subordinationist, theater_ratio, 300, 0.68).

% Extraction over time
narrative_ontology:measurement(logos_sub_extract_t0, john_1_1_logos__subordinationist, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(logos_sub_extract_t150, john_1_1_logos__subordinationist, base_extractiveness, 150, 0.52).
narrative_ontology:measurement(logos_sub_extract_t300, john_1_1_logos__subordinationist, base_extractiveness, 300, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(logos_sub_suppress_t0, john_1_1_logos__subordinationist, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(logos_sub_suppress_t150, john_1_1_logos__subordinationist, suppression_requirement, 150, 0.62).
narrative_ontology:measurement(logos_sub_suppress_t300, john_1_1_logos__subordinationist, suppression_requirement, 300, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(john_1_1_logos__subordinationist, identity_coordination).
narrative_ontology:boltzmann_floor_override(john_1_1_logos__subordinationist, 0.12).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, john_1_1_logos__orthodox_christological).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, john_1_1_logos__non_incarnational_monotheist).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, nicene_creed_enforcement).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, high_church_sacramental_authority).

% DUAL FORMULATION NOTE:
% The subordinationist reading of John 1:1 is one member of a constraint family centered on the kernel at John 1:1-3. The three readings (subordinationist, orthodox, non-incarnational) each produce a distinct constraint with distinct ε values, distinct beneficiaries/victims, and distinct institutional consequences. The subordinationist reading has ε=0.58 (tangled rope baseline); the orthodox reading has lower institutional extraction (ε≈0.20, rope-to-mountain from the orthodox authority perspective) because orthodoxy achieved institutional dominance; the non-incarnational reading has high extraction (ε≈0.65, snare) because it is maximally suppressed with no significant institutional support. All three stories are linked via network.affects_constraints to show their sibling relationship within the contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(john_1_1_logos__subordinationist, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
