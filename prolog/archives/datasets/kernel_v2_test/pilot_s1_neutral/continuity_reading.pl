% ============================================================================
% CONSTRAINT STORY: continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_continuity_reading, []).

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
 *   constraint_id: continuity_reading
 *   human_readable: Continuity Reading: Medieval Latin as Living Evolution of Classical Latin
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   The continuity reading asserts that Medieval Latin is the legitimate
 *   evolved form of Classical Latin, transmitted through continuous living
 *   practice across ecclesiastical communities from late antiquity through
 *   the medieval period and into the modern scholarly tradition. This reading
 *   contests a kernel claim about the nature of language transmission and the
 *   source of linguistic authority. The constraint operates on multiple
 *   levels: (1) as a claim about linguistic fact (is medieval
 *   phonology/morphology a evolution of Classical forms, or a rupture?), (2)
 *   as an institutional framework for organizing textual authority and
 *   transmission (manuscripts inherit legitimacy through unbroken copying
 *   tradition), and (3) as an identity anchor for ecclesiastical and
 *   scholarly communities (continuity with Classical antiquity confers
 *   prestige and legitimacy). The constraint exhibits genuine coordination
 *   function (solves the problem of which Latin to teach, how to interpret
 *   variant forms, how to weight manuscript evidence) alongside asymmetric
 *   extraction (suppresses evidence of linguistic change, enforces classical
 *   purity against medieval innovation, grants institutional authority to
 *   ecclesiastical practice at the expense of empirical linguistic analysis).
 *   The theater_ratio has risen over the interval as modern historical
 *   linguistics reveals the mechanisms of change while the institutional
 *   apparatus continues to invoke 'unbroken tradition' without updating the
 *   framework.
 *
 * KEY AGENTS:
 *   - Ecclesiastical Latin practitioners (medieval): Primary beneficiary (institutional/arbitrage) — continuity reading legitimates their Latin as authentic transmission
 *   - Classical purist scholars (humanist era onward): Secondary beneficiary & enforcer (organized/constrained) — enforce the purity of Classical forms against medieval 'corruption'
 *   - Medieval linguistic innovation (empirical phenomenon): Primary victim (powerless/trapped) — evidence of change must be reframed as evolution or suppressed as corruption
 *   - Diachronic linguists (modern): Organized reformers (organized/constrained) — building alternative verification framework that bypasses continuity/discontinuity debate
 *   - Historical manuscript evidence: Abstract victim (powerless/trapped) — empirical record cannot exit or contest the interpretive frame applied to it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(continuity_reading, 0.38).
domain_priors:suppression_score(continuity_reading, 0.42).
domain_priors:theater_ratio(continuity_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(continuity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(continuity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(continuity_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(continuity_reading, tangled_rope).
narrative_ontology:human_readable(continuity_reading, "Continuity Reading: Medieval Latin as Living Evolution of Classical Latin").
narrative_ontology:topic_domain(continuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(continuity_reading, '600d9885-15cc-4463-b9ec-54b1d10d6408').
narrative_ontology:cs_kernel_codification('600d9885-15cc-4463-b9ec-54b1d10d6408', fixed_text).
narrative_ontology:cs_authority_grounding('600d9885-15cc-4463-b9ec-54b1d10d6408', lineage).
narrative_ontology:cs_interpretation_layer_present('600d9885-15cc-4463-b9ec-54b1d10d6408').
narrative_ontology:cs_reading_relation('600d9885-15cc-4463-b9ec-54b1d10d6408', continuity_reading__discontinuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('600d9885-15cc-4463-b9ec-54b1d10d6408', continuity_reading__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('600d9885-15cc-4463-b9ec-54b1d10d6408', foundational, unbroken_community_entails_linguistic_continuity).
narrative_ontology:cs_axiom_status(unbroken_community_entails_linguistic_continuity, holdable).
narrative_ontology:cs_axiom_grounding('600d9885-15cc-4463-b9ec-54b1d10d6408', unbroken_community_entails_linguistic_continuity, deontological).
narrative_ontology:cs_axiom('600d9885-15cc-4463-b9ec-54b1d10d6408', foundational, ecclesiastical_transmission_preserves_classical_authority).
narrative_ontology:cs_axiom_status(ecclesiastical_transmission_preserves_classical_authority, holdable).
narrative_ontology:cs_axiom_grounding('600d9885-15cc-4463-b9ec-54b1d10d6408', ecclesiastical_transmission_preserves_classical_authority, conventional).
narrative_ontology:cs_reference_frame('600d9885-15cc-4463-b9ec-54b1d10d6408', augustine_bede_continuous_transmission).
narrative_ontology:cs_drift_state('600d9885-15cc-4463-b9ec-54b1d10d6408', contemporary_historical_linguistics, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('600d9885-15cc-4463-b9ec-54b1d10d6408', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(continuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(continuity_reading, continuity_school_scholars).
narrative_ontology:constraint_beneficiary(continuity_reading, medieval_ecclesiastical_tradition).
narrative_ontology:constraint_victim(continuity_reading, discontinuity_empirical_evidence).
narrative_ontology:constraint_victim(continuity_reading, normative_classical_purists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMPIRICAL RECORD (SNARE) — The documentary evidence of phonological collapse, case system simplification, and syntactic restructuring from late antiquity through the medieval period cannot exit the framework. The continuity reading must suppress or reframe evidence of discontinuity (loss of inflectional morphology, phonetic shift, vocabulary replacement) as 'evolution' rather than 'rupture.' The empirical record bears the extraction cost: it is required to validate the tradition but cannot challenge it from within the tradition's own epistemic rules.
constraint_indexing:constraint_classification(continuity_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: ECCLESIASTICAL LATIN TRADITION (ROPE) — Medieval monks and ecclesiastical scholars benefit from the continuity reading: it legitimates their Latin as direct transmission from the Classical tradition, granting their liturgical and scholarly Latin the authority of unbroken lineage rather than innovative reconstruction. The continuity claim solves a genuine coordination problem: it provides a unified epistemic framework for manuscript transmission, textual authority, and scriptural interpretation across centuries and communities. Net beneficiary perspective.
constraint_indexing:constraint_classification(continuity_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: HUMANIST REFORM MOVEMENT (TANGLED ROPE) — Renaissance humanists are both coordinated by and extract from the continuity reading. They benefit from having a single Latin tradition to study and restore (solves the problem of which texts to privilege and how to interpret variant forms). Yet they must suppress their own empirical discoveries about medieval linguistic innovation, stylistic change, and orthographic variation—discoveries that would fragment the unified Latin into historical strata. Their organizing principle (return to Classical purity) requires denying the very historical evidence they uncover. Active enforcement: the humanist critical apparatus must constantly distinguish 'corruption' (medieval innovation to be rejected) from 'genuine Classical transmission' (forms to be preserved), maintaining a boundary that the evidence itself makes unstable.
constraint_indexing:constraint_classification(continuity_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: PHILOLOGICAL CANON (PITON) — The continuity framework has atrophied into institutional theater: modern Latin instruction and textual transmission practices continue to invoke 'the living Latin tradition' and 'unbroken ecclesiastical transmission' long after the historical evidence of rupture and innovation became clear. The framework persists through pedagogical inertia and institutional prestige (universities, seminaries, classical societies continue to teach Latin as a unified tradition) rather than through any active verification that the claim is true. The performative content is high: scholars and teachers assert continuity while simultaneously teaching historical strata (Classical texts vs. medieval texts vs. ecclesiastical Latin as distinct pedagogical units), maintaining the fiction of unity in practice while acknowledging difference in method.
constraint_indexing:constraint_classification(continuity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — At maximum temporal and spatial distance, this perspective sees linguistic continuity as a natural law: all living languages are products of continuous evolution from prior forms; rupture is impossible in any language transmission involving unbroken speaker communities; the Latin of medieval monks cannot be anything other than evolved Latin because evolution is how languages work. From this view, the discontinuity reading makes a claim against natural law (that a community stopped speaking Latin and restarted with a different form), which is empirically implausible. The mountain classification here is vulnerable to false-summit detection: it naturalizes what is actually a definitional choice (whether to count 'evolution' within a grammatical tradition as continuous with 'rupture' across grammatical systems).
constraint_indexing:constraint_classification(continuity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: DIACHRONIC LINGUISTICS COALITION (SCAFFOLD) — Modern historical linguists (specialists in sound change, morphological evolution, language contact) view the continuity/discontinuity binary as a temporary framing problem being resolved through comparative reconstruction and dialect geography. The coalition works to establish what actually happened: which phonological changes occurred, when, in which communities, under what contact conditions. This reconstructive work makes the continuity/discontinuity debate obsolete by replacing it with precise diachronic mechanisms. The scaffold has a sunset: as historical linguistic methods mature and spread to secondary education, the purely literary/philosophical debate about continuity loses institutional force. Sunset horizon: 20–30 years as historical phonology and contact linguistics become standard curriculum components.
constraint_indexing:constraint_classification(continuity_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(continuity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(continuity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(continuity_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(continuity_reading, TR),
    TR >= 0.70.

:- end_tests(continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The continuity reading coordinates genuine transmission practice and provides legitimate institutional framework for ecclesiastical communities. But it extracts by suppressing evidence of morphological collapse, phonetic shift, and syntactic restructuring. The extraction is not maximal because the reading is not purely false—medieval Latin is indeed evolved Latin, and transmission is genuinely continuous; the reading correctly identifies real continuities while denying documented changes. The measurement trajectory (0.32 → 0.38) reflects rising extraction as historical evidence of change accumulates and must be increasingly suppressed or reframed. Suppression (0.42): Moderate. Medieval manuscripts show clear evidence of case-system loss, phonological merging, new word order patterns, and Latin-Romance lexical shift. The suppression required to maintain continuity reading against this evidence is substantial but not total—the framework allows for 'evolution' and 'living language development' as conceptual escape routes. Rising from 0.25 to 0.42 indicates increasing suppression cost as modern linguistics documents mechanisms of change. Theater ratio (0.55): Moderate-high and rising. The reading has shifted from descriptive fact-claim (medieval monks genuinely transmitted Latin) to performative ritual (modern academics invoke 'unbroken tradition' while teaching historical strata as pedagogically distinct units). The rise from 0.28 to 0.55 marks the piton transition: the framework persists through institutional inertia rather than through verification.
 *
 * PERSPECTIVAL GAP:
 *   The empirical record and ecclesiastical tradition experience opposite directionalities. The ecclesiastical perspective (institutional/arbitrage exit) sees coordination: continuity legitimates their practice and solves governance problems about which texts to privilege. The empirical record perspective (powerless/trapped) sees pure extraction: evidence of change must be suppressed or reframed within the continuity framework. The humanist perspective experiences tangled rope: they benefit from having unified Latin to restore and study, but must actively enforce Classical purity against the medieval forms their own scholarship reveals. The modern piton perspective sees degraded function: continuity is invoked as ritual authority while practical teaching recognizes historical strata. The diachronic linguistics coalition sees a temporary scaffold: historical reconstruction methods make the continuity/discontinuity binary obsolete by replacing it with precise mechanisms. The analytical mountain perspective naturalizes what is actually institutional choice: linguistic evolution is inevitable, so continuity is law—but this conflates form-continuity (demonstrably false) with practice-continuity (true).
 *
 * DIRECTIONALITY LOGIC:
 *   The continuity reading's beneficiaries are institutional actors (ecclesiastical tradition, canonical scholarship) with high arbitrage options—they can adopt alternative frames if incentives shift, and they benefit from the unified tradition framework regardless of its empirical accuracy. Their directionality is low (beneficiary + arbitrage exit → d ≈ 0.1–0.2). The victims are abstract (empirical evidence, linguistic innovation) or powerless (medieval scribes whose actual practice is reframed as corrupted transmission). Their directionality is high (victim + trapped/powerless → d ≈ 0.8–0.95). The humanist perspective is mixed (organized/constrained): they benefit from unified Latin but must enforce purity, experiencing moderate extraction (d ≈ 0.45–0.55). The scaffold perspective (organized/constrained with exit via reconstruction) experiences lower extraction (d ≈ 0.3–0.4) because they see an alternative pathway that bypasses the debate. The suppression metric is unscaled and uniform across contexts: the empirical work of maintaining the continuity frame against evidence is a structural property, not a contextual scaling.
 *
 * MANDATROPHY ANALYSIS:
 *   COMMITTER-FRAME ANALYSIS: The continuity reading is one instantiation of a contested kernel (correct_latin). The kernel commits its authority to questions of what constitutes legitimate Latin transmission and usage. The reading's mandate (establish continuous transmission as the source of legitimacy) has not outlived its function—it still controls institutional practice in seminaries, classical societies, and university curricula—but the mandatrophy is approaching. Modern historical linguistics produces increasingly precise evidence of the mechanisms that the continuity reading must suppress or reframe. The reading persists not through epistemic force but through institutional inertia (piton evidence: the theater_ratio rise, the gap between pedagogical practice and theoretical claim). Mandatrophy is not yet resolved (base_properties.mandatrophy_resolved: false) because the reading's institutional beneficiaries continue to enforce it and defend it against empirical challenge. Resolution would require either: (1) acknowledgment of the discontinuity reading's empirical force (reading forecloses), (2) adoption of the hybrid reading's decomposition (reading coexists with reformed version), or (3) replacement by diachronic linguistic mechanisms (reading loses force via scaffold sunset). The constraint's persistence depends on continued suppression of evidence, which means the extraction cost is paid by the empirical record and by those who must suppress their own findings to remain within the tradition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_vs_evolution_definition,
    'Does ''continuity of tradition'' mean phonological/morphological continuity (sameness of linguistic form) or institutional continuity (sameness of transmission practice and speaker community)?',
    'Explicit definitional analysis and empirical testing: does the reading''s argument depend on form-continuity or practice-continuity? Where evidence of form-change exists, does the reading redefine ''continuity'' to accommodate it, or deny the evidence?',
    'If practice-continuity is the definition, the reading is robust against morphological evidence and classifies as Rope (coordination of transmission). If form-continuity is required, the reading meets empirical falsification and reclassifies toward Snare (suppression of evidence). The reading''s actual argumentative strategy uses both definitions opportunistically.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(continuity_vs_evolution_definition, conceptual, 'Ambiguity between form-continuity and practice-continuity definitions').

omega_variable(
    ecclesiastical_community_identity_lock,
    'Is the continuity reading''s persistence primarily maintained by genuine transmission practice (institutional rope) or by identity-fusion with the ecclesiastical tradition (identity-locked extraction)?',
    'Examine how ecclesiastical Latin practitioners respond to evidence of medieval innovation: do they update their understanding of the tradition, or do they defend the continuity claim against evidence? Comparative analysis with secular Latin communities that lack the identity-fusion incentive.',
    'If identity-locked: the ecclesiastical tradition''s exit_options should be identity_locked rather than arbitrage, raising the effective extraction experienced by this perspective (the identity cannot be exited without community rupture). If institutional rope: the perspective''s beneficiary status is robust, and extraction is minimal. The distinction determines whether the ecclesiastical constraint is Rope or Tangled Rope from the institutional perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecclesiastical_community_identity_lock, empirical, 'Whether ecclesiastical continuity claim is maintained by transmission or identity-fusion').

omega_variable(
    humanist_suppression_mechanism,
    'Is the humanist enforcement of Classical purity (against medieval forms) a structural requirement of the continuity reading, or an ideological add-on contingent on humanist aesthetics?',
    'Historical analysis of pre-humanist medieval scholarship: did continuity-tradition scholars actively suppress or reframe evidence before humanist reforms? If suppression predates humanism, it is structural to the reading; if it begins with humanist aesthetics, the reading could survive without enforcement (becoming Rope rather than Tangled Rope).',
    'If structural: the tangled_rope classification of the humanist perspective is confirmed, and the reading requires active enforcement. If contingent: the reading''s extractiveness drops substantially, reclassifying toward pure Rope at the institutional level. The temporal measurement trajectory (does suppression_requirement rise or fall across the medieval-humanist-modern interval?) will signal which.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanist_suppression_mechanism, empirical, 'Whether humanist suppression of medieval forms is structural or ideological').

omega_variable(
    false_summit_natural_law,
    'Does the mountain classification (linguistic continuity is natural law) reflect genuine inevitability of evolution from prior forms, or does it naturalize a specific institutional choice (how to categorize and transmit medieval Latin)?',
    'Cross-linguistic comparison: compare Latin transmission with languages that experienced documented rupture and restart (post-Colonial language reintroduction, Icelandic after literacy gap, Hebrew revival). If these languages show the same ''continuity'' pattern when analyzed through the same institutional lens, the reading naturalizes definition, not law.',
    'If naturalization confirmed: the mountain is a false summit (engine reclassifies via FSM). If genuine natural law: the mountain stands, and the reading is descriptively accurate about linguistic universals. The scaffold perspective (diachronic reconstruction bypasses the debate) will resolve this by producing empirical mechanisms that either require or forbid the continuity framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law, empirical, 'Whether linguistic continuity is natural law or institutional categorization').

omega_variable(
    kernel_decomposition_adequacy,
    'Are the three sibling readings (continuity, discontinuity, hybrid) genuinely distinct constraint stories, or are they three descriptions of the same underlying institutional arrangement viewed from different rhetorical positions?',
    'ε-invariance test: compute the base extractiveness and suppression metrics for each reading under fixed observables. If all three readings produce the same metrics under the same observable, they are perspectival framings of one constraint and should be collapsed; if metrics differ, they are genuinely distinct constraint stories. Compare the beneficiary/victim declarations across readings.',
    'If distinct: each reading is a legitimate constraint story with its own network entry and independent classification. If identical metrics: the kernel decomposition is a rhetorical game (three different authority frames for the same extraction mechanism), not a structural difference. The network effects of the constraint family differ radically between cases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_decomposition_adequacy, empirical, 'Whether three readings are distinct constraints or perspectival framings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cont_theater_0, continuity_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(cont_theater_50, continuity_reading, theater_ratio, 50, 0.45).
narrative_ontology:measurement(cont_theater_100, continuity_reading, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(cont_extract_0, continuity_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cont_extract_50, continuity_reading, base_extractiveness, 50, 0.36).
narrative_ontology:measurement(cont_extract_100, continuity_reading, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(cont_suppress_0, continuity_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(cont_suppress_50, continuity_reading, suppression_requirement, 50, 0.38).
narrative_ontology:measurement(cont_suppress_100, continuity_reading, suppression_requirement, 100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(continuity_reading, discontinuity_reading).
narrative_ontology:affects_constraint(continuity_reading, hybrid_reading).
narrative_ontology:affects_constraint(continuity_reading, classical_purity_enforcement).
narrative_ontology:affects_constraint(continuity_reading, manuscript_authority_transmission).

% DUAL FORMULATION NOTE:
% The continuity reading is one of three constraint stories in the 'correct_latin' kernel family. All three readings share the same kernel (how legitimate Latin is determined) but propose different solutions (continuous evolution, documented rupture, or context-dependent transmission). Each reading has its own ε value, beneficiary structure, and classification. They are linked by network.affects_constraints because each reading influences the others' empirical domain and institutional standing. The ε-invariance principle requires separate stories because the observable used to evaluate 'continuity' (formal change vs. transmission practice) yields different extractiveness values under different measurement methodologies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(continuity_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
