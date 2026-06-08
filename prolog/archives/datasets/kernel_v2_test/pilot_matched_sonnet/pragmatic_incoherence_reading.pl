% ============================================================================
% CONSTRAINT STORY: pragmatic_incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pragmatic_incoherence_reading, []).

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
 *   constraint_id: pragmatic_incoherence_reading
 *   human_readable: Pragmatic Incoherence Reading of Kami-Buddha Ontology
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   The pragmatic incoherence reading of kami-buddha ontology holds that no
 *   unified metaphysical framework existed in pre-modern Japan: practitioners
 *   navigated contradictory frameworks (kami as indigenous deities, buddhas
 *   as enlightened beings, honji suijaku identification,
 *   separate-but-parallel cosmologies) opportunistically without resolving
 *   their incompatibility. This reading is one of three major interpretive
 *   stances on the same historical kernel. The syncretic fusion reading holds
 *   that the frameworks were genuinely integrated into a coherent hybrid
 *   ontology. The domain partition reading holds that kami and buddha
 *   frameworks operated in separate domains without contradiction (kami for
 *   this-worldly benefits, buddhas for soteriological concerns). Each reading
 *   produces different beneficiary/victim structures from the same evidence
 *   base. The pragmatic incoherence reading benefits scholars of
 *   practice-based religion and institutional flexibility while extracting
 *   from doctrinal systematizers who seek coherent metaphysics. The
 *   constraint's theater ratio shows a sharp spike during Meiji
 *   systematization (0.65) when state-sponsored doctrinal clarification
 *   created a theatrical layer of systematic ontology over continuing
 *   practice-level multiplicity, followed by partial decay (0.58
 *   contemporary) as the doctrinal apparatus persists through inertia.
 *   Extractiveness peaks during Meiji (0.48) when systematic reform actively
 *   suppressed pragmatic navigation, then declines but remains elevated
 *   (0.38) as the post-Meiji doctrinal apparatus continues to extract from
 *   practitioners navigating multiple frameworks despite official boundaries.
 *
 * KEY AGENTS:
 *   - Ritual Practitioners: Primary beneficiary (institutional/arbitrage) — gain adaptive flexibility from framework multiplicity; can invoke whichever ontology serves current ritual or political needs
 *   - Institutional Flexibility: Secondary beneficiary (institutional/arbitrage) — religious institutions benefit from ontological ambiguity that permits ritual innovation and political maneuvering
 *   - Ontological Consistency: Primary victim (powerless/identity_locked) — abstract epistemic good that cannot organize or exit; bears full cost of framework incoherence
 *   - Doctrinal Systematizers: Secondary victim (powerless/identity_locked) — scholars and theologians whose professional identity is constituted through systematic ontology construction; identity-locked within commitment to coherent metaphysics
 *   - Meiji Systematization Movement: Organized reformers (organized/mobile) — see pragmatic incoherence as temporary condition with sunset via state-sponsored doctrinal clarification
 *   - Post-Meiji Doctrinal Apparatus: Institutional actor (institutional/constrained) — maintains systematic ontologies theatrically; sees own process as degraded (piton perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pragmatic_incoherence_reading, 0.35).
domain_priors:suppression_score(pragmatic_incoherence_reading, 0.42).
domain_priors:theater_ratio(pragmatic_incoherence_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pragmatic_incoherence_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(pragmatic_incoherence_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(pragmatic_incoherence_reading, theater_ratio, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pragmatic_incoherence_reading, tangled_rope).
narrative_ontology:human_readable(pragmatic_incoherence_reading, "Pragmatic Incoherence Reading of Kami-Buddha Ontology").
narrative_ontology:topic_domain(pragmatic_incoherence_reading, "religious_studies/japanese_history/ontology_of_practice").

domain_priors:requires_active_enforcement(pragmatic_incoherence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(pragmatic_incoherence_reading, 'a455d4b8-13bc-47b8-91a9-0fe0aad9aa1e').
narrative_ontology:cs_kernel_codification('a455d4b8-13bc-47b8-91a9-0fe0aad9aa1e', distributed).
narrative_ontology:cs_authority_grounding('a455d4b8-13bc-47b8-91a9-0fe0aad9aa1e', distributed).
narrative_ontology:cs_reading_relation('a455d4b8-13bc-47b8-91a9-0fe0aad9aa1e', pragmatic_incoherence_reading__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('a455d4b8-13bc-47b8-91a9-0fe0aad9aa1e', pragmatic_incoherence_reading__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('a455d4b8-13bc-47b8-91a9-0fe0aad9aa1e', foundational, ontological_coherence_unnecessary_for_practice).
narrative_ontology:cs_axiom_status(ontological_coherence_unnecessary_for_practice, holdable).
narrative_ontology:cs_axiom_grounding('a455d4b8-13bc-47b8-91a9-0fe0aad9aa1e', ontological_coherence_unnecessary_for_practice, conventional).
narrative_ontology:cs_axiom('a455d4b8-13bc-47b8-91a9-0fe0aad9aa1e', secondary, systematic_theology_illegitimate_imposition).
narrative_ontology:cs_axiom_status(systematic_theology_illegitimate_imposition, holdable).
narrative_ontology:cs_axiom_grounding('a455d4b8-13bc-47b8-91a9-0fe0aad9aa1e', systematic_theology_illegitimate_imposition, instrumental).
narrative_ontology:cs_reference_frame('a455d4b8-13bc-47b8-91a9-0fe0aad9aa1e', pretheoretical_practice_baseline).
narrative_ontology:cs_drift_state('a455d4b8-13bc-47b8-91a9-0fe0aad9aa1e', post_meiji_systematization, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('a455d4b8-13bc-47b8-91a9-0fe0aad9aa1e', '').
narrative_ontology:cs_kernel_id(pragmatic_incoherence_reading, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pragmatic_incoherence_reading, ritual_practitioners).
narrative_ontology:constraint_beneficiary(pragmatic_incoherence_reading, institutional_flexibility).
narrative_ontology:constraint_victim(pragmatic_incoherence_reading, ontological_consistency).
narrative_ontology:constraint_victim(pragmatic_incoherence_reading, doctrinal_systematizers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ONTOLOGICAL SYSTEMATIZER (SNARE) — Identity-locked within the commitment to coherent metaphysics. Cannot exit the framework without abandoning professional identity as a doctrinal scholar. Experiences maximum extraction: the pragmatic incoherence actively suppresses systematic ontology construction. The constraint extracts intellectual labor toward reconciliation projects that the framework itself renders impossible.
constraint_indexing:constraint_classification(pragmatic_incoherence_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: VILLAGE PRACTITIONER (TANGLED ROPE) — Constrained by community expectations and ritual obligations but also benefits from framework flexibility. Experiences mixed extraction: must navigate contradictory frameworks (cost) but gains adaptive capacity to serve diverse community needs (benefit). Can shift between kami and buddha frameworks as situationally appropriate without doctrinal penalty.
constraint_indexing:constraint_classification(pragmatic_incoherence_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL RITUAL AUTHORITY (ROPE) — Benefits from framework multiplicity. Arbitrage-level exit: can invoke whichever ontological framework legitimizes current institutional needs. Experiences the constraint as pure coordination: multiple frameworks provide adaptive repertoire for ritual innovation and political maneuvering. Net beneficiary of the incoherence.
constraint_indexing:constraint_classification(pragmatic_incoherence_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MEIJI SYSTEMATIZATION MOVEMENT (SCAFFOLD) — Organized reformers (Meiji-era Shinto systematizers, Buddhist modernizers) see pragmatic incoherence as a temporary pre-modern condition with a sunset: state-sponsored doctrinal clarification and institutional separation (shinbutsu bunri) will resolve the contradictions. The movement has agency and sees an exit path through systematic reform.
constraint_indexing:constraint_classification(pragmatic_incoherence_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: POST-MEIJI DOCTRINAL APPARATUS (PITON) — The systematic ontologies constructed during Meiji modernization (pure Shinto, rationalized Buddhism) are maintained theatrically but have atrophied functionally. Popular practice continues to navigate multiple frameworks opportunistically despite official doctrinal boundaries. The apparatus persists through institutional inertia, not because it resolved the underlying pragmatic incoherence.
constraint_indexing:constraint_classification(pragmatic_incoherence_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global analytical perspective, the pragmatic incoherence reading itself exhibits tangled-rope structure: it coordinates description of actual practice (genuine function) while extracting from ontological systematizers by naturalizing incoherence as the only viable framework. The reading benefits scholars of practice-based religion while suppressing systematic theology as a legitimate intellectual project.
constraint_indexing:constraint_classification(pragmatic_incoherence_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pragmatic_incoherence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pragmatic_incoherence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pragmatic_incoherence_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(pragmatic_incoherence_reading, TR),
    TR >= 0.70.

:- end_tests(pragmatic_incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The pragmatic incoherence reading itself extracts from ontological systematizers by naturalizing incoherence as the only viable framework, suppressing systematic theology as a legitimate intellectual project. However, extraction is not severe — systematizers can and do produce reconciliation projects, and the framework multiplicity genuinely enables adaptive practice. The value reflects real extraction from coherence-seeking scholars while acknowledging genuine coordination benefits for practitioners. Suppression (0.42): Moderate. Significant barriers to systematic ontology construction include the historical absence of doctrinal authority structures that could adjudicate contradictions, institutional incentives favoring flexibility over coherence, and the pragmatic success of opportunistic navigation. Suppression spikes during Meiji (0.72) when state power actively enforced doctrinal boundaries, then declines (0.42 contemporary) but remains elevated as post-Meiji institutional structures continue to suppress certain forms of practice-level syncretism. Theater ratio (0.28 baseline, 0.65 Meiji peak): Low in pre-modern period — pragmatic navigation was functional, not performative. Spikes during Meiji systematization when doctrinal clarification created a theatrical layer over continuing practice multiplicity. Partial decay in contemporary period (0.58) as the doctrinal apparatus persists through inertia despite limited functional grip on popular practice.
 *
 * PERSPECTIVAL GAP:
 *   The ontological systematizer sees a snare — identity-locked within the commitment to coherent metaphysics, experiencing maximum extraction as the framework actively suppresses systematic ontology construction. The village practitioner sees tangled rope — constrained by contradictory frameworks but benefiting from adaptive flexibility. The institutional ritual authority sees pure coordination (rope) — framework multiplicity provides adaptive repertoire with no experienced cost. The Meiji systematization movement sees a temporary problem with a sunset (scaffold) — state-sponsored reform will resolve the contradictions. The post-Meiji doctrinal apparatus sees its own degraded ritual (piton) — systematic ontologies are maintained theatrically over continuing practice-level multiplicity. The analytical observer sees tangled rope at the meta-level — the pragmatic incoherence reading itself coordinates and extracts simultaneously. The perspectival gap reveals that 'incoherence' is not a neutral description but a structural position that benefits some agents (practitioners, institutions) while extracting from others (systematizers).
 *
 * DIRECTIONALITY LOGIC:
 *   Ritual practitioners and institutional authorities are beneficiaries — they gain adaptive capacity from framework multiplicity and experience low effective extraction. Ontological systematizers are victims — their professional identity is constituted through coherent metaphysics construction, and the pragmatic incoherence reading actively suppresses this project. The identity-lock is cognitive: systematizers cannot exit the commitment to ontological coherence without abandoning their scholarly identity. Village practitioners experience mixed extraction (tangled rope): they must navigate contradictory frameworks (cost) but gain situational flexibility (benefit). The Meiji systematization movement has agency and sees an exit path (scaffold), but the post-Meiji doctrinal apparatus recognizes its own process as degraded (piton). The analytical observer perspective itself exhibits tangled-rope structure: the pragmatic incoherence reading coordinates description of actual practice while extracting from systematic theology.
 *
 * MANDATROPHY ANALYSIS:
 *   The pragmatic incoherence reading resolves mandatrophy by revealing that the absence of unified ontology is not a neutral historical fact but a constraint that coordinates and extracts simultaneously. The reading benefits scholars of practice-based religion (who gain a framework for describing actual behavior) and institutional flexibility (which gains legitimacy for adaptive ritual innovation) while extracting from doctrinal systematizers (whose intellectual project is naturalized as impossible or illegitimate). The tangled-rope classification at the analytical level is critical: the reading itself exhibits the structure it describes. The Meiji systematization movement's scaffold perspective shows that the constraint was not perceived as immutable — organized reformers saw an exit path through state-sponsored doctrinal clarification. The piton perspective of the post-Meiji doctrinal apparatus shows that the 'resolution' was largely theatrical — systematic ontologies were constructed but did not functionally replace practice-level multiplicity. The constraint's mandate (enabling adaptive practice) has not outlived its function, but the Meiji-era attempt to eliminate it created a degraded doctrinal layer that persists through inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is pragmatic incoherence the actual historical structure, or is it one interpretive reading of a contested kernel (kami-buddha ontology) that competes with syncretic fusion and domain partition readings?',
    'Cross-reading analysis: if syncretic fusion or domain partition readings produce different beneficiary/victim structures from the same historical evidence, the kernel is contested and this is one reading. If all readings converge on the same structure, the kernel is resolved.',
    'If contested kernel: this constraint is one reading among siblings, and the committer axis (which reading a scholar adopts) becomes a measurable structural choice. If resolved: this constraint describes the actual historical structure, not an interpretive stance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether pragmatic incoherence is historical fact or interpretive reading').

omega_variable(
    incoherence_vs_pretheoretical,
    'Does the absence of unified ontology constitute incoherence (contradictory frameworks operating simultaneously) or pretheoretical practice (no framework yet constructed)?',
    'Historical evidence of explicit contradiction recognition: if practitioners acknowledged incompatibility but continued practice, it is incoherence. If no evidence of contradiction recognition exists, it may be pretheoretical.',
    'If pretheoretical: extractiveness is lower (no suppression of systematization, just absence of it). If incoherent: extractiveness is higher (active navigation of recognized contradictions extracts cognitive labor).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incoherence_vs_pretheoretical, empirical, 'Whether absence of ontology is incoherence or pretheoretical state').

omega_variable(
    opportunism_vs_strategic_ambiguity,
    'Is opportunistic framework navigation a practitioner survival strategy (agency) or a structural feature imposed by institutional power (extraction)?',
    'Power analysis: if practitioners with institutional backing navigate frameworks differently than those without, opportunism is strategic agency. If navigation patterns are uniform across power levels, it is structural feature.',
    'If strategic agency: practitioners are beneficiaries (using flexibility). If structural imposition: practitioners are victims (forced to navigate incoherence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opportunism_vs_strategic_ambiguity, empirical, 'Whether opportunistic navigation is agency or structural extraction').

omega_variable(
    meiji_resolution_success,
    'Did Meiji-era systematization actually resolve the pragmatic incoherence, or did it create a theatrical doctrinal layer over continuing practice-level multiplicity?',
    'Post-Meiji practice analysis: if popular religious practice after shinbutsu bunri continues to navigate multiple frameworks, systematization failed functionally (piton). If practice conforms to doctrinal boundaries, systematization succeeded (scaffold sunset achieved).',
    'If failed: scaffold perspective is aspirational, piton perspective is structural reality. If succeeded: scaffold sunset was real, and contemporary practice operates under different constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(meiji_resolution_success, empirical, 'Whether Meiji systematization resolved or merely theatricalized the incoherence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pragmatic_incoherence_reading, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prag_incoh_theater_heian, pragmatic_incoherence_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(prag_incoh_theater_kamakura, pragmatic_incoherence_reading, theater_ratio, 400, 0.22).
narrative_ontology:measurement(prag_incoh_theater_edo, pragmatic_incoherence_reading, theater_ratio, 800, 0.28).
narrative_ontology:measurement(prag_incoh_theater_meiji, pragmatic_incoherence_reading, theater_ratio, 900, 0.65).
narrative_ontology:measurement(prag_incoh_theater_contemporary, pragmatic_incoherence_reading, theater_ratio, 1000, 0.58).

% Extraction over time
narrative_ontology:measurement(prag_incoh_extract_heian, pragmatic_incoherence_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(prag_incoh_extract_kamakura, pragmatic_incoherence_reading, base_extractiveness, 400, 0.28).
narrative_ontology:measurement(prag_incoh_extract_edo, pragmatic_incoherence_reading, base_extractiveness, 800, 0.35).
narrative_ontology:measurement(prag_incoh_extract_meiji, pragmatic_incoherence_reading, base_extractiveness, 900, 0.48).
narrative_ontology:measurement(prag_incoh_extract_contemporary, pragmatic_incoherence_reading, base_extractiveness, 1000, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(prag_incoh_suppress_heian, pragmatic_incoherence_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(prag_incoh_suppress_edo, pragmatic_incoherence_reading, suppression_requirement, 800, 0.35).
narrative_ontology:measurement(prag_incoh_suppress_meiji, pragmatic_incoherence_reading, suppression_requirement, 900, 0.72).
narrative_ontology:measurement(prag_incoh_suppress_contemporary, pragmatic_incoherence_reading, suppression_requirement, 1000, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pragmatic_incoherence_reading, identity_coordination).
narrative_ontology:affects_constraint(pragmatic_incoherence_reading, syncretic_fusion_reading).
narrative_ontology:affects_constraint(pragmatic_incoherence_reading, domain_partition_reading).

% DUAL FORMULATION NOTE:
% The pragmatic incoherence reading is one of three constraint stories decomposed from the natural-language concept 'kami-buddha relationship.' Each reading has its own extractiveness value reflecting different beneficiary/victim structures. The readings are linked via network.affects_constraints because adopting one reading creates structural pressure on the others (influences relation) — a scholar who adopts pragmatic incoherence must explain away evidence of systematic integration that the syncretic fusion reading takes as primary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
