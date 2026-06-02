% ============================================================================
% CONSTRAINT STORY: incoherent_bundle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_incoherent_bundle, []).

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
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: incoherent_bundle
 *   human_readable: Shinbutsu-shugo as Institutionally Sustained Incoherent Bundle
 *   domain: religious_studies/philosophy_of_religion/japanese_cultural_history
 *
 * SUMMARY:
 *   Shinbutsu-shugo (神仏習合, fusion of kami and Buddha) is standardly treated
 *   as a coherent religious phenomenon — a syncretic system integrating
 *   Shinto and Buddhism across Japanese institutional, ritual, and
 *   philosophical life. However, this constraint instantiates an alternative
 *   reading: shinbutsu-shugo is not a coherent kernel but an institutionally
 *   sustained bundle of contradictory commitments. The structure
 *   simultaneously affirms fusion (kami are manifestations of Buddha, or
 *   share Buddha-nature) AND separation (kami are fundamentally distinct from
 *   Buddha, requiring parallel but independent ritual devotion). It claims
 *   both hierarchy (honji-suijaku: Buddha is original essence, kami is
 *   manifestation) AND reciprocity (kami and Buddha mutually depend on each
 *   other's worship). It presents itself as a systematic philosophy yet
 *   resists systematization — every attempt to impose coherent logic
 *   (Neo-Confucian schemes, domain partition theories) fails when confronted
 *   with lived ritual practice. The institution maintains all these
 *   contradictions simultaneously without resolving them, extracting from
 *   practitioners the labor of holding incoherent commitments while enjoying
 *   the institutional flexibility this incoherence provides. The constraint
 *   is neither a genuine natural law (the contradictions are not logically
 *   necessary) nor a pure coordination mechanism (the coordination function
 *   is obscured by the incoherence itself). It is tangled rope: institutional
 *   actors benefit from the inability to systematize, while practitioners
 *   bear the cost of maintaining unsustainable commitments.
 *
 * KEY AGENTS:
 *   - Institutional Religious Authorities (temple-shrine complexes, priestly hierarchy): Primary beneficiary (institutional/arbitrage) — capture legitimacy and resources from both Buddhist and Shinto communities; benefit from incoherence's flexibility
 *   - Syncretic Practitioners: Primary victim (moderate/identity_locked) — identity fused with syncretic practice; bear cost of maintaining incompatible commitments; cannot exit without ceasing to be 'proper' practitioners
 *   - Theological Coherence Seekers: Secondary victim (powerless/trapped) — unable to escape through rational systematization; contradictions are enforced as unsystematizable within institutional discourse
 *   - Neo-Confucian and Edo Systematizers: Organized effort (organized/constrained) — attempted to resolve incoherence through frameworks like honji-suijaku; efforts repeatedly failed and degraded into theatrical performance
 *   - Contemporary Academic Framework: Institutional observer (institutional/arbitrage) — names the phenomenon as 'syncretism' or 'dual structure' but does not explain the mechanism; framework maintained through disciplinary inertia
 *   - Logical Analysis: Analytical observer (analytical/analytical) — sees the contradictions as natural-law-like inevitabilities, risking naturalization of institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(incoherent_bundle, 0.52).
domain_priors:suppression_score(incoherent_bundle, 0.58).
domain_priors:theater_ratio(incoherent_bundle, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(incoherent_bundle, extractiveness, 0.52).
narrative_ontology:constraint_metric(incoherent_bundle, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(incoherent_bundle, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(incoherent_bundle, tangled_rope).
narrative_ontology:human_readable(incoherent_bundle, "Shinbutsu-shugo as Institutionally Sustained Incoherent Bundle").
narrative_ontology:topic_domain(incoherent_bundle, "religious_studies/philosophy_of_religion/japanese_cultural_history").

domain_priors:requires_active_enforcement(incoherent_bundle).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(incoherent_bundle, 'f18b03f5-57f0-4ecd-8f26-92ba7a0f6c90').
narrative_ontology:cs_created_at('f18b03f5-57f0-4ecd-8f26-92ba7a0f6c90', '').
narrative_ontology:cs_kernel_codification('f18b03f5-57f0-4ecd-8f26-92ba7a0f6c90', distributed).
narrative_ontology:cs_authority_grounding('f18b03f5-57f0-4ecd-8f26-92ba7a0f6c90', practice).
narrative_ontology:cs_interpretation_layer_present('f18b03f5-57f0-4ecd-8f26-92ba7a0f6c90').
narrative_ontology:cs_kernel_id(incoherent_bundle, kami_buddha_ontology).
narrative_ontology:cs_reading_relation('f18b03f5-57f0-4ecd-8f26-92ba7a0f6c90', honji_suijaku_monism, coexists_with).
narrative_ontology:cs_reading_relation('f18b03f5-57f0-4ecd-8f26-92ba7a0f6c90', domain_partition, coexists_with).
narrative_ontology:cs_axiom('f18b03f5-57f0-4ecd-8f26-92ba7a0f6c90', foundational, incoherence_permanent_feature).
narrative_ontology:cs_axiom_status(incoherence_permanent_feature, holdable).
narrative_ontology:cs_axiom_grounding('f18b03f5-57f0-4ecd-8f26-92ba7a0f6c90', incoherence_permanent_feature, conventional).
narrative_ontology:cs_axiom('f18b03f5-57f0-4ecd-8f26-92ba7a0f6c90', secondary, systematization_repeatedly_fails).
narrative_ontology:cs_axiom_status(systematization_repeatedly_fails, holdable).
narrative_ontology:cs_axiom_grounding('f18b03f5-57f0-4ecd-8f26-92ba7a0f6c90', systematization_repeatedly_fails, empirically_contingent).
narrative_ontology:cs_reference_frame('f18b03f5-57f0-4ecd-8f26-92ba7a0f6c90', institutional_flexibility_through_ambiguity).
narrative_ontology:cs_drift_state('f18b03f5-57f0-4ecd-8f26-92ba7a0f6c90', contemporary_post_meiji_restoration, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(incoherent_bundle, institutional_religious_authorities).
narrative_ontology:constraint_beneficiary(incoherent_bundle, ritual_practitioners).
narrative_ontology:constraint_victim(incoherent_bundle, ontological_coherence).
narrative_ontology:constraint_victim(incoherent_bundle, theological_clarity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THEOLOGICAL COHERENCE SEEKER (SNARE) — The agent trapped within the contradictory framework cannot escape through rational systematization. Attempting to resolve the fusion-separation paradox within institutional discourse produces recursive contradiction. The contradictions are enforced as unsystematizable — exit requires abandoning the entire institutional framework. Maximum extraction experienced as cognitive lock.
constraint_indexing:constraint_classification(incoherent_bundle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SYNCRETIC PRACTITIONER (TANGLED ROPE) — The practitioner benefits from the practical efficacy of the contradictory system (ritual performance works; both kami and Buddha are serviceable) while also bearing the cost of maintaining incompatible commitments. Identity fused with syncretic practice — cannot exit without ceasing to be a 'proper' practitioner. Both coordination (ritual efficacy) and asymmetric extraction (theoretical coherence withheld) present.
constraint_indexing:constraint_classification(incoherent_bundle, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL RELIGIOUS AUTHORITY (ROPE) — The temple-shrine complex benefits from the incoherence itself — maintaining ambiguous commitments allows simultaneous claims of both Buddhist legitimacy and Shinto authenticity. Experiences the constraint as pure coordination: managing contradictions enables institutional flexibility and resource capture from both religious communities. Low experienced extraction — net beneficiary position.
constraint_indexing:constraint_classification(incoherent_bundle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: NEO-CONFUCIAN SYSTEMATIZER (SCAFFOLD) — Organized intellectual effort (particularly during Edo period) to impose rational hierarchy (honji-suijaku systematization, domain partition schemes) on the contradictory bundle. The effort to resolve incoherence through systematization is temporary — it always fails when confronted with lived ritual practice. Theater high: systematic frameworks are performative, superseded by institutional inertia. Sunset: each systematization attempt creates its own degradation.
constraint_indexing:constraint_classification(incoherent_bundle, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONTEMPORARY ACADEMIC FRAMEWORK (PITON) — The academic study of shinbutsu-shugo as a coherent 'syncretism' or 'dual structure' is largely performative categorization. The framework names the phenomenon but does not explain the mechanism sustaining the contradictions. Theater high (0.68+): academic systematization persists through institutional inertia in religious studies departments despite failing to resolve the structural contradiction. The framework is maintained because no replacement has fully superseded it in the disciplinary apparatus.
constraint_indexing:constraint_classification(incoherent_bundle, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LOGICAL CONTRADICTION AS NATURAL LAW (MOUNTAIN) — From a universal analytical context, the constraint appears as an inherent logical structure: simultaneous affirmation of contradictory ontologies cannot be 'resolved' without abandoning one side, making the contradiction an immutable logical limit. However, the structural data reveals this as a false summit — the 'irresolvable' nature is not a logical law but an institutional choice to sustain incoherence despite capacity to choose otherwise.
constraint_indexing:constraint_classification(incoherent_bundle, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(incoherent_bundle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(incoherent_bundle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(incoherent_bundle, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(incoherent_bundle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(incoherent_bundle, TR),
    TR >= 0.70.

:- end_tests(incoherent_bundle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The institutional apparatus extracts theoretical coherence from the system while allowing practitioners to operate functionally. Practitioners pay the cost of maintaining contradictory commitments; institutions capture the flexibility benefit of never having to choose. The extraction is not maximal because the system also provides genuine ritual efficacy and practitioners derive genuine benefits from syncretic practice. Over the measurement interval (0-400 years, representing Heian through contemporary periods), extractiveness has risen from 0.35 to 0.52 as systematization attempts (and their failures) have accumulated — the incoherence is increasingly enforced as a permanent feature rather than a provisional state. Suppression (0.58): Moderate-high. The barrier to coherence is not material but epistemic and institutional: practitioners who attempt to systematize or choose between commitments face institutional resistance, loss of authority recognition, and reputational cost. Attempts at separation (Meiji Shinto nationalism) and systematization (honji-suijaku during Edo) are permitted but fail — not because they are logically impossible but because institutional incentives work against their success. Theater ratio (0.68): High. Both the systematization frameworks (honji-suijaku, domain partition, Edo theological schemes) and the contemporary academic categorization of shinbutsu-shugo as a coherent phenomenon are substantially performative. The frameworks name the structure but do not explain how incoherence is sustained despite institutional capacity to choose otherwise. Theater has risen from 0.42 (early period: sincere systematization attempts) to 0.68 (contemporary: incoherence maintained as unstated institutional strategy).
 *
 * PERSPECTIVAL GAP:
 *   The gap between institutional authorities (who see coordination and flexibility) and practitioners (who experience cognitive and identity lock) is the diagnostic signature of this constraint. The gap reveals that what appears to practitioners as an immutable contradiction is actually an institutional choice sustained by beneficiaries who profit from the incoherence. The academic framework fills this gap by naming the phenomenon 'syncretism,' which names without explaining — it treats incoherence as an already-existing feature rather than asking whether institutions deliberately maintain it.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional religious authorities (beneficiaries with arbitrage exit) experience low effective extraction — they designed the system to work for them and can exit the framework through reframing (as when Japanese imperial nationalism imposed Shinto separation). The syncretic practitioners (moderate power with identity_locked exit) experience high effective extraction — they cannot exit through rational choice because their identity is fused with the contradictory practice. The theological coherence seekers (powerless/trapped) experience maximum extraction — the institution actively prevents coherent systematization and blocks exit. The directionality flow is from practitioners and coherence-seekers toward institutional authorities. The analytical observer risks naturalizing this institutional choice as a logical necessity, which would be a false summit — the incoherence appears inevitable only because the institution enforces it as such.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This reading does not collapse into a single type because the incoherence itself is the structure. From the institutional perspective, the constraint is Rope — managing contradictions enables coordination. From the practitioner's perspective, it is Snare — the system extracts coherence. From the systematizer's perspective, it is Scaffold — repeated attempts to impose order that repeatedly fail. From the logical perspective, it is Mountain — contradiction appears necessary. The reading resolves mandatrophy by claiming: all six perspectives are accurate descriptions of the structure, and the reason they produce different types is that the structure IS contradictory — it genuinely instantiates incompatible commitments. This is not a case of perspectival multiplicity on a coherent constraint; it is a case where the constraint itself embodies the contradiction. The mandatrophy is resolved by accepting that the constraint cannot be reduced to a single type because its nature is incoherent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incoherence_deliberate_or_incidental,
    'Is the bundle''s incoherence a deliberate institutional strategy (supporting flexible legitimacy) or an incidental artifact of historical layering without intentional maintenance?',
    'Historical analysis of explicit institutional choice points: moments when authorities explicitly chose to sustain contradictions vs. moments of passive inheritance. Examination of internal institutional discourse about coherence and systematization efforts.',
    'If deliberate: constraint is Snare (institutional authorities extract coherence denial from practitioners while enjoying flexibility). If incidental: constraint is Piton (institutional inertia sustaining non-functional contradiction). The distinction changes classification from high intentional extraction to low degradation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incoherence_deliberate_or_incidental, empirical, 'Whether incoherence is deliberate institutional strategy or incidental historical artifact').

omega_variable(
    ritual_efficacy_independent_of_ontology,
    'Does the practical efficacy of syncretic ritual practice actually depend on any underlying ontological framework, or is efficacy purely pragmatic and independent of whether fusion, separation, or contradiction is theoretically posited?',
    'Comparative analysis of ritual outcomes under different ontological framings; identification of whether practitioners'' beliefs about kami-Buddha relationship affect ritual effectiveness. Cross-cultural comparison with other syncretic systems (Afro-Caribbean orishas, Mexican saints-spirits).',
    'If efficacy independent: the incoherence is purely institutional theater masking pragmatic systems (Piton). If efficacy dependent: the incoherence serves genuine coordination function despite theoretical contradiction (Tangled Rope confirmed). If efficacy requires internal coherence: the system is extracting theoretical clarity while practitioners operate functionally (Snare confirmed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ritual_efficacy_independent_of_ontology, empirical, 'Whether ritual efficacy depends on underlying ontological coherence').

omega_variable(
    institutional_capacity_to_choose_coherence,
    'Does the institutional apparatus possess the structural capacity to impose coherence (choosing either fusion, separation, or explicit contradiction as canon) but deliberately choose not to?',
    'Historical examination of moments when coherence was imposed (e.g., Edo period systematization attempts, Meiji Shinto nationalism). Analysis of why these impositions failed or were abandoned. Counterfactual: what would prevent current institutions from adopting one coherent framework if they chose to?',
    'If capacity exists but is deliberately withheld: extraction is intentional (Snare). If capacity does not exist: constraint is structural limit (Mountain or Piton). If capacity exists but choosing coherence would destroy institutional flexibility: incoherence is the coordination mechanism (Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capacity_to_choose_coherence, empirical, 'Whether institutions possess but deliberately reject capacity to impose coherence').

omega_variable(
    honji_suijaku_monism_viability,
    'Is the honji-suijaku monistic framework (Buddha as original essence, kami as manifestation) itself coherent when systematically articulated, or does it inevitably collapse into the same contradictions it was designed to resolve?',
    'Formal logical analysis of honji-suijaku doctrine. Reconstruction of historical Neo-Confucian and Buddhist-logician attempts at systematization. Identification of specific logical points where the framework breaks down when applied to actual ritual and devotional practices.',
    'If honji-suijaku is coherent: the incoherent bundle reading is incomplete (sibling ''honji_suijaku_monism'' is a live alternative). If honji-suijaku collapses: the incoherent bundle reading is confirmed as unavoidable (forecloses the monism reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(honji_suijaku_monism_viability, conceptual, 'Whether honji-suijaku monism remains coherent under rigorous systematization').

omega_variable(
    reading_committer_frame_ambiguity,
    'Is this reading instantiating the incoherence as a permanent ontological or institutional feature, or as a contingent historical state that might be resolved through further systematization attempts?',
    'Clarification of whether the reading claims: (a) incoherence is the true nature of shinbutsu-shugo (metaphysical claim), (b) incoherence is a feature of how institutions currently sustain the structure (institutional claim), or (c) incoherence reveals that no single kernel (kami-Buddha ontology) can adjudicate the commitments — the kernel itself is composite and under-specified.',
    'If (a): the constraint is a mountain (logical necessity). If (b): the constraint is tangled rope or piton (institutional choice). If (c): the kernel_id ''kami_buddha_ontology'' is misspecified — there is no single kernel, only institutional layering.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_frame_ambiguity, conceptual, 'Committer-frame specification of what ''incoherent bundle'' instantiates').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(incoherent_bundle, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(incoh_tr_t0, incoherent_bundle, theater_ratio, 0, 0.42).
narrative_ontology:measurement(incoh_tr_t200, incoherent_bundle, theater_ratio, 200, 0.55).
narrative_ontology:measurement(incoh_tr_t400, incoherent_bundle, theater_ratio, 400, 0.68).

% Extraction over time
narrative_ontology:measurement(incoh_be_t0, incoherent_bundle, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(incoh_be_t200, incoherent_bundle, base_extractiveness, 200, 0.45).
narrative_ontology:measurement(incoh_be_t400, incoherent_bundle, base_extractiveness, 400, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(incoherent_bundle, identity_coordination).
narrative_ontology:affects_constraint(incoherent_bundle, honji_suijaku_monism).
narrative_ontology:affects_constraint(incoherent_bundle, domain_partition).

% DUAL FORMULATION NOTE:
% The three readings of the kami_buddha_ontology kernel (incoherent_bundle, honji_suijaku_monism, domain_partition) represent three competing structuralizations of shinbutsu-shugo. Each has different ε: the incoherent_bundle reading (this file) has ε=0.52 (tangled rope, institutional contradiction); honji_suijaku_monism would have lower ε (attempting coherent systematization); domain_partition would have different extraction structure (clean separation). These are not three perspectives on one constraint; they are three distinct constraints instantiated by three different readings of the same kernel. They are linked via network.affects_constraints because systematization attempts (honji-suijaku, domain partition) are causal responses to the incoherent bundle and feed back into institutional structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(incoherent_bundle, powerless, 0.96).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
