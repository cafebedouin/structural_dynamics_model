% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__hybrid_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__hybrid_transformation_reading, []).

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
 *   constraint_id: catastrophe_memory_function__hybrid_transformation_reading
 *   human_readable: Catastrophe Memory Function: Hybrid Transformation Reading (Mourning + Survival Competence)
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of a contested kernel: the
 *   catastrophe_memory_function kernel. The kernel is the stabilized
 *   commitment that ritual encodes collective memory in response to
 *   catastrophe. Three different readings decompose this kernel into
 *   structurally distinct claims. This story instantiates the
 *   hybrid_transformation_reading: ritual simultaneously encodes
 *   mourning-practice (preserving loss-memory, boundary-norms, group
 *   identity) AND survival-competence (transmitting adaptive mechanisms,
 *   institutional continuity knowledge, decentralized resilience). The
 *   Passover ritual exemplifies this hybrid: bitter herbs (maror) and
 *   lamentation liturgy encode mourning (D1/D4), while the seder performance
 *   and narrative structure encode institutional survival knowledge (D5). The
 *   constraint's extractiveness (0.38) reflects that canonical form
 *   enforcement extracts conformity costs beyond what the core transmission
 *   function requires. Theater ratio (0.65) reflects that the ritual has
 *   become increasingly performative — many participants execute the seder
 *   without recognizing the survival-rehearsal component; many recite
 *   loss-memory without activating institutional continuity knowledge.
 *   Suppression (0.52) reflects barriers to heterodox interpretation:
 *   deviation from prescribed form triggers social sanction or spiritual
 *   exclusion. The hybrid_transformation_reading generates a tangled_rope
 *   classification at the analytical center: genuine coordination function
 *   (ritual does preserve both mourning and survival) alongside asymmetric
 *   enforcement (canonical form is coerced regardless of participant's
 *   adaptive capacity).
 *
 * KEY AGENTS:
 *   - Participant Constrained by Ritual Form (powerless/trapped): Individual practitioner or heterodox interpreter faces maximum extraction. Coerced participation in prescribed form; exit risks community exclusion.
 *   - Adaptive Practitioner (moderate/constrained): Community member recognizing the hybrid function but facing barriers to modification. Benefits from survival-competence transmission but constrained by enforcement costs.
 *   - Ritual Authority (institutional/arbitrage): Religious establishment, rabbinical stewards. Benefits from canonical form maintenance; experiences coordination function as institutional validation.
 *   - Innovating Diaspora Community (organized/mobile): Reform movements, adaptive communities. See the enforcement as temporary; developing alternative performance modes maintaining both functions.
 *   - Ceremonial Performance System (institutional/arbitrage): The ritualized apparatus itself, treating mourning and survival as vestigial within performative form.
 *   - Analytical Observer (analytical/analytical): Civilizational perspective risking naturalization of contingent institutional arrangements.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__hybrid_transformation_reading, 0.38).
domain_priors:suppression_score(catastrophe_memory_function__hybrid_transformation_reading, 0.52).
domain_priors:theater_ratio(catastrophe_memory_function__hybrid_transformation_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__hybrid_transformation_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_function__hybrid_transformation_reading, "Catastrophe Memory Function: Hybrid Transformation Reading (Mourning + Survival Competence)").
narrative_ontology:topic_domain(catastrophe_memory_function__hybrid_transformation_reading, "religious_studies/ritual_theory/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_function__hybrid_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__hybrid_transformation_reading, '21dcb403-0ac2-45ed-a1eb-66a3a116f472').
narrative_ontology:cs_kernel_codification('21dcb403-0ac2-45ed-a1eb-66a3a116f472', formalized).
narrative_ontology:cs_authority_grounding('21dcb403-0ac2-45ed-a1eb-66a3a116f472', lineage).
narrative_ontology:cs_interpretation_layer_present('21dcb403-0ac2-45ed-a1eb-66a3a116f472').
narrative_ontology:cs_reading_relation('21dcb403-0ac2-45ed-a1eb-66a3a116f472', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('21dcb403-0ac2-45ed-a1eb-66a3a116f472', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_axiom('21dcb403-0ac2-45ed-a1eb-66a3a116f472', foundational, ritual_encodes_both_mourning_and_survival).
narrative_ontology:cs_axiom_status(ritual_encodes_both_mourning_and_survival, holdable).
narrative_ontology:cs_axiom_grounding('21dcb403-0ac2-45ed-a1eb-66a3a116f472', ritual_encodes_both_mourning_and_survival, deontological).
narrative_ontology:cs_axiom('21dcb403-0ac2-45ed-a1eb-66a3a116f472', foundational, hybrid_form_necessary_for_dual_transmission).
narrative_ontology:cs_axiom_status(hybrid_form_necessary_for_dual_transmission, holdable).
narrative_ontology:cs_axiom_grounding('21dcb403-0ac2-45ed-a1eb-66a3a116f472', hybrid_form_necessary_for_dual_transmission, empirically_contingent).
narrative_ontology:cs_reference_frame('21dcb403-0ac2-45ed-a1eb-66a3a116f472', canonical_ritual_form_as_unified_commemorative_apparatus).
narrative_ontology:cs_drift_state('21dcb403-0ac2-45ed-a1eb-66a3a116f472', contemporary_diaspora_adaptation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('21dcb403-0ac2-45ed-a1eb-66a3a116f472', '2026-02-26T14:32:15Z').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, ritual_authorities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, continuity_preserving_agents).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, individual_heterodox_interpretation).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, adaptive_innovation_outside_ritual_frame).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PARTICIPANT TRAPPED IN RITUAL FORM (SNARE) — Individual participant or heterodox interpreter faces maximum extraction. The ritual prescribes exact form (bitter herbs, seder sequence, liturgical language). Deviation triggers social exclusion or spiritual sanction. Participant cannot exit without abandoning community identity. Experiences the constraint as pure extraction: coerced participation in commemorative form regardless of personal survival practice or mourning style.
constraint_indexing:constraint_classification(catastrophe_memory_function__hybrid_transformation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ADAPTIVE PRACTITIONER (TANGLED ROPE) — Community member who recognizes the hybrid function (mourning + survival rehearsal) but faces barriers to modifying ritual form. Benefits from the ritual's transmission of survival competence (institutional continuity knowledge, decentralized resilience patterns). Constrained by career risk (loss of ritual leadership), relational cost (family conflict over observance), and epistemic cost (marginalization as heretic). Genuine coordination function present (ritual does preserve both mourning and survival capability) alongside asymmetric extraction (enforced conformity).
constraint_indexing:constraint_classification(catastrophe_memory_function__hybrid_transformation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RITUAL AUTHORITY (ROPE) — Religious establishment, rabbinical authorities, liturgical stewards. Benefits from the constraint's maintenance of canonical form and institutional control over interpretation. Experiences the ritual as pure coordination: communication of loss-memory ensures group cohesion across diaspora; transmission of survival competence ensures institutional legitimacy across generations. Low extraction cost because the authority profits from coordination itself — the more perfectly the ritual transmits, the more the authority's stewardship is validated. Arbitrage option: can shift interpretation within hermeneutical bounds without losing institutional standing.
constraint_indexing:constraint_classification(catastrophe_memory_function__hybrid_transformation_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INNOVATING DIASPORA COMMUNITY (SCAFFOLD) — Organized reform movements (Reconstructionist Judaism, contextual Passover adaptations, secular Holocaust memorial rituals) see the constraint as a temporary bottleneck with architectural sunset. The hybrid form (mourning + survival) can be re-weighted or re-performed without abandoning the kernel. Haroset (bitter herbs) adapts to local agronomic realities; seder narrative accommodates contemporary survival challenges (displacement, institutional fragmentation, digital continuity). These agents see the ritual's enforcement as temporary — as adaptive capacity grows, the need for canonical form diminishes. Lowest effective extraction because the coalition has agency, sees an exit path (adaptive performance maintaining both functions), and interprets the sunset as already underway.
constraint_indexing:constraint_classification(catastrophe_memory_function__hybrid_transformation_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CEREMONIAL PERFORMANCE SYSTEM (PITON) — The ritualized Passover apparatus (hagaddah recitation, prescribed foods, liturgical sequence) has become substantially performative. The original functions (mourning-practice, survival-competence transmission) persist in the form, but the form's relationship to its function has attenuated. Many participants perform the seder without recognizing the survival-rehearsal component (D5); many recite loss-memory without activating institutional continuity knowledge. The ritual persists through commemorative obligation and community norm, not because the performance optimally transmits either function. Theater_ratio reflects this degradation: the form is maintained for tradition's sake, the functions are vestigial within it.
constraint_indexing:constraint_classification(catastrophe_memory_function__hybrid_transformation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some ritual formalization is inherent to collective memory: loss-memory requires commemorative structure, survival-competence requires institutional form, and the fusion of mourning and adaptation is a universal feature of cultures that endure catastrophe. This perspective sees the ritual's canonical form as an immutable property of how human communities preserve identity through trauma. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that the 'natural to human culture' framing naturalizes what is actually a contingent historical arrangement.
constraint_indexing:constraint_classification(catastrophe_memory_function__hybrid_transformation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophe_memory_function__hybrid_transformation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophe_memory_function__hybrid_transformation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(catastrophe_memory_function__hybrid_transformation_reading, TR),
    TR >= 0.70.

:- end_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint enforces canonical ritual form as the apparatus for transmitting both mourning-practice and survival-competence. This enforcement extracts conformity costs — individuals must participate in prescribed form regardless of personal adaptive capacity or mourning style. However, the extraction is not maximal because the ritual's core functions (transmitting loss-memory and institutional continuity knowledge) are genuine coordination goods. The measurement trajectory (0.22 → 0.38 over 50 intervals) reflects historical intensification: as institutional Judaism developed formalized liturgy and standardized practice across diaspora, the enforcement apparatus grew more rigid, increasing extraction costs for adaptive practitioners. Theater ratio (0.65): Moderately high. The seder performance, haggadah recitation, and prescribed foods function as communicative vehicles for both mourning and survival knowledge. However, the performance has become increasingly detached from function — many participants execute the ritual without understanding the survival-rehearsal encoding (D5); many recite loss-memory as commemorative obligation rather than identity-preserving transmission. The measurement trajectory (0.48 → 0.65) reflects increasing performativity as institutional standardization has displaced lived practice knowledge. Suppression (0.52): Moderate-high. The constraint suppresses heterodox interpretation through social sanction (family conflict, community exclusion) and spiritual framing (violation of commandment, breach of covenant). However, suppression is not total — significant communities have successfully adapted canonical form (Reconstructionism, secular memorials, diaspora innovations) without complete exclusion. The measurement trajectory (0.38 → 0.52) reflects intensified enforcement during periods of institutional anxiety (diaspora fragmentation, Enlightenment challenge, post-Holocaust recontextualization).
 *
 * PERSPECTIVAL GAP:
 *   The hybrid_transformation_reading generates perspectival variance across all six types. The powerless participant experiences the constraint as pure extraction (Snare) — coerced participation in form regardless of their adaptive needs. The adaptive practitioner experiences tangled coordination and extraction (Tangled Rope) — genuine transmission of survival competence alongside enforced conformity. The ritual authority experiences pure coordination (Rope) — the more perfectly the ritual transmits, the more validated the stewardship. The innovating community experiences a temporary problem with architectural sunset (Scaffold) — adaptive performance can maintain both functions while relaxing canonical form. The ceremonial system itself has become degraded (Piton) — the form persists through obligation, the functions are vestigial within it. The analytical observer risks seeing immutable natural law (Mountain) — ritual formalization as inherent to human collective memory — but the structural data reveals contingent institutional arrangements. The perspectival gap demonstrates that the hybrid_transformation_reading is not 'the correct answer' but rather a specific reading that actualizes differently across positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) in this constraint is determined by each agent's structural relationship to the enforcement apparatus. The powerless participant faces maximum d (0.95, nearly full target) because they bear suppression costs with minimal exit option. The adaptive practitioner faces moderate-high d (0.58) because they benefit from the transmission function but bear enforcement costs. The ritual authority faces low d (0.12, nearly full beneficiary) because they profit from coordinating the transmission and control the form. The innovating community faces low-moderate d (0.38) because they have exit options and see an architectural pathway. These d values feed the sigmoid f(d) to produce experienced effective extractiveness (χ). The analytical observer's d is derived from the canonical analytical fallback (0.73), treating the observer as a measured, non-positioned analyst (though the story notes the risk of false-summit positioning). The directionality override array is empty because the structural derivation from beneficiary/victim + exit + power accurately captures the relationships in this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mourning_survival_function_boundaries,
    'Are the mourning-practice (D1/D4) and survival-competence (D5) functions distinct mechanisms within a single ritual, or are they inseparable aspects of a unified commemorative function?',
    'Historical and ethnographic decomposition: track which ritual elements activate mourning-practice vs survival-competence transmission separately. Test whether ritual performance can activate one function without the other (e.g., seder without loss-memory focus, or lamentation without institutional continuity encoding).',
    'If distinct: hybrid_transformation_reading is correct — the ritual encodes both and enforces both through a single apparatus. If inseparable: the reading collapses; both functions are aspects of a single commemorative act, and the sibling readings are false decompositions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mourning_survival_function_boundaries, empirical, 'Whether mourning and survival-competence are distinct functions or unified').

omega_variable(
    ritual_form_extraction_mechanism,
    'Is the enforcement of canonical ritual form (suppression ≥ 0.52) a necessary condition for transmitting both mourning-practice and survival-competence, or does it extract beyond what the transmission requires?',
    'Comparative analysis: communities that have relaxed canonical form (Reconstructionist, secular, diaspora adaptations) and measure whether they preserve loss-memory and institutional continuity equally. Track fidelity of mourning-preservation and survival-competence transmission as function of ritual form rigidity.',
    'If necessary: suppression is coordination cost, not extraction — the hybrid_transformation_reading validates the enforcement. If beyond necessary: suppression represents asymmetric extraction layered onto coordination — confirms tangled_rope classification and the perspectival gap between adaptive practitioners and ritual authorities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ritual_form_extraction_mechanism, empirical, 'Whether ritual form enforcement is necessary for function transmission').

omega_variable(
    kernel_reading_foreclosure_test,
    'Do the sibling readings (mourning_practice_reading, survival_competence_reading) logically foreclose this reading (hybrid_transformation_reading), or can all three coexist within different frameworks?',
    'Doctrinal analysis: examine whether any of the three readings explicitly denies a core premise of the others. Mourning_practice_reading claims ritual preserves D1/D4 boundary-norms; survival_competence_reading claims ritual preserves D5 adaptive capacity. Does the hybrid reading (both D1/D4 + D5 in single apparatus) contradict either sibling''s core premise, or merely emphasize different aspects?',
    'If logically foreclosed: the kernel contest reflects genuine doctrinal irreconcilability — the readings are rivals in a single framework. If coexistible: the readings are held by different communities (mourning-focused vs continuity-focused traditions) simultaneously, and the constraint involves empire-building of one reading over others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_test, conceptual, 'Whether sibling readings logically foreclose this reading').

omega_variable(
    false_summit_natural_law_risk,
    'Is the analytical observer''s mountain classification (ritual formalization inherent to human collective memory) a genuine natural law, or a false summit naturalizing contingent institutional arrangements?',
    'Ethnographic and historical scope expansion: survey catastrophe-remembering rituals across cultures with different institutional structures (non-Western, pre-literate, stateless communities). Track whether all such communities enforce canonical form, or whether functional mourning-practice and survival-competence transmission occur in more heterogeneous ritual structures.',
    'If universal: mountain classification holds — ritual formalization is a natural law of collective memory. If contingent: false summit fires — the canonical enforcement is a feature of institutional Judaism, not of human memory itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_risk, empirical, 'Whether ritual formalization is universal or culturally contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__hybrid_transformation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catmem_hybrid_tr_t0, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(catmem_hybrid_tr_t25, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 25, 0.58).
narrative_ontology:measurement(catmem_hybrid_tr_t50, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 50, 0.65).

% Extraction over time
narrative_ontology:measurement(catmem_hybrid_be_t0, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(catmem_hybrid_be_t25, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 25, 0.32).
narrative_ontology:measurement(catmem_hybrid_be_t50, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(catmem_hybrid_su_t0, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(catmem_hybrid_su_t25, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 25, 0.47).
narrative_ontology:measurement(catmem_hybrid_su_t50, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__hybrid_transformation_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_function__hybrid_transformation_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, institutional_identity_transmission_diaspora).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, collective_trauma_binding_mechanisms).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_function kernel decomposes into three constraint stories representing different readings. The hybrid_transformation_reading (this story) treats mourning-practice and survival-competence as inseparable aspects of a single ritual apparatus. The sibling readings decompose them into separate constraints (mourning_practice_reading emphasizes D1/D4 boundary preservation; survival_competence_reading emphasizes D5 adaptive transmission). The ε values differ because the readings isolate different structural mechanisms: hybrid_transformation emphasizes the enforcement cost of holding both functions in one form (ε=0.38); mourning_practice isolates the commemorative obligation (ε≈0.25); survival_competence isolates the knowledge transmission (ε≈0.30). Each reading is linked to downstream constraints in the religious ritual network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
