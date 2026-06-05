% ============================================================================
% CONSTRAINT STORY: symbol_survival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_symbol_survival_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: symbol_survival_reading
 *   human_readable: Ritual Preservation of Identity and Boundary-Norms Through Symbolic Experience
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint models how Jewish ritual practice functions to preserve
 *   identity and boundary-norms through repeated symbolic experience,
 *   particularly in the context of diaspora catastrophe survival. The reading
 *   asserts that SURVIVAL = CONTINUITY OF PRACTICE ITSELF: the tradition
 *   persists by maintaining ritual forms that encode identity-markers and
 *   transmit boundary-recognition across generations and geographic
 *   dispersal. The constraint exhibits high extractiveness (0.58) because
 *   ritual participation is not purely voluntary coordination — it requires
 *   identity-fusion (identity_locked exit), carries suppression through
 *   community sanctions and internalized obligation (suppression = 0.62), and
 *   increasingly consists of performative participation rather than live
 *   transmissive function (theater_ratio = 0.68). The core claim: ritual form
 *   preservation serves as the primary mechanism of identity survival, with
 *   rabbinic authority as the beneficiary of this mechanism (maintains
 *   interpretive control and institutional relevance) and secularized or
 *   partially-practicing populations as victims (forced to choose between
 *   identity discontinuity or ritual re-entry). This reading is one
 *   instantiation of a contested kernel — catastrophe_memory_survival — that
 *   describes how Jewish tradition survives catastrophe. Sibling readings
 *   (competence_transmission_reading, hybrid_encoding_reading) offer
 *   alternative mechanisms (knowledge transmission, hybrid form+content
 *   encoding). This reading differs structurally: it privileges ritual FORM
 *   over content; it treats survival as continuity itself rather than as
 *   functional persistence of knowledge; and it positions rabbinic authority
 *   as primary beneficiary rather than as neutral coordinator.
 *
 * KEY AGENTS:
 *   - Secularized/Non-Observant Jews: Primary victims (powerless/identity_locked) — structurally mobile but psychologically bound to identity claim; cannot participate without identity discontinuity; bear extraction of internal conflict
 *   - Committed Ritual Practitioners: Secondary agents (moderate/constrained) — genuinely coordinate through ritual; also bear extraction in labor, time, conformity costs; benefit from community membership and identity continuity
 *   - Rabbinic Authority Structure: Primary beneficiary (institutional/arbitrage) — benefits from ritual form preservation (ensures continued relevance of interpretation); coordinates through guidance and boundary-setting; low experienced extraction
 *   - Progressive/Reform Movements: Organized agents (organized/mobile) — have reorganized constraint to reduce extraction while maintaining coordination; face friction from Orthodox authority; represent mobile response to the bottleneck
 *   - Secularized State Institutions: Institutional observer (institutional/constrained) — recognize Jewish identity category but maintain distance from ritual substrate; see constraint as vestigial; high theater (civic Judaism)
 *   - Anthropological Observer: Analytical perspective (analytical/analytical) — risks naturalizing contingent institutional arrangement as universal law of identity survival
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(symbol_survival_reading, 0.58).
domain_priors:suppression_score(symbol_survival_reading, 0.62).
domain_priors:theater_ratio(symbol_survival_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(symbol_survival_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(symbol_survival_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(symbol_survival_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(symbol_survival_reading, tangled_rope).
narrative_ontology:human_readable(symbol_survival_reading, "Ritual Preservation of Identity and Boundary-Norms Through Symbolic Experience").
narrative_ontology:topic_domain(symbol_survival_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(symbol_survival_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(symbol_survival_reading, '0a9f8d43-4df3-4b22-8a83-10736f64af75').
narrative_ontology:cs_created_at('0a9f8d43-4df3-4b22-8a83-10736f64af75', '').
narrative_ontology:cs_kernel_codification('0a9f8d43-4df3-4b22-8a83-10736f64af75', distributed).
narrative_ontology:cs_authority_grounding('0a9f8d43-4df3-4b22-8a83-10736f64af75', lineage).
narrative_ontology:cs_interpretation_layer_present('0a9f8d43-4df3-4b22-8a83-10736f64af75').
narrative_ontology:cs_kernel_id(symbol_survival_reading, catastrophe_memory_survival).
narrative_ontology:cs_reading_relation('0a9f8d43-4df3-4b22-8a83-10736f64af75', competence_transmission_reading, coexists_with).
narrative_ontology:cs_reading_relation('0a9f8d43-4df3-4b22-8a83-10736f64af75', hybrid_encoding_reading, influences).
narrative_ontology:cs_axiom('0a9f8d43-4df3-4b22-8a83-10736f64af75', foundational, ritual_form_encodes_identity).
narrative_ontology:cs_axiom_status(ritual_form_encodes_identity, holdable).
narrative_ontology:cs_axiom('0a9f8d43-4df3-4b22-8a83-10736f64af75', foundational, practice_continuity_equals_collective_survival).
narrative_ontology:cs_axiom_status(practice_continuity_equals_collective_survival, holdable).
narrative_ontology:cs_reference_frame('0a9f8d43-4df3-4b22-8a83-10736f64af75', ritual_continuity_as_survival).
narrative_ontology:cs_drift_state('0a9f8d43-4df3-4b22-8a83-10736f64af75', contemporary_diaspora_pluralism, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(symbol_survival_reading, rabbinic_authority).
narrative_ontology:constraint_beneficiary(symbol_survival_reading, ritual_practitioners).
narrative_ontology:constraint_victim(symbol_survival_reading, secularized_population).
narrative_ontology:constraint_victim(symbol_survival_reading, transmission_continuity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SECULARIZED JEW (SNARE) — Structurally mobile (no legal bars, economic dependency minimal) but identity-locked to the tradition they have left. Cannot participate without reconstituting their abandoned identity; cannot exit the psychological claim the tradition makes on them. High experienced extraction: the constraint forces a choice between identity discontinuity (abandoning Jewishness) or ritual re-entry (abandoning secular self-conception). Suppression is internalized — the agent carries the identity claim with them after exit.
constraint_indexing:constraint_classification(symbol_survival_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: COMMITTED PRACTITIONER (TANGLED ROPE) — Genuinely coordinates with others through ritual practice (shared symbolic language, boundary maintenance, collective memory). Also bears extraction: ritual demands temporal investment, reproductive labor (teaching children, maintaining spaces), and conformity to interpretive authority. Exit is costly (loss of community, relational identity) but perceivable. Experience is mixed — real coordination function alongside real asymmetric extraction.
constraint_indexing:constraint_classification(symbol_survival_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RABBINIC AUTHORITY (ROPE) — Benefits from the constraint: ritual form preservation ensures continued relevance of authoritative interpretation. Experiences the constraint as coordination: rituals require guidance, textual interpretation, boundary-setting — all functions that institutional rabbinic authority provides. Net beneficiary with arbitrage options (can shift interpretive frameworks, commodify expertise). Low effective extraction from this perspective — the constraint serves the institution's legitimacy.
constraint_indexing:constraint_classification(symbol_survival_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROGRESSIVE/REFORM MOVEMENTS (TANGLED ROPE) — Organized agents that have reorganized the constraint itself: ritual form is modified (shortened, egalitarian, English-language incorporation) to reduce extraction while maintaining coordination function. Experience mixed benefits and costs: genuine coordination remains, but enforcement against reformist interpretations creates friction with Orthodox authority. Mobile exit (can form separate movements) but constrained by shared tradition narrative.
constraint_indexing:constraint_classification(symbol_survival_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: SECULARIZED STATE/CIVIC INSTITUTIONS (PITON) — The constraint persists as vestigial identity-maintenance mechanism within pluralist states. State institutions recognize Jewish identity as category (for census, religious freedom law, etc.) but the ritual substrate has atrophied in institutional relevance. Theater is high: civic Judaism (identity without observance) coexists with ritual Judaism; many Jews maintain symbolic participation (High Holiday attendance, life-cycle rituals) as performative continuity rather than active coordination. Piton classification derives from high theater (0.68) — form persists through institutional inertia despite reduced functional verification of transmissive intent.
constraint_indexing:constraint_classification(symbol_survival_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANTHROPOLOGICAL UNIVERSAL (MOUNTAIN) — From a civilizational perspective, ritual survival of identity is asserted as natural law: all dispersed communities use repeated symbolic action to preserve boundaries; discontinuity of practice means discontinuity of identity; survival = continuity itself. However, the schema analysis reveals this as a FALSE SUMMIT: rabbinic authority is the primary beneficiary; secularized populations are victims; the 'universality' naturalizes what is actually a specific institutional arrangement maintained through suppression and identity-locking.
constraint_indexing:constraint_classification(symbol_survival_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(symbol_survival_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(symbol_survival_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(symbol_survival_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(symbol_survival_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(symbol_survival_reading, TR),
    TR >= 0.70.

:- end_tests(symbol_survival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from secularized populations (forces binary choice: ritual re-entry or identity abandonment) and from practitioners (demands labor, conformity, time). However, extraction is not maximal because the coordination function is genuine — ritual does solve a real problem (maintaining boundaries across dispersal). The measured value reflects that extractiveness has increased over time (0.38→0.58 across interval) as ritual form has become increasingly performative and less transmissively functional. Suppression (0.62): Moderate-high. Barriers to non-participation include community sanctions (social penalty), internalized obligation (identity-lock), absence of secular alternative identity-structures that preserve collective memory, and legal/institutional recognition of Jewish identity as category (which reinforces boundary maintenance). The suppression is not structural imprisonment but is psychologically deep — the identity lock makes exit feel impossible from within the frame. Theater ratio (0.68): High. Over the interval, ritual performance has increasingly decoupled from transmissive function. High Holiday services are attended by many Jews who do not observe year-round practice; life-cycle rituals are performed for social continuity rather than active meaning-transmission; synagogues function as community centers with ritual as partial infrastructure. The theater has increased as institutional Judaism has become embedded in pluralist societies where identity can be maintained through alternate mechanisms (legal status, ethnic self-reporting, cultural reference).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence from a single set of base properties. Rabbinic authority sees coordination (Rope) — the constraint solves the real problem of maintaining boundaries across diaspora. Committed practitioners see mixed benefit and burden (Tangled Rope) — genuine coordination alongside real extraction. Secularized agents see a snare (Snare) — psychological trapping with high extraction and minimal coordination benefit. Progressive movements see a reformable hybrid (Tangled Rope with lower ε) — they retain coordination while reducing extraction. Civic institutions see vestigial ritual (Piton) — high theater, low functional verification. The anthropological universal view (Mountain) risks naturalizing the entire structure as a universal law of identity-persistence. The perspectival gaps reveal that 'ritual preservation of identity' is not a single constraint but a presheaf of constraints indexed by observer position. From rabbinic authority's position, it is coordination. From the secularized agent's position, it is extraction. Both are true descriptions of the same structural phenomenon from different positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The symbol_survival reading presupposes a specific relationship between agents and the constraint. Rabbinic authority (beneficiary/arbitrage) experiences low effective extraction because the constraint serves institutional interests — ritual form preservation ensures continued authority relevance. Committed practitioners (moderate/constrained) experience mixed extraction: genuine coordination benefit (shared symbolic language, boundary clarity, collective memory) alongside real costs (labor, conformity, opportunity cost). Secularized agents (powerless/identity_locked) experience maximum extraction because they face a forcing choice: either re-enter ritual (psychologically costly; requires identity reconstitution) or accept identity discontinuity (socially costly; requires severance from collective narrative). The identity-lock is the key structural feature — the agent is not materially trapped but psychologically bound. Exit would require becoming 'a different person' (ceasing to be Jewish). This is not the same as constrained (where exit is costly but clear) or trapped (where exit is materially impossible). The specific exit classification (identity_locked) drives the high d value for the secularized agent, which produces the high f(d) = 1.28, which amplifies the experienced chi. The beneficiary's arbitrage exit produces low d value and low/negative f(d), which dampens their experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves potential mandatrophy (conflict between coordination and extraction claims) by explicitly embracing tangled_rope as the base classification: ritual BOTH coordinates identity-preservation AND extracts from those who cannot or will not participate. The constraint is hybrid by structural necessity, not by conceptual confusion. Rabbinic authority benefits from the coordination function (benefits from ritual's continued relevance). Secularized agents bear extraction (forced choice: ritual or identity-discontinuity). Committed practitioners experience both. The reading does not attempt to collapse this into pure extraction or pure coordination — it asserts that the constraint genuinely has both functions and that the balance has shifted over the modeled interval (extractiveness increasing as theater increases) toward more performative, less coordinatively-functional ritual. The mandatrophy is dissolved by accepting that identity-based constraints characteristically exhibit high theater and mixed extraction/coordination — the presence of high theater does NOT invalidate the coordination function, and the presence of genuine coordination DOES NOT validate the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transmission_mechanism_adequacy,
    'Does ritual form alone transmit sufficient identity-content and boundary-knowledge for survival of the tradition, or is the survival dependent on external reinforcement (legal status, institutional authority, community sanctions)?',
    'Historical analysis of communities with sustained ritual practice but no institutional authority (diaspora subgroups, underground transmission); correlation between ritual frequency/purity and identity retention across generations',
    'If ritual-alone sufficient: mountain classification appropriate. If external reinforcement required: classification should be snare or tangled_rope (extraction is primary, ritual is theater). This reading assumes hybrid — ritual is primary mechanism but requires institutional enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_mechanism_adequacy, empirical, 'Whether ritual form alone sustains identity transmission').

omega_variable(
    identity_lock_vs_constrained_boundary,
    'Is the secularized agent''s inability to participate best modeled as identity-locked (internalized cognitive frame making exit psychologically impossible) or as constrained (high material/social costs but theoretically possible)?',
    'Psychographic analysis of non-observant Jews: do they report identity fusion with tradition despite non-participation, or do they report intact alternative identities with clear-eyed assessment of ritual costs? Post-entry interviews with returnees: what psychological shift enabled re-entry?',
    'If identity-locked: snare classification correct (the constraint operates through identity itself). If constrained: tangled_rope more appropriate (material barriers, not identity fusion). This reading assumes identity-locked at biographical horizon — the agent cannot see themselves as outside the tradition even when not practicing it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_boundary, empirical, 'Mechanism of psychological binding in secularized non-participants').

omega_variable(
    survival_definition_ambiguity,
    'What counts as successful ''survival''? Does the tradition survive through continuous ritual practice, through identity self-reporting (being Jewish without practice), through institutional authority maintenance, or through cultural reference in secular contexts (Jewish humor, ethical tropes, historical consciousness)?',
    'Definitional clarity from the reading''s own epistemic premises. The symbol_survival reading asserts continuity of practice = survival. Other readings (competence_transmission_reading, hybrid_encoding_reading) may define survival differently. The different definitions produce different victim/beneficiary sets and different ε values.',
    'If ''survival'' means ritual practice continuity: this reading''s high ε (0.58) reflects that secularized non-practitioners are victims — transmission is failing. If ''survival'' means identity persistence without practice: ε would be much lower (close to 0) — the tradition survives in secular contexts without ritual enforcement. The reading''s ε presupposes this definition of survival.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(survival_definition_ambiguity, conceptual, 'Definition of ''survival'' under-determines classification').

omega_variable(
    rabbinic_authority_capture_vs_coordination,
    'Does ritual preserve identity as coordinate function WITH rabbinic guidance (rope), or does rabbinic authority CAPTURE the ritual coordination mechanism to enforce interpretive control (snare/tangled_rope)?',
    'Historical analysis of interpretive authority emergence: did rabbis emerge to solve coordination problems that pre-existed them, or did the coordination problem (preserving identity across diaspora) emerge as a function of rabbinical institutional needs? Comparative analysis: non-rabbinic Jewish communities with ritual persistence (Karaite, Samaritan, ethnically-discrete communities) — what does their ritual structure reveal about the necessity vs contingency of rabbinic authority?',
    'If authority emerged to solve pre-existing coordination: rope classification more appropriate (beneficiary and victim are aligned). If coordination problem was generated by institutional needs: snare/tangled_rope appropriate (authority is primary beneficiary, others are victims). This reading assumes tangled_rope — rabbinic authority is genuine coordinator AND primary beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rabbinic_authority_capture_vs_coordination, empirical, 'Whether rabbinic authority solves or creates the transmission problem').

omega_variable(
    committer_kernel_reading_ambiguity,
    'This constraint is one reading of a contested kernel (catastrophe_memory_survival): how the Jewish tradition survives catastrophe through ritual. The symbol_survival reading emphasizes continuity of practice itself as the mechanism. Sibling readings (competence_transmission_reading, hybrid_encoding_reading) emphasize different mechanisms: knowledge transmission vs hybrid form+content encoding. These readings may foreclose each other, coexist, or influence each other. Which structural relationship is correct?',
    'Textual and historical analysis of how the tradition itself frames survival: does rabbinic literature emphasize practice continuity, knowledge preservation, or adaptive encoding? Do different periods or communities emphasize different mechanisms? The resolution determines whether readings coexist (multiple mechanisms active simultaneously) or foreclose (only one mechanism is structurally viable).',
    'If readings foreclose: one reading is correct; the others are false framings. If readings coexist: they represent genuinely different aspects of the survival mechanism. If readings influence: one (likely competence_transmission) is upstream of this one (symbol_survival) — knowledge transmission creates the conditions for ritual continuity. This reading assumes coexistence: ritual form and knowledge transmission are parallel mechanisms, not competitive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_kernel_reading_ambiguity, conceptual, 'Relationship between symbol_survival and sibling readings in catastrophe_memory_survival kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(symbol_survival_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(symb_tr_t0, symbol_survival_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(symb_tr_t25, symbol_survival_reading, theater_ratio, 25, 0.55).
narrative_ontology:measurement(symb_tr_t50, symbol_survival_reading, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(symb_be_t0, symbol_survival_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(symb_be_t25, symbol_survival_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(symb_be_t50, symbol_survival_reading, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(symbol_survival_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(symbol_survival_reading, 0.12).
narrative_ontology:affects_constraint(symbol_survival_reading, competence_transmission_reading).
narrative_ontology:affects_constraint(symbol_survival_reading, hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% The symbol_survival_reading is one reading of the catastrophe_memory_survival kernel. Sibling readings (competence_transmission_reading, hybrid_encoding_reading) instantiate alternative mechanisms of survival. The three stories are linked through network.affects_constraints: this reading influences siblings by asserting form-priority; siblings influence this reading by offering alternative definitions of what counts as 'survival.' The relationship is coexistent (different mechanisms active simultaneously) rather than foreclosing (one true, others false). Each reading has its own ε, victim set, and beneficiary set because each emphasizes different structural mechanisms and thus measures different extraction flows.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(symbol_survival_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
