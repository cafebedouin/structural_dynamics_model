% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__symbol_survival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__symbol_survival_reading, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_survival__symbol_survival_reading
 *   human_readable: Ritual Form Preservation and Rabbinic Authority in Jewish Post-Catastrophe Memory
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint instantiates the SYMBOL SURVIVAL READING of the
 *   catastrophe_memory_survival kernel. The reading claims that ritual
 *   preserves Jewish identity and boundary-norms through symbolic experience
 *   itself; that survival under catastrophe depends on continuity of symbolic
 *   practice, with identity becoming the primary substrate when material
 *   conditions collapse. The secularized diaspora member experiences this as
 *   identity-lock: structurally mobile (no material barrier prevents exit)
 *   but functionally trapped because the self has been constituted through
 *   ritual participation. Rabbinic authority benefits from this arrangement
 *   by maintaining interpretive control over the tradition's authentic form.
 *   Post-traditional communities experience genuine coordination (shared
 *   time-marking, narrative anchors, intergenerational bonds) alongside
 *   extraction (career/family constraints on participation, identity
 *   fragmentation on exit). The contemporary institutional ritual has become
 *   substantially performative (theater ratio 0.82): fasting no longer
 *   encodes resource scarcity, recitation no longer transmits adaptation
 *   knowledge, observance no longer synchronizes collective labor. Yet the
 *   form persists through inertia and identity maintenance. The constraint's
 *   extractiveness has accumulated over time (0.28→0.58 across the interval)
 *   as material conditions improved and the practical referents dissolved,
 *   leaving only symbolic function. This reading competes with two sibling
 *   readings within the same kernel: the competence_transmission_reading
 *   (ritual encodes practical survival knowledge) and the
 *   hybrid_encoding_reading (ritual operates on dual registers). This story
 *   is the symbolic form preservation instantiation only.
 *
 * KEY AGENTS:
 *   - Secularized Diaspora Member: Primary victim (powerless/identity_locked) — structurally mobile but identity-fused with ritual practice; exit would require becoming a different person
 *   - Post-Traditional Communities: Secondary victim/moderate actor (moderate/constrained) — experience genuine coordination benefits alongside extraction costs; significant but surmountable barriers to exit
 *   - Rabbinic Authority Structures: Primary beneficiary (institutional/arbitrage) — maintain interpretive control through form-enforcement; experience the constraint as pure coordination enabling tradition continuity
 *   - Institutional Ritual Performance: Organized performer (organized/constrained) — the ritual-as-practiced is substantially performative; organized communities see it as degraded but necessary
 *   - Analytical Observer: Universal perspective (analytical/analytical) — risks naturalizing contingent institutional practice as immutable law of collective memory under catastrophe
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, 0.58).
domain_priors:suppression_score(catastrophe_memory_survival__symbol_survival_reading, 0.65).
domain_priors:theater_ratio(catastrophe_memory_survival__symbol_survival_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__symbol_survival_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__symbol_survival_reading, "Ritual Form Preservation and Rabbinic Authority in Jewish Post-Catastrophe Memory").
narrative_ontology:topic_domain(catastrophe_memory_survival__symbol_survival_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__symbol_survival_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__symbol_survival_reading, '4a124798-2fb2-45b7-aaa5-04053d34b7ee').
narrative_ontology:cs_kernel_codification('4a124798-2fb2-45b7-aaa5-04053d34b7ee', distributed).
narrative_ontology:cs_authority_grounding('4a124798-2fb2-45b7-aaa5-04053d34b7ee', lineage).
narrative_ontology:cs_interpretation_layer_present('4a124798-2fb2-45b7-aaa5-04053d34b7ee').
narrative_ontology:cs_reading_relation('4a124798-2fb2-45b7-aaa5-04053d34b7ee', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a124798-2fb2-45b7-aaa5-04053d34b7ee', catastrophe_memory_survival__hybrid_encoding_reading, influences).
narrative_ontology:cs_axiom('4a124798-2fb2-45b7-aaa5-04053d34b7ee', foundational, identity_continuity_as_primary_survival).
narrative_ontology:cs_axiom_status(identity_continuity_as_primary_survival, holdable).
narrative_ontology:cs_axiom_grounding('4a124798-2fb2-45b7-aaa5-04053d34b7ee', identity_continuity_as_primary_survival, deontological).
narrative_ontology:cs_axiom('4a124798-2fb2-45b7-aaa5-04053d34b7ee', foundational, symbolic_form_preservation_necessity).
narrative_ontology:cs_axiom_status(symbolic_form_preservation_necessity, holdable).
narrative_ontology:cs_axiom_grounding('4a124798-2fb2-45b7-aaa5-04053d34b7ee', symbolic_form_preservation_necessity, empirically_contingent).
narrative_ontology:cs_created_at('4a124798-2fb2-45b7-aaa5-04053d34b7ee', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority_structures).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, secularized_diaspora_jews).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, post_traditional_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SECULARIZED DIASPORA MEMBER (SNARE) — Structurally mobile (could cease participation) but identity-fused with the symbolic practice itself. Jewish identity becomes inseparable from ritual performance; exit would require abandoning not just the practice but the self-understanding constituted through it. Experiences maximum extraction: the constraint binds through internalized identity-lock rather than material barriers, making it functionally immovable.
constraint_indexing:constraint_classification(catastrophe_memory_survival__symbol_survival_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: POST-TRADITIONAL COMMUNITY (TANGLED ROPE) — Faces significant costs to exit (social isolation, identity fragmentation, loss of intergenerational anchor) but also genuinely benefits from ritual coordination: community bonds, cyclical time-marking, transmitted narrative frameworks. Moderate extraction with real coordination function. Exit options constrained by career/family embedded in the community but not completely trapped.
constraint_indexing:constraint_classification(catastrophe_memory_survival__symbol_survival_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RABBINIC AUTHORITY (ROPE) — Benefits from ritual continuity through interpretive control and institutional legitimacy. Experiences the constraint as pure coordination: maintaining the symbolic form ensures the tradition persists, enabling the authority structure's continued adjudication of authenticity. Arbitrage options abundant — can reinterpret, innovate, or selectively enforce. Net beneficiary with low experienced extraction.
constraint_indexing:constraint_classification(catastrophe_memory_survival__symbol_survival_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL RITUAL PERFORMANCE (PITON) — The actual ritual as performed in contemporary communities is substantially performative theater (82% theater ratio). Much of the symbolic content has lost practical referent — fasting no longer encodes resource scarcity, Passover recitation no longer transmits adaptation protocols, Shabbat observance no longer synchronizes collective labor. The ritual persists through institutional inertia and identity maintenance rather than functional necessity. Organized agents performing the role see it as degraded.
constraint_indexing:constraint_classification(catastrophe_memory_survival__symbol_survival_reading, piton,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, continuity of group identity under catastrophe necessarily depends on symbolic transmission when practical knowledge breaks down. The form IS the survival mechanism — when material conditions shatter, the community's identity becomes the only recoverable substrate. This perspective risks naturalizing what is actually a contested institutional arrangement, treating contingent symbolic practice as an immutable law of collective memory.
constraint_indexing:constraint_classification(catastrophe_memory_survival__symbol_survival_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophe_memory_survival__symbol_survival_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophe_memory_survival__symbol_survival_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(catastrophe_memory_survival__symbol_survival_reading, TR),
    TR >= 0.70.

:- end_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts from secularized members through identity-lock (they cannot exit without identity dissolution) and from post-traditional communities through constrained participation (career, family, social embeddedness). The rabbinic authority benefits from form-enforcement, which maintains the institutional structure's legitimacy. The extractiveness is not maximal (not 0.70+) because genuine coordination functions exist — the ritual does coordinate the community and transmit narratives, even if practical knowledge transfer has degraded. Suppression (0.65): High. Barriers to exit include identity fusion (internal suppression that persists after material barriers dissolve), social/family penalties for non-participation, and the subtle coercion of belonging (in-group/out-group boundaries enforced through ritual participation). Suppression is not maximal (not 0.85+) because some communities do innovate or selectively participate, suggesting that alternatives exist, albeit at high cost. Theater ratio (0.82): Very high and rising. Pre-catastrophe (t=0), theater was moderate (0.35) because ritual coordinated actual material practices (fasting coordinated food scarcity, Shabbat coordinated labor). As material conditions improved and diaspora distance increased, the practical referent dissolved, leaving only symbolic form. Contemporary communities practice ritual primarily for boundary-maintenance and identity anchoring rather than practical coordination. The rising trajectory reflects context-dependent performativity: theater ratio increases as material constraints decrease.
 *
 * PERSPECTIVAL GAP:
 *   The power and institutional perspectives see coordination (rope); the powerless and organized perspectives see mixed coordination-extraction (tangled rope) or degradation (piton); the analytical view risks seeing natural law (mountain). This gap reveals the constraint's true structure: it is extractive precisely because it binds through identity-lock rather than material necessity. The beneficiary (rabbinic authority) experiences it as coordination that enables tradition continuity. The victim (secularized member) experiences it as immovable because exit requires identity dissolution. The same symbolic form serves opposite functions for different agents.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from structural position: beneficiary status + exit options → low d (rabbinic authority with arbitrage options); victim status + identity-lock → high d (secularized member structurally mobile but identity-bound). The sigmoid function f(d) maps these to experienced extractiveness. Rabbinic authority derives d≈0.10 from beneficiary+arbitrage, producing f(d)≈-0.01 (negative effective extraction — they subsidize the coordination). Secularized member derives d≈0.88 from victim+identity_locked, producing f(d)≈1.28 (high effective extraction). Post-traditional community derives d≈0.52 from mixed victim/beneficiary status + constrained exit, producing f(d)≈0.65 (moderate extraction). Scope is regional-to-global (diaspora networks), applying scope modifier σ(regional)=0.9. The engine computes χ = ε × f(d) × σ(S), giving different experienced chi for each actor despite shared base extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is RESOLVED here through the lens of ritual function. The constraint avoids the snare/rope confusion by explicitly declaring both a coordination function (community bonding, narrative transmission, cyclical time-marking) and an extraction mechanism (identity-lock, suppression via belonging, interpretive control). The extractiveness (0.58) reflects that genuine coordination exists alongside extraction — it is not pure extraction masquerading as coordination (snare/false rope) nor pure coordination masquerading as extraction (rope/false snare). The rising theater ratio over time is the diagnostic signal: as practical function degraded, the coordination function persisted (community still bonds, identities still anchored) but became increasingly performative. The constraint is legitimately tangled rope because both the coordination and extraction are structurally real and necessary to explain community participation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_vs_competence_boundary,
    'Is the constraint''s primary function boundary-maintenance through symbolic experience, or does it covertly encode and transmit practical survival competencies?',
    'Historical-ethnographic analysis: trace which ritual elements correlate with measurable adaptive behaviors vs which are purely performative; interview third-generation practitioners about what knowledge they retained from ritual participation; compare retention rates of symbolic content vs practical protocols across diaspora communities',
    'If primarily symbolic: this constraint (symbol_survival_reading) is correctly classified as tangled rope at moderate ε (0.58). If significantly competence-encoding: ε should be lower (0.35–0.40, rope/tangled rope boundary) because genuine coordination function reduces extraction burden. If hybrid: a separate constraint (hybrid_encoding_reading) should be authored with its own ε profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_vs_competence_boundary, empirical, 'Whether ritual''s survival mechanism is symbolic or competence-based').

omega_variable(
    identity_lock_durability,
    'How durable is the identity-lock binding powerless diaspora members? Does it persist post-catastrophe when material conditions shift, or does it degrade when the symbolic referent''s practical context dissolves?',
    'Longitudinal observation of post-rupture communities: track whether identity-locked members'' participation persists or collapses when (a) material survival conditions improve, (b) diaspora distance increases, (c) generational transmission breaks. Compare communities with active material constraints (refugee, persecuted) vs those with material security.',
    'If durable across material conditions: identity-lock is a distinct binding mechanism (supports classification as snare from powerless perspective). If degrades with material improvement: the constraint''s extraction power depends on material scarcity, making it a situational snare rather than a structural one. Different therapeutic implications for post-trauma communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_durability, empirical, 'Durability of identity-lock binding across changing material conditions').

omega_variable(
    rabbinic_extraction_intentionality,
    'Does rabbinic authority actively enforce ritual form preservation to maintain interpretive control, or does form preservation emerge from organic community investment in continuity?',
    'Historical analysis of rabbinic texts: examine whether responsa (formal legal opinions) emphasize form-preservation for its own sake or whether they accommodate innovation when material conditions or community composition shift. Compare communities with strong rabbinical institutional presence vs those with distributed authority.',
    'If active extraction: beneficiary classification (rabbinic authority) is correct, and the constraint is accurately classified as tangled rope with institutional beneficiary. If organic: the constraint may be rope (pure coordination) rather than tangled rope, and the ''extraction'' is projection of contemporary power structures onto historical practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rabbinic_extraction_intentionality, conceptual, 'Whether rabbinic form-enforcement is active extraction or organic community investment').

omega_variable(
    reading_alternative_competence_transmission,
    'THIS CONSTRAINT IS ONE READING OF A CONTESTED KERNEL: catastrophe_memory_survival. The competence_transmission_reading instantiates a different structural claim about what ritual fundamentally does. Which reading is correct?',
    'This ambiguity is NOT resolvable by empirical data alone — both readings can coexist as live normative positions. Resolution depends on: (1) which epistemic position one adopts about the purpose of ritual (boundary-maintenance vs practical knowledge transfer), and (2) which communities one privileges in the analysis (historical rabbinic texts emphasizing form, contemporary ethnography emphasizing lost competencies). The resolution mechanism is an audit of one''s own framing assumptions.',
    'If symbol_survival_reading is adopted: ε≈0.58, tangled rope, high extraction. If competence_transmission_reading is adopted: ε≈0.35–0.40, rope/tangled rope boundary, lower extraction. If hybrid_encoding_reading is adopted: ε≈0.48–0.52, tangled rope, moderate-high extraction with genuine dual function. The kernel contains all three readings; the constraint story you are reading is the symbol_survival instantiation only.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_alternative_competence_transmission, conceptual, 'Kernel-level ambiguity: symbol vs competence vs hybrid reading').

omega_variable(
    theater_ratio_source_ambiguity,
    'Is the high theater ratio (0.82) measuring performative content in contemporary ritual, or is it an anachronistic projection of modern secular viewpoint onto historically functional practice?',
    'Temporal comparative analysis: measure theater ratio at three points — (a) pre-catastrophe community (material survival context), (b) immediate post-catastrophe (refugee/persecuted context), (c) contemporary diaspora (material security context). If theater ratio increases with material security, the performativity is context-dependent rather than intrinsic.',
    'If theater ratio is context-dependent: the piton classification applies only to contemporary secure diaspora, not to historical communities under material threat. Revise interval analysis and perspectives accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_source_ambiguity, empirical, 'Whether high theater ratio reflects performativity or anachronistic projection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__symbol_survival_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cms_symbol_theater_t0_precat, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cms_symbol_theater_t3_immediatepost, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 3, 0.58).
narrative_ontology:measurement(cms_symbol_theater_t6_generational, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 6, 0.72).
narrative_ontology:measurement(cms_symbol_theater_t10_contemporary, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 10, 0.82).

% Extraction over time
narrative_ontology:measurement(cms_symbol_extract_t0_precat, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cms_symbol_extract_t3_immediatepost, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(cms_symbol_extract_t6_generational, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(cms_symbol_extract_t10_contemporary, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cms_symbol_suppress_t0_precat, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(cms_symbol_suppress_t3_immediatepost, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 3, 0.48).
narrative_ontology:measurement(cms_symbol_suppress_t6_generational, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 6, 0.62).
narrative_ontology:measurement(cms_symbol_suppress_t10_contemporary, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__symbol_survival_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__competence_transmission_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_survival kernel contains three structurally distinct constraints with different ε values, each instantiating a different reading of how ritual preserves group identity under catastrophe. The symbol_survival_reading (this file) ε≈0.58 treats survival as continuity of symbolic practice itself, prioritizing boundary-maintenance and identity anchoring. The competence_transmission_reading ε≈0.35–0.40 treats survival as continuity of practical knowledge encoded in ritual form. The hybrid_encoding_reading ε≈0.48–0.52 treats survival as depending on both registers simultaneously. These are NOT the same constraint viewed from different angles — their ε values differ by a factor of 1.5–1.6, reflecting structurally different claims about ritual's primary function. The three files form a constraint family linked by network.affects_constraints, each with its own perspectives, omegas, and measurements. This decomposition follows the ε-invariance principle: if changing your observable (focusing on symbolic vs practical function) changes ε, you have two constraints, not one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
