% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__liturgical_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__liturgical_preservation, []).

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
 *   constraint_id: hebrew_continuity__liturgical_preservation
 *   human_readable: Hebrew Continuity through Liturgical Preservation and Textual Transmission
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   Hebrew's continuity through two millennia of diaspora, minority status,
 *   and absence of native speakers is structurally anomalous in linguistics —
 *   most languages do not survive these conditions. The mechanism that
 *   enabled Hebrew's survival was liturgical preservation: the requirement
 *   that prayer, study, and scriptural interpretation preserve canonical
 *   textual forms unchanged. This created a constraint that simultaneously
 *   enabled language continuity AND constrained language evolution. The
 *   constraint operates as a tangled rope — it genuinely coordinates the
 *   preservation of a shared linguistic-cultural heritage across dispersed
 *   communities (coordination function: maintaining textual transmission,
 *   enabling religious continuity, preserving group identity).
 *   Simultaneously, it extracts from agents who would innovate the language
 *   generatively: speakers who wish to develop Hebrew as a living, evolving
 *   language face suppression from institutional gatekeeping, educational
 *   enforcement of canonical forms, and identity-lock mechanisms (treating
 *   innovation as betrayal of tradition). The theater ratio (0.68) reflects
 *   that modern Hebrew institutions maintain liturgical forms through
 *   explicit canonical pedagogy, religious authority, and cultural
 *   nationalism — the performative component has increased as the original
 *   religious function has partially secularized. The constraint exhibits all
 *   six classification types from different structural positions, making it a
 *   diagnostic exemplar for how commitment-system constraints operate.
 *
 * KEY AGENTS:
 *   - Religious Institutional Authority: Primary beneficiary (institutional/arbitrage) — maintains textual transmission, religious continuity, institutional legitimacy through liturgical preservation
 *   - Textual Transmission Keepers: Secondary beneficiary (institutional/constrained) — educators, rabbinical scholars, lexicographers who maintain canonical forms and transmit them to new generations
 *   - Secularizing Hebrew Speakers: Primary victim (powerless/identity_locked) — desire to innovate, create modern vernacular, develop living speech but face identity rupture (treating innovation as assimilation/betrayal) and institutional suppression
 *   - Secular Hebrew Innovation Movement: Organized victim (organized/mobile) — modern Hebrew literature, slang communities, youth subcultures creating alternative transmission pathways with sunset logic
 *   - Modern Hebrew Community: Mixed (moderate/constrained) — benefits from liturgical preservation (access to tradition, community identity, textual depth) but constrained by enforcement in formal/religious contexts
 *   - Secular State Language Policy: Institutional actor (institutional/arbitrage) — inherited liturgical preservation role despite lacking liturgical commitment; maintains performative enforcement of canonical forms through education policy
 *   - Analytical Observer: Civilizational (analytical/analytical) — risks naturalizing institutional enforcement as linguistic necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__liturgical_preservation, 0.38).
domain_priors:suppression_score(hebrew_continuity__liturgical_preservation, 0.52).
domain_priors:theater_ratio(hebrew_continuity__liturgical_preservation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, extractiveness, 0.38).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__liturgical_preservation, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__liturgical_preservation, "Hebrew Continuity through Liturgical Preservation and Textual Transmission").
narrative_ontology:topic_domain(hebrew_continuity__liturgical_preservation, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_continuity__liturgical_preservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__liturgical_preservation, 'a0aef513-d86e-4cad-9c47-03ce7648b74d').
narrative_ontology:cs_kernel_codification('a0aef513-d86e-4cad-9c47-03ce7648b74d', fixed_text).
narrative_ontology:cs_authority_grounding('a0aef513-d86e-4cad-9c47-03ce7648b74d', extraction).
narrative_ontology:cs_interpretation_layer_present('a0aef513-d86e-4cad-9c47-03ce7648b74d').
narrative_ontology:cs_reading_relation('a0aef513-d86e-4cad-9c47-03ce7648b74d', hebrew_continuity__native_generative, coexists_with).
narrative_ontology:cs_reading_relation('a0aef513-d86e-4cad-9c47-03ce7648b74d', hebrew_continuity__bridge_pidginized, influences).
narrative_ontology:cs_axiom('a0aef513-d86e-4cad-9c47-03ce7648b74d', foundational, textual_preservation_sufficient_continuity).
narrative_ontology:cs_axiom_status(textual_preservation_sufficient_continuity, holdable).
narrative_ontology:cs_axiom_grounding('a0aef513-d86e-4cad-9c47-03ce7648b74d', textual_preservation_sufficient_continuity, empirically_contingent).
narrative_ontology:cs_axiom('a0aef513-d86e-4cad-9c47-03ce7648b74d', foundational, innovation_as_discontinuity).
narrative_ontology:cs_axiom_status(innovation_as_discontinuity, holdable).
narrative_ontology:cs_axiom_grounding('a0aef513-d86e-4cad-9c47-03ce7648b74d', innovation_as_discontinuity, conventional).
narrative_ontology:cs_reference_frame('a0aef513-d86e-4cad-9c47-03ce7648b74d', canonical_textual_continuity).
narrative_ontology:cs_drift_state('a0aef513-d86e-4cad-9c47-03ce7648b74d', contemporary_secular_israel, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a0aef513-d86e-4cad-9c47-03ce7648b74d', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__liturgical_preservation, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, religious_institutional_authority).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, textual_transmission_keepers).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, hebrew_native_speaker_base).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, secular_hebrew_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SECULARIZING HEBREW SPEAKER (SNARE) — Identity-locked into Hebrew identity but trapped by liturgical containment. Cannot generatively innovate Hebrew beyond prescribed textual boundaries without experiencing identity rupture (betrayal of tradition, assimilation). Structural suppression: innovation paths are blocked; exit from Hebrew entirely requires abandoning community and ancestral identity. Maximum experienced extraction — the constraint forces choices: preserve tradition identically or exit completely.
constraint_indexing:constraint_classification(hebrew_continuity__liturgical_preservation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: MODERN HEBREW COMMUNITY (TANGLED ROPE) — Benefits from the liturgical transmission system (access to ancestral language, community identity, textual continuity). Simultaneously constrained by enforcement of canonical forms — modern speakers face pressure to conform speech to liturgical standards in formal/religious contexts. Mixed: genuine coordination function (preserving shared linguistic heritage) alongside asymmetric extraction (constraining innovation and vernacular development).
constraint_indexing:constraint_classification(hebrew_continuity__liturgical_preservation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RELIGIOUS INSTITUTIONAL AUTHORITY (ROPE) — Primary beneficiary. Experiences liturgical preservation as pure coordination: maintaining canonical Hebrew enables religious continuity, textual transmission, and institutional legitimacy across centuries. Zero experienced extraction because the constraint's function aligns exactly with institutional interest. Arbitrage exit option: institutions can choose to relax, enforce, or transmit selectively.
constraint_indexing:constraint_classification(hebrew_continuity__liturgical_preservation, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SECULAR STATE LANGUAGE POLICY (PITON) — Inherits and performs the role of liturgical Hebrew guardian despite lacking liturgical commitment. Modern Israel's language normalization absorbed religious transmission mechanics but the primary coordination function (religious continuity) has atrophied. State maintains liturgical Hebrew forms through education policy and institutional inertia; the theater persists (teaching biblical Hebrew as canonical) but the functional need (religious transmission) is now dispersed to multiple domains. High theater ratio reflects that state language policy is substantially performative — maintaining classical forms without the religious logic that originally justified them.
constraint_indexing:constraint_classification(hebrew_continuity__liturgical_preservation, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LINGUISTIC INNOVATION MOVEMENTS (SCAFFOLD) — Organized agents (modern Hebrew speakers, literature communities, youth subcultures) are creating alternative Hebrew transmission pathways with clear sunset logic: contemporary Hebrew literature, slang, media innovation, and pedagogical experimentation are building bridges between liturgical forms and living speech. The scaffold has temporal structure — the goal is to establish modern generative Hebrew as a legitimate evolution of the tradition, creating conditions where liturgical preservation becomes optional rather than obligatory. Theater is low because innovation pathways operate transparently in open domains (literature, media, conversation). Sunset: as modern generative Hebrew achieves institutional recognition, the liturgical containment constraint loses binding force.
constraint_indexing:constraint_classification(hebrew_continuity__liturgical_preservation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / IMMUTABILITY VIEW (MOUNTAIN) — From a universalizing analytical perspective, the preservation of Hebrew through liturgical transmission appears to be a natural law of language continuity: only through fixed textual forms and ritualized recitation can a language survive diaspora, minority status, and centuries without native speakers. The constraint appears immutable — any attempt to modernize or innovate would break the transmission chain. However, the structural data contradicts this classification: identifiable beneficiaries (institutional authority, textual keepers) exist, suppression is active (not passive), and innovation pathways are being deliberately closed. This is a false summit — the 'immutability' framing naturalizes what is actually a contingent institutional enforcement choice.
constraint_indexing:constraint_classification(hebrew_continuity__liturgical_preservation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__liturgical_preservation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hebrew_continuity__liturgical_preservation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hebrew_continuity__liturgical_preservation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(hebrew_continuity__liturgical_preservation, TR),
    TR >= 0.70.

:- end_tests(hebrew_continuity__liturgical_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts from agents who would innovate generatively, but the extraction is not maximal because coordination benefits are genuine — the liturgical system genuinely enables language continuity and shared cultural identity. The original research consensus treated this as pure coordination (Rope), but the structural data reveals victims: secular speakers whose innovative impulses are suppressed, and communities where innovation is blocked. The moderate value reflects both real coordination function and real extraction asymmetry. Suppression (0.52): Moderate-high. Institutional gatekeeping of canonical forms is explicit (educational enforcement of biblical Hebrew, rabbinical authority over textual interpretation, publication gatekeeping in religious contexts). Internalization mechanisms are also strong: identity-lock operates through treating innovation as cultural betrayal. However, suppression is not total — modern Hebrew literature, slang, and colloquial innovation persist despite institutional enforcement. Theater ratio (0.68): High. Educational instruction emphasizes biblical/liturgical Hebrew as canonical despite modern Hebrew's existence as a living language. Pedagogical emphasis on historical purity and correctness serves institutional interests (maintaining authority over interpretation) more than linguistic function. Secular state institutions maintain liturgical canonical teaching despite having no liturgical commitment — this is theater of the highest order, institutional inertia dressed as linguistic necessity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates radical perspectival disagreement. Religious institutional authority sees liturgical preservation as pure coordination — enabling religious continuity and textual fidelity (Rope). Modern Hebrew speakers see mixed coordination and extraction — the system enables language access but constrains innovation (Tangled Rope). Secularizing innovators see snare — their generative impulses are suppressed, they bear identity rupture cost if they innovate, the constraint feels immutable (trapped by identity-lock). Linguistic innovation movements see a temporary constraint with sunset — modern Hebrew literature and colloquial practice are building alternative transmission pathways that will eventually displace liturgical enforcement (Scaffold). Secular state language policy sees a degraded ritual it performs without understanding (Piton) — maintaining canonical teaching through institutional inertia, not functional necessity. The civilizational analytical observer risks seeing immutable natural law (Mountain) — 'only through liturgical preservation can minority languages survive' — until the structural data reveals this as naturalization of contingent institutional choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (religious institutional authority, textual transmission keepers) have institutional power and arbitrage exit options — they can choose to enforce, relax, or selectively transmit liturgical forms. Engine derives d ≈ 0.05-0.15 (full beneficiary), producing f(d) ≈ -0.12 to -0.01, negative effective extraction. Victims (secularizing speakers, innovation movements) have powerless or organized power and identity_locked or mobile exit options. Powerless + identity_locked derives d ≈ 0.89, producing f(d) ≈ 1.28, high effective extraction. Organized + mobile derives d ≈ 0.55, producing f(d) ≈ 0.75, moderate extraction. The perspectival gap emerges from this directionality distribution: beneficiaries experience rope (no extraction), victims experience snare (maximum extraction), organized movements experience tangled_rope (mixed). The analytical observer at civilizational scope is tempted toward mountain (immutable law) but the structural data reveals false summit: identifiable beneficiaries, active enforcement, alternative pathways being deliberately constrained.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING WITH TANGLED ROPE RESOLUTION: The mandatrophy is resolved by recognizing that this reading of the kernel (liturgical preservation) genuinely coordinates linguistic-cultural transmission (real coordination function: preserving shared textual heritage, enabling religious continuity, maintaining group identity across diaspora) WHILE ALSO extracting from agents who would innovate generatively (asymmetric cost: identity rupture, institutional gatekeeping, suppression of linguistic evolution). Both functions are real. The constraint is NOT pure coordination (it suppresses innovation) and NOT pure extraction (it enables genuine cultural continuity). Tangled Rope classification captures this hybrid: coordination function + asymmetric extraction + active enforcement. The mandatrophy is further resolved by recognizing that the sibling readings (native_generative, bridge_pidginized) are structurally COEXISTENT with this reading — they describe different mechanisms that can operate simultaneously. Modern Hebrew has BOTH liturgical preservation (religious domains, education policy, institutional authority) AND native speaker reproduction AND contact-zone innovation. The three readings coexist as different institutional layers operating in different domains. The constraint's classification depends which layer you measure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liturgical_necessity_empirical,
    'Is liturgical preservation (fixed textual forms, ritualized recitation) empirically NECESSARY for language continuity, or is it one sufficient but not necessary condition among multiple viable transmission mechanisms?',
    'Comparative historical analysis: other historically revived or minoritized languages (Irish, Basque, Navajo, etc.) and their transmission mechanisms. Analyze which features of those languages'' continuity relied on fixed ritual forms vs. generative community use vs. educational scaffolding. Longitudinal tracking of modern Hebrew literacy and native speaker acquisition with and without liturgical enforcement.',
    'If NECESSARY: liturgical preservation is a genuine natural law constraint (mountain confirmed). If one-sufficient-but-not-necessary: the constraint is a contingent institutional choice (false summit — reclassifies to tangled_rope). Impacts whether innovation movements face structural barriers (if necessary) or institutional barriers (if contingent).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liturgical_necessity_empirical, empirical, 'Whether liturgical preservation is empirically necessary for language continuity').

omega_variable(
    native_speaker_threshold_ambiguity,
    'Below what proportion of native speakers does a liturgical-preservation-dependent language face irreversible loss of transmission, even with institutional enforcement?',
    'Historical case studies: Dead Sea Sect Hebrew, Mishnaic Hebrew transmission rates, Medieval Judeo-Arabic communities with liturgical Arabic forms. Analysis of language acquisition curves when native speaker population drops below 5%, 1%, 0.1%. Experimental data from language immersion programs and second-language literacy in liturgical contexts.',
    'If threshold < 0.1% native speakers: modern Hebrew has crossed the danger line; liturgical preservation alone cannot sustain transmission. If threshold > 5%: institutional enforcement can sustain language with minimal native speakers. Directly affects whether the constraint succeeds long-term or becomes museum preservation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(native_speaker_threshold_ambiguity, empirical, 'Native speaker population threshold for language continuity').

omega_variable(
    generative_coexistence_possibility,
    'Can generative modern Hebrew (productive innovation, new vocabulary, colloquial variation) coexist with liturgical preservation within the same institutional framework, or are they structurally foreclosed to each other?',
    'Empirical: analysis of Hebrew used in contemporary religious contexts (modern Israeli synagogues, contemporary liturgical poetry, religious media). Document extent of innovation in religious domains vs. secular domains. Interview data from religious Hebrew speakers on perceived legitimacy of modern forms. Historical comparison: how Classical Arabic maintained both Quranic forms and generative dialects; how Sanskrit maintained liturgical and classical forms alongside Prakrit vernaculars.',
    'If COEXIST: tangled_rope classification confirmed; the constraint coordinates preservation while permitting innovation. If FORECLOSED: the reading forecloses the native_generative sibling reading (they cannot both be operative in the same framework). Affects sustainability of modern Hebrew revival — whether innovation must fight the constraint or can be channeled through it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generative_coexistence_possibility, conceptual, 'Whether generative modern Hebrew and liturgical preservation can coexist structurally').

omega_variable(
    reading_kernel_ambiguity,
    'Is the contested kernel (hebrew_continuity) fundamentally a LINGUISTIC claim (what mechanisms sustain language across time/space) or a CULTURAL/RELIGIOUS claim (what preserves Jewish identity through textual tradition), or both inseparably?',
    'Analyze the kernel as formulated by different reading communities: Haredi interpreters emphasize religious continuity and textual fidelity; secular Zionists emphasized linguistic revival; contemporary scholarship treats it as both. Map which features of each reading depend on linguistic necessity vs. religious commitment vs. cultural identity. Identify whether reframing the kernel as primarily cultural rather than linguistic changes the reading''s structural relationship to native speaker requirements.',
    'If primarily LINGUISTIC: native speaker generation becomes essential (favors native_generative sibling). If primarily RELIGIOUS: liturgical preservation is sufficient regardless of generative competence (supports current reading). If inseparably BOTH: the three sibling readings may be incompletely specified — additional readings needed. Affects whether modernization represents continuity or rupture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether the kernel is fundamentally linguistic or cultural or inseparably both').

omega_variable(
    suppression_internalization_mechanism,
    'Is the measured suppression (0.52) primarily STRUCTURAL (institutional enforcement, educational gatekeeping, publication barriers) or primarily INTERNALIZED (identity fusion with tradition, cognitive capture by canonical forms, shame at innovation)?',
    'Post-enforcement data: documentation of speakers'' language choices when institutional enforcement is removed (online spaces, private contexts, diaspora communities with weaker institutional oversight). Cognitive interviewing on identity-lock mechanisms: do speakers avoid innovation because they fear institutional punishment or because they experience innovation as identity rupture? Measurement of suppression trajectories in communities with high vs. low institutional enforcement of canonical forms.',
    'If primarily STRUCTURAL: exit from the constraint is possible when enforcement is removed (snare with mobile escape options). If primarily INTERNALIZED: exit requires identity transformation even without enforcement (snare with identity_locked exit). Affects whether linguistic innovation movements can succeed or will face internalized resistance even as institutional barriers erode.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether suppression is structural enforcement or internalized identity lock').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__liturgical_preservation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebrlit_tr_t0, hebrew_continuity__liturgical_preservation, theater_ratio, 0, 0.52).
narrative_ontology:measurement(hebrlit_tr_t10, hebrew_continuity__liturgical_preservation, theater_ratio, 10, 0.58).
narrative_ontology:measurement(hebrlit_tr_t20, hebrew_continuity__liturgical_preservation, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(hebrlit_be_t0, hebrew_continuity__liturgical_preservation, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(hebrlit_be_t10, hebrew_continuity__liturgical_preservation, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(hebrlit_be_t20, hebrew_continuity__liturgical_preservation, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__liturgical_preservation, identity_coordination).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__native_generative).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__bridge_pidginized).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, jewish_diaspora_identity_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, religious_institutional_legitimacy_textual_grounding).

% DUAL FORMULATION NOTE:
% Hebrew continuity decomposes into three structurally distinct constraints with different ε values. Liturgical_preservation (ε=0.38, tangled_rope) coordinates textual transmission while constraining innovation. Native_generative (ε=0.45+, likely snare or tangled_rope from different perspectives) requires reproducing native speakers and permits/requires innovation. Bridge_pidginized (ε=0.35-0.50, tangled_rope) operates through code-switching and contact effects in diaspora communities. Each reading activates different victim sets and beneficiary structures. The three constraints are linked through network relationships: liturgical preservation establishes the textual kernel that native generation must eventually achieve competence in; bridge pidginization operates in the margins where liturgical enforcement is weakest. Authors generating the sibling readings should reference this file for the kernel_id and shared reading_relations structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
