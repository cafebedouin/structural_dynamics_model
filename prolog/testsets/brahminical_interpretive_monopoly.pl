% ============================================================================
% CONSTRAINT STORY: brahminical_interpretive_monopoly
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_brahminical_interpretive_monopoly, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: brahminical_interpretive_monopoly
 *   human_readable: Brahminical Interpretive Monopoly in Hindu Knowledge Systems
 *   domain: epistemology/religious_authority/knowledge_production
 *
 * SUMMARY:
 *   The brahminical interpretive monopoly represents a centuries-long
 *   structure through which brahminical priesthood maintained exclusive
 *   authority over the interpretation of Hindu sacred texts (Vedas,
 *   Upanishads, Brahma Sutras, Bhagavad Gita) and ritual knowledge. This
 *   constraint operated through multiple mechanisms: Sanskrit literacy was
 *   restricted to brahmins; interpretive methodology was transmitted through
 *   brahminical lineages; institutional authority was concentrated in
 *   brahminical ritual specialists; non-brahmin challenges to interpretation
 *   were systematically suppressed. The constraint exhibits all six DR types
 *   from different structural perspectives. For non-brahmin interpreters, it
 *   appears as a Snare (pure extraction, no escape). For brahminical
 *   institutions, it appears as a Rope (coordination function, low
 *   experienced extraction). For organized movements challenging the
 *   monopoly, it appears as Tangled Rope (mixed coordination and extraction).
 *   For contemporary secular institutions and accessibility movements, it
 *   appears as Scaffold with sunset logic (alternative pathways
 *   proliferating). The constraint's trajectory shows declining
 *   extractiveness (0.72 → 0.58) and rising theater (0.35 → 0.68) over the
 *   interval, indicating that while the monopoly persists institutionally,
 *   its functional necessity has eroded and it is now maintained increasingly
 *   through cultural prestige and performative authority rather than through
 *   epistemic necessity. This pattern is characteristic of constraints
 *   transitioning from Tangled Rope (active extraction) toward Piton
 *   (inertial theater).
 *
 * KEY AGENTS:
 *   - Brahminical Priesthood: Primary beneficiary (institutional/arbitrage) — maintains interpretive authority, controls ritual knowledge transmission, benefits from cultural prestige
 *   - Non-Brahmin Interpreters: Primary victim (powerless/trapped) — systematically excluded from authoritative interpretation; face barriers to Sanskrit literacy, lineage legitimacy, institutional recognition
 *   - Vernacular Knowledge Traditions: Secondary victim (powerless/constrained) — regional languages, oral traditions, non-brahminical philosophies marginalized in 'authoritative' discourse
 *   - Reform Movement Scholars: Secondary agent (moderate/constrained) — attempted to reinterpret texts for modernity while navigating brahminical frameworks; mixed extraction and coordination
 *   - Anti-Caste Knowledge Movements: Organized challenger (organized/constrained) — Dalit intellectuals built alternative epistemologies; faced suppression but built epistemic communities
 *   - Vernacular Accessibility Movement: Scaffold builder (organized/mobile) — contemporary democratization of Sanskrit, digital accessibility, translation movements creating alternative authority pathways
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing caste-contingent authority as inherent to religious knowledge systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(brahminical_interpretive_monopoly, 0.58).
domain_priors:suppression_score(brahminical_interpretive_monopoly, 0.65).
domain_priors:theater_ratio(brahminical_interpretive_monopoly, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(brahminical_interpretive_monopoly, extractiveness, 0.58).
narrative_ontology:constraint_metric(brahminical_interpretive_monopoly, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(brahminical_interpretive_monopoly, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(brahminical_interpretive_monopoly, tangled_rope).
narrative_ontology:human_readable(brahminical_interpretive_monopoly, "Brahminical Interpretive Monopoly in Hindu Knowledge Systems").
narrative_ontology:topic_domain(brahminical_interpretive_monopoly, "epistemology/religious_authority/knowledge_production").

domain_priors:requires_active_enforcement(brahminical_interpretive_monopoly).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(brahminical_interpretive_monopoly, brahminical_priesthood).
narrative_ontology:constraint_beneficiary(brahminical_interpretive_monopoly, sanskrit_textual_authority).
narrative_ontology:constraint_victim(brahminical_interpretive_monopoly, non_brahmin_interpreters).
narrative_ontology:constraint_victim(brahminical_interpretive_monopoly, vernacular_knowledge_traditions).
narrative_ontology:constraint_victim(brahminical_interpretive_monopoly, epistemological_pluralism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VERNACULAR INTERPRETER (SNARE) — Non-brahmin scholars and practitioners attempting to offer interpretations of Vedic/Upanishadic texts face structural exclusion from authoritative discourse. Sanskrit literacy was historically restricted; ritual knowledge transmission required brahminical lineage. Even when textual access is available, the interpretive framework itself — what counts as valid commentary, proper philosophical methodology, legitimate textual authority — is gatekept by brahminical institutional structures. Exit costs are maximal: one cannot produce authoritative interpretation without brahminical certification or acceptance. The vernacular interpreter experiences pure extraction with no meaningful coordination benefit.
constraint_indexing:constraint_classification(brahminical_interpretive_monopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REFORM MOVEMENT SCHOLAR (TANGLED ROPE) — 19th-20th century reform movements (Brahmo Samaj, Arya Samaj, Hindu modernism) sought to reinterpret Vedic texts for contemporary contexts. These movements genuinely coordinated new readings of sacred texts — they solved the problem of making ancient authority relevant to modern practice. Simultaneously, they faced suppression from orthodox brahminical establishments and were forced to navigate brahminical epistemological frameworks even while challenging brahminical privilege. Mixed extraction: benefited from coordination function (enabled new interpretations) while bearing extraction costs (denied full authority, subjected to condemnation as heretical). Exit was possible but at high cost — career damage, excommunication threats, community division.
constraint_indexing:constraint_classification(brahminical_interpretive_monopoly, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: BRAHMINICAL PRIESTHOOD (ROPE) — The brahminical establishment experiences the constraint as a coordination mechanism: maintaining Sanskrit textual authority, controlling ritual knowledge transmission, and preserving lineage legitimacy are genuine coordinating functions. The priesthood benefits from exclusive interpretive authority while also providing coordinated spiritual leadership and textual preservation. From their position, the constraint solves the collective action problem of maintaining authoritative tradition across generations. Low experienced extraction because the beneficiary can arbitrage — they can exit the constraint entirely (become modern, secular) without massive loss, yet choose not to. This perspective perceives no meaningful coercion.
constraint_indexing:constraint_classification(brahminical_interpretive_monopoly, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANTI-CASTE KNOWLEDGE MOVEMENT (TANGLED ROPE) — Dalit intellectual and knowledge production movements (Phule, Ambedkar, contemporary Dalit scholarship) explicitly confronted brahminical interpretive monopoly. These movements coordinated alternative epistemologies and challenged the authority structure itself. They benefited from collective organizing and knowledge sharing while facing severe suppression: censorship, denial of access to educational institutions, intellectual delegitimation. The constraint involved both genuine alternative coordination (building Dalit epistemic communities) and asymmetric extraction (facing suppression and exclusion). Organized power gave them agency to challenge but not to fully escape the brahminical framework.
constraint_indexing:constraint_classification(brahminical_interpretive_monopoly, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: CONTEMPORARY INSTITUTIONAL FRAMEWORK (PITON) — Modern secular universities, translation movements, and global academic discourse have substantially eroded brahminical interpretive monopoly. Sanskrit is now taught in secular contexts; non-brahmin scholars produce authoritative interpretations; classical texts are accessible in vernacular translations. Yet brahminical institutional positions persist through cultural weight, temple authority structures, and narrative prestige. The constraint is maintained through theater — cultural authority claims, ritual positioning, heritage framing — rather than through functional necessity. The monopoly continues because the institutional legacy persists despite reduced structural function. Theater ratio is high: much brahminical authority now rests on performative tradition maintenance rather than epistemic necessity.
constraint_indexing:constraint_classification(brahminical_interpretive_monopoly, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: VERNACULAR ACCESSIBILITY MOVEMENT (SCAFFOLD) — Contemporary movements for democratizing Sanskrit knowledge — public Sanskrit education, digital accessibility projects, Dalit scholarship in English and regional languages, feminist reinterpretations, queer Hindu theology — represent a scaffold with genuine sunset logic. These movements reduce the extractive mechanism by lowering barriers to authoritative interpretation: Sanskrit becomes learnable outside brahminical lineages; interpretations multiply beyond brahminical gatekeeping; alternative authority sources emerge (academic credentials, community recognition, popular resonance). The extraction declines because alternatives proliferate. Estimated sunset: 20-40 years as digital accessibility and educational pluralism complete. Low effective extraction because exit pathways are multiplying.
constraint_indexing:constraint_classification(brahminical_interpretive_monopoly, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk perspective: At a civilizational/universal scale, some concentration of interpretive authority may appear structurally inevitable — specialized knowledge requires training lineages; sacred texts require expert mediators; interpretive traditions require continuity across generations. From this view, brahminical interpretive monopoly looks like an immutable feature of how complex religious knowledge systems must organize. However, this perspective risks naturalizing what are actually contingent caste structures. The constraint is not inherent to sacred knowledge systems generally — other traditions (Islamic scholarship, Jewish textual authority, Buddhist hermeneutics) distribute interpretive authority more pluralistically without losing coherence. The false summit indicates that the 'natural law' framing disguises contingent institutional power.
constraint_indexing:constraint_classification(brahminical_interpretive_monopoly, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(brahminical_interpretive_monopoly_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(brahminical_interpretive_monopoly, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(brahminical_interpretive_monopoly, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(brahminical_interpretive_monopoly, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(brahminical_interpretive_monopoly, TR),
    TR >= 0.70.

:- end_tests(brahminical_interpretive_monopoly_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high, declining over interval. The constraint extracts through control of interpretive authority, denial of equal epistemological standing to non-brahmin scholars, and gatekeeping of authoritative textual interpretation. The extractiveness is not as high as a pure Snare (0.72) because brahminical institutions also provide genuine coordinating functions — textual preservation, ritual expertise transmission, theological elaboration. However, extractiveness is substantially higher than pure Rope (≤0.35) because the coordination benefits flow overwhelmingly to brahmins while costs are borne by non-brahmins. The declining trajectory (0.72 → 0.58) reflects the erosion of the monopoly: non-brahmin scholars now publish authoritative interpretations; vernacular translations make texts accessible; academic expertise provides alternative authority sources. Suppression (0.65): High, stable. The suppression is structural (institutional barriers to Sanskrit learning, denial of ritual authority) and historically internalized (many non-brahmins have internalized brahminical authority as legitimate). While contemporary institutional suppression is less active than historical coercion, the framework persists through cultural weight, temple control structures, and the prestige of brahminical philosophical schools. Theater ratio (0.35 → 0.68): The rising trajectory indicates that brahminical interpretive authority increasingly relies on cultural prestige and performative tradition maintenance rather than on functional necessity. Early in the interval, brahminical authority rested on genuine epistemic barriers (near-monopoly on Sanskrit knowledge); by the end, non-brahmin scholars can interpret texts but brahminical institutions retain cultural authority through heritage positioning and ritual prestige. This rising theater indicates transition toward Piton classification.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gaps are profound and reveal the constraint's structural contradictions. Brahminical priesthood perceives Rope (pure coordination, solving the problem of maintaining authoritative tradition). Non-brahmin interpreters perceive Snare (pure extraction, no path to equal epistemic standing). Reform movements perceived Tangled Rope (attempting genuine alternative coordination while facing suppression from orthodoxy). Dalit knowledge movements perceived Tangled Rope with acute suppression (building alternative epistemologies while facing active condemnation). Contemporary accessibility movements perceive Scaffold (barriers are falling, alternative authorities proliferating, sunset approaching). The civilizational analytical observer risks perceiving Mountain (treating brahminical authority as inherent to sacred knowledge systems). The perspectival gaps are not measurement artifacts — they reflect real structural differences in how agents experience the same constraint. A brahmin claiming interpretive authority faces no extraction; a non-brahmin claiming equal interpretive authority faces institutional resistance. These are not the same constraint from two angles; they are different structural positions in the same extractive system.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status and exit options. Brahminical priesthood (institutional/arbitrage): beneficiary status + arbitrage exit (can become secular, cosmopolitan, modern) yields low d (d ≈ 0.15), producing negative f(d) and low/negative χ. They perceive minimal extraction because exit is available and they benefit from the constraint. Non-brahmin interpreters (powerless/trapped): victim status + trapped exit (cannot gain brahminical legitimacy without adopting brahminical frameworks) yields high d (d ≈ 0.92), producing high f(d) and high χ. They perceive maximum extraction because exit is not available within the epistemic system. Reform scholars (moderate/constrained): beneficiary of alternative coordination + constrained exit (can interpret but with career/community costs) yields moderate d (d ≈ 0.58), producing moderate f(d) and moderate χ. Dalit movements (organized/constrained): victim of suppression + organized power + constrained exit (can challenge but with suppression costs) yields moderate-high d (d ≈ 0.62), producing moderate-high χ. Accessibility movements (organized/mobile): victim of monopoly + mobile exit (can build alternatives, work outside brahminical institutions) yields lower d (d ≈ 0.42), producing lower χ. These derivations capture the real structural differences in how agents experience the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint demonstrates how mandatrophy appears when a single institutional structure appears as both pure coordination (Rope from beneficiary perspective) and pure extraction (Snare from victim perspective). The resolution lies not in choosing a single classification but in recognizing that the constraint is Tangled Rope at the system level — it provides genuine coordination functions (textual preservation, theological continuity) while distributing extraction asymmetrically (benefits to brahmins, costs to non-brahmins). The mandatrophy dissolves when we recognize that the question 'is brahminical authority coordination or extraction?' has a context-dependent answer: it is coordination for brahmins, extraction for non-brahmins, and Tangled Rope from an analytical perspective that sees both functions. The constraint's trajectory (declining extractiveness, rising theater) indicates it is transitioning from active Tangled Rope (extractive enforcement) toward Piton (inertial theater). This transition is itself diagnostic: the monopoly persists not because it is functionally necessary but because institutional inertia and cultural prestige maintain it. Were the constraint truly Mountain (inherent natural law), the theater ratio would not rise — functional necessity would remain constant. Rising theater indicates degradation toward Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lineage_necessity_threshold,
    'Is brahminical lineage training genuinely necessary for authoritative interpretation, or is it contingently required by gatekeeping?',
    'Historical comparison with non-brahminical interpretive traditions; analysis of interpretive quality and textual coherence in scholar works across caste backgrounds; longitudinal tracking of acceptance patterns as non-brahmin scholarship proliferates',
    'If lineage training is necessary: constraint reflects coordination efficiency (lower extraction estimates). If contingent gatekeeping: constraint is primarily extractive (higher extraction estimates, classification shifts toward Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lineage_necessity_threshold, empirical, 'Whether brahminical lineage is functionally necessary for interpretation').

omega_variable(
    suppression_mechanism_internalization,
    'How much of the suppression of non-brahmin interpreters is structural (institutional barriers) versus internalized (non-brahmin scholars believing brahminical authority is legitimate)?',
    'Post-suppression trajectory analysis: do non-brahmin interpreters continue self-censoring after institutional barriers are removed? Cognitive framing studies of scholarly confidence across caste backgrounds; historical analysis of when and why non-brahmin scholars began claiming interpretive authority',
    'If primarily structural: suppression metric is accurate (0.65). If partially internalized: actual suppression effectiveness is higher than the metric suggests (non-brahmin scholars carry internalization after barrier removal). Affects omega variables around identity_locked exit options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of non-brahmin interpreters is structural or internalized').

omega_variable(
    alternative_authority_legitimacy,
    'Do non-brahmin, vernacular, and academic alternative interpretations actually constitute legitimate alternative authorities, or do they remain dependent on brahminical frameworks for legitimacy?',
    'Discourse analysis of how non-brahmin interpretations are framed (as ''modern reinterpretations,'' ''contextual readings,'' ''academic approaches'' vs. as ''authority'' in their own right); frequency of citations between brahminical and non-brahmin scholarship; institutional positioning (do universities position non-brahmin scholarship as equally authoritative or as specialized subfields?)',
    'If legitimately alternative: constraint is being dissolved (scaffold sunset is real). If still dependent on brahminical frameworks: alternative authorities are pseudo-authorities, extraction persists despite appearance of democratization (constraint persists as Snare with improved theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_authority_legitimacy, empirical, 'Whether alternative authorities are genuinely legitimate or dependent on brahminical frameworks').

omega_variable(
    contemporary_gatekeeping_persistence,
    'Is brahminical interpretive monopoly persisting through conscious institutional gatekeeping or through inertial cultural authority?',
    'Analysis of institutional decision-making in temples, cultural organizations, and religious authority structures; interviews with authority holders; tracking of conflicts over interpretive authority in contemporary Hindu institutions; comparison of gatekeeping intensity across urban/rural and modern/traditional contexts',
    'If conscious gatekeeping: suppression metric should be higher (active enforcement ongoing). If inertial authority: theater ratio is more significant (constraint maintained through prestige, not active exclusion). Affects whether constraint is Tangled Rope (active enforcement) or Piton (inertial theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contemporary_gatekeeping_persistence, empirical, 'Whether brahminical monopoly persists through active gatekeeping or cultural inertia').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(brahminical_interpretive_monopoly, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(brah_tr_t0, brahminical_interpretive_monopoly, theater_ratio, 0, 0.35).
narrative_ontology:measurement(brah_tr_t3, brahminical_interpretive_monopoly, theater_ratio, 3, 0.48).
narrative_ontology:measurement(brah_tr_t6, brahminical_interpretive_monopoly, theater_ratio, 6, 0.62).
narrative_ontology:measurement(brah_tr_t10, brahminical_interpretive_monopoly, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(brah_be_t0, brahminical_interpretive_monopoly, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(brah_be_t3, brahminical_interpretive_monopoly, base_extractiveness, 3, 0.68).
narrative_ontology:measurement(brah_be_t6, brahminical_interpretive_monopoly, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(brah_be_t10, brahminical_interpretive_monopoly, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(brahminical_interpretive_monopoly, identity_coordination).
narrative_ontology:affects_constraint(brahminical_interpretive_monopoly, varna_jati_hierarchy).
narrative_ontology:affects_constraint(brahminical_interpretive_monopoly, sanskrit_literacy_restriction).
narrative_ontology:affects_constraint(brahminical_interpretive_monopoly, ritual_authority_concentration).

% DUAL FORMULATION NOTE:
% The brahminical interpretive monopoly is upstream of more specific constraints on Sanskrit literacy and ritual authority. The interpretive monopoly is the overarching structural constraint that enables and perpetuates specific gatekeeping mechanisms. Decomposition into separate stories: brahminical_interpretive_monopoly (this story, system-level extraction through knowledge authority) affects sanskrit_literacy_restriction (ε≈0.68, more acute Snare for non-brahmin learners) and ritual_authority_concentration (ε≈0.62, gatekeeping of priesthood roles). All three stories should be linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(brahminical_interpretive_monopoly, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
