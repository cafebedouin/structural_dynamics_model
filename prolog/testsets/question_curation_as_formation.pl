% ============================================================================
% CONSTRAINT STORY: question_curation_as_formation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_question_curation_as_formation, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: question_curation_as_formation
 *   human_readable: Question Curation as Cognitive Formation in Subscription Media
 *   domain: media_studies/political_economy/cognitive_infrastructure
 *
 * SUMMARY:
 *   Question curation in subscription media operates as a dual-function
 *   constraint: it genuinely coordinates expert-audience connection
 *   (filtering thousands of submissions to enable manageable Q&A formats)
 *   while simultaneously training readership to understand geopolitical
 *   events through asset-protection and consumer-inconvenience frames. When a
 *   major geopolitical crisis occurs (invasion, coup, supply chain
 *   disruption), the selected questions consistently emphasize personal
 *   financial exposure ('how does this affect my 401k?'), consumer disruption
 *   ('will this cause food shortages in my area?'), and proximity to
 *   financial centers ('I live near NYC — should I be worried?'). Structural
 *   questions about power dynamics, resource extraction, or historical
 *   patterns are systematically filtered. Over biographical timescales,
 *   readers internalize the selection pattern: they learn to ask
 *   curated-compatible questions and stop formulating structural ones. The
 *   constraint exhibits rising extractiveness over the measurement interval
 *   (0.35 → 0.48) as subscription business models intensify pressure to avoid
 *   questions that might alienate advertisers or challenge reader comfort.
 *   Theater ratio rises moderately (0.25 → 0.38) as the appearance of reader
 *   participation masks the narrowing of acceptable question frames.
 *
 * KEY AGENTS:
 *   - Trained Reader: Primary victim (powerless/identity_locked) — structural question-asking capacity atrophied through years of exposure to curated patterns; exit requires reconstructing cognitive apparatus
 *   - Question Submitter: Secondary victim (moderate/constrained) — experiences both coordination (expert access) and extraction (reframing pressure); can exit at cost of social capital loss
 *   - Editorial Curation Team: Primary beneficiary (institutional/arbitrage) — solves genuine coordination problem while training readership toward advertiser-compatible frames
 *   - Alternative Media Ecosystem: Organized actors (organized/mobile) — benefits from subscriber migration but inherits cognitively formed audiences
 *   - Media Literacy Movement: Organized actors (organized/constrained) — building exit pathways through critical pedagogy; sees constraint as temporary with generational sunset
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes irreducible hybridity of coordination and extraction functions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(question_curation_as_formation, 0.48).
domain_priors:suppression_score(question_curation_as_formation, 0.52).
domain_priors:theater_ratio(question_curation_as_formation, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(question_curation_as_formation, extractiveness, 0.48).
narrative_ontology:constraint_metric(question_curation_as_formation, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(question_curation_as_formation, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(question_curation_as_formation, tangled_rope).
narrative_ontology:human_readable(question_curation_as_formation, "Question Curation as Cognitive Formation in Subscription Media").
narrative_ontology:topic_domain(question_curation_as_formation, "media_studies/political_economy/cognitive_infrastructure").

domain_priors:requires_active_enforcement(question_curation_as_formation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(question_curation_as_formation, subscription_business_model).
narrative_ontology:constraint_beneficiary(question_curation_as_formation, advertiser_aligned_framing).
narrative_ontology:constraint_victim(question_curation_as_formation, structural_question_asking_capacity).
narrative_ontology:constraint_victim(question_curation_as_formation, collective_sense_making).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE TRAINED READER (SNARE) — Identity-locked through internalized framing. The reader's capacity to formulate structural questions has atrophied through years of exposure to curated question patterns. Exit would require not just switching media sources but reconstructing their question-asking apparatus — recognizing that 'how does this affect my 401k?' is not a natural response to geopolitical crisis but a trained one. The constraint is structurally mobile (alternative media exists) but cognitively binding.
constraint_indexing:constraint_classification(question_curation_as_formation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: THE QUESTION SUBMITTER (TANGLED ROPE) — Constrained by selection pressure but also benefits from participation in the Q&A ecosystem. Submitting questions provides access to expert response and community visibility. The submitter experiences both coordination (getting questions answered, participating in discourse) and extraction (questions reframed or rejected if they challenge asset-protection framing). Can exit by switching platforms but at cost of losing accumulated social capital and expert access.
constraint_indexing:constraint_classification(question_curation_as_formation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE EDITORIAL CURATION TEAM (ROPE) — Primary beneficiary. Experiences the constraint as coordination: selecting questions that resonate with subscriber base, maintaining engagement metrics, protecting subscription renewal rates. The curation function genuinely solves a coordination problem (filtering signal from noise in submission pool) while simultaneously training readership toward advertiser-compatible frames. Net beneficiary — the constraint enables their institutional function.
constraint_indexing:constraint_classification(question_curation_as_formation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ALTERNATIVE MEDIA ECOSYSTEM (TANGLED ROPE) — Organized actors (independent substacks, cooperative newsrooms, open-source journalism platforms) see the constraint as both obstacle and opportunity. The mainstream curation pattern creates demand for structural analysis, but the trained readership brings asset-protection framing to alternative platforms. The ecosystem benefits from subscriber migration but inherits cognitively formed audiences. Mobile exit options but contamination persists.
constraint_indexing:constraint_classification(question_curation_as_formation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: THE MEDIA LITERACY MOVEMENT (SCAFFOLD) — Organized educational initiatives (critical media pedagogy, question-formulation technique training, structural analysis workshops) see the constraint as temporary. As media literacy spreads and readers develop meta-cognitive awareness of framing effects, the curation mechanism loses its formative power. Estimated sunset: 15-25 years as generational cohorts trained in structural question-asking reach critical mass. Constrained by institutional resistance and funding but sees clear exit path.
constraint_indexing:constraint_classification(question_curation_as_formation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, question curation serves a genuine coordination function (managing information flow, enabling expert-audience connection) while simultaneously extracting from collective sense-making capacity. The constraint is not reducible to pure extraction (readers do get questions answered) nor pure coordination (the selection pattern systematically excludes structural inquiry). The tangled rope classification reflects irreducible hybridity: both functions are real and structurally coupled.
constraint_indexing:constraint_classification(question_curation_as_formation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(question_curation_as_formation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(question_curation_as_formation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(question_curation_as_formation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(question_curation_as_formation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(question_curation_as_formation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint extracts from collective sense-making capacity by systematically filtering structural questions, but it also provides genuine coordination value (expert access, information filtering). The extraction is real but not total — some structural questions do get through, and readers do receive answers to the questions that are selected. The value reflects that roughly half the constraint's function is extractive overhead beyond coordination necessity. Suppression (0.52): Moderate. Significant barriers to formulating structural questions include internalized framing (identity lock), social pressure (peer question patterns), platform design (character limits, submission friction), and selection feedback (learning what gets chosen). But suppression is not total — alternative platforms exist, media literacy resources are available, and some readers do maintain structural question-asking capacity. Theater ratio (0.38): Moderate-low. The curation process has genuine functional content (actual filtering, actual expert matching) but also performative elements (appearance of reader participation, democratic veneer on editorial control). The theater has increased over the interval as subscription pressure intensifies the gap between stated mission (serving reader curiosity) and actual function (protecting renewal rates).
 *
 * PERSPECTIVAL GAP:
 *   The editorial team sees coordination (Rope) — they are solving the legitimate problem of managing information flow and enabling expert-audience connection. The media literacy movement sees a temporary problem with a sunset (Scaffold) — critical pedagogy is building cognitive exit pathways. The alternative media ecosystem sees mixed coordination and extraction (Tangled Rope) — they benefit from subscriber migration but inherit formed audiences. The question submitter sees mixed coordination and extraction (Tangled Rope) — the system both enables and constrains their participation. The trained reader sees pure extraction (Snare) — their structural question-asking capacity has been systematically degraded with no self-correction mechanism. The analytical observer sees irreducible hybridity (Tangled Rope) — both coordination and extraction functions are real and structurally coupled. The perspectival gap reveals that 'reader participation' means different things from different structural positions: from above it is coordination, from below it is formation.
 *
 * DIRECTIONALITY LOGIC:
 *   The trained reader is identity-locked: their question-asking capacity has been formed by the constraint, making exit cognitively costly even though alternative platforms are structurally available. This produces high directionality (d ≈ 0.89) — the reader is a victim with internalized binding. The question submitter is constrained but not identity-locked: they can switch platforms at cost of losing social capital and expert access. This produces moderate-high directionality (d ≈ 0.55) — victim status with exit friction. The editorial team is the primary beneficiary with arbitrage exit options: they benefit from the constraint's function and can move between platforms freely. This produces low directionality (d ≈ 0.05) — beneficiary with maximum mobility. The alternative media ecosystem is organized and mobile but still experiences extraction through audience contamination: they inherit readers trained in asset-protection framing. This produces moderate directionality (d ≈ 0.45) — mixed beneficiary/victim status with exit options. The media literacy movement is organized but constrained by institutional resistance: they see an exit path but face resource barriers. This produces moderate directionality (d ≈ 0.50) — organized victim with partial mobility.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that question curation is neither pure coordination (the editorial team's perspective) nor pure extraction (the trained reader's perspective) but an irreducible hybrid. The coordination function is real: without curation, expert-audience connection would collapse under submission volume. The extraction function is real: the selection pattern systematically excludes structural inquiry and trains readership toward asset-protection frames. The two functions are structurally coupled: the same editorial decision that enables coordination (selecting manageable question volume) also enables extraction (filtering for advertiser-compatible frames). The tangled rope classification captures this irreducible hybridity. The constraint cannot be decomposed into separate coordination and extraction stories because the same mechanism performs both functions simultaneously. The mandatrophy is resolved by recognizing that some constraints are genuinely hybrid at the structural level, not just perspectivally.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    submission_pool_composition,
    'What proportion of submitted questions already reflect asset-protection framing vs. structural inquiry? Is the curation selecting from a pre-formed pool or actively reshaping question patterns?',
    'Access to full submission data with longitudinal tracking; comparison of submission patterns across platforms with different curation policies; natural experiments when editorial teams change',
    'If submission pool is already 70%+ asset-protection framing: curation is amplifying existing reader formation (lower extractiveness, more coordination). If submission pool shows 50%+ structural questions that are systematically filtered: curation is primary formation mechanism (higher extractiveness, less coordination).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(submission_pool_composition, empirical, 'Whether curation selects from or shapes submission pool composition').

omega_variable(
    alternative_platform_decontamination,
    'Do readers who migrate to alternative media platforms gradually recover structural question-asking capacity, or does the cognitive formation persist across platform switches?',
    'Longitudinal study of question patterns from readers who switch from mainstream to alternative platforms; comparison of question sophistication at T=0 (switch point) vs T=1year, T=3years',
    'If decontamination occurs: the constraint is primarily institutional (tied to specific editorial practices) and exit is effective. If formation persists: the constraint is cognitive infrastructure that travels with the reader, making exit insufficient for recovery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_platform_decontamination, empirical, 'Whether cognitive formation persists after platform exit').

omega_variable(
    coordination_floor_threshold,
    'What is the minimum level of question curation required to maintain functional expert-audience connection? How much of the observed curation is coordination overhead vs extractive selection?',
    'Comparison of Q&A platforms with minimal curation (Reddit AMAs, open forums) vs heavy curation (NYT reader questions); measurement of signal-to-noise ratios, expert engagement rates, and question diversity across curation intensities',
    'If minimal curation maintains function: most observed curation is extractive (higher ε). If heavy curation is necessary: more of the constraint is coordination cost (lower ε, stronger rope component).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_floor_threshold, empirical, 'Minimum curation threshold for functional coordination').

omega_variable(
    generational_formation_reversibility,
    'Can readers trained under asset-protection curation for 10+ years recover structural question-asking capacity through media literacy intervention, or is the formation effectively permanent for biographical timescales?',
    'Intervention studies with long-term subscribers; pre/post assessment of question formulation patterns after critical media pedagogy; control groups with no intervention',
    'If reversible: identity_locked classification is appropriate (cognitive but not permanent). If irreversible at biographical scale: trapped classification more accurate (functionally permanent binding).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generational_formation_reversibility, empirical, 'Reversibility of cognitive formation at biographical timescales').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(question_curation_as_formation, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qcf_theater_2000, question_curation_as_formation, theater_ratio, 0, 0.25).
narrative_ontology:measurement(qcf_theater_2008, question_curation_as_formation, theater_ratio, 8, 0.32).
narrative_ontology:measurement(qcf_theater_2016, question_curation_as_formation, theater_ratio, 16, 0.38).

% Extraction over time
narrative_ontology:measurement(qcf_extract_2000, question_curation_as_formation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qcf_extract_2008, question_curation_as_formation, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(qcf_extract_2016, question_curation_as_formation, base_extractiveness, 16, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(question_curation_as_formation, information_standard).
narrative_ontology:affects_constraint(question_curation_as_formation, subscription_retention_imperative).

% DUAL FORMULATION NOTE:
% Question curation is downstream of the subscription retention imperative (the business model constraint that creates pressure to avoid challenging questions) but represents a distinct structural constraint with its own extractiveness value. The upstream constraint (subscription_retention_imperative) is modeled as a mountain — the business model is treated as an unchangeable background condition. The downstream constraint (question_curation_as_formation) is a tangled rope — the editorial practice that implements the business model's requirements while also serving a genuine coordination function. The network edge captures that the upstream constraint's immutability (from the editorial team's perspective) shapes the downstream constraint's hybrid character.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
