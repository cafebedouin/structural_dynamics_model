% ============================================================================
% CONSTRAINT STORY: native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_native_generation_reading, []).

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
 *   constraint_id: native_generation_reading
 *   human_readable: Native Generation Reading of Living Language Status
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   The native-generation reading is one institutionalized criterion for
 *   assessing whether a language is 'living' or 'dead.' This reading asserts
 *   that linguistic vitality requires native speaker transmission in
 *   daily-life domains — liturgical recitation, literary maintenance, or
 *   restricted-domain use are reframed as 'preservation of a corpse' rather
 *   than authentic language life. This reading has become institutionalized
 *   in UNESCO language documentation priorities, national education policy,
 *   and academic linguistics. However, it is ONE reading of a contested
 *   kernel (living-language-status) that admits at least two other coherent
 *   readings: the liturgical-preservation reading (linguistic vitality can
 *   persist through restricted-domain transmission with strong community
 *   identity markers) and the literary-continuity reading (written
 *   transmission and literary innovation demonstrate language vitality
 *   independent of native speaker generation). The native-generation reading
 *   is not a discovered fact but a committer position — a specific
 *   institutional interpretation that naturalizes secular nationalist
 *   assumptions (that national identity requires linguistic sovereignty, that
 *   daily-life multilingualism is subordinate to monoglossic national
 *   languages, that religious or literary transmission is inherently less
 *   vital than secular vernacular use). The constraint exhibits tangled rope
 *   classification: it coordinates genuine institutional interests (building
 *   national linguistic coherence, establishing standardization
 *   infrastructure) while simultaneously extracting from liturgical-only
 *   communities by delegitimizing their transmission pathway.
 *
 * KEY AGENTS:
 *   - Secular Nationalist Movement: Primary beneficiary (institutional/arbitrage) — gains legitimacy for linguistic sovereignty claims and national consolidation through the native-generation criterion
 *   - Liturgical-Only Communities: Primary victim (powerless/trapped) — cannot generate native speakers in daily life due to religious law or social structure; are delegitimized despite active transmission within restricted domain
 *   - Institutional Language Standardization: Secondary beneficiary (institutional/arbitrage) — the criterion justifies schools, media infrastructure, standardization bodies focused on daily-life transmission
 *   - Minority Language Speakers (Mixed Situation): Secondary victim (moderate/constrained) — face high institutional barriers to daily-life transmission but benefit from revitalization infrastructure the criterion generates
 *   - Liturgical Preservation Movement: Organized counteragent (organized/constrained) — benefits from language preservation resources generally but suffers delegitimization of their primary transmission pathway
 *   - Linguistics Academic Establishment: Institutional custodian (institutional/arbitrage) — maintains the criterion through disciplinary prestige despite mounting counterexamples; relies on it for institutional boundary maintenance
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the reading as a universal law rather than recognizing it as one committer position among multiple coherent readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(native_generation_reading, 0.52).
domain_priors:suppression_score(native_generation_reading, 0.65).
domain_priors:theater_ratio(native_generation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(native_generation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(native_generation_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(native_generation_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(native_generation_reading, tangled_rope).
narrative_ontology:human_readable(native_generation_reading, "Native Generation Reading of Living Language Status").
narrative_ontology:topic_domain(native_generation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(native_generation_reading, formalized).
narrative_ontology:cs_authority_grounding(native_generation_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(native_generation_reading).
narrative_ontology:cs_kernel_id(native_generation_reading, living_language_status).
narrative_ontology:cs_reading_relation(native_generation_reading, liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation(native_generation_reading, literary_continuity_reading, influences).
narrative_ontology:cs_axiom(native_generation_reading, foundational, native_speaker_daily_life_requirement).
narrative_ontology:cs_axiom_status(native_speaker_daily_life_requirement, holdable).
narrative_ontology:cs_axiom_grounding(native_generation_reading, native_speaker_daily_life_requirement, empirically_contingent).
narrative_ontology:cs_axiom(native_generation_reading, secondary, secular_national_identity_priority).
narrative_ontology:cs_axiom_status(secular_national_identity_priority, holdable).
narrative_ontology:cs_axiom_grounding(native_generation_reading, secular_national_identity_priority, conventional).
narrative_ontology:cs_reference_frame(native_generation_reading, living_language_secular_vernacular_nation).
narrative_ontology:cs_drift_state(native_generation_reading, contemporary_digital_multilingual_era, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(native_generation_reading, secular_nationalist_movement).
narrative_ontology:constraint_beneficiary(native_generation_reading, institutional_language_standardization).
narrative_ontology:constraint_victim(native_generation_reading, liturgical_only_communities).
narrative_ontology:constraint_victim(native_generation_reading, minority_religious_linguistic_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LITURGICAL-ONLY COMMUNITY (SNARE) — Trapped by the native-generation criterion. Cannot generate native speakers in daily secular life (no secular domain to speak in, or religious law prohibits secular use). Experiences total delegitimization: their language transmission is reframed as 'preservation of a corpse,' not living culture. No exit — cannot simultaneously maintain religious identity and satisfy the native-generation requirement. Maximum extraction: community's language is declared dead despite active transmission and religious vitality.
constraint_indexing:constraint_classification(native_generation_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MINORITY LANGUAGE SPEAKER WITH MIXED TRANSMISSION (TANGLED ROPE) — Faces high costs to establish native daily-life transmission (no secular institutions, limited employment domains, educational barriers) but also benefits from the coordination function: the native-generation criterion drives language revitalization infrastructure (schools, media, standardization). Asymmetric extraction — the speaker bears cost of proving 'liveness' but benefits from revival resources the criterion generates. Constrained exit: can maintain the language liturgically without meeting the criterion, but cannot access institutional support or cultural legitimacy without daily-life transmission.
constraint_indexing:constraint_classification(native_generation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SECULAR NATIONALIST MOVEMENT (ROPE) — Primary beneficiary (institutional/arbitrage). The native-generation criterion legitimates linguistic sovereignty claims and national consolidation around a standardized vernacular. Experiences the constraint as pure coordination: mobilizing native speakers for daily-life transmission solves the collective action problem of binding disparate speakers into a unified national linguistic community. Benefits from the criterion without extraction cost — can arbitrage it (shift to other linguistic ideologies if beneficial). The movement sees the criterion as solving a genuine coordination problem.
constraint_indexing:constraint_classification(native_generation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LITURGICAL PRESERVATION MOVEMENT (TANGLED ROPE) — Organized counteragent (organized/constrained). Benefits from the linguistic attention the native-generation criterion directs toward language preservation in general (institutional resources, academic study, documentation efforts). Simultaneously bears extraction: their primary transmission pathway (liturgy) is delegitimized as non-vital. Constrained exit: can argue for alternative criteria but cannot simply opt out of the native-generation framework — it has become institutionalized in education policy, cultural recognition systems, and language documentation priorities. The movement must work within the criterion's epistemic structure to defend liturgical transmission.
constraint_indexing:constraint_classification(native_generation_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LINGUISTICS ACADEMIC ESTABLISHMENT (PITON) — Maintains the native-generation criterion through institutional inertia and disciplinary prestige despite mounting empirical counterexamples. The criterion serves the academic establishment's coordination function (defining 'genuine' languages vs dialects, justifying fieldwork priorities, establishing expertise monopolies) but is degrading as a functional classification: many liturgically-transmitted languages show measurable vitality (diglossia maintenance, community identity, transmission to new generations within religious contexts). The establishment sees the criterion as scientifically rigorous but relies on it more for institutional boundary maintenance than for actual descriptive accuracy. Theater_ratio reflects that academic articles asserting the criterion publish without engaging counterexamples.
constraint_indexing:constraint_classification(native_generation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risks framing the native-generation criterion as a universal law of linguistic vitality rooted in immutable features of language acquisition and transmission. From this perspective, the criterion appears to express a natural fact about how languages survive: they must be acquired natively by children in daily life, or they degrade into ritual artifacts. However, the structural data (identifiable beneficiaries in the secular nationalist movement, victims in liturgical communities, institutional enforcement) reveal this as a false summit — the 'law of nature' is actually a contingent institutional reading that naturalizes one specific transmission pathway while delegitimizing others.
constraint_indexing:constraint_classification(native_generation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(native_generation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(native_generation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(native_generation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(native_generation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(native_generation_reading, TR),
    TR >= 0.70.

:- end_tests(native_generation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The native-generation reading creates asymmetric benefits and costs. The secular nationalist movement and institutional language standardizers benefit from the criterion without bearing its enforcement costs — they design policy and curriculum based on it. Liturgical-only communities and minority speakers bear the burden: they must either shift transmission patterns or accept delegitimization. The extraction is not maximal (0.70+) because the criterion does generate legitimate coordination benefits (it drives language documentation, revitalization programs, educational infrastructure that minority speakers can partially access). Suppression (0.65): High. Significant institutional structures enforce the native-generation reading: education policy prioritizes daily-life transmission; UNESCO language documentation emphasizes native speaker data; academic linguistics polices the boundary between 'living' languages and 'dead' ones or 'artificial' ones; national language policies often condition recognition on daily-life transmission statistics. Barriers to exit include institutional funding allocation, prestige hierarchies in linguistics, educational accreditation tied to language-in-schools metrics. However, suppression is not total (trapped-level 0.85+) because some communities can maintain liturgical transmission despite non-recognition, and the reading remains contested within sociolinguistics. Theater ratio (0.58): Moderate-high. The native-generation criterion involves substantial performative institutional activity: language documentation programs that prioritize native speaker interviews but may ignore literacy contexts; school-based revitalization that teaches formal language without ensuring community adoption; academic articles asserting the criterion with minimal empirical engagement with diglossic or liturgical communities. The theater reflects that much of the institutional infrastructure serves institutional legitimation more than actual linguistic vitality assessment.
 *
 * PERSPECTIVAL GAP:
 *   The native-generation reading produces dramatic perspectival gaps. The secular nationalist movement sees a coordination mechanism (Rope) — they are solving a collective action problem of binding speakers into a linguistic nation-state. The liturgical-only community sees extraction and delegitimization (Snare) — their transmission pathway is declared non-vital regardless of actual transmission success or community vitality. The minority language speaker with mixed transmission sees mixed benefits and burdens (Tangled Rope) — high institutional support for daily-life transmission but costs of learning standard forms and abandoning restricted-domain literacy. The liturgical preservation movement sees institutional pressure and counterargument work (Tangled Rope) — they benefit from preservation infrastructure generally but must constantly defend against the criterion's delegitimization. The academic establishment sees the criterion as scientifically rigorous (Piton) but maintains it largely through institutional inertia — empirical counterexamples (stable diglossia, community vitality in liturgical contexts) are published but do not dislodge the criterion. The analytical observer risks naturalizing the reading as a law of language (Mountain) but the structural data reveals beneficiaries and victims, indicating institutional contingency rather than natural necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   The native-generation reading's directionality is structured around the beneficiary/victim asymmetry. The secular nationalist movement benefits from the criterion without bearing enforcement costs — they have institutional power and can arbitrage other linguistic ideologies if needed. Their directionality d is low (0.15-0.20). Liturgical-only communities have no exit option: they cannot simultaneously maintain religious identity and satisfy the native-generation requirement in institutional contexts. Their directionality d is high (0.90+). The institutional language standardization apparatus benefits from the criterion's legitimacy; their d is low (0.20). Minority speakers with mixed transmission face constrained options: they can maintain liturgical use but at institutional cost; their d is moderate-high (0.55-0.65). The academic establishment maintains the criterion through prestige and disciplinary gatekeeping; their d is low (0.15-0.25). These directionality values feed into the chi calculation: beneficiaries experience low or negative effective extraction; victims experience high effective extraction; organized counteragents experience moderate extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by acknowledging that the native-generation reading is ONE reading of a contested kernel, not 'the' truth about living language. The tangled-rope classification (ε=0.52, suppression=0.65) reflects that the reading performs genuine coordination (standardizing national linguistic infrastructure, mobilizing revitalization resources) while extracting from liturgical communities by delegitimizing their transmission pathway. The false-summit risk occurs when the reading is naturalized as a universal law (Mountain) rather than recognized as an institutional interpretation grounded in secular nationalist ideology. The omega variables document the irreducible ambiguity in how the kernel is contested: different readings operationalize 'native speaker,' 'daily life,' and 'vitality' differently, and no neutral observation point resolves which reading is 'correct.' The mandatrophy is resolved by keeping the reading precise and perspectival: this is how the native-generation reading structures extraction and coordination, not how language vitality operates universally.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vitality_measurement_criterion,
    'What empirical markers constitute ''living'' linguistic vitality independent of transmission domain?',
    'Cross-cultural linguistic documentation comparing self-reported speaker vitality, intergenerational transmission rates, domain-specific maintenance, identity markers, and community language attitudes across liturgical, daily-life, and mixed transmission contexts',
    'If domain-agnostic markers (community identity, intergenerational knowledge retention, linguistic innovation) are equally valid: native-generation criterion is one reading among multiple, not a universal law. If domain matters fundamentally: the native-generation reading is justified. Current literature shows domain-independent vitality markers exist (liturgical communities with high transmission within restricted domain, strong identity markers, linguistic innovation), suggesting the criterion is a framing choice, not an empirical necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vitality_measurement_criterion, empirical, 'Whether linguistic vitality can be measured independently of transmission domain').

omega_variable(
    functional_diglossia_sustainability,
    'Can stable diglossia (high/liturgical vs low/daily-life) persist indefinitely as a self-sustaining transmission pattern, or does it inevitably collapse toward monoglossia?',
    'Longitudinal sociolinguistic studies of stable diglossic communities across 3+ generations; comparison of transmission success rates in High and Low domains; documentation of language shift patterns in communities with functional diglossia vs those with single-domain transmission',
    'If diglossia is stable: the native-generation criterion misidentifies a sustainable transmission pattern as degradation. If diglossia is inevitably unstable: the criterion reflects a real structural constraint about what transmission pathways preserve language across generations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_diglossia_sustainability, empirical, 'Whether functional diglossia can sustain indefinitely').

omega_variable(
    institutional_reading_contingency,
    'Is the native-generation reading a historically contingent product of 19th/20th century European nationalist ideology, or does it reflect universal principles of language transmission?',
    'Historical analysis of when and how the native-generation criterion became institutionalized in linguistics, UNESCO language documentation programs, and national education policy; comparison with pre-20th-century linguistic typologies and transmission concepts; documentation of non-European linguistic ideologies and how they frame ''living'' language',
    'If contingent: the reading is a committer choice reflecting a specific ideological position (secular nationalism), not a discovered fact. Sibling readings become genuinely coexistent rather than subordinate. If universal: the reading captures a real structural property of language vitality across cultures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_reading_contingency, conceptual, 'Whether the native-generation criterion reflects universal principles or nationalist ideology').

omega_variable(
    definition_kernel_ambiguity,
    'What counts as ''native speaker'' and ''daily life transmission''? How does this reading''s operationalization of these terms differ from sibling readings, and what are the epistemic consequences?',
    'Comparative analysis of how the native-generation reading operationalizes core terms (native speaker definition, daily-life domain scope, generational transmission success criteria) vs how liturgical-preservation and literary-continuity readings operationalize the same terms; documentation of speaker populations that fit one reading''s definition but not another''s',
    'If definitions are dependent on the reading''s embedded assumptions: the kernel (living-language-status) is genuinely ambiguous and different readings are incommensurable rather than competing for truth. If definitions are shared across readings: they compete as empirical claims about the same phenomenon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_kernel_ambiguity, conceptual, 'How this reading operationalizes core definitional terms and whether those operations are shared across sibling readings').

omega_variable(
    revitalization_effectiveness,
    'Do programs based on the native-generation reading actually produce stable intergenerational transmission in daily-life contexts, or do they produce performative language-in-schools without sustained community use?',
    'Longitudinal evaluation of language revitalization programs using native-generation criterion as a basis (e.g., immersion schools, daily-life promotion campaigns); measurement of post-program transmission success vs program investment; comparison with outcomes in communities that maintain linguistic identity through liturgical or literary transmission without pursuing daily-life nativization',
    'If native-generation programs are effective: the criterion is justified by empirical outcomes. If they produce theater (language instruction without community adoption): the criterion is sustaining institutional arrangements rather than linguistic vitality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revitalization_effectiveness, empirical, 'Whether language revitalization programs based on the native-generation criterion achieve sustained intergenerational transmission').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(native_generation_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(natgen_tr_t0, native_generation_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(natgen_tr_t3, native_generation_reading, theater_ratio, 3, 0.5).
narrative_ontology:measurement(natgen_tr_t6, native_generation_reading, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(natgen_be_t0, native_generation_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(natgen_be_t3, native_generation_reading, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(natgen_be_t6, native_generation_reading, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(native_generation_reading, identity_coordination).
narrative_ontology:affects_constraint(native_generation_reading, liturgical_preservation_reading).
narrative_ontology:affects_constraint(native_generation_reading, literary_continuity_reading).

% DUAL FORMULATION NOTE:
% The native-generation reading is part of a three-story constraint family unified by the contested kernel living-language-status. Each reading has a distinct ε value reflecting different empirical claims and structural relationships. The native-generation reading (ε=0.52, Tangled Rope) emphasizes institutional coordination and nationalist ideology; the liturgical-preservation reading (lower ε, Rope or Mountain) emphasizes domain-independent vitality markers; the literary-continuity reading (moderate ε, Rope or Tangled Rope) emphasizes written transmission and innovation. The three readings are linked through network.affects_constraints to show the kernel family structure and the empirical dependencies between competing readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(native_generation_reading, organized, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
