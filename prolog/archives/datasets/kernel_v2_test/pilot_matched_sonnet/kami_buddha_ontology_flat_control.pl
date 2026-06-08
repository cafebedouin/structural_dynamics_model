% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology_flat_control, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kami_buddha_ontology_flat_control
 *   human_readable: Ontological Status of Kami-Buddha Relationship in Japanese Religious Practice
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   The ontological status of the kami-buddha relationship in Japanese
 *   religious practice represents a millennium-long coordination mechanism
 *   that enabled simultaneous veneration of indigenous kami and imported
 *   Buddhist deities without requiring practitioners to resolve the
 *   metaphysical question of whether these constitute a unified cosmology or
 *   domain-partitioned coexistence. From the Nara period (8th century)
 *   introduction of Buddhism through the present, the vast majority of
 *   Japanese practitioners have engaged both traditions without experiencing
 *   the ontological ambiguity as problematic. The constraint's primary
 *   function is coordination: it allows access to complementary spiritual
 *   resources (kami for this-worldly benefits and ritual purity, buddhas for
 *   afterlife concerns and karmic resolution) without forcing exclusive
 *   commitment to either tradition's truth claims. The Meiji period
 *   (1868-1945) represents a dramatic but temporary spike in extraction and
 *   suppression: the state's shinbutsu bunri edicts forced institutional
 *   separation, destroyed jingu-ji complexes, and attempted to subordinate
 *   both traditions to state Shinto ideology. This period shows high
 *   theater_ratio (0.65) because the forced separation was largely
 *   performative — it rearranged institutions without transforming popular
 *   practice, which remained syncretistic. Post-1945, metrics return to
 *   pre-Meiji baselines as legal restrictions lifted and practitioners
 *   resumed dual veneration. The constraint's low base extractiveness (0.18)
 *   reflects that the ontological ambiguity primarily enables rather than
 *   constrains practice, though modest extraction exists in institutional
 *   arrangements that benefit from maintaining the ambiguity rather than
 *   resolving it.
 *
 * KEY AGENTS:
 *   - Village and Urban Practitioners: Primary beneficiaries (powerless to moderate / mobile) — gain flexible access to complementary spiritual resources without ontological commitment costs
 *   - Temple-Shrine Complexes: Institutional beneficiaries (institutional / constrained) — jingu-ji arrangements enabled resource sharing and expanded institutional capacity through syncretism
 *   - Ritual Specialists: Professional beneficiaries (institutional / mobile) — kami-buddha ambiguity created stable complementary professional niches for Shinto priests and Buddhist monks
 *   - Meiji State Apparatus: Temporary extractor (institutional / constrained) — forced disambiguation for state control purposes, disrupting centuries of functional syncretism
 *   - Comparative Religionist: Analytical observer (analytical / analytical) — risks naturalizing historically contingent Japanese syncretism as universal religious pluralism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology_flat_control, 0.18).
domain_priors:suppression_score(kami_buddha_ontology_flat_control, 0.25).
domain_priors:theater_ratio(kami_buddha_ontology_flat_control, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology_flat_control, extractiveness, 0.18).
narrative_ontology:constraint_metric(kami_buddha_ontology_flat_control, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(kami_buddha_ontology_flat_control, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology_flat_control, rope).
narrative_ontology:human_readable(kami_buddha_ontology_flat_control, "Ontological Status of Kami-Buddha Relationship in Japanese Religious Practice").
narrative_ontology:topic_domain(kami_buddha_ontology_flat_control, "religious_studies/japanese_history/ontology_of_practice").

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(kami_buddha_ontology_flat_control, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology_flat_control, practitioners_of_dual_veneration).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology_flat_control, temple_shrine_complexes).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology_flat_control, ritual_specialists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology_flat_control, village_practitioners).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology_flat_control, urban_lay_practitioners).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology_flat_control, shinto_priests).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology_flat_control, buddhist_monks).
narrative_ontology:constraint_vindicates(kami_buddha_ontology_flat_control, religious_syncretism_stability).
narrative_ontology:constraint_vindicates(kami_buddha_ontology_flat_control, ontological_pluralism_coherence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage both shrine and temple practices for complementary spiritual needs (harvest blessings from kami, ancestor memorials with Buddhist clergy) without experiencing ontological tension. Can choose which practices to engage based on immediate needs. The ontological question of whether kami and buddhas are unified or separate is not salient to their practice.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology_flat_control, village_practitioners, beneficiary,
    powerless, biographical, mobile, local).

% Navigate multiple religious institutions (shrine visits for life transitions like births and weddings, temple affiliation for funerals and ancestor veneration) as complementary services. Have resources to access both traditions and can choose level of engagement. The dual veneration pattern is culturally normative and requires no doctrinal commitment.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology_flat_control, urban_lay_practitioners, beneficiary,
    moderate, biographical, mobile, regional).

% Jingu-ji (shrine-temple complexes) institutionalized dual veneration from Nara through early Meiji periods. The ontological ambiguity allowed both traditions to coexist within single institutional sites, coordinating ritual calendars, sharing resources, and expanding institutional capacity. Face institutional inertia and property arrangements that constrain rapid change, but benefit from the syncretistic arrangement.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology_flat_control, temple_shrine_complexes, beneficiary,
    institutional, generational, constrained, national).

% Developed specialized ritual domain focused on this-worldly benefits (harvest, fertility, purification) and life-cycle transitions. The kami-buddha ambiguity enabled functional specialization without requiring metaphysical resolution or competition with Buddhist clergy. Can emphasize Shinto tradition while coexisting with Buddhist institutions.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology_flat_control, shinto_priests, beneficiary,
    institutional, civilizational, mobile, national).

% Developed specialized ritual domain focused on afterlife concerns, karmic resolution, and ancestor veneration. The kami-buddha ambiguity enabled functional specialization and institutional expansion without requiring suppression of indigenous kami worship. Can emphasize Buddhist tradition while coexisting with Shinto institutions.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology_flat_control, buddhist_monks, beneficiary,
    institutional, civilizational, mobile, national).

% Imposed shinbutsu bunri (kami-buddha separation) edicts 1868-1874 to standardize religious administration for modern state purposes and subordinate both traditions to state Shinto ideology. Extracted institutional control by forcing explicit categorization (shrine vs temple, kami vs buddha), disrupting centuries of syncretism and destroying jingu-ji complexes. The separation served state interests but disrupted functional coordination.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology_flat_control, meiji_state_apparatus, agenda_setter,
    institutional, biographical, constrained, national).

% Study Japanese syncretism as instance of broader patterns in religious practice. Risk naturalizing historically contingent institutional arrangements as universal laws of religious cognition (ontological pluralism as inherent to lived religion). The analytical distance enables cross-cultural comparison but may obscure the specific political and cultural conditions that shaped Japanese kami-buddha syncretism.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology_flat_control, comparative_religionists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables simultaneous access to complementary spiritual resources (kami for this-worldly benefits and ritual purity, buddhas for afterlife concerns and karmic resolution) without requiring practitioners to resolve metaphysical questions or commit exclusively to either tradition's truth claims. Coordinates institutional coexistence through ontological ambiguity.
% TRANSFER_FUNCTION: Practitioners provide ritual participation and material support (offerings, fees) to both shrine and temple institutions. In return, they receive complementary spiritual services and social legitimation. The ontological ambiguity allows this dual flow without forcing zero-sum competition between traditions.
% ABSENT_VOICES: Doctrinal purists from both traditions (Buddhist exclusivists who reject kami worship as non-Buddhist, Shinto nativists who reject Buddhist influence as foreign contamination) are structurally excluded from the mainstream syncretistic arrangement. These voices became prominent during Meiji shinbutsu bunri but were marginal during most of Japanese history. Their absence from the dominant arrangement is not suppression but lack of popular resonance — most practitioners did not experience ontological ambiguity as problematic.
% DISAPPEARANCE_RATIONALE: If the ontological ambiguity disappeared and practitioners were forced to choose exclusive commitment to either kami or buddha veneration, the entire institutional landscape would rearrange. Temple-shrine complexes would dissolve, complementary ritual specializations would collapse into competition, and practitioners would lose flexible access to spiritual resources. The Meiji period demonstrates this: forced disambiguation disrupted centuries of functional syncretism and required massive institutional reorganization. The rapid return to syncretistic practice after 1945 legal restrictions lifted shows that the arrangement is not natural law but depends on the ontological ambiguity being maintained.
% FOUNDING_PROBLEM: The introduction of Buddhism to Japan (6th-8th centuries) created a potential conflict with indigenous kami worship. The founding problem was how to integrate the new tradition without suppressing the old, and how to enable practitioners to access both without requiring them to resolve metaphysical questions about the relationship between kami and buddhas. The ontological ambiguity emerged as the solution: allow simultaneous veneration without forcing cosmological disambiguation.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary ethnographic studies (Reader & Tanabe 1998, Nelson 2000) document that practitioners continue to engage both traditions without experiencing ontological tension. Post-1945 revival of syncretistic practice after Meiji separation demonstrates that the coordination function remains live. The founding problem (how to integrate multiple traditions without forced exclusive commitment) persists in contemporary Japanese religious practice, corroborated by both scholarly observers and practitioners themselves who describe their practice as naturally incorporating both traditions.
narrative_ontology:disappearance_verdict(kami_buddha_ontology_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology_flat_control, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VILLAGE PRACTITIONER (ROPE) — Experiences simultaneous kami-buddha veneration as practical coordination solving real spiritual needs. Visits shrine for harvest blessing, temple for ancestor memorial, with no ontological crisis. Mobile exit (can choose which practices to engage) and low extraction — the constraint coordinates access to complementary spiritual resources without imposing costs.
constraint_indexing:constraint_classification(kami_buddha_ontology_flat_control, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: URBAN LAY PRACTITIONER (ROPE) — Navigates multiple religious institutions (shrine visits for life transitions, temple affiliation for funerals) as complementary services. The ontological question of whether kami and buddhas occupy unified or partitioned domains is not salient to practice. Coordination function: the constraint allows access to both traditions without requiring doctrinal commitment to either's exclusive truth claims.
constraint_indexing:constraint_classification(kami_buddha_ontology_flat_control, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: TEMPLE-SHRINE COMPLEX (ROPE) — Jingu-ji (shrine-temple complexes) institutionalized dual veneration from Nara through early Meiji. The ontological ambiguity was structurally functional: it allowed both traditions to coexist within single institutional sites, coordinating ritual calendars and resource sharing. Constrained exit (institutional inertia, property arrangements) but net beneficiary — the arrangement expanded rather than contracted institutional capacity.
constraint_indexing:constraint_classification(kami_buddha_ontology_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MEIJI STATE APPARATUS (TANGLED ROPE) — Shinbutsu bunri (kami-buddha separation) edicts 1868-1874 forced ontological disambiguation for state purposes. The state extracted institutional control by requiring explicit categorization (shrine vs temple, kami vs buddha), disrupting centuries of syncretism. Genuine coordination function (standardizing religious administration for modern state) but asymmetric extraction (destroyed jingu-ji complexes, forced clergy to choose affiliation, subordinated both traditions to state Shinto ideology). Requires active enforcement — the separation was not natural to practice.
constraint_indexing:constraint_classification(kami_buddha_ontology_flat_control, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RITUAL SPECIALIST (ROPE) — Shinto priests and Buddhist monks developed complementary ritual domains over centuries: kami for this-worldly benefits and purity, buddhas for afterlife and karmic resolution. The ontological ambiguity enabled functional specialization without requiring metaphysical resolution. Mobile exit (specialists can emphasize one tradition) and net beneficiary — the constraint created stable professional niches.
constraint_indexing:constraint_classification(kami_buddha_ontology_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: COMPARATIVE RELIGIONIST / PLURALISM VIEW (MOUNTAIN) — From a civilizational analytical perspective, ontological ambiguity in religious practice is a universal feature of lived religion. Practitioners across cultures hold multiple cosmological frameworks simultaneously without experiencing contradiction. This view sees the kami-buddha relationship as an instance of a general pattern: religious practice does not require metaphysical consistency. However, this risks naturalizing what is actually a historically contingent institutional arrangement — the specific form of Japanese syncretism emerged from particular political and cultural conditions, not from universal laws of religious cognition.
constraint_indexing:constraint_classification(kami_buddha_ontology_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kami_buddha_ontology_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kami_buddha_ontology_flat_control, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(kami_buddha_ontology_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The ontological ambiguity primarily coordinates rather than extracts. Practitioners benefit from flexible access to both traditions. Institutional arrangements (temple-shrine complexes, complementary ritual domains) enabled resource sharing rather than zero-sum competition. Modest extraction exists: some institutional actors benefit from maintaining ambiguity rather than resolving it (e.g., dual affiliation revenue streams), and honji suijaku doctrine may naturalize Buddhist cosmological priority. But the dominant function is coordination, not extraction. Suppression (0.25): Low in baseline periods. Practitioners face minimal barriers to dual veneration — the constraint does not suppress alternatives because it IS the alternative to forced exclusive commitment. Meiji period shows dramatic spike (0.75) as state edicts forced institutional separation and suppressed syncretistic practice, but this was temporary. Post-1945 return to low suppression as legal restrictions lifted. Theater ratio (0.15): Low in baseline periods. The ontological ambiguity is functional, not performative — it genuinely enables practice rather than masking dysfunction. Modest theater exists in doctrinal elaborations (honji suijaku scholasticism) that may serve institutional legitimation more than practitioner needs. Meiji period spike (0.65) reflects that forced separation was largely theater — institutional rearrangement without ontological transformation, as popular practice remained syncretistic despite official categorization.
 *
 * PERSPECTIVAL GAP:
 *   Village and urban practitioners experience pure coordination (Rope) — the ontological ambiguity is invisible or irrelevant to practice; they simply access complementary spiritual resources as needed. Temple-shrine complexes and ritual specialists also see coordination (Rope) — the ambiguity enabled institutional cooperation and professional specialization. The Meiji state apparatus experienced and imposed tangled rope — genuine coordination function (standardizing religious administration) combined with asymmetric extraction (forced disambiguation for state control, destruction of jingu-ji complexes, subordination to state ideology). The analytical observer risks seeing mountain (ontological pluralism as universal law of religious cognition) but this naturalizes what is actually a historically contingent institutional arrangement — Japanese syncretism took its specific form due to particular political and cultural conditions, not universal cognitive laws. The perspectival gap is narrow for most agents (most see rope) except during the Meiji disruption, when the state's tangled rope perspective temporarily dominated.
 *
 * DIRECTIONALITY LOGIC:
 *   Practitioners, temple-shrine complexes, and ritual specialists are all beneficiaries — they gain from the coordination function without bearing significant costs. Directionality for these agents is low (near 0.0), producing low or negative effective extraction (they experience the constraint as enabling). The Meiji state apparatus is the primary extractor during 1868-1945 — it benefits from forced disambiguation while practitioners and institutions bear the costs of disruption. During this period, practitioners and institutions become victims with higher directionality values, experiencing elevated extraction. Post-1945, the system returns to its baseline coordination structure with most agents as beneficiaries. No victims are declared for the baseline constraint because the ontological ambiguity does not systematically extract from any agent — it coordinates access to complementary resources. The Meiji period is the exception, but that is a temporary perturbation rather than the constraint's stable structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates how ontological ambiguity can be primarily coordinative rather than extractive. The mandatrophy risk is labeling all religious syncretism as extraction (institutional actors maintaining ambiguity to collect from both traditions) when the structural data shows that practitioners genuinely benefit from flexible access to complementary spiritual resources. The ontological question (unified vs partitioned cosmology) is not resolved because resolution is not necessary for function — the ambiguity IS the coordination mechanism. The Meiji period shows what extraction looks like when imposed on this substrate: forced disambiguation disrupts the coordination function and concentrates benefits in state control. The constraint's low extractiveness and suppression in baseline periods, combined with clear beneficiary structure and no systematic victims, supports rope classification from most perspectives. The analytical mountain perspective is flagged as potential false summit — it risks naturalizing contingent historical arrangements as universal laws of religious cognition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unified_vs_partitioned_cosmology,
    'Does simultaneous kami-buddha veneration reflect a unified cosmology (honji suijaku doctrine: buddhas as true nature, kami as local manifestations) or domain-partitioned coexistence (kami and buddhas occupy separate ontological spheres with complementary functions)?',
    'Historical analysis of doctrinal texts, ritual manuals, and practitioner testimony across periods. Ethnographic study of contemporary practitioners'' cosmological models. Comparison of pre-Meiji syncretism vs post-Meiji separation vs post-1945 revival patterns.',
    'If unified: the constraint is primarily coordination (Rope from more perspectives) — a shared cosmological framework enabling institutional cooperation. If partitioned: the constraint is more extractive (Tangled Rope from more perspectives) — institutional arrangements that suppress ontological clarity to maintain dual revenue streams.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unified_vs_partitioned_cosmology, conceptual, 'Whether dual veneration reflects unified or partitioned cosmology').

omega_variable(
    practitioner_ontological_commitment,
    'Do practitioners experience ontological ambiguity as a feature (enabling flexible engagement with multiple spiritual resources) or as a bug (creating cognitive dissonance that must be managed)?',
    'Ethnographic interviews with contemporary practitioners across age cohorts and urban/rural contexts. Historical analysis of popular religious texts and pilgrimage records. Psychological studies of religious cognition in syncretistic contexts.',
    'If feature: the constraint is pure coordination (Rope) — the ambiguity is the solution, not the problem. If bug: the constraint has extractive elements (Tangled Rope) — institutions benefit from practitioners'' inability to resolve the tension.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(practitioner_ontological_commitment, empirical, 'Whether practitioners experience ontological ambiguity as functional or problematic').

omega_variable(
    meiji_separation_persistence,
    'Did Meiji shinbutsu bunri successfully disambiguate kami-buddha ontology, or did it merely force institutional separation while leaving popular practice syncretistic?',
    'Comparison of pre-1868 and post-1945 practice patterns. Analysis of how quickly jingu-ji complexes reformed after legal restrictions lifted. Survey of contemporary practitioners'' ontological models vs institutional affiliations.',
    'If successful: Meiji period represents a genuine phase transition in the constraint''s structure (high suppression period that permanently altered the substrate). If unsuccessful: Meiji separation was theater (high theater_ratio during that period) — institutional rearrangement without ontological transformation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(meiji_separation_persistence, empirical, 'Whether Meiji separation achieved ontological disambiguation or only institutional separation').

omega_variable(
    honji_suijaku_naturalization,
    'Is honji suijaku doctrine (buddhas as original nature, kami as trace manifestations) a genuine metaphysical synthesis or an institutional accommodation that naturalizes Buddhist priority?',
    'Doctrinal history analysis: who developed honji suijaku, under what institutional conditions, and who benefited from its adoption. Comparison with alternative syncretistic models (shinbutsu shugo, ryobu shinto). Analysis of power dynamics between Buddhist temples and Shinto shrines during Heian-Kamakura periods.',
    'If genuine synthesis: honji suijaku is coordination (Rope). If institutional accommodation: honji suijaku is extraction (Tangled Rope or Snare) — Buddhist institutions naturalized their cosmological priority to extract resources and authority from indigenous kami worship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honji_suijaku_naturalization, conceptual, 'Whether honji suijaku doctrine is metaphysical synthesis or institutional extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology_flat_control, 0, 1280).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kb_onto_theater_nara, kami_buddha_ontology_flat_control, theater_ratio, 0, 0.1).
narrative_ontology:measurement(kb_onto_theater_heian, kami_buddha_ontology_flat_control, theater_ratio, 200, 0.12).
narrative_ontology:measurement(kb_onto_theater_kamakura, kami_buddha_ontology_flat_control, theater_ratio, 600, 0.15).
narrative_ontology:measurement(kb_onto_theater_edo, kami_buddha_ontology_flat_control, theater_ratio, 1100, 0.18).
narrative_ontology:measurement(kb_onto_theater_meiji, kami_buddha_ontology_flat_control, theater_ratio, 1200, 0.65).
narrative_ontology:measurement(kb_onto_theater_postwar, kami_buddha_ontology_flat_control, theater_ratio, 1280, 0.2).

% Extraction over time
narrative_ontology:measurement(kb_onto_extract_nara, kami_buddha_ontology_flat_control, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(kb_onto_extract_heian, kami_buddha_ontology_flat_control, base_extractiveness, 200, 0.12).
narrative_ontology:measurement(kb_onto_extract_kamakura, kami_buddha_ontology_flat_control, base_extractiveness, 600, 0.15).
narrative_ontology:measurement(kb_onto_extract_edo, kami_buddha_ontology_flat_control, base_extractiveness, 1100, 0.18).
narrative_ontology:measurement(kb_onto_extract_meiji, kami_buddha_ontology_flat_control, base_extractiveness, 1200, 0.45).
narrative_ontology:measurement(kb_onto_extract_postwar, kami_buddha_ontology_flat_control, base_extractiveness, 1280, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(kb_onto_suppress_nara, kami_buddha_ontology_flat_control, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(kb_onto_suppress_edo, kami_buddha_ontology_flat_control, suppression_requirement, 1100, 0.2).
narrative_ontology:measurement(kb_onto_suppress_meiji, kami_buddha_ontology_flat_control, suppression_requirement, 1200, 0.75).
narrative_ontology:measurement(kb_onto_suppress_postwar, kami_buddha_ontology_flat_control, suppression_requirement, 1280, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology_flat_control, identity_coordination).

% DUAL FORMULATION NOTE:
% This is the flat construction of the kami-buddha ontology substrate. Alternative framings (honji suijaku as unified cosmology vs domain partition as separate spheres) are captured in omega variables rather than separate constraint stories, per the flat construction protocol.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
