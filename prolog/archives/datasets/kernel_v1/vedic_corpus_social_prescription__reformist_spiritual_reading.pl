% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__reformist_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__reformist_spiritual_reading, []).

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
 *   constraint_id: vedic_corpus_social_prescription__reformist_spiritual_reading
 *   human_readable: Vedic Corpus as Spiritual Unity and Metaphorical Cosmology (Reformist Reading)
 *   domain: religious_studies/hermeneutics/social_philosophy
 *
 * SUMMARY:
 *   The reformist spiritual reading of the Vedic corpus reframes ancient
 *   Sanskrit texts as primarily philosophical and cosmological rather than as
 *   prescriptive social codes. This reading emphasizes the UpaniȘadic layers
 *   of Vedic philosophy, interprets varna (traditionally the hereditary caste
 *   system) as metaphorical or as describing natural aptitudes rather than
 *   birth-based hierarchy, and treats ritual prescriptions and
 *   gender-specific passages as historical accommodations rather than eternal
 *   law. The reading emerged prominently during the 19th-20th century Hindu
 *   reform movements (Brahmo Samaj, Arya Samaj, Ramakrishna Mission) as both
 *   a religious reclamation and a response to colonial critique of
 *   caste-based Hinduism. The constraint is ONE READING of the contested
 *   kernel: vedic_corpus_social_prescription. Other readings (the orthodox
 *   varna reading, the colonial orientalist reading) interpret the same texts
 *   differently. This story instantiates the reformist reading as a clean
 *   ε-invariant constraint: a low-extraction pure-coordination mechanism that
 *   enables cross-caste spiritual community without prescriptive social
 *   hierarchy.
 *
 * KEY AGENTS:
 *   - Spiritual Practitioners across Castes: Primary beneficiaries (moderate/mobile) — gain access to Vedic philosophy and spiritual community without requiring birth-based hierarchy or ritual exclusion
 *   - Reform Movement Coalition: Secondary beneficiary (organized/mobile) — Hindu reformers coordinate around reinterpreted Vedic authority to modernize religious practice and challenge caste orthodoxy
 *   - Institutional Religious Authorities: Mixed (powerful/arbitrage) — state-sponsored Sanskrit academies, Vedanta centers, modern temples benefit from presenting Vedas as modernizable and universalizable; also extract institutional gatekeeping authority from exclusive interpretation control
 *   - Orthodox Varna Authorities: Affected but not primary victims (institutional/constrained) — their institutional function degrades as reformist reading gains legitimacy; forced to defend varna prescriptions through new hermeneutic work rather than enforcing them through ritual gatekeeping
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing the reformist reading as discovery rather than interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.12).
domain_priors:suppression_score(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.08).
domain_priors:theater_ratio(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__reformist_spiritual_reading, rope).
narrative_ontology:human_readable(vedic_corpus_social_prescription__reformist_spiritual_reading, "Vedic Corpus as Spiritual Unity and Metaphorical Cosmology (Reformist Reading)").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__reformist_spiritual_reading, "religious_studies/hermeneutics/social_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__reformist_spiritual_reading, '3e7d2f34-64eb-413d-9736-b913aae371cb').
narrative_ontology:cs_kernel_codification('3e7d2f34-64eb-413d-9736-b913aae371cb', fixed_text).
narrative_ontology:cs_authority_grounding('3e7d2f34-64eb-413d-9736-b913aae371cb', lineage).
narrative_ontology:cs_interpretation_layer_present('3e7d2f34-64eb-413d-9736-b913aae371cb').
narrative_ontology:cs_reading_relation('3e7d2f34-64eb-413d-9736-b913aae371cb', vedic_corpus_social_prescription__orthodox_varna_reading, coexists_with).
narrative_ontology:cs_reading_relation('3e7d2f34-64eb-413d-9736-b913aae371cb', vedic_corpus_social_prescription__colonial_orientalist_reading, influences).
narrative_ontology:cs_axiom('3e7d2f34-64eb-413d-9736-b913aae371cb', foundational, vedic_essence_is_spiritual_not_social).
narrative_ontology:cs_axiom_status(vedic_essence_is_spiritual_not_social, holdable).
narrative_ontology:cs_axiom_grounding('3e7d2f34-64eb-413d-9736-b913aae371cb', vedic_essence_is_spiritual_not_social, deontological).
narrative_ontology:cs_axiom('3e7d2f34-64eb-413d-9736-b913aae371cb', foundational, interpretive_access_transcends_birth_status).
narrative_ontology:cs_axiom_status(interpretive_access_transcends_birth_status, holdable).
narrative_ontology:cs_axiom_grounding('3e7d2f34-64eb-413d-9736-b913aae371cb', interpretive_access_transcends_birth_status, deontological).
narrative_ontology:cs_reference_frame('3e7d2f34-64eb-413d-9736-b913aae371cb', vedic_essence_as_spiritual_universalism).
narrative_ontology:cs_drift_state('3e7d2f34-64eb-413d-9736-b913aae371cb', contemporary_institutional_embedding, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3e7d2f34-64eb-413d-9736-b913aae371cb', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, spiritual_practitioners).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, cross_caste_religious_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SPIRITUAL PRACTITIONER (ROPE) — The reformist reading enables coordination of diverse practitioners (across caste lines, gender, vocation) into shared spiritual community without prescriptive hierarchy. The constraint functions as pure coordination — establishing common textual reference points and interpretive frameworks for meditation, philosophy, and ethical conduct. No victim set. Extractiveness derived from legitimate coordination cost (maintaining textual transmission, teaching lineages) not asymmetric extraction.
constraint_indexing:constraint_classification(vedic_corpus_social_prescription__reformist_spiritual_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 2: REFORM MOVEMENT COALITION (ROPE) — Organized religious actors (19th–20th century Hindu reformers: Brahmo Samaj, Arya Samaj, Ramakrishna Vedanta centers) coordinate around reinterpreting the Vedas as spiritual-philosophical rather than as prescriptive social codes. The constraint is pure coordination: establishing alternative interpretive authority, building communities of practice, transmitting reframed readings. Low extraction, high coordination function, mobile exit (coalition members can adopt, modify, or abandon the reading without material penalty).
constraint_indexing:constraint_classification(vedic_corpus_social_prescription__reformist_spiritual_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL RELIGIOUS AUTHORITY — TANGLED ROPE (NOTE: This perspective shows mixed outcomes.) Some institutional actors (Ramakrishna Order, All-India Radio Vedanta broadcasts, state-sponsored Sanskrit academies) benefit from presenting the Vedas as modernizable spiritual wisdom compatible with secular nationalism. The constraint coordinates a nation-state's religious legitimacy; simultaneously, it enables these institutions to extract authority from control over authoritative interpretation and transmission lineages. Extraction is moderate and active (maintaining institutional gatekeeping while appearing universalizing). High effective power, arbitrage exit (institutional actors can shift back to orthodox readings if political winds change). This perspective instantiates mandatrophy: the same text and reading enable both coordination (building inclusive spiritual community) and extraction (institutional control over interpretation).
constraint_indexing:constraint_classification(vedic_corpus_social_prescription__reformist_spiritual_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER — MOUNTAIN (FALSE SUMMIT CANDIDATE) — From a civilizational perspective, the reformist reading risks naturalizing its interpretive choices as inherent to the text itself. The premise: 'The Vedas describe spiritual unity with no prescriptive social content' can appear as a discovered fact about the texts rather than as an active reframing. This naturalizes the reformist reading's hermeneutic work — the choice to emphasize UpaniȘadic philosophy over Vedic ritual prescription, to read varna as metaphorical rather than descriptive, to treat gender-exclusive passages as historical accommodation. The engine's false summit detector will identify this as a naturalized reading, revealing that the constraint's binding force is interpretive (human agents chose this frame and maintained it) not ontological (the text contains no other valid reading).
constraint_indexing:constraint_classification(vedic_corpus_social_prescription__reformist_spiritual_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: ORTHODOX VARNA AUTHORITY — PITON — From the perspective of traditional varna-based ritual authority, the reformist reading has degraded institutional function while remaining formally in place. Brahminical gatekeeping of Vedic interpretation persists but has lost legitimacy authority: Sanskrit scholars must now defend varna prescriptions as metaphorical (where once they were taught as natural law), teaching communities face cross-caste participation they cannot prevent through ritual rules alone, and institutional claims to exclusive Vedic authority are theatricalized through academic credentials rather than enforced through social structure. The orthodox institutional position continues to exist (with theater_ratio rising as enforcement capacity decays) but its primary function has atrophied. This is institutional inertia: the orthodox reading persists in formal transmission lineages even as the reformist reading has captured institutional and intellectual legitimacy.
constraint_indexing:constraint_classification(vedic_corpus_social_prescription__reformist_spiritual_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__reformist_spiritual_reading_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(vedic_corpus_social_prescription__reformist_spiritual_reading, TR),
    TR >= 0.70.

:- end_tests(vedic_corpus_social_prescription__reformist_spiritual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The reformist reading functions as pure coordination: establishing shared interpretive frameworks and spiritual practices accessible across caste and gender boundaries. The low extractiveness reflects that the reading's coordination function is genuine — it solves the collective action problem of building diverse spiritual communities without the transaction costs and coercive mechanisms of caste-based hierarchy. The modest rise from 0.08 to 0.12 over the interval reflects minor increases in institutional gatekeeping (Vedanta centers developing formal training lineages, state Sanskrit academies establishing credentialing systems) but the core extraction remains minimal. Suppression (0.08): Very low. The reformist reading suppresses orthodox counter-claims through intellectual persuasion and institutional legitimacy, not through coercive or material barriers. Practitioners can shift between reformist and orthodox readings; communities are not legally bound; alternative interpretations are not criminalized (unlike in some historical contexts). The low suppression reflects that exit from the constraint is materially and legally open — agents participate because the reading's coordination benefits are attractive, not because they are trapped. Theater ratio (0.25): Low. The reformist reading minimizes performative elements: it emphasizes textual exegesis, philosophical consistency, and experiential practice over ritual purification, access restriction, or institutional status signaling. Where theater might rise (institutional credentialing, formal Vedanta teaching orders), it remains modest because the reading's legitimacy rests on intellectual coherence, not on performative gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the constraint's dual nature as both genuine coordination and potential institutional extraction. From the spiritual practitioner's view (Rope), the reading enables community participation. From the reform coalition's view (Rope), it coordinates modernization and inclusivity. From the institutional religious authority's view (Tangled Rope), the same reading provides both coordination (modernizing Hinduism for nation-state compatibility) and extraction (capturing interpretive authority). From the orthodox varna authority's view (Piton), the reformist reading has degraded their institutional function while persisting through inertia. The analytical observer risks missing this distinction by naturalizing the reformist reading as the texts' true content rather than recognizing it as an interpretive choice maintained through active hermeneutic and institutional work.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries of the reformist reading derive low directionality values: spiritual practitioners and reform coalitions benefit from access to Vedic authority and community coordination without bearing extraction costs. Their d values (derived from beneficiary status + mobile/arbitrage exit) are low, producing low or negative f(d) values that amplify their experience of coordination benefit. The institutional religious authority, though powerful and extracting gatekeeping benefit, experiences moderate directionality because their arbitrage exit (ability to shift back to orthodox readings) limits their effective entrenchment in the constraint. The orthodox varna authority experiences higher directionality because their constrained exit (institutional commitment to varna doctrine) and degraded function create friction. The constraint's structure does not concentrate extraction; directionality remains distributed because the reading itself has no inherent victim set — unlike orthodox social prescription, the reformist reading does not designate categories for systematic disadvantage.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hermetic_intentionality_ambiguity,
    'Are the Vedas'' non-prescriptive cosmological passages evidence that the texts have no social prescription as their intent, or are they evidence only that the reformist reading has successfully reframed prescriptive passages as corruptions or metaphors?',
    'Comparative analysis of Vedic passages treated as social prescription (varna system, gender roles, ritual hierarchy) across orthodox and reformist commentaries; historical reconstruction of the hermeneutic choices that enabled the reformist frame; examination of which textual features are reinterpreted as metaphorical and which are treated as literal within each reading.',
    'If the passages are intrinsically non-prescriptive (evidence of intentional authorial spirituality): the reformist reading discovers a true property of the texts. Rope classification holds. If the passages are made non-prescriptive through interpretive work (reframing, selective emphasis, philosophical alignment with modernism): the constraint is an imposed reading with active enforcement. Classification revises toward Tangled Rope or Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hermetic_intentionality_ambiguity, conceptual, 'Whether the texts'' non-prescriptive cosmology is discovered or constructed by the reformist reading').

omega_variable(
    varna_reinterpretation_scope,
    'Can varna (caste categories) be coherently reread as metaphorical (spiritual qualities, natural aptitudes) without logically erasing their prescriptive social content within the same textual framework?',
    'Close hermeneutical analysis of passages explicitly linking varna to birth, occupation, ritual access, and social role; assessment of whether these links can be reinterpreted as purely symbolic without dissolving the logical coherence of the varna system itself; examination of whether the reformist reading requires treating some Vedic passages as inauthentic, corrupted, or historically contingent.',
    'If varna can be reread as purely spiritual without logical inconsistency: the reformist reading is a legitimate alternative interpretation. If the reinterpretation requires dismissing or demoting textual passages as non-Vedic: the reformist reading actively rewrites the text rather than discovering its true content. Extractiveness would rise (active enforcement of interpretation) and classification would shift toward Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(varna_reinterpretation_scope, conceptual, 'Logical coherence of varna reinterpretation as purely metaphorical').

omega_variable(
    cross_caste_community_sustainability,
    'Do cross-caste spiritual communities organized around the reformist reading sustain themselves through voluntary coordination, or do they require active suppression of orthodox counter-claims and institutional reinforcement of the reformist frame?',
    'Ethnographic or historical study of cross-caste Hindu communities (Brahmo Samaj, contemporary Vedanta centers, reform temples): measurement of coordination sustainability, frequency and severity of orthodox objections and counter-teaching, institutional effort to maintain reformist boundaries against orthodox encroachment, cost-benefit analysis of membership (are members primarily attracted by coordination benefits or by institutional protection from orthodox social pressure?).',
    'If sustainable through pure coordination (low institutional cost, high voluntary participation): Rope classification holds. If sustainability requires active enforcement (suppressing orthodox claims, institutional gatekeeping, political pressure): classification revises to Tangled Rope; extractiveness rises. If cross-caste communities face sustained orthodox hostility and collapse without institutional support: suppression rises and classification shifts toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_caste_community_sustainability, empirical, 'Whether cross-caste spiritual communities sustain themselves through voluntary coordination or require institutional enforcement').

omega_variable(
    reformist_reading_naturalness,
    'Is the reformist reading a natural or obvious interpretation of the Vedic texts, or is it a 19th-20th century innovation driven by political and social contingency (colonial encounter, nation-state construction, modernization)?',
    'Intellectual history: examination of pre-colonial Vedic interpretation traditions (medieval Vedanta schools, Puranic hermeneutics, regional traditions) to assess whether the reformist frame appears in earlier interpretive lineages; analysis of 19th century social and political contexts (colonial domination, Western philosophy influence, caste reform movements) to trace the innovation''s historical contingency.',
    'If the reformist reading appears in pre-colonial traditions: it is a recovered or renewed interpretation with deep roots. The reading''s authority is enhanced; the constraint is more plausibly Rope. If the reading is a modern innovation: its authority depends on the convincingness of the reformist hermeneutic work and the institutional power to transmit it (not on textual inherence). The constraint''s binding force becomes explicitly interpretive and institutional, not textual. Classification may shift toward Tangled Rope if institutional enforcement is required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformist_reading_naturalness, empirical, 'Historical novelty vs. recovery of pre-colonial interpretation traditions in the reformist reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__reformist_spiritual_reading, 1800, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ved_reform_theater_1800s, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(ved_reform_theater_1900s, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement(ved_reform_theater_2000s, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 100, 0.25).

% Extraction over time
narrative_ontology:measurement(ved_reform_extract_1800s, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(ved_reform_extract_1900s, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 50, 0.1).
narrative_ontology:measurement(ved_reform_extract_2000s, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__reformist_spiritual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.06).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription__orthodox_varna_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription__colonial_orientalist_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, caste_reform_institutional_legitimacy).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, hindu_nationalism_vedic_authority).

% DUAL FORMULATION NOTE:
% The vedic_corpus_social_prescription is a contested kernel with three structurally distinct readings: reformist_spiritual_reading (this story, ε≈0.12, Rope), orthodox_varna_reading (ε≈0.45, Tangled Rope or Snare), and colonial_orientalist_reading (ε≈0.35, Piton or degraded Rope). Each reading has different ε values because they interpret the same texts as coordinating different functions with different extraction profiles. The readings are not observable variants of one constraint — they are distinct constraints instantiated by different interpretive communities. All three are linked via network.affects_constraints to reflect their mutual structural dependence: the reformist reading's legitimacy partly derives from refuting the orthodox varna reading; the orientalist reading's authority partly rests on dismissing both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vedic_corpus_social_prescription__reformist_spiritual_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
