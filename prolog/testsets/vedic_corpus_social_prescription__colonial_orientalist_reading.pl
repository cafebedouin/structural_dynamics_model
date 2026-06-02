% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__colonial_orientalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__colonial_orientalist_reading, []).

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
 *   constraint_id: vedic_corpus_social_prescription__colonial_orientalist_reading
 *   human_readable: Vedic Corpus as Unified Hindu Law (Colonial Orientalist Reading)
 *   domain: religious_studies/hermeneutics/colonial_administration
 *
 * SUMMARY:
 *   The colonial orientalist reading of the Vedic corpus crystallizes fluid,
 *   historically contingent social practices into a unified, timeless legal
 *   system to serve administrative governance. This reading instantiates a
 *   specific hermeneutical project: selecting from Sanskrit textual
 *   traditions those passages that support a coherent legal ordering,
 *   presenting this ordering as the authentic, natural Hindu system, and
 *   enforcing it through colonial law codes and census categories. The
 *   constraint exhibits tangled-rope structure: genuine coordination function
 *   (the Vedic codification does reduce heterogeneous social practices to
 *   administrative categories, enabling efficient governance) combined with
 *   asymmetric extraction (the codification benefits colonial administration
 *   and upper-caste beneficiaries while suppressing alternative legal
 *   frameworks and transforming caste from a fluid identity into a fixed,
 *   heritable status). Theater increases over time as the constraint's
 *   functional necessity (colonial administrative efficiency) gradually
 *   declines, but the codification persists through institutional inertia,
 *   Hindu nationalist identity claims, and the difficulty of dismantling
 *   legal categories once embedded in statutes and bureaucratic practice. The
 *   analytical observer perspective risks naturalizing the constraint as a
 *   mountain — presenting Vedic law as a timeless universal system — when the
 *   naturalization is precisely what makes the constraint appear
 *   unchallengeable.
 *
 * KEY AGENTS:
 *   - Colonial Administration: Primary beneficiary (institutional/arbitrage) — codification creates legible subjects for taxation, census, adjudication, and indirect rule
 *   - Orientalist Scholars: Secondary beneficiary (powerful/mobile) — career and intellectual authority depend on discovering 'unified Hindu law' within Vedic texts
 *   - Upper-Caste Landed Elite: Mixed actor (moderate/constrained) — benefits from legal recognition of property and occupational privilege; bears cost of maintaining caste duty and ritual purity
 *   - Colonized Legal Subjects: Primary victim (powerless/trapped) — fluid social position crystallized into fixed, documented administrative category with no exit
 *   - Caste-Subordinated Groups: Primary victim (powerless/trapped) — codification transforms caste from negotiable identity to immutable legal status
 *   - Women: Victim set (powerless/trapped) — dharmaśāstra prescriptions codified as law enforce patriarchal restrictions more rigidly than pre-colonial practice
 *   - Indian Reform Movement: Organized agents (organized/constrained) — see colonial codification as temporary scaffold with sunset clause (decolonization)
 *   - Post-Colonial Hindu Nationalists: Institutional actors (institutional/arbitrage) — appropriate the codification to claim 'recovery of authentic tradition'; constraint degrades to piton (theater-maintained inertia)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.58).
domain_priors:suppression_score(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.62).
domain_priors:theater_ratio(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__colonial_orientalist_reading, tangled_rope).
narrative_ontology:human_readable(vedic_corpus_social_prescription__colonial_orientalist_reading, "Vedic Corpus as Unified Hindu Law (Colonial Orientalist Reading)").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__colonial_orientalist_reading, "religious_studies/hermeneutics/colonial_administration").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__colonial_orientalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__colonial_orientalist_reading, '7f55bc07-2377-4f7d-8686-066c1d8f07e7').
narrative_ontology:cs_kernel_codification('7f55bc07-2377-4f7d-8686-066c1d8f07e7', fixed_text).
narrative_ontology:cs_authority_grounding('7f55bc07-2377-4f7d-8686-066c1d8f07e7', extraction).
narrative_ontology:cs_interpretation_layer_present('7f55bc07-2377-4f7d-8686-066c1d8f07e7').
narrative_ontology:cs_reading_relation('7f55bc07-2377-4f7d-8686-066c1d8f07e7', orthodox_varna_reading, coexists_with).
narrative_ontology:cs_reading_relation('7f55bc07-2377-4f7d-8686-066c1d8f07e7', reformist_spiritual_reading, influences).
narrative_ontology:cs_axiom('7f55bc07-2377-4f7d-8686-066c1d8f07e7', foundational, vedic_texts_unified_legal_system).
narrative_ontology:cs_axiom_status(vedic_texts_unified_legal_system, holdable).
narrative_ontology:cs_axiom_grounding('7f55bc07-2377-4f7d-8686-066c1d8f07e7', vedic_texts_unified_legal_system, empirically_contingent).
narrative_ontology:cs_axiom('7f55bc07-2377-4f7d-8686-066c1d8f07e7', foundational, administrative_legibility_justifies_codification).
narrative_ontology:cs_axiom_status(administrative_legibility_justifies_codification, holdable).
narrative_ontology:cs_axiom_grounding('7f55bc07-2377-4f7d-8686-066c1d8f07e7', administrative_legibility_justifies_codification, instrumental).
narrative_ontology:cs_reference_frame('7f55bc07-2377-4f7d-8686-066c1d8f07e7', vedic_texts_as_discoverable_law).
narrative_ontology:cs_drift_state('7f55bc07-2377-4f7d-8686-066c1d8f07e7', contemporary_post_colonial, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7f55bc07-2377-4f7d-8686-066c1d8f07e7', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, orientalist_scholars).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, colonized_legal_subjects).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, caste_subordinated_groups).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, women_under_patriarchal_codification).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COLONIZED LEGAL SUBJECT (SNARE) — Trapped within codified caste law crystallized from Vedic texts. Previously fluid, negotiable social position becomes fixed, documented administrative category. Exit impossible: legal status, land rights, occupational restriction all bound to caste assignment. No alternative legal framework available — traditional customary law is superseded, modern rights discourse excluded. Suppression is total because the codification itself removes the possibility of reclassification or mobility.
constraint_indexing:constraint_classification(vedic_corpus_social_prescription__colonial_orientalist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: UPPER-CASTE LANDED ELITE (TANGLED ROPE) — Benefits from Vedic codification (legal recognition of property rights, occupational privilege, ritual status) while bearing enforcement costs (must perform customary duties, maintain ritual purity standards, manage subordinate castes). Significant coordination function exists: the codified Vedic system does organize agricultural relations and ritual hierarchy. But substantial extraction runs toward this agent through legal recognition of landholding and caste privilege. Constrained exit: could theoretically abandon caste identity, but social/economic costs are severe.
constraint_indexing:constraint_classification(vedic_corpus_social_prescription__colonial_orientalist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: COLONIAL ADMINISTRATION (ROPE) — Net beneficiary. The Vedic codification creates legible subjects for taxation, census, adjudication, and indirect rule. Reducing Indian law to unified written categories enables administrative efficiency and eliminates need for direct governance of social custom. The extraction mechanism is the entire point: codifying Vedic law generates extractive administrative capacity while appearing to preserve 'native tradition.' Exit available via arbitrage: colonial administration can switch to alternative governance models, or exit India entirely. Experiences the constraint as pure coordination benefit.
constraint_indexing:constraint_classification(vedic_corpus_social_prescription__colonial_orientalist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ORIENTALIST SCHOLAR-ADMINISTRATOR (TANGLED ROPE) — Secondary beneficiary. Career, status, and intellectual authority depend on discovering 'the' unified Hindu legal system within Vedic texts. Significant genuine scholarly coordination function: comparative law, systematic classification of principles, archive creation. But also extraction: the scholar's authority depends on Vedas being unified and timeless (not historically contingent), which requires suppressing evidence of textual contradiction, historical layering, and exegetical plurality. Mobile exit: scholar could abandon the unification project and return to philological study, but would lose administrative influence and intellectual authority. The constraint is real (career depends on codification project) but structurally exitable.
constraint_indexing:constraint_classification(vedic_corpus_social_prescription__colonial_orientalist_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INDIAN REFORM MOVEMENT (SCAFFOLD) — Organized agents (Brahmo Samaj, social reformers, independence movement intellectuals) see the colonial codification as temporary and reversible. They articulate a sunset: the Vedic law codification is scaffolding for colonial indirect rule that will be dismantled when Indians govern themselves. Constrained exit: reformers cannot immediately escape the codified framework (colonial law is enforced), but they see a structural exit path (decolonization). Theater is present but lower than from the administration perspective: reformers distinguish between the Vedic texts themselves and the colonial codification. Low effective extraction because the coalition has organized agency and sees an explicit sunset clause.
constraint_indexing:constraint_classification(vedic_corpus_social_prescription__colonial_orientalist_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: POST-COLONIAL HINDU NATIONALIST INVOCATION (PITON) — After decolonization, the codified Vedic law persists not through colonial administration but through institutional inertia and Hindu nationalist claims to 'recover authentic tradition.' The constraint's primary function (colonial administrative legibility) has atrophied, but the codification remains embedded in personal law statutes, caste census categories, and nationalist identity claims. Theater ratio extremely high: the codification is maintained through nationalist rhetoric ('Hindu dharma'), institutional precedent, and identity performance rather than functional necessity. Piton classification: degraded constraint maintained by theater and inertia, not by active beneficiary.
constraint_indexing:constraint_classification(vedic_corpus_social_prescription__colonial_orientalist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universalizing analytical perspective, Vedic law appears as an immutable natural ordering system: caste is presented as a permanent feature of cosmic/social order, varna is presented as a natural classification of human types, and the Vedic texts are presented as timeless pronouncements of universal dharma. This perspective risks naturalizing what is actually a colonial construct — the 'natural law' framing is precisely what makes the constraint appear unchallengeable. The engine will flag this as a false summit: the mountain classification depends on accepting the orientalist premise that Vedic texts constitute a unified, timeless legal system, which is itself the contested kernel reading.
constraint_indexing:constraint_classification(vedic_corpus_social_prescription__colonial_orientalist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__colonial_orientalist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vedic_corpus_social_prescription__colonial_orientalist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vedic_corpus_social_prescription__colonial_orientalist_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_corpus_social_prescription__colonial_orientalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(vedic_corpus_social_prescription__colonial_orientalist_reading, TR),
    TR >= 0.70.

:- end_tests(vedic_corpus_social_prescription__colonial_orientalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts administrative value for colonial governance and consolidates legal privilege for upper castes. The codification is not pure extraction (coordination function is genuine — it does organize heterogeneous practices into an administratively manageable system), but extraction dominates. Measurement trajectory (0.42 → 0.58 → 0.68) reflects increasing extraction as the codification deepens: early codification is presented as 'recovering' existing tradition; later phases enforce prescriptions as law, creating new rigidity. Suppression (0.62): High. Multiple suppression mechanisms: legal prohibition of caste mobility, removal of alternative legal frameworks, colonial criminal code enforcement, census categories preventing self-identification outside caste. Theater ratio (0.68): Moderate-high. The codification involves substantial performance: orientalist claims to have 'discovered' a unified system (when actually performing hermeneutical synthesis), formal legal ceremony and written codes performing 'ancient wisdom,' Hindu nationalist post-colonial rhetoric performing recovery of 'authentic tradition.' Theater increases (0.35 → 0.68) as the constraint's functional necessity declines after decolonization and persists through identity claims rather than administrative efficiency.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates profound perspectival divergence. The colonial administration and orientalist scholar see pure coordination (Rope) and career benefit. The upper-caste elite see mixed coordination and privilege (Tangled Rope). The colonized legal subject sees immobilization (Snare). The reform movement sees a temporary problem with a sunset (Scaffold). Post-colonial nationalism sees a degraded constraint maintained by theater (Piton). The analytical observer risks seeing a natural law (Mountain). Each perspective emerges from a different structural position: the beneficiary's experience of coordination, the trapped agent's experience of extraction, the organized agent's experience of sunset, the degraded institution's experience of inertia. The perspectival gap reveals what Mandatrophy resolution requires: the constraint is BOTH a genuine coordination mechanism (organizing heterogeneous practices) AND an extractive apparatus (consolidating power and suppressing alternatives). Neither reduction is false — both are structurally true from different positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is computed from beneficiary/victim status and exit options. Colonial administration (beneficiary + arbitrage) derives d ≈ 0.05 → f(d) ≈ -0.12 (low effective extraction, institutional subsidy). Orientalist scholar (secondary beneficiary + mobile) derives d ≈ 0.20 → f(d) ≈ 0.02 (near-zero effective extraction, intellectual benefit). Upper-caste elite (mixed + constrained) derives d ≈ 0.45 → f(d) ≈ 0.55 (moderate effective extraction via privilege). Colonized subject (victim + trapped) derives d ≈ 0.95 → f(d) ≈ 1.42 (maximum experienced extraction). Caste-subordinated groups (victim + trapped) derive same d (high extraction). Women (victim + trapped) derive d ≈ 0.92 → f(d) ≈ 1.35 (severe extraction via legal immobilization). Reform movement (organized + constrained) derives d ≈ 0.50 → f(d) ≈ 0.65 (moderate extraction with agency). Post-colonial nationalists (institutional + arbitrage, different institution than colonial admin) derive d ≈ 0.10 → f(d) ≈ -0.08 (low effective extraction for nationalist authority; constraint functions as theater of authenticity). The chi formula χ = ε × f(d) × σ(S) with regional scope σ(regional) = 0.9 produces experienced extractiveness that varies dramatically across perspectives while the base extractiveness (ε) remains constant.
 *
 * MANDATROPHY ANALYSIS:
 *   READING-SPECIFIC RESOLUTION: This constraint instantiates the colonial orientalist reading of a contested kernel. The mandatrophy is not 'is Vedic law unified and timeless?' but 'which reading of the kernel are we instantiating?' This reading answers: the constraint extracts by codifying texts that are heterogeneous and historically contingent as if they were unified and timeless. The mandatrophy resolves through the committer frame: acknowledge that the codification is ONE reading (the colonial orientalist reading) among multiple possible readings (orthodox brahminical reading, reformist spiritual reading, post-colonial nationalist reading, etc.). Each reading would produce different ε and different classification. This reading produces tangled_rope because it combines genuine coordination (organizing heterogeneous practice) with asymmetric extraction (suppressing alternatives and consolidating power). A reformist spiritual reading (focusing on philosophical unity of dharma concepts) would produce lower ε. An orthodox brahminical reading (treating Vedic hierarchy as metaphysically grounded) would produce different victim set and different omega variables. The analytical observer's mountain perspective does NOT resolve the mandatrophy — it naturalizes the constraint, which is itself what the constraint system does. The resolution requires acknowledging that the naturalization (the 'immutable law' framing) is the extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vedic_textual_unity_assumption,
    'Are Vedic texts a unified legal corpus, or a heterogeneous collection of ritual, philosophical, and social material composed over centuries with internal contradictions?',
    'Philological and historical-critical analysis: dating of textual layers, comparison of dharmaśāstra prescriptions, identification of exegetical contradictions, archaeological evidence of actual social practice vs. textual prescription',
    'If unified: orientalist reading is structurally justified; codification extracts from the texts a pre-existing system. If heterogeneous: unification is a colonial hermeneutical project; codification imposes synthetic order on diverse materials. This determines whether ε should be 0.35 (coordination of existing materials) or 0.60+ (creative extraction disguised as discovery).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vedic_textual_unity_assumption, empirical, 'Whether Vedic texts constitute a unified legal system or heterogeneous collection').

omega_variable(
    colonial_caste_cadastration_effect,
    'Did British colonial codification of caste law fundamentally transform caste from a fluid, context-dependent identity to a fixed, heritable administrative category?',
    'Historical analysis of caste flexibility in pre-colonial legal/administrative records; comparison of pre-colonial to colonial census data; ethnographic evidence of caste mobility before and after codification; analysis of colonial revenue records and district gazetteers',
    'If transformation occurred: codification is genuinely extractive (created new legal immobility). If caste was already rigid: codification merely formalized existing structure (lower ε, shifts classification toward rope). This determines suppression magnitude and victim identification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_caste_cadastration_effect, empirical, 'Whether colonial codification transformed caste from fluid to fixed category').

omega_variable(
    patriarchal_prescriptive_gap,
    'Do dharmaśāstra prescriptions about women''s legal status match the actual social practices documented in pre-colonial records, or do texts prescribe more extreme subordination than was practiced?',
    'Comparative analysis of text vs. practice: dharmaśāstra prescriptions of widow-immolation, property rights restriction, reproductive control vs. evidence from inscriptions, legal documents, narrative literature of women''s actual autonomy and property holdings',
    'If prescriptive gap exists: colonial codification enforced textual extremes as law, increasing actual oppression (raises ε and suppression). If practices matched texts: codification merely formalized existing conditions (lowers victim-impact claim). Critical for determining whether women constitute a distinct victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patriarchal_prescriptive_gap, empirical, 'Gap between dharmaśāstra prescriptions and pre-colonial women''s actual legal status').

omega_variable(
    orientalist_interpretation_circularity,
    'Did orientalist scholars select and emphasize Vedic passages that fit a pre-existing theory of Hindu social order, or did they derive the theory inductively from the texts?',
    'Historiography of orientalism: analysis of which texts were translated/emphasized (Manusmriti prominence vs. other dharmaśāstra texts), comparison to original Sanskrit manuscript traditions (what was actually copied and circulated vs. what was selected by orientalists), archival evidence of colonial scholars'' explicit methodological choices',
    'If circular (theory-driven selection): codification is a constructed reading, not a discovered system; oracle gap confirmed. If inductive: texts do support some unified ordering. This affects whether axiom_unification_discovery is holdable or overridden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(orientalist_interpretation_circularity, empirical, 'Whether orientalist interpretation was theory-driven or inductively derived').

omega_variable(
    dharma_plurality_suppression,
    'Did the codification process suppress the existence of multiple dharma frameworks (śruti vs. smṛti, brahminical vs. non-brahminical, regional/sectarian variations) by imposing a false unitary ''Hindu dharma''?',
    'Analysis of pre-colonial legal pluralism: evidence of coexisting legal traditions (brahminical, Islamic, regional customary law), texts explicitly acknowledging multiple valid dharma frameworks, post-codification loss of institutional recognition for non-brahminical legal authorities',
    'If suppression occurred: codification is extractive because it eliminates alternative legal legitimacy; victim set expands to include non-brahminical communities. If plural frameworks already marginalized: codification crystallizes existing hierarchy (lower ε, shifts classification toward piton of prior hierarchy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dharma_plurality_suppression, empirical, 'Whether codification suppressed recognition of multiple dharma frameworks').

omega_variable(
    post_colonial_nationalist_appropriation,
    'Is the post-colonial persistence of Vedic legal codification driven by genuine continuity with pre-colonial practice, or by Hindu nationalist ideology seeking to reclaim ''authentic Hindu tradition'' against Christian and Islamic law?',
    'Historical analysis of post-colonial Indian law: which aspects of colonial codification were retained, which reformed; examination of Hindu nationalist rhetoric in constitution-drafting and personal law debates; comparison of personal law application patterns to pre-colonial caste law documentation',
    'If nationalist appropriation: the constraint''s function shifted after decolonization from colonial extraction to nationalist identity theater (piton classification confirmed). If genuine continuity: the constraint retains some coordination function (tangled_rope persists). This determines whether the constraint is truly degraded or merely transformed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_colonial_nationalist_appropriation, conceptual, 'Whether post-colonial persistence is nationalist ideology or genuine continuity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__colonial_orientalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedic_col_tr_t0, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(vedic_col_tr_t25, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 25, 0.52).
narrative_ontology:measurement(vedic_col_tr_t50, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(vedic_col_be_t0, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(vedic_col_be_t25, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(vedic_col_be_t50, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vedic_col_su_t0, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(vedic_col_su_t25, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(vedic_col_su_t50, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__colonial_orientalist_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, brahminical_caste_metaphysics).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_census_subject_formation).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, post_colonial_personal_law_statutes).

% DUAL FORMULATION NOTE:
% The vedic_corpus_social_prescription kernel contains multiple structurally distinct constraints corresponding to different readings. This story models the colonial orientalist reading (ε ≈ 0.58, tangled_rope). The orthodox brahminical reading would have different ε (likely higher, ≈ 0.70–0.80) and different beneficiary/victim structure (brahminical authority vs. non-brahminical exclusion rather than colonized subjects vs. colonial administrators). The reformist reading would have lower ε (≈ 0.25–0.35) and classify as rope or scaffold. Each reading is a separate constraint story linked by network.affects_constraints to show how the kernel-level debate propagates to downstream institutional structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vedic_corpus_social_prescription__colonial_orientalist_reading, powerful, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
