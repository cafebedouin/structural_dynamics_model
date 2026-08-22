% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__universalist_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__universalist_devotional_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gita_kurukshetra_discourse__universalist_devotional_reading
 *   human_readable: Bhakti-Universalist Reading of the Bhagavad Gita: Devotion Beyond Caste
 *   domain: religious_studies/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Bhagavad Gita's kernel
 *   discourse: the universalist-devotional (bhakti) reading, which holds that
 *   the text's central teaching is path-independent devotional surrender to
 *   the divine, accessible regardless of caste or social role, and that
 *   dharma properly understood is surrender to divine will rather than fixed
 *   social duty. This reading draws primarily on chapters 9, 12, and 18's
 *   closing verses, and is the textual backbone of centuries of bhakti
 *   movements (Chaitanya, Kabir, and later reform Hinduism) that used the
 *   Gita to extend religious legitimacy to populations excluded by orthodox
 *   ritual gatekeeping. This is a genuinely coordinating reading — it lowers
 *   the cost of religious participation for the historically excluded — but
 *   it also has an identifiable beneficiary class (devotional teachers,
 *   reform institutions) whose authority grows as the orthodox gatekeeping
 *   function shrinks, and an identifiable payer class (hereditary ritual
 *   specialists) whose distinctive institutional role is eroded by it. ε is
 *   authored for the standing bhakti-universalist arrangement itself, not for
 *   the fully egalitarian religious order it envisions.
 *
 * KEY AGENTS:
 *   - low_caste_devotees: primary beneficiary (powerless/constrained) — gains scriptural warrant for direct religious access
 *   - women_devotees: primary beneficiary (powerless/constrained) — gains scriptural warrant for direct religious access
 *   - bhakti_movement_teachers: agenda-setting interpretive authority (organized/mobile) — administers and propagates the reading
 *   - reformist_hindu_institutions: institutional beneficiary (institutional/mobile) — uses the reading for modernizing legitimacy
 *   - hereditary_brahmin_ritual_specialists: primary payer (moderate/identity_locked) — loses interpretive and ritual exclusivity
 *   - orthodox_commentarial_tradition: excluded rival interpretive authority (institutional/identity_locked) — sidelined rather than engaged
 *   - comparative_religion_scholars: analytical observer (analytical/analytical) — assesses textual warrant across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__universalist_devotional_reading, 0.28).
domain_priors:suppression_score(gita_kurukshetra_discourse__universalist_devotional_reading, 0.32).
domain_priors:theater_ratio(gita_kurukshetra_discourse__universalist_devotional_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__universalist_devotional_reading, rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__universalist_devotional_reading, "Bhakti-Universalist Reading of the Bhagavad Gita: Devotion Beyond Caste").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__universalist_devotional_reading, "religious_studies/textual_hermeneutics/ethical_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__universalist_devotional_reading, 'dc56493c-55ac-4d68-a27c-b82415c1f0fa').
narrative_ontology:cs_kernel_codification('dc56493c-55ac-4d68-a27c-b82415c1f0fa', fixed_text).
narrative_ontology:cs_authority_grounding('dc56493c-55ac-4d68-a27c-b82415c1f0fa', practice).
narrative_ontology:cs_interpretation_layer_present('dc56493c-55ac-4d68-a27c-b82415c1f0fa').
narrative_ontology:cs_reading_relation('dc56493c-55ac-4d68-a27c-b82415c1f0fa', gita_kurukshetra_discourse__orthodox_literal_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc56493c-55ac-4d68-a27c-b82415c1f0fa', gita_kurukshetra_discourse__gandhian_allegorical_reading, influences).
narrative_ontology:cs_axiom('dc56493c-55ac-4d68-a27c-b82415c1f0fa', foundational, devotional_access_is_caste_independent).
narrative_ontology:cs_axiom_status(devotional_access_is_caste_independent, holdable).
narrative_ontology:cs_axiom_grounding('dc56493c-55ac-4d68-a27c-b82415c1f0fa', devotional_access_is_caste_independent, deontological).
narrative_ontology:cs_axiom('dc56493c-55ac-4d68-a27c-b82415c1f0fa', foundational, dharma_is_surrender_not_social_role).
narrative_ontology:cs_axiom_status(dharma_is_surrender_not_social_role, holdable).
narrative_ontology:cs_axiom_grounding('dc56493c-55ac-4d68-a27c-b82415c1f0fa', dharma_is_surrender_not_social_role, conventional).
narrative_ontology:cs_reference_frame('dc56493c-55ac-4d68-a27c-b82415c1f0fa', vedic_ritual_caste_qualification).
narrative_ontology:cs_drift_state('dc56493c-55ac-4d68-a27c-b82415c1f0fa', post_bhakti_movement_consolidation, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('dc56493c-55ac-4d68-a27c-b82415c1f0fa', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, low_caste_devotees).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, women_devotees).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, bhakti_movement_teachers).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, reformist_hindu_institutions).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, hereditary_brahmin_ritual_specialists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically barred from Vedic study, temple access, and ritual officiancy under orthodox readings. Under this reading, the text's own words (chapter 9's promise that even those of 'sinful birth' who take refuge in devotion attain the supreme goal) grant direct access to liberation without priestly mediation or caste qualification. Exit from the orthodox gatekeeping structure is difficult but the reading itself supplies the argument for it.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, low_caste_devotees, beneficiary,
    powerless, generational, constrained, regional).

% Excluded from Vedic recitation and formal ritual roles under orthodox interpretation. This reading extends the same devotional-access argument to them, since the criterion for salvation becomes surrender and devotion rather than ritual or caste qualification.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, women_devotees, beneficiary,
    powerless, generational, constrained, regional).

% Historical and contemporary devotional lineages (from medieval bhakti saints to modern gurus and movements) that construct and propagate this reading. They administer the interpretive tradition, teach it to lay audiences, and derive institutional legitimacy and following from making liberation appear universally accessible. They have genuine textual grounding but also gain authority, adherents, and resources by advancing this reading over the orthodox one.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, bhakti_movement_teachers, agenda_setter,
    organized, generational, mobile, continental).

% Nineteenth- and twentieth-century reform movements and their institutional descendants use this reading to present Hinduism as compatible with egalitarian modernity, both to internal reformist ends and to external audiences skeptical of caste. They benefit from a reading that recasts the tradition's central text as anti-hierarchical.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, reformist_hindu_institutions, beneficiary,
    institutional, generational, mobile, global).

% Their social and economic position has historically rested partly on being the sanctioned interpreters and ritual intermediaries required for orthodox religious practice. A reading that makes devotion path-independent and caste-irrelevant erodes the exclusivity of their interpretive and ritual authority; their professional identity is substantially constituted by the gatekeeping role this reading dissolves. Exit is difficult because their status is inherited and occupationally specific, not freely chosen.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, hereditary_brahmin_ritual_specialists, payer,
    moderate, generational, identity_locked, regional).

% Commentators in the Shankara/Madhva/orthodox Vedantic line who read the same verses as compatible with caste-based duty would object that this reading selectively foregrounds the devotional chapters while minimizing the text's explicit endorsement of svadharma tied to birth-caste (chapter 18's caste-duty taxonomy). They are not erased from the tradition but are structurally absent from THIS reading's own self-presentation, which treats their concerns as superseded rather than engaging them directly.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_commentarial_tradition, excluded,
    institutional, civilizational, identity_locked, continental).

% Philologists and historians of religion who examine how the bhakti-universalist reading emerged historically (via bhakti movements, colonial-era apologetics, and reform Hinduism) and assess its textual warrant against rival readings, without a stake in which reading prevails devotionally.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared soteriological framework that lets devotees across caste, gender, and social position coordinate around a single accessible path to liberation (bhakti/surrender) rather than requiring costly, exclusionary ritual and scriptural qualification — solving the real problem of religious participation for populations excluded by orthodox gatekeeping.
% TRANSFER_FUNCTION: Moves religious authority and legitimacy away from hereditary ritual specialists (who administer caste-gated qualification) toward devotional teachers and lay practitioners; moves social prestige and institutional following toward reform movements and bhakti lineages that champion this reading.
% ABSENT_VOICES: Orthodox commentators who read chapter 18's caste-duty taxonomy as binding and central are not directly engaged within this reading's own frame — their textual claims are treated as superseded rather than rebutted verse-by-verse. They would object that this reading resolves genuine tension in the text by suppression rather than synthesis.
% DISAPPEARANCE_RATIONALE: If this reading vanished as a live interpretive option, bhakti-movement institutions, reform Hindu organizations, and lay devotional practice built on universalist access to liberation would lose their primary textual warrant; religious authority would concentrate more heavily back toward hereditary ritual qualification, and the historical gains in devotional access for excluded castes and women would lose scriptural grounding (though not necessarily disappear as social practice).
% FOUNDING_PROBLEM: Orthodox Vedic religion restricted ritual officiancy, scriptural study, and by extension guaranteed paths to liberation to a narrow hereditary and gendered elite, leaving the vast majority of the population without a sanctioned route to religious fulfillment within the tradition's own terms.
% FOUNDING_PROBLEM_CORROBORATION: Bhakti movement historians and scholars of medieval devotional literature (outside the modern institutions that benefit from this reading) corroborate that access exclusion was a real and long-standing problem the bhakti movements organized against, citing the historical record of caste and gender exclusion from Vedic study. Orthodox commentators and some philologists dispute that the Gita itself was composed to solve this problem, arguing the text's own caste-duty passages were original and unambiguous, and that the universalist reading is a later devotional overlay responding to a problem the text did not originally address.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__universalist_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__universalist_devotional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__universalist_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).
:- end_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.28) because this reading's dominant function is genuinely coordinative — it solves the real access problem for excluded populations rather than primarily extracting from them. The modest upward creep in the measurement series reflects that as bhakti-derived institutions have grown (medieval devotional orders through modern reform movements and globalized neo-Hindu organizations), some rent-collection by devotional teacher lineages and institutional gurus has accumulated on top of the coordination function, without approaching the extraction profile of a tangled rope. Suppression (0.32) is moderate: this reading does not require coercive enforcement against dissenters, but it does structurally sideline the orthodox commentarial tradition's textual claims rather than adjudicating them, which is a soft form of interpretive foreclosure. Accessibility collapse (0.35) is moderate-low because the orthodox reading and Gandhian allegorical reading both remain fully live and practiced alternatives — this reading has not collapsed the interpretive field. Resistance (0.55) is comparatively high because orthodox commentators and hereditary ritual authorities have historically and continue to actively contest this reading's textual warrant.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-caste devotees and women sit near the beneficiary end: the reading directly removes a qualification barrier that previously excluded them, at negligible cost to them. Bhakti-movement teachers and reformist institutions are also beneficiaries, but structurally different from the excluded-population beneficiaries — they gain institutional authority and following, which is a secondary rent riding on top of the genuine coordination function. Hereditary ritual specialists sit toward the target end: their institutional position depends partly on caste-based interpretive exclusivity, which this reading structurally erodes; their exit is constrained by inherited occupational identity, which is why exit_options is identity_locked rather than mobile or constrained.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — exclusion of the majority of the population from sanctioned religious participation — was genuinely live historically and remains at least partly live today in some traditional contexts, which is why founding_problem_status is authored as contested rather than dead. This prevents mislabeling the reading as pure extraction: the coordination function (universal devotional access) is not a decayed cover story for rent-seeking, because excluded populations continue to derive real access benefits from it. At the same time, the modest accumulation of institutional rent-collection by devotional lineages over centuries (reflected in the rising extractiveness/theater series) shows the reading has not remained purely coordinative either — some drift toward institutional self-interest has occurred without converting the whole structure into extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_selectivity_of_universalist_reading,
    'Does the universalist-devotional reading represent the Gita''s own internal resolution of caste and duty, or does it foreground chapters 9/12 while minimizing chapter 18''s explicit caste-duty taxonomy (svadharma tied to birth) to produce an egalitarian reading the text does not fully support on its own terms?',
    'Close philological comparison of the frequency, placement, and rhetorical weight of devotional-access passages versus caste-duty passages across the full eighteen chapters, cross-referenced against the text''s own internal logic (e.g., whether chapter 18 is presented as qualification or supersession of earlier teaching) and independent Sanskritist consensus outside any single devotional or orthodox institution.',
    'If the universalist reading is textually well-warranted as the culminating teaching, its coordination function is more clearly primary and extraction lower; if it is a selective devotional overlay, more of its apparent coordination function is retrospective legitimation for bhakti-movement institutional interests, raising the effective extraction and moving the classification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_selectivity_of_universalist_reading, conceptual, 'Whether the universalist reading resolves or selectively suppresses the text''s caste-duty material.').

omega_variable(
    kernel_reading_relation_to_siblings,
    'Where exactly is the disagreement between this reading and the orthodox_literal_reading located — is it a disagreement about what the text says (philological), about which passages are authoritative relative to others (canonical weighting), or about how devotional and duty-based soteriology should be synthesized (theological)?',
    'Comparative analysis of how each reading''s own tradition of commentary (bhakti Acharyas vs. orthodox Vedantins) explicitly addresses the disputed passages, to locate whether the disagreement is at the level of translation, canonical hierarchy, or theological synthesis.',
    'Locating the disagreement clarifies whether the two readings are genuinely incompatible (foreclosing) or represent different emphases within a shared textual field (coexisting) — this affects how the reading_relations edge to orthodox_literal_reading should be understood in future kernel analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_relation_to_siblings, conceptual, 'Locating precisely where the universalist and orthodox readings diverge structurally.').

omega_variable(
    institutional_rent_vs_genuine_access,
    'As bhakti-derived institutions and modern devotional organizations have grown, how much of their authority and resource base still tracks the genuine access function for excluded populations, versus having become self-sustaining institutional interests independent of that original function?',
    'Historical and contemporary sociological study of bhakti-lineage institutions: membership composition by caste/gender over time, resource flows, and whether institutional growth continues to correlate with expanding access for historically excluded groups or has decoupled from it.',
    'If institutional growth has substantially decoupled from the access function, the beneficiary structure shifts from excluded devotees toward institutional elites, which would raise measured extractiveness and could push the classification toward tangled_rope over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_rent_vs_genuine_access, empirical, 'Whether devotional-institution growth still tracks the original access-extension function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__universalist_devotional_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(gita_tr_t0, observed).
narrative_ontology:measurement(gita_tr_t40, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 40, 0.13).
narrative_ontology:measurement_basis(gita_tr_t40, observed).
narrative_ontology:measurement(gita_tr_t80, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 80, 0.16).
narrative_ontology:measurement_basis(gita_tr_t80, observed).
narrative_ontology:measurement(gita_tr_t120, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 120, 0.18).
narrative_ontology:measurement_basis(gita_tr_t120, observed).
narrative_ontology:measurement(gita_tr_t160, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 160, 0.2).
narrative_ontology:measurement_basis(gita_tr_t160, observed).
narrative_ontology:measurement(gita_tr_t200, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 200, 0.22).
narrative_ontology:measurement_basis(gita_tr_t200, observed).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(gita_be_t0, observed).
narrative_ontology:measurement(gita_be_t40, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement_basis(gita_be_t40, observed).
narrative_ontology:measurement(gita_be_t80, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 80, 0.2).
narrative_ontology:measurement_basis(gita_be_t80, observed).
narrative_ontology:measurement(gita_be_t120, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 120, 0.23).
narrative_ontology:measurement_basis(gita_be_t120, observed).
narrative_ontology:measurement(gita_be_t160, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 160, 0.26).
narrative_ontology:measurement_basis(gita_be_t160, observed).
narrative_ontology:measurement(gita_be_t200, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 200, 0.28).
narrative_ontology:measurement_basis(gita_be_t200, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(gita_kurukshetra_discourse__universalist_devotional_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__universalist_devotional_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gita_kurukshetra_discourse__universalist_devotional_reading, 0.1).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gandhian_allegorical_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the shared gita_kurukshetra_discourse kernel. orthodox_literal_reading holds the text mandates caste-based duty and legitimates righteous violence (high ε, concentrated Brahminical beneficiary). gandhian_allegorical_reading holds the battlefield is metaphor for internal struggle (low ε, ethical-pacifist beneficiary class). This universalist_devotional_reading holds bhakti is caste-independent and dharma is surrender to divine will (moderate-low ε, universal devotee beneficiary, hereditary ritual specialists as payer class). Each reading carries its own stable ε and stakeholder structure per the ε-invariance principle; the readings are linked via affects_constraints because gains in legitimacy or institutional resourcing for one reading structurally pressure the interpretive authority claimed by the others (e.g., growth of bhakti-institutional legitimacy erodes orthodox ritual-gatekeeping authority, and vice versa).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
