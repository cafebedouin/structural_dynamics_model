% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__orthodox_varna_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__orthodox_varna_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: vedic_corpus_social_prescription__orthodox_varna_reading
 *   human_readable: Orthodox Reading: Varna Hierarchy as Divinely Mandated Cosmic Order
 *   domain: religious_studies/social_stratification/hermeneutics
 *
 * SUMMARY:
 *   This story instantiates the orthodox reading of the
 *   vedic_corpus_social_prescription kernel: the claim that Vedic and
 *   Dharmashastra texts literally and prescriptively mandate the varna
 *   hierarchy as an unalterable feature of cosmic order (grounded textually
 *   in the purusha sukta cosmogony and elaborated in later Dharmashastra
 *   literature), rather than describing a metaphorical spiritual unity (the
 *   reformist_spiritual_reading) or constituting a unified administrable
 *   legal code (the colonial_orientalist_reading). Under this reading
 *   specifically, occupational assignment, ritual eligibility, and marital
 *   boundaries are treated as divinely fixed rather than socially negotiable,
 *   which is what generates the high extractiveness and suppression scores
 *   authored here: the same textual authority that assigns Brahmin ritual
 *   supremacy also forecloses the terms on which lower castes could contest
 *   the assignment. This is a distinct constraint from its siblings, not a
 *   different measurement angle on one constraint — the epsilon value here is
 *   intrinsic to THIS reading's structural commitment (literal cosmic
 *   prescription) and would not transfer to a reading that denies the text
 *   has prescriptive social content at all.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, 0.86).
domain_priors:suppression_score(vedic_corpus_social_prescription__orthodox_varna_reading, 0.88).
domain_priors:theater_ratio(vedic_corpus_social_prescription__orthodox_varna_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__orthodox_varna_reading, snare).
narrative_ontology:human_readable(vedic_corpus_social_prescription__orthodox_varna_reading, "Orthodox Reading: Varna Hierarchy as Divinely Mandated Cosmic Order").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__orthodox_varna_reading, "religious_studies/social_stratification/hermeneutics").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__orthodox_varna_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__orthodox_varna_reading, '1dd21ae2-e02c-469c-b194-9f24efd5d1f8').
narrative_ontology:cs_kernel_codification('1dd21ae2-e02c-469c-b194-9f24efd5d1f8', fixed_text).
narrative_ontology:cs_authority_grounding('1dd21ae2-e02c-469c-b194-9f24efd5d1f8', lineage).
narrative_ontology:cs_interpretation_layer_present('1dd21ae2-e02c-469c-b194-9f24efd5d1f8').
narrative_ontology:cs_reading_relation('1dd21ae2-e02c-469c-b194-9f24efd5d1f8', vedic_corpus_social_prescription__reformist_spiritual_reading, coexists_with).
narrative_ontology:cs_reading_relation('1dd21ae2-e02c-469c-b194-9f24efd5d1f8', vedic_corpus_social_prescription__colonial_orientalist_reading, influences).
narrative_ontology:cs_axiom('1dd21ae2-e02c-469c-b194-9f24efd5d1f8', foundational, varna_station_is_cosmically_fixed_by_birth).
narrative_ontology:cs_axiom_status(varna_station_is_cosmically_fixed_by_birth, holdable).
narrative_ontology:cs_axiom_grounding('1dd21ae2-e02c-469c-b194-9f24efd5d1f8', varna_station_is_cosmically_fixed_by_birth, theological).
narrative_ontology:cs_axiom('1dd21ae2-e02c-469c-b194-9f24efd5d1f8', secondary, ritual_purity_hierarchy_reflects_metaphysical_reality).
narrative_ontology:cs_axiom_status(ritual_purity_hierarchy_reflects_metaphysical_reality, holdable).
narrative_ontology:cs_axiom_grounding('1dd21ae2-e02c-469c-b194-9f24efd5d1f8', ritual_purity_hierarchy_reflects_metaphysical_reality, theological).
narrative_ontology:cs_reference_frame('1dd21ae2-e02c-469c-b194-9f24efd5d1f8', purusha_sukta_cosmogonic_order).
narrative_ontology:cs_drift_state('1dd21ae2-e02c-469c-b194-9f24efd5d1f8', contemporary_constitutional_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('1dd21ae2-e02c-469c-b194-9f24efd5d1f8', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_ruling_lineages).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_laboring_castes).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_excluded_castes).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, intercaste_marriage_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, colonial_administrators_historical).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds interpretive monopoly over Sanskrit textual transmission, ritual performance, and Dharmashastra commentary. Administers the varna framework as cosmic law (purusha sukta cosmogony, karma-linked birth station), collects ritual fees, land grants, and social deference that flow structurally from occupying the apex of the hierarchy it also interprets and enforces. Can revise or relax the reading without losing standing; the interpretive seat itself is portable across any social configuration.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste, beneficiary).

% Receives political and martial legitimacy from the same cosmic ordering that ranks Brahmins above them but everyone else below both; historically underwrites Brahmin ritual authority in exchange for sanctified rule. Benefits from the hierarchy's stability but is dependent on Brahmin cooperation for legitimation.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_ruling_lineages, beneficiary,
    powerful, generational, constrained, national).

% Assigned by birth to service and labor occupations under textual injunction (e.g. Manusmriti's occupational duties); barred from Vedic study, priestly function, and many ritual and marital options. Labor value is extracted through obligatory service relationships justified as cosmic duty (svadharma); exit requires either conversion, migration, or organized political mobilization against a framework that claims to describe reality itself, not policy.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_laboring_castes, payer,
    powerless, generational, trapped, national).

% Placed outside the fourfold varna schema entirely under the orthodox reading's operative extension (ritual impurity, untouchability doctrine); excluded from temple entry, water sources, and residential integration in many historical and persisting local enforcements. Bears the most severe accessibility collapse of any seat — the framework treats their exclusion as ritually necessary, not merely customary, foreclosing appeal to shared textual authority since the same authority names their exclusion.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_excluded_castes, payer,
    powerless, generational, trapped, national).

% Face textual and community sanction (loss of caste status, family repudiation, in extreme historical and contemporary cases violence) for marrying across varna lines, which the orthodox reading treats as violation of cosmic and genealogical order (varnasankara pollution doctrine) rather than a social custom open to renegotiation.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, intercaste_marriage_seekers, payer,
    moderate, biographical, constrained, regional).

% Argue from within the same textual corpus that the prescriptive social content is later interpolation or metaphorical extension, not the core teaching; they are excluded from orthodox institutional authority (temple boards, traditional pandit lineages) precisely because their reading would dissolve the hierarchy's textual warrant. Would object loudly if given institutional standing equal to orthodox commentary.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, reformist_theologians, excluded,
    moderate, generational, constrained, national).

% Historically codified Dharmashastra materials into administrable civil law categories, treating the orthodox varna reading as settled fact for governance convenience; this is a distinct constraint (colonial_orientalist_reading) but interacts with and hardened the orthodox reading's institutional reach during the colonial period.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, colonial_administrators_historical, observer,
    institutional, biographical, analytical, national).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__orthodox_varna_reading, colonial_administrators_historical, beneficiary).

% Study the textual layers (Rigveda purusha sukta, later Dharmashastra elaboration, regional custom) and can trace how much of the hierarchy is textually attested versus locally accreted, without institutional stake in the outcome.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, the varna schema coordinates social role differentiation (priestly, martial/administrative, mercantile/agricultural, service) by grounding role assignment in a shared cosmological account, reducing negotiation costs over occupational and ritual boundaries within a large, diverse subcontinental population.
% TRANSFER_FUNCTION: Moves labor obligation, ritual purity status, marital eligibility, and access to sacred knowledge from lower-ranked castes to higher-ranked castes; Shudra and Dalit labor and deference flow upward to Brahmin and Kshatriya seats under textual sanction that forecloses renegotiation by treating the allocation as cosmic fact rather than social arrangement.
% ABSENT_VOICES: Shudra and Dalit textual interpreters were historically barred from the Sanskrit learning that would let them contest the orthodox reading on its own textual terms; reformist and heterodox voices within the tradition (bhakti movements, Buddhist and Jain counter-traditions, later Dalit theologians) exist but are structurally excluded from the orthodox commentarial lineage that administers the reading.
% DISAPPEARANCE_RATIONALE: If the orthodox reading's institutional and social enforcement vanished overnight, caste-based occupational assignment, marriage restriction, temple access barriers, and ritual purity hierarchies premised on divine cosmic mandate would lose their textual warrant; social mobility, intermarriage rates, and temple/institutional access patterns would shift substantially, though informal caste practice could persist on custom alone for a period.
% FOUNDING_PROBLEM: Early stratified agrarian and priestly societies needed a stable account of occupational and ritual role differentiation that would hold across generations without continuous renegotiation; a cosmological grounding solved the coordination problem of role assignment at low ongoing bargaining cost.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox commentators (Brahmin institutional lineages) attest the cosmic mandate is a live metaphysical truth, not merely historical function. Reformist theologians, Dalit scholars (e.g. B.R. Ambedkar's textual critique), and independent historians of religion attest the founding coordination problem is no longer live in the same form and that the arrangement now functions primarily to preserve inherited status and labor extraction; this corroboration originates outside the beneficiary set.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__orthodox_varna_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__orthodox_varna_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__orthodox_varna_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, 0.86, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__orthodox_varna_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vedic_corpus_social_prescription__orthodox_varna_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.86) because the orthodox reading structurally routes labor, ritual deference, and marital eligibility upward through a hierarchy justified by claims the framework itself renders non-negotiable (karma-linked birth station). Suppression is authored even higher (0.88) because persistence depends on active enforcement — historical and in places contemporary — against exit (caste excommunication, denial of temple access, violence against intercaste unions), not on voluntary participant preference. Theater ratio is comparatively low (0.28) because the ritual and occupational functions this reading assigns were, and in places remain, genuinely operative rather than merely performative; this distinguishes it from a piton reading where the function would have visibly atrophied into pure ceremony. Accessibility collapse (0.62) is authored below mountain-tier because heterodox traditions (bhakti, Buddhist, Jain, and modern Dalit theological counter-readings) have persistently existed within reach of the same textual corpus, so alternatives have not fully collapsed even though the orthodox institutional reading suppresses them.
 *
 * PERSPECTIVAL GAP:
 *   From the Brahmin agenda-setter seat, the constraint reads as coordination — a stable, cosmically warranted account of social differentiation that reduces negotiation costs. From the Shudra/Dalit payer seats, the identical structure reads as enforced extraction with no legitimate exit, because the coordination story and the extraction mechanism run through the same textual and institutional channel. The engine's per-seat computation should register this divergence directly from the power/exit/beneficiary data rather than from any claim adjudication.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin caste sits at the structural beneficiary pole: it authors, administers, and collects from the hierarchy's operation (d near 0), with arbitrage-grade exit because its interpretive authority is portable independent of the specific social configuration it currently ratifies. Kshatriya lineages are secondary beneficiaries with more constrained exit, since their legitimacy is derivative on Brahmin ritual sanction. Shudra and Dalit castes sit at the full-target pole (d near 1): trapped exit, generational time horizon, and the specific compounding feature that the framework claiming authority over their situation is the same framework denying them the interpretive standing to contest it. Intercaste marriage seekers are moderate-power payers with constrained rather than trapped exit, since the constraint bears on a specific biographical decision rather than lifelong occupational assignment.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem framing prevents this story from being mislabeled as pure functionless extraction with no history: the varna schema plausibly solved a genuine role-coordination problem in early stratified society. But founding_problem_status is authored contested rather than dead outright, because orthodox institutional voices still assert the cosmic mandate as live truth, while outside corroboration (reformist theologians, Dalit scholars, comparative historians) attests the coordination function has long since detached from the enforcement machinery, which now persists primarily to preserve inherited status. The mismatch between status=contested (trending dead outside the beneficiary set) and disappearance_verdict=world_rearranges is itself the diagnostic signal this framework is built to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_layering_ambiguity,
    'Is the prescriptive social content (varna occupational assignment, marital restriction) part of the Vedic corpus''s earliest stratum, or a later Dharmashastra accretion read back onto the Vedas by orthodox commentary?',
    'Philological dating of textual strata (Rigveda purusha sukta hymn versus later Dharmashastra elaboration such as Manusmriti); comparative analysis of regional custom absorbed into commentarial tradition over centuries.',
    'If the prescriptive content is substantially later accretion, the orthodox reading''s claim to represent the ''original'' cosmic mandate weakens considerably, supporting the reformist reading''s account of the earlier corpus as primarily cosmological/spiritual rather than social-prescriptive — this would not change THIS story''s epsilon (which is intrinsic to the orthodox reading as actually practiced and enforced) but would bear on how much institutional weight the orthodox reading''s textual warrant can bear going forward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_layering_ambiguity, empirical, 'Whether varna''s prescriptive content is early-textual or later-accreted.').

omega_variable(
    orthodox_reading_naturalness_vs_construction,
    'Is the varna hierarchy, under the orthodox reading, a genuine feature of cosmic/metaphysical order (as its own tradition claims) or a constructed social arrangement retrofitted with cosmological justification by an identifiable beneficiary group?',
    'This question is not resolvable by empirical textual analysis alone — it depends on metaphysical commitments the orthodox tradition holds as first-order truth claims. What is resolvable empirically is the correlational fact that the reading''s chief institutional beneficiaries (Brahmin lineages) are also its chief interpretive authorities, which is independently documentable through the historical record of commentarial control and land/ritual-fee flows.',
    'If treated as constructed rather than natural, this reading is correctly classified as snare (extraction dressed as cosmic necessity); if the metaphysical claim is bracketed as a live first-order commitment within the tradition, the classification still holds structurally, since the schema evaluates the constraint''s actual operation (who benefits, who is coerced) independent of the truth-value of its metaphysical warrant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(orthodox_reading_naturalness_vs_construction, conceptual, 'Whether the orthodox reading names a genuine cosmic order or a constructed hierarchy with cosmological cover.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does the orthodox reading''s literal-cosmic-mandate premise logically foreclose the reformist reading''s no-prescriptive-content premise within a single interpretive framework, or can both be held as live but competing positions across different communities?',
    'Examine whether any single interpretive tradition (a specific lineage, school, or commentator) has coherently held both premises simultaneously, versus whether the two premises only coexist across distinct, non-overlapping communities of practice.',
    'If no single framework can coherently hold both, the relation to reformist_spiritual_reading should be forecloses rather than coexists_with; the current authoring treats them as coexisting across different communities (orthodox institutions versus reformist theological movements), which this omega flags as contestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether orthodox and reformist readings can coexist within one framework or logically foreclose each other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__orthodox_varna_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vedi_tr_t40, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(vedi_tr_t80, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement(vedi_tr_t120, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 120, 0.23).
narrative_ontology:measurement(vedi_tr_t160, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 160, 0.26).
narrative_ontology:measurement(vedi_tr_t200, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 200, 0.28).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(vedi_be_t40, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 40, 0.8).
narrative_ontology:measurement(vedi_be_t80, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 80, 0.83).
narrative_ontology:measurement(vedi_be_t120, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 120, 0.85).
narrative_ontology:measurement(vedi_be_t160, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 160, 0.85).
narrative_ontology:measurement(vedi_be_t200, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 200, 0.86).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(vedi_su_t40, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(vedi_su_t80, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 80, 0.8).
narrative_ontology:measurement(vedi_su_t120, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 120, 0.84).
narrative_ontology:measurement(vedi_su_t160, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 160, 0.86).
narrative_ontology:measurement(vedi_su_t200, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 200, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__orthodox_varna_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_corpus_social_prescription__orthodox_varna_reading, 0.08).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription__reformist_spiritual_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription__colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% Three constraints decompose the natural-language label 'the Vedic texts on social order': orthodox_varna_reading (this story — literal cosmic mandate, snare-grade epsilon, Brahmin/Kshatriya beneficiaries, Shudra/Dalit victims), reformist_spiritual_reading (metaphorical cosmology with no prescriptive social content — expected near-mountain, negligible extraction), and colonial_orientalist_reading (Dharmashastra as unified administrable legal code — expected tangled_rope, with colonial administrative apparatus and collaborating elites as beneficiaries and the diversity of pre-colonial regional custom as the suppressed alternative). The orthodox reading historically influenced the colonial reading's codification project (colonial administrators drew on orthodox commentarial authority to construct 'Hindu law'), hence the influences edge; the reformist reading coexists as a live counter-tradition within the same textual corpus rather than logically foreclosing the orthodox reading, since both persist as positions held by different communities today.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
