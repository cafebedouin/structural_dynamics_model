% ============================================================================
% CONSTRAINT STORY: biblical_source_text__formal_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_source_text__formal_equivalence_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: biblical_source_text__formal_equivalence_reading
 *   human_readable: Formal Equivalence Translation Norm (Source-Structure Primacy)
 *   domain: religious/biblical_studies/translation_theory
 *
 * SUMMARY:
 *   The formal equivalence reading of the biblical source text kernel asserts
 *   that fidelity to source-language structure (morphology, syntax, word
 *   order, idiom) is the primary translational obligation; intelligibility in
 *   the target language is a subordinate responsibility discharged through
 *   teaching, not through translation choices. This reading instantiates a
 *   constraint that governs conservative Protestant translation philosophy,
 *   publishing, and pedagogy. It coordinates a global community around a
 *   stable textual standard but extracts heavily from non-specialist readers
 *   who cannot access the source languages and from majority-world
 *   translators forced to produce unnatural target texts. The beneficiary
 *   communities (conservative denominations, seminaries, publishing boards)
 *   maintain authority through the norm's persistence. The constraint is
 *   actively enforced through denominational approval processes, seminary
 *   curricula, and publishing gatekeeping.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__formal_equivalence_reading, 0.68).
domain_priors:suppression_score(biblical_source_text__formal_equivalence_reading, 0.55).
domain_priors:theater_ratio(biblical_source_text__formal_equivalence_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__formal_equivalence_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__formal_equivalence_reading, "Formal Equivalence Translation Norm (Source-Structure Primacy)").
narrative_ontology:topic_domain(biblical_source_text__formal_equivalence_reading, "religious/biblical_studies/translation_theory").

domain_priors:requires_active_enforcement(biblical_source_text__formal_equivalence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__formal_equivalence_reading, 'd44088fc-5ccf-415f-b303-3bdcb2fbabc7').
narrative_ontology:cs_kernel_codification('d44088fc-5ccf-415f-b303-3bdcb2fbabc7', fixed_text).
narrative_ontology:cs_authority_grounding('d44088fc-5ccf-415f-b303-3bdcb2fbabc7', lineage).
narrative_ontology:cs_interpretation_layer_present('d44088fc-5ccf-415f-b303-3bdcb2fbabc7').
narrative_ontology:cs_reading_relation('d44088fc-5ccf-415f-b303-3bdcb2fbabc7', biblical_source_text__dynamic_equivalence_reading, forecloses).
narrative_ontology:cs_reading_relation('d44088fc-5ccf-415f-b303-3bdcb2fbabc7', biblical_source_text__critical_reconstructive_reading, coexists_with).
narrative_ontology:cs_axiom('d44088fc-5ccf-415f-b303-3bdcb2fbabc7', foundational, source_language_structure_is_revelatory_vehicle).
narrative_ontology:cs_axiom_status(source_language_structure_is_revelatory_vehicle, holdable).
narrative_ontology:cs_axiom_grounding('d44088fc-5ccf-415f-b303-3bdcb2fbabc7', source_language_structure_is_revelatory_vehicle, deontological).
narrative_ontology:cs_axiom('d44088fc-5ccf-415f-b303-3bdcb2fbabc7', foundational, intelligibility_burden_belongs_to_community_not_translator).
narrative_ontology:cs_axiom_status(intelligibility_burden_belongs_to_community_not_translator, holdable).
narrative_ontology:cs_axiom_grounding('d44088fc-5ccf-415f-b303-3bdcb2fbabc7', intelligibility_burden_belongs_to_community_not_translator, conventional).
narrative_ontology:cs_reference_frame('d44088fc-5ccf-415f-b303-3bdcb2fbabc7', reformation_textus_receptus_standard).
narrative_ontology:cs_drift_state('d44088fc-5ccf-415f-b303-3bdcb2fbabc7', contemporary_critical_text_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d44088fc-5ccf-415f-b303-3bdcb2fbabc7', '').
narrative_ontology:cs_kernel_id(biblical_source_text__formal_equivalence_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, hermeneutically_conservative_communities).
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, seminary_faculty_guardians).
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, denominational_publishing_boards).
narrative_ontology:constraint_victim(biblical_source_text__formal_equivalence_reading, non_specialist_readers).
narrative_ontology:constraint_victim(biblical_source_text__formal_equivalence_reading, vernacular_congregations).
narrative_ontology:constraint_victim(biblical_source_text__formal_equivalence_reading, minority_language_translators).
narrative_ontology:constraint_vindicates(biblical_source_text__formal_equivalence_reading, verbal_plenary_inspiration).
narrative_ontology:constraint_vindicates(biblical_source_text__formal_equivalence_reading, textual_stability_as_authority_anchor).
narrative_ontology:constraint_vindicates(biblical_source_text__formal_equivalence_reading, source_language_primacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain doctrinal authority through textual stability; the formal equivalence norm guarantees that translation cannot become a vector for doctrinal drift. Their identity is fused to the claim that the source text's structure is the vehicle of revelation itself — exit means surrendering the epistemological ground of their tradition.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, hermeneutically_conservative_communities, beneficiary,
    organized, generational, identity_locked, global).

% Control the pedagogical pipeline: they teach the languages, set the curricula, gatekeep ordination, and author the commentaries that define 'faithful' translation. Their professional capital is invested in the formal equivalence framework; shifting to dynamic equivalence would devalue their specialized training and institutional authority.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, seminary_faculty_guardians, agenda_setter,
    institutional, biographical, constrained, global).

% Own the copyrighted translations (ESV, NASB, NKJV, etc.) that instantiate formal equivalence. They profit from stable translation bases that don't require frequent revision, and they control the distribution channels to conservative churches. Their revenue depends on the norm's persistence.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, denominational_publishing_boards, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__formal_equivalence_reading, denominational_publishing_boards, agenda_setter).

% Encounter translations that preserve source-language syntax (Hebrew/Greek word order, idiom, grammar) at the expense of target-language intelligibility. They must rely on teaching ministries to explain what the text means — they cannot simply read and understand. Exit requires either learning biblical languages (high barrier) or leaving the community (identity cost).
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, non_specialist_readers, payer,
    powerless, biographical, constrained, global).

% Congregations in majority-world contexts using translations produced under formal equivalence strictures. The translations often sound alien in their own language — preserving Greek syntax in Swahili or Hebrew idiom in Quechua. They bear the intelligibility cost while the beneficiary communities control the translation philosophy. Exit means adopting 'unapproved' translations and risking institutional sanction.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, vernacular_congregations, payer,
    moderate, biographical, constrained, regional).

% Translators working under formal equivalence mandates from sending agencies or partner denominations. They are required to mirror source-language structures that have no equivalent in the target language, producing translations that native speakers find unnatural or incomprehensible. They cannot advocate for dynamic equivalence without risking funding, partnership, or accusations of theological liberalism.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, minority_language_translators, payer,
    powerless, biographical, trapped, global).

% Translators and scholars (SIL, Wycliffe, UBS, Nida-influenced) who prioritize communicative effectiveness. They are structurally excluded from conservative publication channels and denominational approval processes. Their translations (NLT, CEV, many majority-world vernacular versions) are treated as second-tier or dangerous by the formal equivalence establishment.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, dynamic_equivalence_practitioners, excluded,
    organized, biographical, mobile, global).

% Scholars focused on textual criticism, manuscript evidence, and historical reconstruction (the critical_reconstructive_reading). They observe that both formal and dynamic equivalence readings presuppose a stable source text that the manuscript tradition does not uniformly support. Their work undermines the shared premise of both rival readings.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, critical_text_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, verifiable translation standard that prevents doctrinal fragmentation across generations and geographies; enables shared textual reference point for teaching, liturgy, and cross-cultural mission within conservative communities.
% TRANSFER_FUNCTION: Moves the cognitive burden of intelligibility from the translator/text to the reader/community — the translator preserves structure; the community pays in teaching labor, educational infrastructure, and mediated access. Moves institutional authority and revenue to publishing boards and seminaries that control the formal equivalence pipeline.
% ABSENT_VOICES: Majority-world vernacular readers who encounter formal equivalence translations as foreign impositions; oral-preference communities for whom written structural fidelity is irrelevant to communicative effectiveness; women and marginalized groups within conservative communities whose interpretive access is mediated entirely through male-dominated teaching structures.
% DISAPPEARANCE_RATIONALE: If the formal equivalence norm vanished, conservative denominations would fracture over translation philosophy; publishing boards would lose copyright monopolies on stable translations; seminaries would lose curricular coherence; vernacular congregations would gain intelligible texts but lose institutional covering; the global conservative Protestant ecosystem would reorganize around a new (contested) translation consensus.
% FOUNDING_PROBLEM: The Reformation's sola scriptura required a stable, accessible text — but vernacular translations varied wildly, and the Catholic Church charged that Protestant translation was arbitrary. Formal equivalence (via the Textus Receptus/Masoretic Text tradition) provided a fixed, verifiable standard that anchored authority in the source text rather than the translator's choices.
% FOUNDING_PROBLEM_CORROBORATION: Reformation historians (e.g., David Steinmetz, Alister McGrath) attest the founding problem was real: textual stability was epistemologically necessary for sola scriptura. Modern textual critics (Ehrman, Wallace, Metzger) and mission linguists (Nida, Wendland) attest the problem is substantially altered: we now know the source text itself is reconstructed, not given; and communicative effectiveness data shows formal equivalence fails its own accessibility test for non-specialists. The conservative establishment disputes both.
narrative_ontology:disappearance_verdict(biblical_source_text__formal_equivalence_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_source_text__formal_equivalence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__formal_equivalence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(biblical_source_text__formal_equivalence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__formal_equivalence_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__formal_equivalence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_source_text__formal_equivalence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_source_text__formal_equivalence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the cognitive and pedagogical burden is systematically transferred to the least powerful agents (non-specialist readers, minority-language translators) while beneficiaries capture authority and revenue. Suppression (0.55) is moderate-high: the norm is enforced through institutional gatekeeping (ordination requirements, publication approval, funding conditions) rather than state coercion, but exit costs are severe for identity-locked agents. Theater ratio (0.22) is low-moderate: the coordination function (textual stability for doctrinal unity) is genuine but increasingly performative as the manuscript basis itself is recognized as reconstructed. Accessibility collapse (0.65) reflects that alternatives (dynamic equivalence, critical reconstruction) are institutionally marginalized within the conservative ecosystem. Resistance (0.48) is significant: dynamic equivalence practitioners, majority-world translators, and critical scholars all contest the norm, but their resistance is fragmented across different frameworks.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (seminary faculty, publishing boards), the constraint appears as a rope — genuine coordination solving the Reformation's textual stability problem. From the payer seats (non-specialist readers, vernacular congregations, minority-language translators), it operates as a snare — extraction of intelligibility and labor enforced through identity-lock and institutional capture. The engine computes this divergence from the structural data: beneficiaries are organized/institutional with constrained exit; payers are powerless/moderate with constrained/trapped exit; the same constraint structure produces opposite type classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (hermeneutically conservative communities, seminary faculty, publishing boards) collect authority, revenue, and institutional coherence — their directionality d is near 0.1 (beneficiary end). Victims (non-specialist readers, vernacular congregations, minority-language translators) bear cognitive burden, mediated access, and professional risk — their d is near 0.85-0.95 (target end). The excluded (dynamic equivalence practitioners) are structurally locked out of the coordination benefits but also free from the extraction — their d is undefined (analytical). The analytical observer (critical text scholars) sees the kernel itself as unstable — d is not applicable.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Reformation textual stability) was live in 1517-1611 but is contested today: textual criticism has shown the source text is reconstructed, not given; mission linguistics has shown formal equivalence fails communicative effectiveness. The constraint persists because beneficiaries (publishing boards, seminaries, conservative denominations) extract value from its persistence — authority, revenue, curricular stability. This is mandatrophy: the mandate (textual fidelity for doctrinal stability) has outlived its function (the text itself is not stable; the stability is performed), but the arrangement continues because beneficiaries capture the extraction. The tangled_rope classification captures this: genuine coordination function (community cohesion around shared text) + asymmetric extraction (burden on non-specialists, revenue to institutions) + active enforcement (gatekeeping).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    source_text_stability_ontology,
    'Is the source text whose structure formal equivalence claims to preserve a stable historical object or a reconstructed critical edition?',
    'Textual criticism consensus on the degree of certainty in the NA28/UBS5 and BHS critical texts; manuscript evidence for variant readings affecting translation choices.',
    'If the source text is reconstructed, the formal equivalence constraint coordinates around a performed stability rather than a given one — extraction increases because the ''fidelity'' is to an editorial product, not an ontological given. The coordination function becomes partially theatrical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(source_text_stability_ontology, empirical, 'Whether the source text''s stability is historical fact or critical reconstruction').

omega_variable(
    teaching_mediation_effectiveness,
    'Does the teaching-mediated intelligibility model actually work for non-specialist readers in conservative communities, or does it produce performative comprehension?',
    'Empirical studies of biblical literacy in formal-equivalence-using congregations vs. dynamic-equivalence-using congregations; qualitative research on reader experience.',
    'If teaching mediation fails systematically, the constraint''s coordination function collapses — it neither preserves structure for specialists (who read the source languages) nor enables intelligibility for non-specialists. The constraint becomes a piton or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teaching_mediation_effectiveness, empirical, 'Whether the subordinate intelligibility responsibility is actually discharged').

omega_variable(
    identity_lock_mechanism,
    'Is the identity_locked exit option for conservative communities theological conviction, social pressure, or institutional capture?',
    'Sociological studies of conservative Protestant communities facing translation changes; longitudinal data on schism rates when translations shift.',
    'If identity lock is primarily institutional (fear of job loss, ordination denial, funding cutoff) rather than theological, the constraint''s extraction is more coercive than its beneficiaries claim. The suppression metric understates the structural force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, conceptual, 'Nature of the identity fusion binding conservative communities to formal equivalence').

omega_variable(
    kernel_framing_underdetermination,
    'Does the biblical_source_text kernel admit a single coherent framing, or do the three readings instantiate different kernels?',
    'Philosophical analysis of whether ''the biblical text'' refers to the same object across readings: the autographs (critical), the received text (formal), or the communicative event (dynamic).',
    'If the readings operate on different ontological referents, they are not sibling readings of one kernel but constraints on different objects. The kernel_id would be a category error. The engine''s inferred_coupling_protocol would misfire.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel_id ''biblical_source_text'' univocally identifies one contested commitment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__formal_equivalence_reading, 1517, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t1517, biblical_source_text__formal_equivalence_reading, theater_ratio, 1517, 0.08).
narrative_ontology:measurement(bibl_tr_t1611, biblical_source_text__formal_equivalence_reading, theater_ratio, 1611, 0.1).
narrative_ontology:measurement(bibl_tr_t1881, biblical_source_text__formal_equivalence_reading, theater_ratio, 1881, 0.12).
narrative_ontology:measurement(bibl_tr_t1971, biblical_source_text__formal_equivalence_reading, theater_ratio, 1971, 0.16).
narrative_ontology:measurement(bibl_tr_t1995, biblical_source_text__formal_equivalence_reading, theater_ratio, 1995, 0.19).
narrative_ontology:measurement(bibl_tr_t2001, biblical_source_text__formal_equivalence_reading, theater_ratio, 2001, 0.2).
narrative_ontology:measurement(bibl_tr_t2011, biblical_source_text__formal_equivalence_reading, theater_ratio, 2011, 0.21).
narrative_ontology:measurement(bibl_tr_t2025, biblical_source_text__formal_equivalence_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(bibl_be_t1517, biblical_source_text__formal_equivalence_reading, base_extractiveness, 1517, 0.35).
narrative_ontology:measurement(bibl_be_t1611, biblical_source_text__formal_equivalence_reading, base_extractiveness, 1611, 0.42).
narrative_ontology:measurement(bibl_be_t1881, biblical_source_text__formal_equivalence_reading, base_extractiveness, 1881, 0.48).
narrative_ontology:measurement(bibl_be_t1971, biblical_source_text__formal_equivalence_reading, base_extractiveness, 1971, 0.58).
narrative_ontology:measurement(bibl_be_t1995, biblical_source_text__formal_equivalence_reading, base_extractiveness, 1995, 0.62).
narrative_ontology:measurement(bibl_be_t2001, biblical_source_text__formal_equivalence_reading, base_extractiveness, 2001, 0.65).
narrative_ontology:measurement(bibl_be_t2011, biblical_source_text__formal_equivalence_reading, base_extractiveness, 2011, 0.68).
narrative_ontology:measurement(bibl_be_t2025, biblical_source_text__formal_equivalence_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t1517, biblical_source_text__formal_equivalence_reading, suppression_requirement, 1517, 0.25).
narrative_ontology:measurement(bibl_su_t1611, biblical_source_text__formal_equivalence_reading, suppression_requirement, 1611, 0.3).
narrative_ontology:measurement(bibl_su_t1881, biblical_source_text__formal_equivalence_reading, suppression_requirement, 1881, 0.38).
narrative_ontology:measurement(bibl_su_t1971, biblical_source_text__formal_equivalence_reading, suppression_requirement, 1971, 0.45).
narrative_ontology:measurement(bibl_su_t1995, biblical_source_text__formal_equivalence_reading, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement(bibl_su_t2001, biblical_source_text__formal_equivalence_reading, suppression_requirement, 2001, 0.52).
narrative_ontology:measurement(bibl_su_t2011, biblical_source_text__formal_equivalence_reading, suppression_requirement, 2011, 0.54).
narrative_ontology:measurement(bibl_su_t2025, biblical_source_text__formal_equivalence_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__formal_equivalence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_source_text__formal_equivalence_reading, 0.08).
narrative_ontology:affects_constraint(biblical_source_text__formal_equivalence_reading, biblical_source_text__dynamic_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__formal_equivalence_reading, biblical_source_text__critical_reconstructive_reading).
narrative_ontology:affects_constraint(biblical_source_text__formal_equivalence_reading, vernacular_translation_policy).
narrative_ontology:affects_constraint(biblical_source_text__formal_equivalence_reading, seminary_hebrew_greek_curriculum).
narrative_ontology:affects_constraint(biblical_source_text__formal_equivalence_reading, denominational_ordination_requirements).

% DUAL FORMULATION NOTE:
% The biblical_source_text kernel decomposes into three constraint stories by ε-invariance: formal_equivalence_reading (ε=0.68, tangled_rope), dynamic_equivalence_reading (ε≈0.35, rope), critical_reconstructive_reading (ε≈0.25, mountain/rope boundary). The formal equivalence reading has highest extraction because it transfers intelligibility burden to non-specialists while capturing institutional authority. The dynamic equivalence reading coordinates missionary translation with lower extraction. The critical reading approaches mountain status — the textual data is what it is, independent of translational philosophy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_source_text__formal_equivalence_reading, institutional, 0.15).
constraint_indexing:directionality_override(biblical_source_text__formal_equivalence_reading, organized, 0.1).
constraint_indexing:directionality_override(biblical_source_text__formal_equivalence_reading, powerless, 0.9).
constraint_indexing:directionality_override(biblical_source_text__formal_equivalence_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
