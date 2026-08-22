% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__reformist_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: vedic_corpus_social_prescription__reformist_spiritual_reading
 *   human_readable: Reformist Spiritual Reading of Vedic Corpus
 *   domain: religious_studies/social_stratification/hermeneutics
 *
 * SUMMARY:
 *   The Vedic corpus is a contested kernel: three structurally distinct
 *   readings extract different constraints from the same textual base. This
 *   story instantiates the reformist spiritual reading, which claims the
 *   Vedas describe universal spiritual unity and metaphorical cosmology with
 *   no prescriptive social content. Under this reading, varna is either
 *   metaphorical or a later corruption; the constraint functions as a
 *   low-cost coordination mechanism for egalitarian spiritual practice.
 *   Because the reading explicitly denies that the texts prescribe social
 *   hierarchy, it carries no victim set and minimal extraction. The sibling
 *   readingsâorthodox_varna_reading (literal hierarchy as divine mandate)
 *   and colonial_orientalist_reading (texts as codifiable law)âare
 *   structurally distinct constraints with different epsilon values and must
 *   be authored separately per the epsilon-invariance principle. This reading
 *   forecloses the orthodox premise and influences the colonial premise by
 *   denying the textual foundation for prescriptive social codification.
 *
 * KEY AGENTS:
 *   - reformist_spiritual_practitioners: Primary beneficiary (moderate/mobile) â gain egalitarian spiritual framework free from caste prescription
 *   - reformist_religious_leaders: Agenda setter (organized/mobile) â administers interpretive norms without material extraction
 *   - orthodox_brahminical_authorities: Excluded voice (institutional/mobile) â objects from outside the reformist framework
 *   - anti_caste_secularists: Analytical observer (organized/analytical) â corroborates the founding problem from outside the beneficiary set
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.13).
domain_priors:suppression_score(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.12).
domain_priors:theater_ratio(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, extractiveness, 0.13).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__reformist_spiritual_reading, rope).
narrative_ontology:human_readable(vedic_corpus_social_prescription__reformist_spiritual_reading, "Reformist Spiritual Reading of Vedic Corpus").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__reformist_spiritual_reading, "religious_studies/social_stratification/hermeneutics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__reformist_spiritual_reading, '2ecd4ddf-3566-4207-97ef-b7f6465a19bf').
narrative_ontology:cs_kernel_codification('2ecd4ddf-3566-4207-97ef-b7f6465a19bf', fixed_text).
narrative_ontology:cs_authority_grounding('2ecd4ddf-3566-4207-97ef-b7f6465a19bf', expertise).
narrative_ontology:cs_interpretation_layer_present('2ecd4ddf-3566-4207-97ef-b7f6465a19bf').
narrative_ontology:cs_reading_relation('2ecd4ddf-3566-4207-97ef-b7f6465a19bf', vedic_corpus_social_prescription__orthodox_varna_reading, forecloses).
narrative_ontology:cs_reading_relation('2ecd4ddf-3566-4207-97ef-b7f6465a19bf', vedic_corpus_social_prescription__colonial_orientalist_reading, influences).
narrative_ontology:cs_axiom('2ecd4ddf-3566-4207-97ef-b7f6465a19bf', foundational, vedic_texts_purely_spiritual_no_prescription).
narrative_ontology:cs_axiom_status(vedic_texts_purely_spiritual_no_prescription, holdable).
narrative_ontology:cs_axiom_grounding('2ecd4ddf-3566-4207-97ef-b7f6465a19bf', vedic_texts_purely_spiritual_no_prescription, empirically_contingent).
narrative_ontology:cs_axiom('2ecd4ddf-3566-4207-97ef-b7f6465a19bf', secondary, varna_as_later_accretion).
narrative_ontology:cs_axiom_status(varna_as_later_accretion, holdable).
narrative_ontology:cs_axiom_grounding('2ecd4ddf-3566-4207-97ef-b7f6465a19bf', varna_as_later_accretion, empirically_contingent).
narrative_ontology:cs_reference_frame('2ecd4ddf-3566-4207-97ef-b7f6465a19bf', primordial_spiritual_unity).
narrative_ontology:cs_drift_state('2ecd4ddf-3566-4207-97ef-b7f6465a19bf', contemporary_orthodox_practice, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('2ecd4ddf-3566-4207-97ef-b7f6465a19bf', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_spiritual_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage with Vedic texts as sources of universal spiritual truth and metaphysical symbolism without accepting caste-based social obligations. They gain a religious framework compatible with egalitarian values and modern citizenship. Exit means abandoning the reformist community for orthodox or secular frameworks.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_spiritual_practitioners, beneficiary,
    moderate, biographical, mobile, national).

% Propagate the spiritual-only reading through teaching, publishing, and institution-building. They administer the interpretive norms that distinguish spiritual content from social prescription. They do not collect material rents from the constraint's operation but gain social authority and follower trust within reformist circles.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_religious_leaders, agenda_setter,
    organized, generational, mobile, national).

% Maintain that Vedic texts literally prescribe varna duties and social hierarchy. They would object to the reformist reading's erasure of prescriptive content but are structurally excluded from the reformist hermeneutic framework's conversation; their objections are heard in the broader public sphere but not within the reformist textual community.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, orthodox_brahminical_authorities, excluded,
    institutional, generational, mobile, national).

% Observe the reformist reading as a strategy for religious modernization. They corroborate the persistence of caste hierarchy that the reformist reading seeks to dissolve, but do not depend on the Vedic spiritual framework for their own anti-caste politics.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, anti_caste_secularists, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates shared spiritual practice and textual reverence across diverse practitioners without imposing social stratification; provides a common religious vocabulary that transcends caste boundaries and unites adherents around metaphysical rather than social content.
% TRANSFER_FUNCTION: Moves spiritual authority from hereditary institutional interpreters to individual or direct-community access to texts; transfers social legitimacy from orthodox hierarchy to egalitarian fellowship. No material transfer occurs.
% ABSENT_VOICES: Orthodox Brahminical authorities and traditional Dharmashastra interpreters who assert literal varna prescription; they are present in the broader religious field but structurally excluded from the reformist reading's hermeneutic circle.
% DISAPPEARANCE_RATIONALE: If the reformist spiritual reading vanished, egalitarian practitioners would lose their primary textual grounding for non-hierarchical religious identity; many would face re-absorption into orthodox varna frameworks or secular exit, rearranging the landscape of Hindu reform movements.
% FOUNDING_PROBLEM: Scriptural authority was monopolized by orthodox interpreters to legitimate caste hierarchy, creating a collective-action problem for egalitarian spirituality: how to maintain Vedic reverence without accepting social stratification.
% FOUNDING_PROBLEM_CORROBORATION: Dalit rights movements, anti-caste intellectuals, and secular critics of hierarchy attest from outside the reformist beneficiary set that caste-based social prescription remains operative; they corroborate the founding problem's persistence even where they do not share the reformist spiritual solution.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__reformist_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__reformist_spiritual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__reformist_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.13, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__reformist_spiritual_reading_tests).
:- end_tests(vedic_corpus_social_prescription__reformist_spiritual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.13 at interval end) because the reformist reading explicitly refuses to extract labor, status, or resources through social prescription; its operation is hermeneutic and voluntary. Suppression is low (0.12) because adherence is not coercedâpractitioners adopt the reading freely. Theater ratio is moderate-low (0.30) and rising gently: as the reformist tradition institutionalizes, a growing share of interpretive labor is devoted to harmonizing apparent social prescriptions in the text with the spiritual-only claim, producing performative hermeneutics. Resistance is moderate (0.55) because the reading meets sustained opposition from orthodox authorities, but this resistance is external to the constraint's own operation. The absence of a victim set means directionality is uniformly low across all seated agents.
 *
 * PERSPECTIVAL GAP:
 *   Within the reformist constraint, all seated agents experience low extraction: practitioners benefit from an egalitarian framework, and leaders gain status without material rent. There is no seated payer. The primary perspectival gap is not internal to this constraint but between this reading and its orthodox sibling: the same kernel computes as rope from the reformist seat and as snare from the orthodox seat (where varna hierarchy extracts from lower-varna agents). The engine will compute divergent per-seat types across the constraint family.
 *
 * DIRECTIONALITY LOGIC:
 *   Practitioners are beneficiaries (d near 0.0) because the constraint subsidizes their spiritual identity by removing hierarchical obligations. Leaders are near-symmetric to slightly beneficiary (d ~0.2) because they invest interpretive labor but gain community authority. No victim is declared, so no agent sits near d=1.0. Orthodox authorities are excluded from the stakeholder surface of this constraint because they do not participate in its operationâthey are the absent voice.
 *
 * MANDATROPHY ANALYSIS:
 *   The reformist reading resists mandatrophy mislabeling because its founding problemâcaste hierarchy legitimated by scriptureâremains live, and the reading continues to coordinate egalitarian spiritual practice. The low theater ratio and stable founding-problem status indicate the constraint has not atrophied into piton-like performance. Were the caste system to dissolve completely, the reading would face mandatrophy pressure; as of the interval end, that pressure is theoretical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reformist_reading_kernel_position,
    'Is this constraint one reading of the vedic_corpus_social_prescription kernel, and does the reformist spiritual reading structurally displace the orthodox varna reading or merely coexist with it in parallel communities?',
    'Historical sociology of interpretive communities: track whether reformist institutions have replaced orthodox ones or only parallelized over the interval.',
    'If displacement, the reformist reading functions as a competing authority structure; if parallelization, it is a sectarian rope alongside the orthodox snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformist_reading_kernel_position, conceptual, 'Structural relationship between reformist and orthodox readings of the same kernel.').

omega_variable(
    vedic_social_content_ambiguity,
    'Do the earliest Vedic strata (Samhitas) contain prescriptive social content, or is all varna reference confined to later Brahmana and Dharmashastra layers?',
    'Philological and historical-linguistic analysis of Samhita texts independent of theological commitment.',
    'If the earliest strata contain varna prescription, the reformist reading''s empirical foundation weakens and its balance shifts toward identity_coordination cover; if absent, the reading gains empirical support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vedic_social_content_ambiguity, empirical, 'Whether the Vedic kernel itself contains prescriptive social content.').

omega_variable(
    reformist_colonial_genealogy,
    'To what extent is the reformist spiritual reading a product of colonial-era apologetics rather than an autonomous hermeneutic recovery?',
    'Intellectual history tracing the pre-colonial vs. colonial emergence of spiritual-only Vedic exegesis.',
    'If primarily colonial, the reading''s coordination function may be identity_coordination under external pressure rather than endogenous rope; this changes its naturalness profile.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reformist_colonial_genealogy, empirical, 'Colonial genealogy of the reformist reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__reformist_spiritual_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vedi_tr_t20, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(vedi_tr_t40, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(vedi_tr_t60, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 60, 0.23).
narrative_ontology:measurement(vedi_tr_t80, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement(vedi_tr_t100, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(vedi_be_t20, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 20, 0.09).
narrative_ontology:measurement(vedi_be_t40, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 40, 0.1).
narrative_ontology:measurement(vedi_be_t60, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 60, 0.11).
narrative_ontology:measurement(vedi_be_t80, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 80, 0.12).
narrative_ontology:measurement(vedi_be_t100, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 100, 0.13).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(vedic_corpus_social_prescription__reformist_spiritual_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__reformist_spiritual_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, orthodox_varna_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is the reformist spiritual reading of the vedic_corpus_social_prescription kernel, decomposed per the epsilon-invariance principle from the orthodox varna reading and colonial orientalist reading due to structurally distinct epsilon values, beneficiary/victim profiles, and stakeholder configurations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
