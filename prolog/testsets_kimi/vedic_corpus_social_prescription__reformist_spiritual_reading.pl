% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__reformist_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
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
 *   domain: religious_studies/hermeneutics/social_stratification
 *
 * SUMMARY:
 *   This constraint instantiates the reformist_spiritual_reading of the
 *   vedic_corpus_social_prescription kernel. It holds that Vedic texts
 *   describe spiritual unity and metaphorical cosmology with no prescriptive
 *   social content, thereby coordinating egalitarian religious practice while
 *   denying scriptural sanction to caste hierarchy. The reading is contested
 *   by an orthodox_varna_reading (literal prescription) and a
 *   colonial_orientalist_reading (codifiable legal system). As a rope, it
 *   exhibits low extractiveness, low suppression, and low theater ratio; its
 *   coordination function is genuine spiritual community formation across
 *   caste lines.
 *
 * KEY AGENTS:
 *   - Reformist communities (beneficiary, moderate/constrained): coordinate on egalitarian spiritual practice through metaphorical hermeneutics.
 *   - Orthodox varna proponents (excluded, institutional/trapped): resist the reading because it erodes their hereditary authority.
 *   - Postcolonial scholars (observer, analytical): analyze the hermeneutic contest and its political entanglements.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.15).
domain_priors:suppression_score(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.12).
domain_priors:theater_ratio(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__reformist_spiritual_reading, rope).
narrative_ontology:human_readable(vedic_corpus_social_prescription__reformist_spiritual_reading, "Reformist Spiritual Reading of Vedic Corpus").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__reformist_spiritual_reading, "religious_studies/hermeneutics/social_stratification").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__reformist_spiritual_reading, 'dacd6d48-2183-4d62-91bd-bcc83a98098a').
narrative_ontology:cs_kernel_codification('dacd6d48-2183-4d62-91bd-bcc83a98098a', fixed_text).
narrative_ontology:cs_authority_grounding('dacd6d48-2183-4d62-91bd-bcc83a98098a', practice).
narrative_ontology:cs_interpretation_layer_present('dacd6d48-2183-4d62-91bd-bcc83a98098a').
narrative_ontology:cs_reading_relation('dacd6d48-2183-4d62-91bd-bcc83a98098a', vedic_corpus_social_prescription__orthodox_varna_reading, coexists_with).
narrative_ontology:cs_reading_relation('dacd6d48-2183-4d62-91bd-bcc83a98098a', vedic_corpus_social_prescription__colonial_orientalist_reading, influences).
narrative_ontology:cs_axiom('dacd6d48-2183-4d62-91bd-bcc83a98098a', foundational, spiritual_unity_over_hierarchy).
narrative_ontology:cs_axiom_status(spiritual_unity_over_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('dacd6d48-2183-4d62-91bd-bcc83a98098a', spiritual_unity_over_hierarchy, deontological).
narrative_ontology:cs_axiom('dacd6d48-2183-4d62-91bd-bcc83a98098a', foundational, vedic_texts_metaphorical_not_prescriptive).
narrative_ontology:cs_axiom_status(vedic_texts_metaphorical_not_prescriptive, holdable).
narrative_ontology:cs_axiom_grounding('dacd6d48-2183-4d62-91bd-bcc83a98098a', vedic_texts_metaphorical_not_prescriptive, empirically_contingent).
narrative_ontology:cs_reference_frame('dacd6d48-2183-4d62-91bd-bcc83a98098a', spiritual_egalitarian_praxis).
narrative_ontology:cs_drift_state('dacd6d48-2183-4d62-91bd-bcc83a98098a', contemporary_politicized_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('dacd6d48-2183-4d62-91bd-bcc83a98098a', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_communities).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__reformist_spiritual_reading, spiritual_egalitarianism).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__reformist_spiritual_reading, textual_metaphoricity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret Vedic texts as describing spiritual unity and metaphorical cosmology, rejecting caste prescriptions as later accretions. Gather in worship and study circles that do not recognize varna distinctions. Benefit from a shared framework that enables egalitarian spiritual coordination without hereditary gatekeepers. Exit is socially constrained by orthodox pressure but not identity-locked.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_communities, beneficiary,
    moderate, biographical, constrained, national).

% Maintain that Vedic texts literally prescribe varna hierarchy as cosmic order. They are structurally excluded from the reformist reading's legitimacy framework, which denies the textual basis of their authority. They resist the reformist reading because it erodes their institutional role as hereditary interpreters.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, orthodox_varna_proponents, excluded,
    institutional, generational, trapped, national).

% Analyze the contest between readings through critical hermeneutics and subaltern studies. They observe how the reformist reading dismantles colonial and orthodox authority claims, while noting its own modernist entanglements.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, postcolonial_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates spiritual practice and communal worship across caste boundaries by interpreting Vedic authority as sanctioning egalitarian metaphysical unity rather than social hierarchy.
% TRANSFER_FUNCTION: Moves interpretive authority from hereditary priestly gatekeepers to individual seekers and egalitarian communities; transfers social legitimacy from caste identity to shared spiritual practice.
% ABSENT_VOICES: Orthodox institutional authorities who hold the literal varna reading are excluded from the reformist framework's legitimacy structure; colonial administrators seeking to codify Hindu law are also absent, as the reading denies prescriptive content usable for governance.
% DISAPPEARANCE_RATIONALE: If the reformist reading vanished, egalitarian spiritual communities would lose their primary textual grounding for rejecting caste hierarchy, and many practitioners would be pushed back toward orthodox or secular frameworks â the arrangement of authority and community identity would shift.
% FOUNDING_PROBLEM: Hereditary caste hierarchy justified by sacred textual interpretation created rigid social stratification and excluded large populations from spiritual and social participation.
% FOUNDING_PROBLEM_CORROBORATION: Subaltern studies scholars and anti-caste intellectuals attest the problem from outside the reformist beneficiary communities; orthodox institutions dispute that the problem exists, claiming varna order is legitimate cosmic structure.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__reformist_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__reformist_spiritual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__reformist_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.15, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low (0.15) because the reading moves no material rents; its cost is interpretive labor, which participants bear voluntarily. Suppression is very low (0.12) because the reading is adopted, not enforced. Theater ratio is low (0.10) because the coordination is functional rather than performative. Accessibility collapse is moderate (0.40): once the metaphorical reading is accepted, literalist alternatives lose salience but remain cognitively available. Resistance is moderate (0.45) because orthodox institutions actively contest the reading's legitimacy. The measurement series share one time grid and show slight, stable drift without any enforcement-capacity narrative.
 *
 * PERSPECTIVAL GAP:
 *   The reformist beneficiary seat computes as rope: low extraction, genuine coordination. The orthodox excluded seat experiences the same constraint as an authority-eroding threat, but because the constraint does not extract from them structurally (it denies their claims rather than taxing them), their seat does not compute as victim. The divergence between seats is asymmetric: one gains coordination, the other loses legitimacy, but the engine detects no extraction because no victim set is declared and none exists.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist communities are the structural beneficiaries: they gain coordination (shared spiritual practice, caste-transcendent community) and incur no extractive transfer. Their directionality is near the beneficiary end. Orthodox varna proponents are not victims of extraction by this constraint â the reading does not take from them â but they experience authority erosion, which places their directional interest in opposition. Postcolonial scholars occupy an analytical seat with no stake in the transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â caste hierarchy justified by scripture â is contested. If it were universally acknowledged as solved, the reading might risk piton status (performative maintenance of a solved problem). However, because orthodox institutions still enforce caste hierarchy and the reformist reading remains necessary for egalitarian coordination, the problem is live enough to sustain the rope classification. The low theater ratio confirms that the coordination is not primarily theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_prescription_ambiguity,
    'Do the Vedic texts contain no prescriptive social content whatsoever, or do they contain such content that is later reinterpreted?',
    'Historical-philological analysis of the stratification of Vedic layers (Samhitas vs Brahmanas vs Dharmashastras) to determine whether prescriptive social content is present in the earliest textual strata or only in later accretions.',
    'If prescriptive content is found in the earliest strata, the reformist reading''s empirical foundation weakens and it shifts toward conventional/normative rather than descriptive hermeneutics; if absent, the reading gains textual support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_prescription_ambiguity, empirical, 'Whether the Vedic corpus genuinely lacks prescriptive social content.').

omega_variable(
    reformist_institutionalization_drift,
    'Has the reformist spiritual reading remained a decentralized interpretive stance, or has it institutionalized into a new hierarchy with its own extraction potential?',
    'Sociological mapping of authority and resource flows within major reformist institutions to detect hierarchical crystallization.',
    'If institutionalized with asymmetric resource capture, the constraint may shift from rope to tangled rope or piton; if it remains decentralized and low-cost, it stays rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformist_institutionalization_drift, empirical, 'Institutionalization and potential hierarchical drift of reformist communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__reformist_spiritual_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedic_reformist_tr_t0, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(vedic_reformist_tr_t20, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(vedic_reformist_tr_t40, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(vedic_reformist_tr_t60, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 60, 0.11).
narrative_ontology:measurement(vedic_reformist_tr_t80, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(vedic_reformist_tr_t100, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(vedic_reformist_be_t0, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(vedic_reformist_be_t20, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 20, 0.13).
narrative_ontology:measurement(vedic_reformist_be_t40, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 40, 0.14).
narrative_ontology:measurement(vedic_reformist_be_t60, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 60, 0.14).
narrative_ontology:measurement(vedic_reformist_be_t80, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 80, 0.15).
narrative_ontology:measurement(vedic_reformist_be_t100, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 100, 0.15).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(vedic_corpus_social_prescription__reformist_spiritual_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__reformist_spiritual_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, orthodox_varna_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is the reformist_spiritual_reading of the vedic_corpus_social_prescription kernel. Its sibling readings instantiate structurally distinct constraints from the same textual kernel due to divergent hermeneutic premises: the orthodox_varna_reading treats the texts as literally prescriptive, while the colonial_orientalist_reading treats them as a codifiable legal system. The reformist reading's low epsilon and absence of a victim set reflect its functioning as coordination rather than extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
