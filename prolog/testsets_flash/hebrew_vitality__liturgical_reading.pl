% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__liturgical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__liturgical_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: hebrew_vitality__liturgical_reading
 *   human_readable: Hebrew Vitality: Liturgical Reading
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   This constraint represents the 'liturgical reading' of Hebrew vitality,
 *   where unbroken ritual preservation and liturgical use are considered
 *   sufficient for the language to be 'living.' It is presented as a Mountain
 *   because, within this specific interpretive framework, the continuity of
 *   sacred use is seen as an inherent, unchangeable property of Hebrew's
 *   vitality, not a human-constructed choice that extracts from anyone. The
 *   beneficiaries are those whose authority and identity are reinforced by
 *   this definition. This is one reading of the 'hebrew_vitality' kernel,
 *   with sibling readings 'native_daily_reading' and
 *   'hybrid_continuity_reading' offering alternative definitions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__liturgical_reading, 0.05).
domain_priors:suppression_score(hebrew_vitality__liturgical_reading, 0.1).
domain_priors:theater_ratio(hebrew_vitality__liturgical_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__liturgical_reading, mountain).
narrative_ontology:human_readable(hebrew_vitality__liturgical_reading, "Hebrew Vitality: Liturgical Reading").
narrative_ontology:topic_domain(hebrew_vitality__liturgical_reading, "sociolinguistics/language_revitalization/jewish_studies").

domain_priors:emerges_naturally(hebrew_vitality__liturgical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__liturgical_reading, '19e365a3-04a4-4752-82e8-4ed083e2f72e').
narrative_ontology:cs_kernel_codification('19e365a3-04a4-4752-82e8-4ed083e2f72e', formalized).
narrative_ontology:cs_authority_grounding('19e365a3-04a4-4752-82e8-4ed083e2f72e', lineage).
narrative_ontology:cs_interpretation_layer_present('19e365a3-04a4-4752-82e8-4ed083e2f72e').
narrative_ontology:cs_reading_relation('19e365a3-04a4-4752-82e8-4ed083e2f72e', hebrew_vitality__native_daily_reading, coexists_with).
narrative_ontology:cs_reading_relation('19e365a3-04a4-4752-82e8-4ed083e2f72e', hebrew_vitality__hybrid_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('19e365a3-04a4-4752-82e8-4ed083e2f72e', foundational, liturgical_continuity_is_vitality).
narrative_ontology:cs_axiom_status(liturgical_continuity_is_vitality, holdable).
narrative_ontology:cs_axiom_grounding('19e365a3-04a4-4752-82e8-4ed083e2f72e', liturgical_continuity_is_vitality, deontological).
narrative_ontology:cs_reference_frame('19e365a3-04a4-4752-82e8-4ed083e2f72e', unbroken_sacred_tradition).
narrative_ontology:cs_drift_state('19e365a3-04a4-4752-82e8-4ed083e2f72e', contemporary_secular_linguistics, gap(stable, minor, false)).
narrative_ontology:cs_created_at('19e365a3-04a4-4752-82e8-4ed083e2f72e', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__liturgical_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, traditional_jewish_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreters and custodians of Jewish law and tradition, for whom the unbroken liturgical use of Hebrew is a foundational element of religious continuity and authority. They benefit from the stability and perceived naturalness of this definition of vitality.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, rabbinic_authorities, beneficiary,
    institutional, generational, identity_locked, global).

% Communities that define Hebrew's vitality primarily through its sacred and ritual function. For them, the continuous use in prayer and study is the essence of its living status, reinforcing their cultural and religious identity.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, traditional_jewish_communities, beneficiary,
    organized, generational, identity_locked, global).

% Advocates for Hebrew as a spoken, everyday language, who would argue that liturgical use alone is insufficient for true vitality. Their perspective is often marginalized in discussions centered on traditional religious definitions.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, secular_hebrew_revivalists, excluded,
    moderate, biographical, constrained, national).

% Academics who study language death and revitalization, offering empirical definitions of vitality that may or may not align with the liturgical reading. They analyze the structural properties of language use without necessarily endorsing a particular normative claim.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, linguistic_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the understanding of Hebrew's status as a living language within traditional Jewish religious and cultural frameworks, ensuring continuity of sacred practice and identity across generations.
% TRANSFER_FUNCTION: Transfers the authority for defining Hebrew's vitality to religious institutions and practices, rather than secular or vernacular usage, from linguistic criteria to ritual continuity.
% ABSENT_VOICES: Secular Hebrew revivalists and many modern linguistic scholars are absent from the core definition of vitality in this reading; they would argue for criteria based on native speakers, daily use, and functional domains beyond the liturgical.
% DISAPPEARANCE_RATIONALE: If the belief that liturgical use constitutes Hebrew's vitality vanished, it would fundamentally alter the self-understanding of traditional Jewish communities and rabbinic authority regarding their language. The entire framework of Hebrew's 'living' status within these contexts would need to be re-evaluated, leading to significant cultural and religious reorganization.
% FOUNDING_PROBLEM: The problem of maintaining Hebrew's sacred status and continuity as a 'living' language despite its decline as a vernacular, particularly during periods of diaspora and persecution.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities and traditional communities attest that the problem of maintaining sacred language continuity is still live. While secular scholars might contest the definition of 'vitality,' they generally acknowledge the historical challenge of language preservation in diaspora, corroborating the problem's existence, if not its proposed solution.
narrative_ontology:disappearance_verdict(hebrew_vitality__liturgical_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__liturgical_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__liturgical_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_vitality__liturgical_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__liturgical_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, ExtMetricName, E),
    domain_priors:suppression_score(hebrew_vitality__liturgical_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hebrew_vitality__liturgical_reading),
    narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hebrew_vitality__liturgical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very low (0.05) because this reading primarily defines a state of being (vitality through ritual) rather than imposing costs or extracting resources. Suppression is low (0.1) as it's a definitional framework, not an actively enforced coercive mechanism against alternative views, though it does implicitly marginalize them. Theater ratio is low (0.05) as the liturgical use is genuine and central to the claim. Accessibility collapse is high (0.9) because, within this framework, alternatives to liturgical continuity for defining vitality are largely 'collapsed' or deemed irrelevant. Resistance is low (0.02) because, within the communities holding this view, it is largely uncontested.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic authorities, this is a self-evident truth (Mountain). From the perspective of secular linguists, it's a conceptual framing that may not align with empirical definitions of language vitality, but it doesn't directly extract from them. The divergence is in the definition of 'vitality' itself, not in the experience of extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities and traditional Jewish communities are beneficiaries (d near 0.0) because this reading of Hebrew's vitality reinforces their religious and cultural identity and the legitimacy of their practices. There are no direct 'victims' in this reading, as it's a definitional framework rather than an extractive mechanism. Secular revivalists are 'excluded' from this definition's scope, but not directly harmed by its operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_vitality,
    'Is ''vitality'' in the context of a language best defined by continuous liturgical use, or by native speakers and daily vernacular use?',
    'Adoption of a universally accepted, empirically grounded definition of language vitality by all relevant stakeholders, or a consensus on domain-specific definitions.',
    'If an empirical, vernacular-focused definition of vitality were adopted, this constraint would cease to be a Mountain and would be reclassified as a conceptual Snare or Tangled Rope, as it would then be seen as actively suppressing alternative definitions to maintain a specific institutional authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_vitality, conceptual, 'Ambiguity in the definition of ''language vitality'' itself.').

omega_variable(
    natural_vs_constructed_vitality,
    'Is the vitality of Hebrew through liturgical use an inherent, ''natural'' property of the language''s sacred status, or a constructed definition maintained by specific religious institutions?',
    'Historical and sociological analysis of how this definition of vitality was established and maintained, particularly in response to challenges from secularization and vernacular revival movements.',
    'If found to be a constructed definition, the ''emerges_naturally'' flag would be false, and the constraint would likely reclassify from Mountain to a form of Rope or Tangled Rope, reflecting its institutional maintenance and the benefits it confers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_vitality, empirical, 'Whether the liturgical reading of Hebrew vitality is natural or constructed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__liturgical_reading, 1000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1000, hebrew_vitality__liturgical_reading, theater_ratio, 1000, 0.05).
narrative_ontology:measurement(hebr_tr_t1500, hebrew_vitality__liturgical_reading, theater_ratio, 1500, 0.05).
narrative_ontology:measurement(hebr_tr_t2024, hebrew_vitality__liturgical_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1000, hebrew_vitality__liturgical_reading, base_extractiveness, 1000, 0.05).
narrative_ontology:measurement(hebr_be_t1500, hebrew_vitality__liturgical_reading, base_extractiveness, 1500, 0.05).
narrative_ontology:measurement(hebr_be_t2024, hebrew_vitality__liturgical_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1000, hebrew_vitality__liturgical_reading, suppression_requirement, 1000, 0.1).
narrative_ontology:measurement(hebr_su_t1500, hebrew_vitality__liturgical_reading, suppression_requirement, 1500, 0.1).
narrative_ontology:measurement(hebr_su_t2024, hebrew_vitality__liturgical_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
