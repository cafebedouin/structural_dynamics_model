% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__autonomy_primacy_reading, []).

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
 *   constraint_id: one_country_two_systems_framework__autonomy_primacy_reading
 *   human_readable: One Country Two Systems â Autonomy Primacy Reading
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The autonomy primacy reading of the One Country, Two Systems framework
 *   treats Hong Kong's Basic Law and the Sino-British Joint Declaration as
 *   creating legally enforceable autonomy guarantees that constitutionally
 *   check mainland interference. Under this reading, the constraint
 *   coordinates Hong Kong's distinct legal and economic system by protecting
 *   civil liberties and judicial independence, while asymmetrically
 *   extracting from PRC sovereign authority by limiting Beijing's power to
 *   legislate for or override local institutions. The constraint is actively
 *   enforced through judicial review and international legal monitoring. This
 *   story isolates the autonomy-primacy reading as a distinct constraint from
 *   its sibling sovereignty-primacy and balanced-coexistence readings.
 *
 * KEY AGENTS:
 *   - hk_residents: Primary beneficiary (organized/constrained) â receive civil-liberties protection
 *   - hk_judiciary: Agenda-setter and beneficiary (institutional/identity_locked) â enforces autonomy through constitutional review
 *   - prc_central_government: Primary target (institutional/constrained) â bears sovereignty limitation
 *   - hk_chief_executive: Secondary target (powerful/constrained) â executive power checked by judiciary
 *   - pro_autonomy_legislators: Secondary beneficiary (moderate/constrained) â political space protected by framework
 *   - international_legal_community: Analytical observer (institutional/analytical) â external treaty monitoring
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__autonomy_primacy_reading, 0.65).
domain_priors:suppression_score(one_country_two_systems_framework__autonomy_primacy_reading, 0.6).
domain_priors:theater_ratio(one_country_two_systems_framework__autonomy_primacy_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__autonomy_primacy_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__autonomy_primacy_reading, "One Country Two Systems â Autonomy Primacy Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__autonomy_primacy_reading, "constitutional/political").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__autonomy_primacy_reading, '044baaac-f436-4230-a4f3-f4ba54588268').
narrative_ontology:cs_kernel_codification('044baaac-f436-4230-a4f3-f4ba54588268', formalized).
narrative_ontology:cs_authority_grounding('044baaac-f436-4230-a4f3-f4ba54588268', lineage).
narrative_ontology:cs_interpretation_layer_present('044baaac-f436-4230-a4f3-f4ba54588268').
narrative_ontology:cs_reading_relation('044baaac-f436-4230-a4f3-f4ba54588268', one_country_two_systems_framework__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('044baaac-f436-4230-a4f3-f4ba54588268', one_country_two_systems_framework__balanced_coexistence_reading, coexists_with).
narrative_ontology:cs_axiom('044baaac-f436-4230-a4f3-f4ba54588268', foundational, judicial_review_constrains_sovereignty).
narrative_ontology:cs_axiom_status(judicial_review_constrains_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('044baaac-f436-4230-a4f3-f4ba54588268', judicial_review_constrains_sovereignty, conventional).
narrative_ontology:cs_axiom('044baaac-f436-4230-a4f3-f4ba54588268', foundational, treaty_autonomy_internationally_enforceable).
narrative_ontology:cs_axiom_status(treaty_autonomy_internationally_enforceable, holdable).
narrative_ontology:cs_axiom_grounding('044baaac-f436-4230-a4f3-f4ba54588268', treaty_autonomy_internationally_enforceable, conventional).
narrative_ontology:cs_reference_frame('044baaac-f436-4230-a4f3-f4ba54588268', treaty_guaranteed_autonomy).
narrative_ontology:cs_drift_state('044baaac-f436-4230-a4f3-f4ba54588268', contemporary_post_national_security_law, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('044baaac-f436-4230-a4f3-f4ba54588268', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hk_residents).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hk_judiciary).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, pro_autonomy_legislators).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, prc_central_government).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, hk_chief_executive).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live under a constitutional framework that promises civil liberties and judicial independence distinct from mainland China. Their speech, assembly, and due-process rights are formally protected by the Basic Law and international treaty. Exit means emigration or accepting mainland legal standards.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hk_residents, beneficiary,
    organized, biographical, constrained, regional).

% Exercises constitutional review over Hong Kong executive and legislative acts under the Basic Law, serving as the primary enforcement mechanism for the autonomy guarantee. Their professional identity as common-law judges binds them to the independence of this constraint.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hk_judiciary, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__autonomy_primacy_reading, hk_judiciary, beneficiary).

% Holds formal sovereignty over Hong Kong but is constrained by treaty obligations and judicial review from exercising direct control over local legislation and civil liberties. Bears the cost of limited sovereignty: cannot directly legislate for HK or override local courts without triggering international breach.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, prc_central_government, payer,
    institutional, civilizational, constrained, national).

% Exercises local executive authority but is subject to judicial review and Basic Law constraints that limit how far they can align with mainland policy preferences. Their policy autonomy is checked by the courts.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hk_chief_executive, payer,
    powerful, biographical, constrained, regional).

% Use the autonomy framework to resist mainland-policy integration and advocate for democratic reform. They rely on the constitutional guarantee to protect their political space and legislative veto points.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, pro_autonomy_legislators, beneficiary,
    moderate, biographical, constrained, regional).

% Monitors PRC compliance with the Joint Declaration, issues legal opinions, and supports HK judicial independence through professional exchange and treaty-body reporting. They do not benefit directly but serve as external corroborators of the autonomy frame.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, international_legal_community, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects Hong Kong's distinct common-law legal system, civil liberties, and economic institutions from direct integration into the PRC's socialist legal and political system, enabling continued rule of law and judicial independence within Chinese sovereignty.
% TRANSFER_FUNCTION: Transfers sovereignty-limitation obligations from Hong Kong society to the PRC central government; transfers judicial oversight authority from the executive to an independent judiciary; transfers international monitoring legitimacy to treaty bodies.
% ABSENT_VOICES: Pro-integration elites and PRC constitutional hardliners who view autonomy as provisional delegation rather than treaty-guaranteed right are structurally marginalized in this reading's interpretive framework; their objections are ruled out by the premise of international enforceability.
% DISAPPEARANCE_RATIONALE: Without the autonomy guarantee, Hong Kong's legal and political order would reorganize around direct PRC sovereignty, civil liberties protections would collapse into mainland standards, the judiciary would lose independence, and the international legal monitoring framework would lose its object â the constitutional order would fundamentally rearrange.
% FOUNDING_PROBLEM: How to reunify Hong Kong with the People's Republic of China while preserving its distinct capitalist economy, common-law legal system, and lifestyle, and reassuring residents and international investors that communist party control would not be imposed.
% FOUNDING_PROBLEM_CORROBORATION: The Sino-British Joint Declaration and the Basic Law attest the founding problem and solution. International legal scholars, the Hong Kong Bar Association, and UN treaty bodies corroborate the live status of the autonomy guarantee from outside the beneficiary set. The PRC Ministry of Foreign Affairs asserts the arrangement is a dead letter superseded by national security imperatives.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__autonomy_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__autonomy_primacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(one_country_two_systems_framework__autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__autonomy_primacy_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__autonomy_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint scores moderate-high extractiveness (0.65) because it formally strips the PRC of full sovereignty over a subnational territory, imposing treaty-bound limits that are costly to override. Suppression is moderate (0.60) because the constraint's persistence depends on judicial enforcement and international diplomatic pressure rather than pure physical coercion. Theater ratio is moderate (0.50) because while the judicial institutions remain partially functional, a growing share of autonomy discourse performs compliance that is increasingly hollowed out by mainland encroachment. Accessibility collapse is high (0.70) because the alternatives â full independence or full integration â are politically foreclosed by the same treaty structure. Resistance is high (0.75) from the PRC and pro-integration elites who contest the legal supremacy of autonomy. The metrics and the claimed type are authored independently: the claimed type is tangled_rope because the constraint has both a genuine coordination function (civil-liberties protection) and asymmetric extraction (sovereignty limitation), but the metrics are descriptively authored to reflect actual operation rather than idealized treaty text. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The HK resident and judiciary seats experience the constraint as protective constitutional architecture (low effective extraction, possibly computing toward rope from their seat), while the PRC central government seat experiences it as an external straitjacket on sovereign power (high effective extraction, computing toward snare). The engine derives this divergence from the same structural data: identical scope and power levels produce opposite effective-extraction values because directionality inverts between beneficiary and victim declarations. The HK Chief Executive sits nearer symmetric because they both gain local authority and lose policy discretion to judicial review.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (HK residents, judiciary, pro-autonomy legislators) receive low directionality because the constraint subsidizes their civil liberties and institutional independence. The PRC central government receives high directionality because the constraint extracts sovereignty from it. The HK Chief Executive sits near symmetric (0.5) because they both gain local authority and lose policy discretion to judicial review. International observers are analytical (no directionality feed).
 *
 * MANDATROPHY ANALYSIS:
 *   The R5 genealogy interview prevents mislabeling: the founding problem (reunification while preserving distinct systems) was real, but its status is contested. The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges flags that the arrangement persists beyond consensus about its original justification, which supports the tangled_rope classification over pure rope. If the problem were dead and the world would rearrange, that would signal piton; here the coordination function is partially live but contested, fitting tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    npcsc_interpretation_supremacy,
    'Does the NPCSC''s power to interpret the Basic Law override judicial review, rendering the autonomy constraint hollow?',
    'Comparative analysis of NPCSC interpretation practice post-1997 and HK courts'' reception of those interpretations in constitutional review cases.',
    'If NPCSC interpretation is supreme, the judicial enforcement mechanism is bypassed and the constraint''s effective extractiveness collapses toward a piton or snare on HK residents rather than extraction from mainland sovereignty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(npcsc_interpretation_supremacy, conceptual, 'Whether mainland constitutional interpretation supersedes local judicial review.').

omega_variable(
    international_enforceability_gap,
    'Is the Joint Declaration actually enforceable by international tribunals against the PRC, or only diplomatically protestable?',
    'ICJ jurisdiction analysis, state practice on bilateral declaratory treaties, and examination of actual remedial mechanisms available to signatories.',
    'If not enforceable, the ''internationally enforceable'' claim is substantially theatrical, raising theater_ratio and shifting the constraint type toward piton or snare as the autonomy guarantee becomes performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_enforceability_gap, empirical, 'Whether the treaty basis creates legally enforceable obligations or merely diplomatic leverage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__autonomy_primacy_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t0, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(one__tr_t7, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 7, 0.28).
narrative_ontology:measurement(one__tr_t14, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 14, 0.38).
narrative_ontology:measurement(one__tr_t21, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 21, 0.48).
narrative_ontology:measurement(one__tr_t27, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 27, 0.5).

% Extraction over time
narrative_ontology:measurement(one__be_t0, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(one__be_t7, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 7, 0.53).
narrative_ontology:measurement(one__be_t14, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 14, 0.58).
narrative_ontology:measurement(one__be_t21, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 21, 0.63).
narrative_ontology:measurement(one__be_t27, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 27, 0.65).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(one_country_two_systems_framework__autonomy_primacy_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__autonomy_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework__balanced_coexistence_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework__sovereignty_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the one_country_two_systems_framework kernel. It is linked to its siblings as a constraint family; each reading has a distinct epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
