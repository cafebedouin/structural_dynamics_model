% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__antisubordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__antisubordination_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: equal_protection_kernel__antisubordination_reading
 *   human_readable: Equal Protection Antisubordination Reading
 *   domain: constitutional/law/civil_rights
 *
 * SUMMARY:
 *   This constraint instantiates the antisubordination reading of the Equal
 *   Protection Clause: the constitutional prohibition targets caste-like
 *   subordination of historically oppressed groups rather than racial
 *   classification per se. State action that entrenches hierarchy is
 *   forbidden; state action that dismantles it through race-conscious
 *   measures is permitted. Dominant groups cannot successfully invoke equal
 *   protection to block such remedial measures. The reading is one of three
 *   structurally distinct interpretations of the same constitutional kernel
 *   (the 14th Amendment text), differing from the colorblind reading (which
 *   forbids all racial classifications) and the remedial reading (which
 *   permits narrowly tailored race-conscious remedies only for documented
 *   discrimination or diversity).
 *
 * KEY AGENTS:
 *   - federal_judiciary: Agenda-setter (institutional/constrained) â interprets and enforces the constitutional framework, determining which race-conscious state actions survive review.
 *   - subordinated_castes: Beneficiary (powerless/identity_locked) â receive constitutional shelter from caste entrenchment and authorization for remedial race-conscious measures.
 *   - dominant_groups: Payer (powerful/constrained) â lose equal protection capacity to challenge hierarchy-dismantling state action; bear the doctrinal cost of asymmetric constitutional closure.
 *   - colorblind_claimants: Excluded (organized/constrained) â argue for categorical colorblindness but are structurally denied constitutional protection under this reading.
 *   - antisubordination_scholars: Observer (analytical/analytical) â develop the theoretical framework and document the gap between the reading's normative claims and its judicial adoption.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__antisubordination_reading, 0.62).
domain_priors:suppression_score(equal_protection_kernel__antisubordination_reading, 0.68).
domain_priors:theater_ratio(equal_protection_kernel__antisubordination_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__antisubordination_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__antisubordination_reading, "Equal Protection Antisubordination Reading").
narrative_ontology:topic_domain(equal_protection_kernel__antisubordination_reading, "constitutional/law/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__antisubordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__antisubordination_reading, 'f98ce11f-5421-423a-ad22-c7970c9ba8d3').
narrative_ontology:cs_kernel_codification('f98ce11f-5421-423a-ad22-c7970c9ba8d3', fixed_text).
narrative_ontology:cs_authority_grounding('f98ce11f-5421-423a-ad22-c7970c9ba8d3', lineage).
narrative_ontology:cs_interpretation_layer_present('f98ce11f-5421-423a-ad22-c7970c9ba8d3').
narrative_ontology:cs_reading_relation('f98ce11f-5421-423a-ad22-c7970c9ba8d3', equal_protection_kernel__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('f98ce11f-5421-423a-ad22-c7970c9ba8d3', equal_protection_kernel__remedial_reading, influences).
narrative_ontology:cs_axiom('f98ce11f-5421-423a-ad22-c7970c9ba8d3', foundational, ep_clause_targets_subordination_not_classification).
narrative_ontology:cs_axiom_status(ep_clause_targets_subordination_not_classification, holdable).
narrative_ontology:cs_axiom_grounding('f98ce11f-5421-423a-ad22-c7970c9ba8d3', ep_clause_targets_subordination_not_classification, empirically_contingent).
narrative_ontology:cs_axiom('f98ce11f-5421-423a-ad22-c7970c9ba8d3', foundational, dominant_groups_cannot_veto_remedial_measures).
narrative_ontology:cs_axiom_status(dominant_groups_cannot_veto_remedial_measures, holdable).
narrative_ontology:cs_axiom_grounding('f98ce11f-5421-423a-ad22-c7970c9ba8d3', dominant_groups_cannot_veto_remedial_measures, deontological).
narrative_ontology:cs_reference_frame('f98ce11f-5421-423a-ad22-c7970c9ba8d3', antisubordination_constitutional_order).
narrative_ontology:cs_drift_state('f98ce11f-5421-423a-ad22-c7970c9ba8d3', contemporary_colorblind_resurgence, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f98ce11f-5421-423a-ad22-c7970c9ba8d3', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__antisubordination_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, subordinated_castes).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, dominant_groups).
narrative_ontology:constraint_vindicates(equal_protection_kernel__antisubordination_reading, reconstruction_egalitarianism).
narrative_ontology:constraint_vindicates(equal_protection_kernel__antisubordination_reading, anti_caste_constitutionalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Equal Protection Clause to permit or require race-conscious dismantling of caste-like hierarchy and to reject colorblind constitutional challenges to such measures. Bound by precedent, text, and the political economy of judicial legitimacy.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Receive constitutional authorization for race-conscious remedial measures and are shielded from state-enforced caste hierarchy. Their historical social position is the doctrinal center of the framework; exit from the identity category is structurally unavailable.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, subordinated_castes, beneficiary,
    powerless, generational, identity_locked, national).

% Lose the ability to successfully challenge race-conscious remedial state action under the Equal Protection Clause. Their equal protection claims against hierarchy-dismantling measures are structurally denied, though they retain other political and legal channels.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, dominant_groups, payer,
    powerful, biographical, constrained, national).

% Argue that all racial classifications are unconstitutional. Under the antisubordination reading, their framework is excluded from constitutional legitimacy and their claims are dismissed as incompatible with the Clause's anti-caste purpose.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, colorblind_claimants, excluded,
    organized, biographical, constrained, national).

% Develop and defend the theoretical framework; observe and document the gap between the reading's normative claims and its limited judicial adoption.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, antisubordination_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_kernel__antisubordination_reading, subordinated_castes).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents caste-like subordination of historically oppressed groups by authorizing state action that dismantles hierarchical status systems, thereby maintaining democratic legitimacy and preventing permanent underclass formation.
% TRANSFER_FUNCTION: Transfers constitutional protection and remedial legal capacity from dominant groups to subordinated castes; moves authority from colorblind constitutional challengers to the state and courts implementing hierarchy-dismantling measures.
% ABSENT_VOICES: Colorblind constitutionalists and dominant-group litigants are structurally excluded from protection under this reading; they would argue for categorical racial neutrality but are kept out by the doctrinal definition of the constitutional injury.
% DISAPPEARANCE_RATIONALE: If the antisubordination reading vanished overnight, equal protection doctrine would revert toward colorblind or remedial frameworks; race-conscious remedial state action would face heightened constitutional vulnerability, and the legal authorization for broad hierarchy-dismantling measures would collapse.
% FOUNDING_PROBLEM: The post-Reconstruction collapse of civil rights protections and the entrenchment of Jim Crow caste systems that formal equality doctrine failed to dismantle.
% FOUNDING_PROBLEM_CORROBORATION: Reconstruction historians and critical race scholars outside the immediate beneficiary groups attest to the persistence of caste-like subordination and the anti-caste intent of the 14th Amendment; mainstream constitutional historians corroborate the Reconstruction framing, though they dispute its contemporary doctrinal application. Colorblind advocates dispute the founding problem's current vitality.
narrative_ontology:disappearance_verdict(equal_protection_kernel__antisubordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__antisubordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__antisubordination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_kernel__antisubordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__antisubordination_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__antisubordination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_kernel__antisubordination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_kernel__antisubordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the reading explicitly withholds a constitutional protection from dominant groups when they challenge remedial measures, producing asymmetric doctrinal extraction. Suppression (0.68) is high because the reading must actively suppress colorblind constitutional challenges and hierarchical state action to persist. Theater ratio (0.20) is low but rising: the core doctrine is substantive, though political performative allyship increasingly substitutes for structural remedy. Accessibility collapse (0.70) reflects that once the antisubordination frame governs, colorblind alternatives become legally non-viable. Resistance (0.80) is strong from originalists, colorblind advocates, and dominant-group litigants. The temporal grid is shared across all tracked metrics to prevent misaligned drift dating.
 *
 * PERSPECTIVAL GAP:
 *   The subordinated_castes seat should compute the constraint as protective coordination (a rope or scaffold), while the dominant_groups seat should compute it as extraction (a snare or tangled rope). The federal_judiciary seat may compute it as either coordination or inertial doctrine depending on the Court's composition. The engine derives this divergence from the same structural data: beneficiary declarations plus identity_locked exit for subordinated castes versus constrained exit for dominant groups.
 *
 * DIRECTIONALITY LOGIC:
 *   Subordinated_castes are structural beneficiaries (d near 0.0): the constraint subsidizes their legal capacity to secure remedial measures and shields them from state-enforced hierarchy. Dominant_groups are structural targets (d near 1.0): the constraint extracts from them by foreclosing a constitutional claim they would otherwise hold. Colorblind_claimants are excluded rather than coordinated â their exclusion is the enforcement object. The federal_judiciary sits near symmetric (d ~0.5) as the administering authority, though with slight beneficiary tilt because the reading expands state remedial power.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading prevents mislabeling by requiring both genuine coordination (the anti-caste function is real and historically grounded) and asymmetric extraction (dominant groups are doctrinally excluded). Without the victim declaration, the constraint would read as a pure coordination mechanism (rope). Without the beneficiary declaration, it would read as pure suppression of colorblindness (snare). The tangled_rope classification captures that the same constitutional structure coordinates protection for one group while extracting constitutional capacity from another.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ep_historical_grounding,
    'Is the antisubordination reading grounded in empirically recoverable Reconstruction-era intent, or is it a normative reconstruction of the constitutional text?',
    'Archival historical research and linguistic analysis of the 14th Amendment''s drafting and ratification debates; comparison with the 1875 Civil Rights Act legislative history.',
    'If historically grounded, the reading claims conventional legal authority; if normative, its authority depends on moral/political theory and the classification may shift toward preference-based.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ep_historical_grounding, empirical, 'Whether the antisubordination principle derives from historical constitutional meaning').

omega_variable(
    subordination_measurement,
    'How is ''caste-like subordination'' operationalized such that courts can distinguish it from mere socioeconomic inequality or cultural disadvantage?',
    'Sociological and empirical research establishing measurable indicators of caste-like hierarchy in contemporary American institutions.',
    'Without operationalizable criteria, the reading risks arbitrary application or capture by institutional interpreters; with clear criteria, extraction is bounded and targeted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subordination_measurement, conceptual, 'Whether subordination can be measured to constrain judicial discretion').

omega_variable(
    dominant_group_cost_asymmetry,
    'Does the constraint''s exclusion of dominant groups from equal protection claims against remedial measures constitute asymmetric extraction, or is it a neutral boundary condition of a coordination mechanism?',
    'Comparative analysis of how constitutional rights are allocated across different equality regimes; examination of whether dominant groups retain adequate alternative political and legal channels.',
    'If the exclusion is a neutral boundary, the coordination function dominates; if it extracts disproportionate political capacity from one group, the tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dominant_group_cost_asymmetry, conceptual, 'Whether the exclusion of dominant groups from EP claims is extractive or structural').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__antisubordination_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ep_antisubordination_tr_t0, equal_protection_kernel__antisubordination_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ep_antisubordination_tr_t10, equal_protection_kernel__antisubordination_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(ep_antisubordination_tr_t20, equal_protection_kernel__antisubordination_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(ep_antisubordination_tr_t30, equal_protection_kernel__antisubordination_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(ep_antisubordination_tr_t40, equal_protection_kernel__antisubordination_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(ep_antisubordination_tr_t50, equal_protection_kernel__antisubordination_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(ep_antisubordination_be_t0, equal_protection_kernel__antisubordination_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ep_antisubordination_be_t10, equal_protection_kernel__antisubordination_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(ep_antisubordination_be_t20, equal_protection_kernel__antisubordination_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(ep_antisubordination_be_t30, equal_protection_kernel__antisubordination_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(ep_antisubordination_be_t40, equal_protection_kernel__antisubordination_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(ep_antisubordination_be_t50, equal_protection_kernel__antisubordination_reading, base_extractiveness, 50, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(ep_antisubordination_su_t0, equal_protection_kernel__antisubordination_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(ep_antisubordination_su_t10, equal_protection_kernel__antisubordination_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(ep_antisubordination_su_t20, equal_protection_kernel__antisubordination_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(ep_antisubordination_su_t30, equal_protection_kernel__antisubordination_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(ep_antisubordination_su_t40, equal_protection_kernel__antisubordination_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement(ep_antisubordination_su_t50, equal_protection_kernel__antisubordination_reading, suppression_requirement, 50, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__antisubordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, equal_protection_kernel__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, equal_protection_kernel__remedial_reading).

% DUAL FORMULATION NOTE:
% The equal_protection_kernel decomposes into three structurally distinct constraints from the same constitutional text. The colorblind_reading has high extraction from subordinated castes (by forbidding all race-conscious remedy), the remedial_reading has moderate extraction bounded by narrow tailoring, and the antisubordination_reading has moderate-high extraction from dominant groups (by permitting broad remedial race-consciousness). They form a constraint family linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
