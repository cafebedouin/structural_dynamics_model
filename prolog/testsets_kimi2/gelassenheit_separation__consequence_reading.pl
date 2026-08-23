% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__consequence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__consequence_reading, []).

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
 *   constraint_id: gelassenheit_separation__consequence_reading
 *   human_readable: Gelassenheit Separation: Consequence-Based Technology Ordnung
 *   domain: religious_studies/technology_governance/commitment_systems
 *
 * SUMMARY:
 *   The Amish Anabaptist doctrine of Gelassenheit (yieldedness) includes
 *   separation from 'the world', but what 'separation' means is contested.
 *   This constraint story captures the CONSEQUENCE READING: technology is
 *   evaluated not by its appearance nor by its systemic entanglements, but by
 *   its concrete effects on community practicesâvisiting, mutual aid, and
 *   geographic rootedness. Under this reading, a telephone is permitted in a
 *   barn (because it preserves the home as a sacred space while allowing
 *   necessary communication) but forbidden in the home (because it erodes
 *   visiting and fragments attention); a tractor may power a belt but not
 *   pull a plow with rubber tires (because the latter enables farming alone,
 *   eroding neighborly interdependence). The constraint is actively enforced
 *   by bishops and ministers through the Ordnung and church discipline,
 *   coordinates genuine communal preservation, but asymmetrically extracts
 *   from members who would prefer labor-saving or communication technology.
 *   Epsilon is low because the rules are fine-grained and minimize
 *   unnecessary burden.
 *
 * KEY AGENTS:
 *   - bishops_and_ministers: agenda_setter (organized/identity_locked/local) â interpret and enforce the Ordnung
 *   - baptized_community_members: primary beneficiary (moderate/identity_locked/local) â receive preserved communal bonds
 *   - efficiency_seeking_members: primary payer (powerless/constrained/local) â bear costs of forgone technology
 *   - external_advocates: excluded observer (organized/analytical/national) â outside the deliberation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__consequence_reading, 0.22).
domain_priors:suppression_score(gelassenheit_separation__consequence_reading, 0.45).
domain_priors:theater_ratio(gelassenheit_separation__consequence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__consequence_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__consequence_reading, "Gelassenheit Separation: Consequence-Based Technology Ordnung").
narrative_ontology:topic_domain(gelassenheit_separation__consequence_reading, "religious_studies/technology_governance/commitment_systems").

domain_priors:requires_active_enforcement(gelassenheit_separation__consequence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__consequence_reading, '03ee1b18-6a66-4a45-a951-42c0752a0026').
narrative_ontology:cs_kernel_codification('03ee1b18-6a66-4a45-a951-42c0752a0026', distributed).
narrative_ontology:cs_authority_grounding('03ee1b18-6a66-4a45-a951-42c0752a0026', practice).
narrative_ontology:cs_interpretation_layer_present('03ee1b18-6a66-4a45-a951-42c0752a0026').
narrative_ontology:cs_reading_relation('03ee1b18-6a66-4a45-a951-42c0752a0026', gelassenheit_separation__artifact_reading, forecloses).
narrative_ontology:cs_reading_relation('03ee1b18-6a66-4a45-a951-42c0752a0026', gelassenheit_separation__principle_reading, coexists_with).
narrative_ontology:cs_axiom('03ee1b18-6a66-4a45-a951-42c0752a0026', foundational, technology_judged_by_communal_effect).
narrative_ontology:cs_axiom_status(technology_judged_by_communal_effect, holdable).
narrative_ontology:cs_axiom_grounding('03ee1b18-6a66-4a45-a951-42c0752a0026', technology_judged_by_communal_effect, theological).
narrative_ontology:cs_axiom('03ee1b18-6a66-4a45-a951-42c0752a0026', foundational, preservation_of_visiting_and_mutual_aid).
narrative_ontology:cs_axiom_status(preservation_of_visiting_and_mutual_aid, holdable).
narrative_ontology:cs_axiom_grounding('03ee1b18-6a66-4a45-a951-42c0752a0026', preservation_of_visiting_and_mutual_aid, conventional).
narrative_ontology:cs_reference_frame('03ee1b18-6a66-4a45-a951-42c0752a0026', communal_practice_as_separation).
narrative_ontology:cs_drift_state('03ee1b18-6a66-4a45-a951-42c0752a0026', smartphone_and_internet_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('03ee1b18-6a66-4a45-a951-42c0752a0026', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__consequence_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, baptized_community_members).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, efficiency_seeking_members).
narrative_ontology:constraint_vindicates(gelassenheit_separation__consequence_reading, communal_preservation_doctrine).
narrative_ontology:constraint_vindicates(gelassenheit_separation__consequence_reading, gelassenheit_as_yieldedness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce the Ordnung for their church district. Decide whether a technology preserves visiting and mutual aid or erodes it. Apply church discipline, including shunning, for violations. Their authority is constituted by the community's practice and tradition.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, bishops_and_ministers, agenda_setter,
    organized, generational, identity_locked, local).

% Participate in mutual aid, visiting, and shared labor. Accept technology restrictions as safeguarding communal bonds. Receive the benefits of cohesion, geographic rootedness, and interdependence. Their identity is fused with the church district.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, baptized_community_members, beneficiary,
    moderate, biographical, identity_locked, local).

% Desire labor-saving or communication technology for personal or business use. Forbidden from installing telephones in homes, using rubber-tired tractors for fieldwork, or owning computers. Bear the opportunity cost of reduced convenience and economic efficiency to remain in fellowship.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, efficiency_seeking_members, payer,
    powerless, biographical, constrained, local).

% Civil liberties and technology advocates outside the Amish community who would argue for individual autonomy in technology adoption. They are not part of the Ordnung deliberation and their objections are not heard within the church district boundary.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, external_advocates, excluded,
    organized, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the preservation of communal practicesâvisiting, mutual aid, and geographic rootednessâby aligning technology use with effects that strengthen rather than erode face-to-face interdependence.
% TRANSFER_FUNCTION: Moves the opportunity cost of forgone technology (restricted communication, labor inefficiency) from individual members to the communal account of preserved social cohesion.
% ABSENT_VOICES: English technology advocates, ex-Amish, and individual-rights frameworks that would argue for unrestricted personal technology access; they are outside the church district boundary and excluded from Ordnung deliberation.
% DISAPPEARANCE_RATIONALE: If the consequence-based Ordnung vanished, members would adopt home telephones, rubber-tired tractors, and personal computers; visiting patterns would fragment, mutual aid would shift toward monetized services, and geographic rootedness would erode as members commute farther for work.
% FOUNDING_PROBLEM: How to maintain a separatist religious community's distinctive social fabric and mutual interdependence when surrounded by a high-technology society whose tools silently restructure time, space, and obligation.
% FOUNDING_PROBLEM_CORROBORATION: Bishops and elders attest the problem is live, citing accelerated assimilation in less conservative affiliations. External ethnographers corroborate that districts lacking fine-grained consequence-based rules show faster erosion of communal practices, though they do not normatively endorse the Ordnung.
narrative_ontology:disappearance_verdict(gelassenheit_separation__consequence_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__consequence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__consequence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gelassenheit_separation__consequence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__consequence_reading, 0.22, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__consequence_reading_tests).
:- end_tests(gelassenheit_separation__consequence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22) because the constraint permits considerable technology (tractors, telephones in barns, gas engines) and only forbids what directly undermines visiting and mutual aid. Suppression is moderate (0.45): enforcement relies on social sanction and shunning rather than violence, but it is credible and actively applied. Theater ratio is low (0.15) because the rules are functionally integrated into daily practice and not primarily performative. Accessibility collapse is moderate (0.60): once a member accepts the Ordnung, the alternative of adopting restricted technology while remaining in fellowship collapses. Resistance is low (0.25): most members affirm the rules, though covert adoption of smartphones in some districts signals rising tension. The measurement series uses a shared time grid across the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the bishops' seat, the Ordnung is a protective rope preserving a way of life against assimilationist pressure. From the efficiency-seeking member's seat, the same rule is a tangible cost: slower communication, less efficient farming, and isolation from broader economic opportunity. The engine computes this divergence from the structural dataâsame constraint, different directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Baptized community members are named beneficiaries because the preserved visiting and mutual aid flows to them as communal goods; their directionality is toward the beneficiary end. Efficiency-seeking members are named payers because they bear the direct opportunity cost of technology restriction; their directionality is toward the target end. Bishops and ministers are agenda setters whose authority is constituted by the constraint, placing them near the beneficiary end though they do not capture material rents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâhow to preserve communal separatism in a high-technology surrounding societyâremains live, as corroborated by ethnographic comparison with less restrictive affiliations. The constraint is not a piton because it is still functionally central to Amish identity, and not a snare because the coordination function is genuine and the extraction is low. It is best classified as a low-epsilon tangled rope: a real coordination mechanism whose persistence requires active enforcement and imposes asymmetric costs on a subset of members.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_vs_voluntary_compliance,
    'Does the low extraction reflect genuine voluntary compliance, or is it maintained by the credible threat of shunning and social exclusion?',
    'Compare compliance rates across districts with varying enforcement intensity; if compliance holds without active enforcement, extraction is lower than structurally measured.',
    'If maintained by threat, effective extraction is higher due to the latent coercive premium; if voluntary, the constraint operates closer to a pure rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vs_voluntary_compliance, empirical, 'Whether low epsilon reflects consent or credible threat').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (shunning, social exclusion) or internalized (members believe home telephones are spiritually harmful)?',
    'Post-exit suppression trajectory: if members adopt restricted technology quickly after leaving, suppression was structural; if they avoid it, internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, and the member''s directionality moves toward full target.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    consequence_vs_principle_boundary,
    'Can the consequence reading consistently distinguish itself from the principle reading when evaluating technologies like solar panels or propane refrigerators?',
    'Track Ordnung decisions across affiliations; if the same technology is permitted in one district and forbidden in another despite similar communal effects, the boundary is indeterminate.',
    'Indeterminacy would suggest the consequence reading is partially a cover for traditionalist inertia, raising epsilon and shifting classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consequence_vs_principle_boundary, conceptual, 'Indeterminacy between consequence and principle readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__consequence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t0, gelassenheit_separation__consequence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gela_tr_t10, gelassenheit_separation__consequence_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(gela_tr_t20, gelassenheit_separation__consequence_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(gela_tr_t30, gelassenheit_separation__consequence_reading, theater_ratio, 30, 0.13).
narrative_ontology:measurement(gela_tr_t40, gelassenheit_separation__consequence_reading, theater_ratio, 40, 0.14).
narrative_ontology:measurement(gela_tr_t50, gelassenheit_separation__consequence_reading, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(gela_be_t0, gelassenheit_separation__consequence_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(gela_be_t10, gelassenheit_separation__consequence_reading, base_extractiveness, 10, 0.17).
narrative_ontology:measurement(gela_be_t20, gelassenheit_separation__consequence_reading, base_extractiveness, 20, 0.19).
narrative_ontology:measurement(gela_be_t30, gelassenheit_separation__consequence_reading, base_extractiveness, 30, 0.2).
narrative_ontology:measurement(gela_be_t40, gelassenheit_separation__consequence_reading, base_extractiveness, 40, 0.21).
narrative_ontology:measurement(gela_be_t50, gelassenheit_separation__consequence_reading, base_extractiveness, 50, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(gelassenheit_separation__consequence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, gelassenheit_separation__artifact_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, gelassenheit_separation__principle_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the gelassenheit_separation kernel, decomposed per the epsilon-invariance principle because the natural-language label 'Gelassenheit separation' conflates three structurally distinct claims: appearance-based rejection (artifact), consequence-based functional evaluation (consequence), and systemic-entanglement avoidance (principle).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
