% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__consequence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Amish Consequence-Reading Technology Ordnung
 *   domain: religious/technology_governance
 *
 * SUMMARY:
 *   The Amish consequence-reading of the Gelassenheit separation kernel
 *   evaluates technology not by its artifactual resemblance to worldly things
 *   nor by abstract structural isolation, but by its concrete effects on
 *   community practices: visiting, mutual aid, and geographic rootedness.
 *   This yields fine-grained contextual rulesâtelephones permitted in barns
 *   (to preserve home visiting) but not in homes; tractors permitted for belt
 *   power (to preserve local mutual aid) but not for road transport. The
 *   constraint is claimed as tangled_rope because it carries both a genuine
 *   coordination function (preserving communal social infrastructure) and
 *   asymmetric extraction (innovation-leaning members bear the costs of
 *   restricted technology access). The metrics are authored independently:
 *   low extractiveness (0.28) reflects the fine-grained, context-sensitive
 *   nature of the rules, while moderate suppression (0.48) reflects the
 *   active social enforcement required to maintain them against rising
 *   technological pressure.
 *
 * KEY AGENTS:
 *   - church_elders (agenda_setter/organized/constrained): Interpret the Ordnung and rule on technology by communal effects
 *   - traditional_community (beneficiary/moderate/identity_locked): Benefit from preserved visiting, mutual aid, and rootedness
 *   - innovation_leaning_members (payer/moderate/constrained): Bear costs of technology restrictions
 *   - sociologists_of_religion (observer/analytical/analytical): Study the system from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__consequence_reading, 0.28).
domain_priors:suppression_score(gelassenheit_separation__consequence_reading, 0.48).
domain_priors:theater_ratio(gelassenheit_separation__consequence_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__consequence_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__consequence_reading, "Amish Consequence-Reading Technology Ordnung").
narrative_ontology:topic_domain(gelassenheit_separation__consequence_reading, "religious/technology_governance").

domain_priors:requires_active_enforcement(gelassenheit_separation__consequence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__consequence_reading, '8d012dde-46c9-4bbc-a70c-f07eb17a2a67').
narrative_ontology:cs_kernel_codification('8d012dde-46c9-4bbc-a70c-f07eb17a2a67', distributed).
narrative_ontology:cs_authority_grounding('8d012dde-46c9-4bbc-a70c-f07eb17a2a67', practice).
narrative_ontology:cs_interpretation_layer_present('8d012dde-46c9-4bbc-a70c-f07eb17a2a67').
narrative_ontology:cs_reading_relation('8d012dde-46c9-4bbc-a70c-f07eb17a2a67', gelassenheit_separation__artifact_reading, forecloses).
narrative_ontology:cs_reading_relation('8d012dde-46c9-4bbc-a70c-f07eb17a2a67', gelassenheit_separation__principle_reading, coexists_with).
narrative_ontology:cs_axiom('8d012dde-46c9-4bbc-a70c-f07eb17a2a67', foundational, technology_judged_by_communal_effects).
narrative_ontology:cs_axiom_status(technology_judged_by_communal_effects, holdable).
narrative_ontology:cs_axiom_grounding('8d012dde-46c9-4bbc-a70c-f07eb17a2a67', technology_judged_by_communal_effects, conventional).
narrative_ontology:cs_axiom('8d012dde-46c9-4bbc-a70c-f07eb17a2a67', secondary, geographic_rootedness_preservable_through_restriction).
narrative_ontology:cs_axiom_status(geographic_rootedness_preservable_through_restriction, holdable).
narrative_ontology:cs_axiom_grounding('8d012dde-46c9-4bbc-a70c-f07eb17a2a67', geographic_rootedness_preservable_through_restriction, empirically_contingent).
narrative_ontology:cs_reference_frame('8d012dde-46c9-4bbc-a70c-f07eb17a2a67', separated_communal_practice).
narrative_ontology:cs_drift_state('8d012dde-46c9-4bbc-a70c-f07eb17a2a67', digital_ubiquity_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8d012dde-46c9-4bbc-a70c-f07eb17a2a67', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__consequence_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, church_elders).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, traditional_community).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, innovation_leaning_members).
narrative_ontology:constraint_vindicates(gelassenheit_separation__consequence_reading, gelassenheit_as_practical_preservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the Ordnung and issue rulings on new technology by evaluating its concrete effects on visiting, mutual aid, and geographic rootedness. Enforce compliance through church discipline, including the threat of shunning. Their authority is bounded by community consensus and tradition; they cannot unilaterally abandon the consequence-reading without risking schism.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, church_elders, agenda_setter,
    organized, generational, constrained, local).

% Benefit from preserved community practicesâneighbors visit in person, barn raisings and mutual aid labor continue, and families remain geographically rooted. They accept restrictions on home telephones and road tractors as the price of this social cohesion. Their identity is fused with the church district.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, traditional_community, beneficiary,
    moderate, generational, identity_locked, local).

% Bear the costs of fine-grained technology restrictions. They cannot install telephones in homes even for business or safety, must use tractors only for belt power, and face social sanction for adopting digital tools. Exit is possible but costly: leaving means severing family ties and losing identity.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, innovation_leaning_members, payer,
    moderate, biographical, constrained, local).

% Mainstream technology users, vendors, and economists who would advocate for unrestricted adoption and frame restrictions as efficiency losses. They are structurally excluded from Ordnung deliberations and have no standing in church district decisions.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, external_modernizers, excluded,
    moderate, biographical, mobile, regional).

% Study Amish technology governance as a case of alternative modernity, documenting how the consequence-reading preserves social capital. They observe from outside and do not participate in normative deliberations.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, sociologists_of_religion, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gelassenheit_separation__consequence_reading, diffuse).
narrative_ontology:fixing_cost_class(gelassenheit_separation__consequence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the informal social infrastructure of face-to-face visiting, mutual aid labor, and geographic rootedness by evaluating each proposed technology for its concrete effects on these practices rather than by its artifactual form or structural isolation.
% TRANSFER_FUNCTION: Moves the cost of forgoing technological convenience from individual households to the collective preservation of communal social practices; moves interpretive authority over technology from individual preference to church elders and district consensus.
% ABSENT_VOICES: Innovation-leaning youth and ex-Amish who left over technology restrictions are structurally absent from Ordnung deliberations; English technology vendors and mainstream economists who would frame restrictions as efficiency losses are also excluded.
% DISAPPEARANCE_RATIONALE: If the consequence-reading vanished, home internet and smartphones would proliferate rapidly, visiting would be displaced by digital communication, mutual aid would weaken as hired services replaced communal labor, and geographic mobility would increaseâdissolving the distinctively Amish pattern of community life within one generation.
% FOUNDING_PROBLEM: The encroachment of twentieth-century mass technology (automobile, telephone, electric grid) threatened to dissolve the face-to-face communal bonds and local mutual aid systems that had historically sustained Anabaptist communities without centralized welfare or state support.
% FOUNDING_PROBLEM_CORROBORATION: Sociologists Donald Kraybill and Marc Olshan document the founding problem from outside the beneficiary set; internal church historians and bishops corroborate it from within. Dissenting voices among ex-Amish and some progressive members contest whether current restrictions still address the founding problem or have become self-perpetuating.
narrative_ontology:disappearance_verdict(gelassenheit_separation__consequence_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__consequence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__consequence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gelassenheit_separation__consequence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__consequence_reading, 0.28, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low (0.28) because the rules are fine-grained and permit significant technological access where it does not threaten communal practices; it is not zero because some members still bear real costs. Suppression is moderate (0.48) because the constraint depends on active church discipline and social sanction, which must be continually renewed as external technology pressure intensifies. Theater ratio is low-moderate (0.25) because most enforcement activity is functional, though some performative maintenance exists around visible abstinence. Accessibility collapse (0.65) is high because once a member commits to the community, the alternative of adopting restricted technology while remaining in good standing is effectively closed. Resistance (0.30) reflects persistent but contained dissent, especially among youth.
 *
 * PERSPECTIVAL GAP:
 *   From the elder and traditional-community seats, the constraint is experienced as self-governance preserving goods they value; from the innovation-leaning member seat, it is experienced as paternalistic restriction. The engine computes this divergence from the structural dataâbeneficiaries have identity-locked or constrained exit and low directionality, while payers face higher directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional households and elders are structural beneficiaries (d near 0.0) because the constraint subsidizes their preferred social world. Innovation-leaning members are structural targets (d near 1.0) because the constraint extracts technology options from them. The divergence is tempered by the fact that community membership is voluntary in principle, but exit is identity-locked and socially costly, keeping payer directionality high.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpreserving communal social infrastructure against technological dissolutionâis contested but not dead. The constraint has not atrophied into a piton because the coordination function remains substantively operative: visiting and mutual aid are still measurably higher in districts maintaining this reading. The classification as tangled_rope prevents mislabeling the restriction as pure extraction (which would ignore the genuine social goods produced) or as pure coordination (which would ignore the costs borne by innovation-leaning members).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the consequence-reading the authentic expression of the Gelassenheit separation kernel, or has it displaced an earlier artifact-based or principle-based reading through post-hoc rationalization?',
    'Historical genealogy of Ordnung rulings across Amish affiliations: trace when the ''effect on visiting'' criterion first appears in documented bishop decisions and whether it correlates with specific technological pressures.',
    'If the consequence-reading emerged as post-hoc rationalization, its epsilon may be underestimated because the rule structure serves to legitimize other unacknowledged power arrangements; if authentic, the low epsilon genuinely reflects a coordination-priority constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Contested kernel status of the consequence reading').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of technology adoption structural (enforced by church discipline and threat of shunning) or internalized (members believe the restrictions are their own religious duty)?',
    'Post-exit technology adoption trajectory: compare speed and completeness of technology uptake among members who left voluntarily versus those who were shunned.',
    'If internalized, effective suppression is higher than the structural measure suggests because members carry the constraint with them after exit; if purely structural, suppression drops sharply upon leaving.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    empirical_preservation_claim,
    'Does restricting telephones to barns and tractors to belt power actually preserve visiting and mutual aid, or have these practices already eroded through other mechanisms?',
    'Comparative study of church districts with stricter versus looser consequence-readings: measure visiting frequency, mutual aid event participation, and out-migration rates.',
    'If the practices have eroded despite restrictions, the coordination function is weaker than claimed and the constraint extracts compliance without delivering the promised social goods; if preserved, the low epsilon is justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_preservation_claim, empirical, 'Empirical efficacy of technology restrictions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__consequence_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t0, gelassenheit_separation__consequence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(gela_tr_t10, gelassenheit_separation__consequence_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(gela_tr_t20, gelassenheit_separation__consequence_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(gela_tr_t30, gelassenheit_separation__consequence_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(gela_tr_t40, gelassenheit_separation__consequence_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(gela_tr_t50, gelassenheit_separation__consequence_reading, theater_ratio, 50, 0.21).
narrative_ontology:measurement(gela_tr_t60, gelassenheit_separation__consequence_reading, theater_ratio, 60, 0.25).

% Extraction over time
narrative_ontology:measurement(gela_be_t0, gelassenheit_separation__consequence_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(gela_be_t10, gelassenheit_separation__consequence_reading, base_extractiveness, 10, 0.17).
narrative_ontology:measurement(gela_be_t20, gelassenheit_separation__consequence_reading, base_extractiveness, 20, 0.19).
narrative_ontology:measurement(gela_be_t30, gelassenheit_separation__consequence_reading, base_extractiveness, 30, 0.22).
narrative_ontology:measurement(gela_be_t40, gelassenheit_separation__consequence_reading, base_extractiveness, 40, 0.24).
narrative_ontology:measurement(gela_be_t50, gelassenheit_separation__consequence_reading, base_extractiveness, 50, 0.26).
narrative_ontology:measurement(gela_be_t60, gelassenheit_separation__consequence_reading, base_extractiveness, 60, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t0, gelassenheit_separation__consequence_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(gela_su_t10, gelassenheit_separation__consequence_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(gela_su_t20, gelassenheit_separation__consequence_reading, suppression_requirement, 20, 0.32).
narrative_ontology:measurement(gela_su_t30, gelassenheit_separation__consequence_reading, suppression_requirement, 30, 0.36).
narrative_ontology:measurement(gela_su_t40, gelassenheit_separation__consequence_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(gela_su_t50, gelassenheit_separation__consequence_reading, suppression_requirement, 50, 0.44).
narrative_ontology:measurement(gela_su_t60, gelassenheit_separation__consequence_reading, suppression_requirement, 60, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__consequence_reading, identity_coordination).
narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, gelassenheit_separation__artifact_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, gelassenheit_separation__principle_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the gelassenheit_separation kernel, decomposed per the epsilon-invariance principle from the artifact_reading and principle_reading siblings. Each sibling instantiates a structurally distinct constraint with its own epsilon, stakeholders, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
