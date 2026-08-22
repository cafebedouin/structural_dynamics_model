% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__artifact_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__artifact_reading, []).

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
 *   constraint_id: gelassenheit_separation__artifact_reading
 *   human_readable: Amish Gelassenheit Separation — Artifact-Form Reading
 *   domain: religious/technology_governance/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the artifact-form reading of the
 *   Gelassenheit separation kernel: separation is defined by visible
 *   nonconformity to English society's material artifacts, regardless of
 *   functional entanglement. Off-grid solar panels are forbidden because
 *   panels 'look modern'; synthetic fabrics are forbidden because they 'look
 *   worldly'; medical devices are forbidden because they 'resemble English
 *   technology.' The constraint operates through the Ordnung — the unwritten
 *   but authoritatively interpreted rule system administered by the ordained
 *   ministry and bishop council. Extraction is high and rising because each
 *   new technology that resembles a worldly artifact expands the
 *   prohibition's scope without expanding its coordination function.
 *   Suppression is maximal because the constraint's persistence depends on
 *   actively forbidding exits (shunning) and suppressing internal dissent
 *   (silencing petitions). Theater is low because the enforcement is genuine,
 *   not performative — the community really does forego solar, medical
 *   devices, and modern fabrics. This is NOT the principle reading (which
 *   evaluates technology by structural entanglement) nor the consequence
 *   reading (which evaluates by effect on communal practices).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, 0.82).
domain_priors:suppression_score(gelassenheit_separation__artifact_reading, 0.91).
domain_priors:theater_ratio(gelassenheit_separation__artifact_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__artifact_reading, snare).
narrative_ontology:human_readable(gelassenheit_separation__artifact_reading, "Amish Gelassenheit Separation — Artifact-Form Reading").
narrative_ontology:topic_domain(gelassenheit_separation__artifact_reading, "religious/technology_governance/commitment_systems").

domain_priors:requires_active_enforcement(gelassenheit_separation__artifact_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__artifact_reading, 'ab4c7dc9-6c05-4cc5-ab88-c8092a42aeb2').
narrative_ontology:cs_kernel_codification('ab4c7dc9-6c05-4cc5-ab88-c8092a42aeb2', implicit).
narrative_ontology:cs_authority_grounding('ab4c7dc9-6c05-4cc5-ab88-c8092a42aeb2', lineage).
narrative_ontology:cs_interpretation_layer_present('ab4c7dc9-6c05-4cc5-ab88-c8092a42aeb2').
narrative_ontology:cs_reading_relation('ab4c7dc9-6c05-4cc5-ab88-c8092a42aeb2', gelassenheit_separation__principle_reading, forecloses).
narrative_ontology:cs_reading_relation('ab4c7dc9-6c05-4cc5-ab88-c8092a42aeb2', gelassenheit_separation__consequence_reading, coexists_with).
narrative_ontology:cs_axiom('ab4c7dc9-6c05-4cc5-ab88-c8092a42aeb2', foundational, separation_requires_visible_nonconformity).
narrative_ontology:cs_axiom_status(separation_requires_visible_nonconformity, holdable).
narrative_ontology:cs_axiom_grounding('ab4c7dc9-6c05-4cc5-ab88-c8092a42aeb2', separation_requires_visible_nonconformity, deontological).
narrative_ontology:cs_axiom('ab4c7dc9-6c05-4cc5-ab88-c8092a42aeb2', foundational, artifact_form_determines_worldliness).
narrative_ontology:cs_axiom_status(artifact_form_determines_worldliness, holdable).
narrative_ontology:cs_axiom_grounding('ab4c7dc9-6c05-4cc5-ab88-c8092a42aeb2', artifact_form_determines_worldliness, conventional).
narrative_ontology:cs_reference_frame('ab4c7dc9-6c05-4cc5-ab88-c8092a42aeb2', visible_separation_witness).
narrative_ontology:cs_drift_state('ab4c7dc9-6c05-4cc5-ab88-c8092a42aeb2', contemporary_technology_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ab4c7dc9-6c05-4cc5-ab88-c8092a42aeb2', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__artifact_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, ordained_ministry).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, bishop_council).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, community_elders).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, young_families).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, aging_householders).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, economically_marginal_members).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, health_dependent_members).
narrative_ontology:constraint_vindicates(gelassenheit_separation__artifact_reading, visible_separation_from_world).
narrative_ontology:constraint_vindicates(gelassenheit_separation__artifact_reading, nonconformity_as_witness).
narrative_ontology:constraint_vindicates(gelassenheit_separation__artifact_reading, uniformity_of_ordnung).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the Ordnung through twice-yearly Council meetings; interprets Gelassenheit as requiring visible nonconformity to 'English' artifacts. Their authority derives from ordination lineage and the claim that uniformity of visible markers preserves the community's witness. Exit would mean renouncing ordination vows and the only social identity they have known since youth.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, ordained_ministry, agenda_setter,
    institutional, generational, identity_locked, regional).

% Collectively adjudicates technology petitions; benefits from the deference and material support the community provides to ordained leadership. The artifact reading gives them a clear, administrable boundary — 'does it look English?' — that requires no technical evaluation. Their position is both administratively convenient and identity-constitutive.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, bishop_council, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(gelassenheit_separation__artifact_reading, bishop_council, beneficiary).

% Have invested a lifetime in the visible markers of separation (plain dress, horse-and-buggy, non-electric homes). The artifact reading validates their life choices as the standard of faithfulness. They benefit from social honor and care structures that flow to those who have 'borne the yoke.' Exit is unthinkable — their identity is fused with the visible order.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, community_elders, beneficiary,
    organized, biographical, identity_locked, local).

% Bear the daily costs of artifact-based prohibition: no solar panels for off-grid wells (hand-pumping water), no synthetic fabrics for children's winter clothing (wool only, despite cost and allergies), no power tools for home construction (hiring English contractors at premium rates). Exit means leaving the only community they know, losing land access, and facing shunning — but some do leave, making their exit 'constrained' not 'trapped.'
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, young_families, payer,
    moderate, biographical, constrained, local).

% Face compounding physical costs: maintaining wood stoves and kerosene lighting with declining strength; unable to adopt assistive devices (stair lifts, medical alert systems) that resemble English artifacts. The artifact reading extracts disproportionate labor from aging bodies. Exit is constrained by age, property ties, and care dependencies.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, aging_householders, payer,
    moderate, biographical, constrained, local).

% Cannot afford the artifact-compliant alternatives: hand-powered laundry equipment, horse-drawn transport, custom-made plain clothing. The artifact reading functions as a regressive tax — the poorest pay the highest share of income to maintain visible separation. Exit is trapped: no capital to restart elsewhere, no English-language education, no trade skills recognized outside.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, economically_marginal_members, payer,
    powerless, immediate, trapped, local).

% Denied medical technologies that resemble worldly artifacts: CPAP machines (electric), insulin pumps (electronic), telehealth (video screens). The artifact reading extracts health and life itself. Some bishops grant quiet exceptions, but the rule's logic forbids them — creating a shadow economy of concealment. Exit is trapped by medical dependency and community care structures.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, health_dependent_members, payer,
    powerless, immediate, trapped, local).

% Scholars and adjacent-community members who study the Ordnung's operation. They see the artifact reading's structural extraction but have no standing to petition. Their analytical seat computes the constraint's type from the outside.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, seeker_observers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legible, administrable boundary that maintains communal identity against assimilation — the artifact test ('does it look English?') is instantly verifiable by any member without technical expertise, creating a shared semantic field of separation.
% TRANSFER_FUNCTION: Moves labor, health, income, and life-years from households (especially young, aging, poor, and ill members) to the maintenance of visible markers — plain dress production, non-electric infrastructure, horse-powered transport, manual substitutes for prohibited tools. The ordained ministry and bishop council receive deference, authority, and material support as the boundary's administrators.
% ABSENT_VOICES: Former members who left over artifact prohibitions (especially solar, medical devices, synthetic fabrics) — they would testify that the visible-marker test is incoherent (off-grid solar does not entangle) and extractive. They are structurally excluded by shunning; their testimony is inadmissible in Council. Also absent: youth who have not yet joined church but already internalize the constraint — they cannot object before baptism.
% DISAPPEARANCE_RATIONALE: If the artifact reading vanished overnight, households would adopt off-grid solar, synthetic fabrics, power tools, and medical devices within months — the visible marker economy would collapse, the ministry's administrative authority would shrink to behavioral/relational matters, and the community would face a crisis of identity definition. The arrangements of daily life, labor allocation, and health access would fundamentally reorganize.
% FOUNDING_PROBLEM: Early Anabaptist communities faced persecution and assimilation pressure in Europe; visible distinction (plain dress, nonresistance, separation from state churches) was a survival strategy that made the community legible to itself and opaque to authorities.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Anabaptism (e.g., John D. Roth, Steven Nolt) document that the founding persecution context ended centuries ago; the artifact-specific prohibitions (electricity, rubber tires, synthetic fabrics) postdate the founding era by generations and were adopted in response to 19th/20th century technological change, not 16th century persecution. No non-beneficiary source attests that the artifact test solves a live founding problem.
narrative_ontology:disappearance_verdict(gelassenheit_separation__artifact_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__artifact_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__artifact_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(gelassenheit_separation__artifact_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__artifact_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__artifact_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gelassenheit_separation__artifact_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness 0.82: the artifact test forbids technologies that would reduce labor, improve health, and lower costs — the gap between what households could access (off-grid solar, medical devices, power tools) and what the artifact test permits is wide and growing. Suppression 0.91: shunning enforces compliance; petitions for exception are denied on principle; shadow economies of concealment prove the rule's coercive force. Theater ratio 0.15: the enforcement is real — there is no gap between the Ordnung's claims and its operation. Accessibility collapse 0.88: once the artifact logic is internalized, alternatives become unthinkable — 'we don't do that' replaces 'we can't do that.' Resistance 0.72: substantial but channeled into concealment, quiet noncompliance, and eventual exit rather than open challenge. The measurement grid shows extraction rising monotonically as the artifact boundary encounters more technologies; suppression hardening as enforcement machinery (Council discipline, shunning protocol) matures; theater stable-low because the constraint never claimed to be temporary or transitional.
 *
 * PERSPECTIVAL GAP:
 *   From the ministry/elder seats, the artifact test is genuine coordination — it makes separation legible and administrable. From the payer seats (especially trapped ones), the same structure is pure extraction — it takes health, labor, and income while forbidding exit. The engine computes this divergence from the structural data: beneficiaries have identity_locked exit (low d), payers have constrained/trapped exit (high d). The claimed_type 'snare' reflects the payer-seat reality; the ministry would claim 'rope' or 'mountain.' This seat divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The ordained ministry and bishop council are structural beneficiaries (d near 0.0): they collect deference, authority, and material support from administering the artifact boundary. Community elders are beneficiaries (d ~0.15): their life investments are validated. Young families, aging householders, economically marginal, and health-dependent members are targets (d 0.7–1.0): they bear the extractive costs with constrained or trapped exit. The artifact reading's directionality is sharply bimodal — the same constraint that coordinates identity for beneficiaries extracts from payers. Seeker observers sit at d=0.5 (analytical seat).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (persecution-era survival via visible distinction) is dead — corroborated by Anabaptist historians outside the beneficiary set. The artifact-specific prohibitions postdate the founding era and were responses to technological change, not persecution. Yet the arrangement persists and intensifies (extraction rising from 0.25 to 0.82). This is classic mandatrophy: the mandate (visible separation) outlived its function (survival under persecution) and became self-justifying. The artifact reading prevents mandatrophy resolution by treating the marker itself as the mandate — any relaxation is framed as 'compromise with the world.' The coordinate-reading siblings (principle, consequence) are the unresolved mandatrophy alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    artifact_vs_entanglement_boundary,
    'Is the artifact-form criterion (''resembles English artifacts'') a stable, administrable boundary, or does it collapse under technological convergence (e.g., solar panels that look like roofing, medical devices that look like watches)?',
    'Track technology-petition outcomes over 20 years: if the artifact test produces increasing exceptions, ad-hoc distinctions, or schisms, the boundary is unstable.',
    'If unstable, the artifact reading''s coordination function degrades — it becomes a piton (theatrical maintenance of a failed boundary) rather than a snare (active extraction). The claimed_type would shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(artifact_vs_entanglement_boundary, empirical, 'Whether the artifact test remains administrable as technology converges on invisible/embedded form factors.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.91) primarily structural (shunning, property loss, care withdrawal) or internalized (members believe artifact compliance is salvation; concealment carries spiritual terror)?',
    'Post-exit suppression trajectory: track former members'' psychological and social outcomes — if suppression persists after exit (terror, guilt, identity fragmentation), reclassify as partially internalized.',
    'If substantially internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression with them. This would amplify χ for trapped/identity_locked seats beyond the engine''s structural derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in a high-identity-lock context.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the gelassenheit_separation kernel legitimately support three readings, or does the artifact reading foreclose the others by capturing the administrative machinery?',
    'Analyze Council petition records: if artifact-reading bishops consistently deny principle/consequence petitions on procedural grounds (not substantive), the kernel''s interpretive structure is captured.',
    'If captured, the kernel is not a genuine site of contestation — the artifact reading has colonized the authority structure. The sibling readings are ''allowed'' only as performative dissent. This would reclassify the kernel from distributed to extraction-grounded authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel''s contestation is genuine or administered by the artifact reading''s capture of the bishop council.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__artifact_reading, 1850, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t1850, gelassenheit_separation__artifact_reading, theater_ratio, 1850, 0.05).
narrative_ontology:measurement(gela_tr_t1890, gelassenheit_separation__artifact_reading, theater_ratio, 1890, 0.08).
narrative_ontology:measurement(gela_tr_t1930, gelassenheit_separation__artifact_reading, theater_ratio, 1930, 0.1).
narrative_ontology:measurement(gela_tr_t1970, gelassenheit_separation__artifact_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(gela_tr_t2000, gelassenheit_separation__artifact_reading, theater_ratio, 2000, 0.14).
narrative_ontology:measurement(gela_tr_t2025, gelassenheit_separation__artifact_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(gela_be_t1850, gelassenheit_separation__artifact_reading, base_extractiveness, 1850, 0.25).
narrative_ontology:measurement(gela_be_t1890, gelassenheit_separation__artifact_reading, base_extractiveness, 1890, 0.35).
narrative_ontology:measurement(gela_be_t1930, gelassenheit_separation__artifact_reading, base_extractiveness, 1930, 0.52).
narrative_ontology:measurement(gela_be_t1970, gelassenheit_separation__artifact_reading, base_extractiveness, 1970, 0.68).
narrative_ontology:measurement(gela_be_t2000, gelassenheit_separation__artifact_reading, base_extractiveness, 2000, 0.76).
narrative_ontology:measurement(gela_be_t2025, gelassenheit_separation__artifact_reading, base_extractiveness, 2025, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t1850, gelassenheit_separation__artifact_reading, suppression_requirement, 1850, 0.45).
narrative_ontology:measurement(gela_su_t1890, gelassenheit_separation__artifact_reading, suppression_requirement, 1890, 0.55).
narrative_ontology:measurement(gela_su_t1930, gelassenheit_separation__artifact_reading, suppression_requirement, 1930, 0.7).
narrative_ontology:measurement(gela_su_t1970, gelassenheit_separation__artifact_reading, suppression_requirement, 1970, 0.82).
narrative_ontology:measurement(gela_su_t2000, gelassenheit_separation__artifact_reading, suppression_requirement, 2000, 0.88).
narrative_ontology:measurement(gela_su_t2025, gelassenheit_separation__artifact_reading, suppression_requirement, 2025, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__artifact_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gelassenheit_separation__artifact_reading, 0.08).
narrative_ontology:affects_constraint(gelassenheit_separation__artifact_reading, gelassenheit_separation__principle_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__artifact_reading, gelassenheit_separation__consequence_reading).

% DUAL FORMULATION NOTE:
% The gelassenheit_separation kernel decomposes into three constraint stories: artifact_reading (this story — high ε, snare), principle_reading (low ε, rope/tangled_rope), consequence_reading (moderate ε, scaffold/tangled_rope). The artifact reading's administrative capture of the bishop council creates structural pressure on the siblings — it sets the default that petitions must overcome. All three stories link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gelassenheit_separation__artifact_reading, institutional, 0.1).
constraint_indexing:directionality_override(gelassenheit_separation__artifact_reading, moderate, 0.75).
constraint_indexing:directionality_override(gelassenheit_separation__artifact_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
