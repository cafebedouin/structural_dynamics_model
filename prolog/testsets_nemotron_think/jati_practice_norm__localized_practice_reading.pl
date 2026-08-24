% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__localized_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__localized_practice_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: jati_practice_norm__localized_practice_reading
 *   human_readable: Jati Boundaries as Localized Coordination Norms
 *   domain: social/religious/political_economy
 *
 * SUMMARY:
 *   This reading treats jati boundaries as emergent coordination norms that
 *   proliferate because they solve real collective-action problems at the
 *   village level. The 3000+ recorded categories are not evidence of
 *   oppression but of adaptive granularity: each new sub-caste marks a
 *   successful negotiation of a specific coordination challenge (e.g., a
 *   migrant group gaining recognition, an occupational niche splitting).
 *   Extraction is low because no single actor collects rents from the system
 *   as a whole; suppression is low because exit, while socially costly, is
 *   structurally possible (adoption, relocation, sub-caste formation). The
 *   colonial census reading reifies this fluidity into a fixed taxonomy; the
 *   orthodox textual reading imposes a scriptural grid that local practice
 *   routinely ignores. This reading claims the kernel is a rope — genuine
 *   coordination with minimal coercion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__localized_practice_reading, 0.18).
domain_priors:suppression_score(jati_practice_norm__localized_practice_reading, 0.12).
domain_priors:theater_ratio(jati_practice_norm__localized_practice_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__localized_practice_reading, rope).
narrative_ontology:human_readable(jati_practice_norm__localized_practice_reading, "Jati Boundaries as Localized Coordination Norms").
narrative_ontology:topic_domain(jati_practice_norm__localized_practice_reading, "social/religious/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__localized_practice_reading, 'f31abfcc-5dee-47f8-9462-bbe76a4ae330').
narrative_ontology:cs_kernel_codification('f31abfcc-5dee-47f8-9462-bbe76a4ae330', distributed).
narrative_ontology:cs_authority_grounding('f31abfcc-5dee-47f8-9462-bbe76a4ae330', practice).
narrative_ontology:cs_reading_relation('f31abfcc-5dee-47f8-9462-bbe76a4ae330', jati_practice_norm__colonial_census_reading, coexists_with).
narrative_ontology:cs_reading_relation('f31abfcc-5dee-47f8-9462-bbe76a4ae330', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_axiom('f31abfcc-5dee-47f8-9462-bbe76a4ae330', foundational, jati_boundaries_are_emergent_from_local_practice).
narrative_ontology:cs_axiom_status(jati_boundaries_are_emergent_from_local_practice, holdable).
narrative_ontology:cs_axiom_grounding('f31abfcc-5dee-47f8-9462-bbe76a4ae330', jati_boundaries_are_emergent_from_local_practice, empirically_contingent).
narrative_ontology:cs_axiom('f31abfcc-5dee-47f8-9462-bbe76a4ae330', secondary, local_renegotiation_is_legitimate_authority).
narrative_ontology:cs_axiom_status(local_renegotiation_is_legitimate_authority, holdable).
narrative_ontology:cs_axiom_grounding('f31abfcc-5dee-47f8-9462-bbe76a4ae330', local_renegotiation_is_legitimate_authority, deontological).
narrative_ontology:cs_reference_frame('f31abfcc-5dee-47f8-9462-bbe76a4ae330', local_practice_autonomy).
narrative_ontology:cs_drift_state('f31abfcc-5dee-47f8-9462-bbe76a4ae330', contemporary_anthropological_observation, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f31abfcc-5dee-47f8-9462-bbe76a4ae330', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__localized_practice_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, local_communities).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, village_elders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, itinerant_merchants).
narrative_ontology:constraint_vindicates(jati_practice_norm__localized_practice_reading, social_categories_are_emergent_from_practice).
narrative_ontology:constraint_vindicates(jati_practice_norm__localized_practice_reading, local_autonomy_in_boundary_setting_is_legitimate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Local communities use jati boundaries to coordinate marriage, occupation, mutual aid, and ritual participation. Boundaries are continuously renegotiated in response to migration, economic change, and internal disputes. Membership provides access to dense trust networks and collective insurance; exit means losing those networks but is possible through adoption, relocation, or negotiated boundary shifts.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, local_communities, beneficiary,
    organized, generational, constrained, local).

% Elders and respected figures convene caste councils (panchayats) to adjudicate boundary disputes, sanction norm violations, and ratify new sub-caste formations. Their authority derives from recognized knowledge of local precedent and ritual propriety, not from coercive power. They benefit from the coordination function but also bear responsibility for maintaining it.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, village_elders, agenda_setter,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__localized_practice_reading, village_elders, beneficiary).

% Groups historically placed outside the jati system (e.g., Dalit communities) are structurally excluded from the coordination benefits of jati membership — no access to marriage networks, occupational guilds, or mutual aid. They develop parallel coordination structures but remain vulnerable to violence and economic boycott when challenging the boundary order.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, outcaste_groups, excluded,
    powerless, generational, trapped, local).

% Traveling traders and artisans navigate multiple jati jurisdictions by adopting situational identities, leveraging the system's fluidity to gain trust and market access across villages. They benefit from the coordination norms without being bound by a single jati's rules.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, itinerant_merchants, beneficiary,
    moderate, biographical, mobile, regional).

% Colonial administrators and ethnographers documented the proliferation of jati categories (3000+), noting the gap between textual varna theory and local practice. Their records inadvertently fossilized fluid boundaries into fixed census categories, creating a reference point that later readings treat as authoritative.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, colonial_ethnographers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of trust and cooperation in a fragmented, stateless social landscape: jati boundaries provide a low-overhead mechanism for credentialing strangers, enforcing contracts, pooling risk, and organizing collective labor without a central bureaucracy.
% TRANSFER_FUNCTION: Moves social capital (trust, marriageability, occupational access, ritual status) from the community to the individual member; in return the member contributes labor, conformity, and dues to the collective. No monetary rent is extracted by a central operator — the transfer is reciprocal and diffuse.
% ABSENT_VOICES: Outcaste groups and women within jatis would object to the patriarchal and exclusionary dimensions of boundary-setting if they had standing in the councils; their exclusion is structural — panchayats are male-dominated and jati-endogamous by definition.
% DISAPPEARANCE_RATIONALE: If localized jati coordination vanished overnight, villages would lose their primary mechanism for marriage alliance, occupational apprenticeship, dispute resolution, and famine insurance. New institutions (state welfare, formal contracts, religious reform movements) would eventually fill the gap but at much higher transaction cost and with a generation of social disruption.
% FOUNDING_PROBLEM: Pre-colonial Indian villages needed a way to coordinate production, reproduction, and mutual aid across diverse occupational groups without a strong central state. Jati boundaries emerged as a decentralized solution: each group managed its own affairs, and inter-group relations were negotiated through ritualized exchange.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological consensus (Dumont, Srinivas, Dirks) and contemporary village studies confirm the coordination function persists; the founding problem — stateless cooperation at scale — remains live in rural India where state penetration is thin. No beneficiary group disputes the functional origin; the contest is over whether the current form still serves it.
narrative_ontology:disappearance_verdict(jati_practice_norm__localized_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__localized_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__localized_practice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jati_practice_norm__localized_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__localized_practice_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__localized_practice_reading_tests).
:- end_tests(jati_practice_norm__localized_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18) reflects the diffuse, reciprocal nature of jati membership costs (conformity, endogamy, dues) which are offset by collective benefits. Suppression (0.12) captures the social sanction for boundary violation (excommunication, boycott) but notes that sanctions are enforced by peers, not a state, and that new boundaries form constantly — the system absorbs dissent by proliferation. Theater ratio (0.15) measures the gap between ritualized boundary rhetoric (purity/pollution language) and the pragmatic negotiation that actually occurs in panchayats. Accessibility collapse (0.22) is low because alternative coordination forms (sectarian movements, trade guilds, state welfare) coexist and compete. Resistance (0.18) is low because the system's flexibility co-opts most challenges into new categories rather than provoking open revolt.
 *
 * PERSPECTIVAL GAP:
 *   The colonial census reading sees the same proliferation as evidence of a society fragmented into thousands of rigid castes; the orthodox textual reading sees it as corruption of the fourfold varna order. This reading sees it as a healthy, adaptive coordination ecology. The divergence is not in the data (all three see 3000+ categories) but in the structural interpretation: coordination vs. extraction vs. pollution. The engine's per-seat classification will show this reading as rope from the community seat, while the colonial reading computes as snare from the outcaste seat and the orthodox reading as mountain from the textual authority seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Local communities and elders are beneficiaries (d ~ 0.2) — they gain coordination services and status from maintaining the system. Outcaste groups are excluded (not payers in this reading) — they are outside the constraint's scope, not extracted from by it. Itinerant merchants are mobile beneficiaries (d ~ 0.1) — they exploit the system's fluidity without being bound. Colonial ethnographers are analytical observers. The engine will compute low effective extraction for all seated agents because directionality is near-symmetric and scope is local.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate (stateless cooperation) is still live in rural India; the arrangement has not atrophied into piton because it continues to generate new categories in response to new problems (e.g., OBC mobilization, urban caste associations). Theater remains low because panchayats still do real work (dispute resolution, marriage negotiation). The constraint would only become piton if state welfare fully replaced its insurance function — not yet the case.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colonial_fossilization_effect,
    'To what extent did the colonial census''s reification of fluid jati categories into fixed legal identities increase the extractiveness and suppression of the localized practice norm itself?',
    'Compare pre-census (pre-1870) and post-census village records for changes in boundary rigidity, inter-caste violence, and state-mediated rent extraction. If suppression and extractiveness metrics rise sharply after census enumeration, the colonial reading''s intervention altered the localized practice''s structural properties.',
    'If the colonial intervention made the localized practice more extractive/suppressive, then the current low metrics reflect a recovery (post-independence) rather than the original state. The reading''s claim of ''always low extraction'' would need qualification: the rope was temporarily deformed into a snare by external imposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_fossilization_effect, empirical, 'Whether colonial administration structurally altered the localized practice norm''s extractiveness.').

omega_variable(
    coordination_extraction_boundary,
    'Is the reciprocal transfer (dues, conformity, endogamy) a genuine coordination cost or a disguised extraction that benefits elders and dominant lineages within the jati?',
    'Micro-level household surveys measuring net resource flows: do poorer members pay proportionally more in dues/labor than they receive in insurance/credit? If yes, the coordination function masks intra-jati extraction.',
    'If intra-jati extraction is significant, the rope classification holds only at the inter-jati level; within each jati, a tangled_rope or snare operates. The constraint story would need decomposition (inter-jati coordination vs. intra-jati hierarchy).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the coordination norm conceals internal extraction hierarchies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__localized_practice_reading, 1700, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_localized_tr_t1700, jati_practice_norm__localized_practice_reading, theater_ratio, 1700, 0.08).
narrative_ontology:measurement(jati_localized_tr_t1800, jati_practice_norm__localized_practice_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(jati_localized_tr_t1850, jati_practice_norm__localized_practice_reading, theater_ratio, 1850, 0.14).
narrative_ontology:measurement(jati_localized_tr_t1900, jati_practice_norm__localized_practice_reading, theater_ratio, 1900, 0.18).
narrative_ontology:measurement(jati_localized_tr_t1950, jati_practice_norm__localized_practice_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(jati_localized_tr_t2000, jati_practice_norm__localized_practice_reading, theater_ratio, 2000, 0.13).
narrative_ontology:measurement(jati_localized_tr_t2025, jati_practice_norm__localized_practice_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(jati_localized_be_t1700, jati_practice_norm__localized_practice_reading, base_extractiveness, 1700, 0.12).
narrative_ontology:measurement(jati_localized_be_t1800, jati_practice_norm__localized_practice_reading, base_extractiveness, 1800, 0.15).
narrative_ontology:measurement(jati_localized_be_t1850, jati_practice_norm__localized_practice_reading, base_extractiveness, 1850, 0.18).
narrative_ontology:measurement(jati_localized_be_t1900, jati_practice_norm__localized_practice_reading, base_extractiveness, 1900, 0.22).
narrative_ontology:measurement(jati_localized_be_t1950, jati_practice_norm__localized_practice_reading, base_extractiveness, 1950, 0.19).
narrative_ontology:measurement(jati_localized_be_t2000, jati_practice_norm__localized_practice_reading, base_extractiveness, 2000, 0.16).
narrative_ontology:measurement(jati_localized_be_t2025, jati_practice_norm__localized_practice_reading, base_extractiveness, 2025, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(jati_localized_su_t1700, jati_practice_norm__localized_practice_reading, suppression_requirement, 1700, 0.08).
narrative_ontology:measurement(jati_localized_su_t1800, jati_practice_norm__localized_practice_reading, suppression_requirement, 1800, 0.1).
narrative_ontology:measurement(jati_localized_su_t1850, jati_practice_norm__localized_practice_reading, suppression_requirement, 1850, 0.15).
narrative_ontology:measurement(jati_localized_su_t1900, jati_practice_norm__localized_practice_reading, suppression_requirement, 1900, 0.2).
narrative_ontology:measurement(jati_localized_su_t1950, jati_practice_norm__localized_practice_reading, suppression_requirement, 1950, 0.12).
narrative_ontology:measurement(jati_localized_su_t2000, jati_practice_norm__localized_practice_reading, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement(jati_localized_su_t2025, jati_practice_norm__localized_practice_reading, suppression_requirement, 2025, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__localized_practice_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jati_practice_norm__localized_practice_reading, 0.08).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, jati_practice_norm__colonial_census_reading).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, jati_practice_norm__orthodox_textual_reading).

% DUAL FORMULATION NOTE:
% The jati_practice_norm kernel decomposes into three structurally distinct constraints: (1) localized_practice_reading — rope, low extraction, coordination via fluid boundaries; (2) colonial_census_reading — snare/tangled_rope, high extraction, state-imposed fixed categories for revenue and control; (3) orthodox_textual_reading — mountain/piton, claims scriptural fixity but functions as identity coordination with varying enforcement. The ε values differ by an order of magnitude: this reading ε≈0.18, colonial reading ε≈0.7, orthodox reading ε≈0.05 (as textual claim) but ε≈0.4 when enforced. They are linked because the colonial reading used the orthodox reading's textual authority to legitimize its taxonomy, and the localized reading's fluidity is the empirical reality both others misrepresent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
