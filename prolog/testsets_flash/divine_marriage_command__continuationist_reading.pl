% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__continuationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__continuationist_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: divine_marriage_command__continuationist_reading
 *   human_readable: Divine Marriage Command (Continuationist Reading)
 *   domain: religious/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the 'continuationist' reading of the divine
 *   command for plural marriage within a specific religious tradition. This
 *   reading asserts that the original command remains doctrinally valid, and
 *   the 1890 Manifesto, which officially suspended the practice of polygamy,
 *   was a prudential response to federal duress (legal persecution,
 *   confiscation of property, disenfranchisement), not a doctrinal rescission
 *   or new revelation. This interpretation allows for theological continuity
 *   with the past while navigating present legal realities, but creates
 *   significant tension for those who wish to practice polygamy and for the
 *   mainstream church leadership that must enforce its current monogamous
 *   policy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, 0.6).
domain_priors:suppression_score(divine_marriage_command__continuationist_reading, 0.7).
domain_priors:theater_ratio(divine_marriage_command__continuationist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__continuationist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__continuationist_reading, "Divine Marriage Command (Continuationist Reading)").
narrative_ontology:topic_domain(divine_marriage_command__continuationist_reading, "religious/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(divine_marriage_command__continuationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__continuationist_reading, '94f4ffc4-3bcc-4eb2-a87b-1ccf23efa41f').
narrative_ontology:cs_kernel_codification('94f4ffc4-3bcc-4eb2-a87b-1ccf23efa41f', fixed_text).
narrative_ontology:cs_authority_grounding('94f4ffc4-3bcc-4eb2-a87b-1ccf23efa41f', lineage).
narrative_ontology:cs_interpretation_layer_present('94f4ffc4-3bcc-4eb2-a87b-1ccf23efa41f').
narrative_ontology:cs_reading_relation('94f4ffc4-3bcc-4eb2-a87b-1ccf23efa41f', divine_marriage_command__substitutionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('94f4ffc4-3bcc-4eb2-a87b-1ccf23efa41f', divine_marriage_command__coercion_visibility_reading, coexists_with).
narrative_ontology:cs_axiom('94f4ffc4-3bcc-4eb2-a87b-1ccf23efa41f', foundational, divine_command_is_immutable).
narrative_ontology:cs_axiom_status(divine_command_is_immutable, holdable).
narrative_ontology:cs_axiom_grounding('94f4ffc4-3bcc-4eb2-a87b-1ccf23efa41f', divine_command_is_immutable, theological).
narrative_ontology:cs_axiom('94f4ffc4-3bcc-4eb2-a87b-1ccf23efa41f', foundational, manifesto_is_prudential_suspension).
narrative_ontology:cs_axiom_status(manifesto_is_prudential_suspension, holdable).
narrative_ontology:cs_axiom_grounding('94f4ffc4-3bcc-4eb2-a87b-1ccf23efa41f', manifesto_is_prudential_suspension, conventional).
narrative_ontology:cs_reference_frame('94f4ffc4-3bcc-4eb2-a87b-1ccf23efa41f', original_divine_command_unmodified).
narrative_ontology:cs_drift_state('94f4ffc4-3bcc-4eb2-a87b-1ccf23efa41f', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('94f4ffc4-3bcc-4eb2-a87b-1ccf23efa41f', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__continuationist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, continuationist_adherents).
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, fundamentalist_splinter_groups).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, polygamous_families_in_mainstream_church).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, mainstream_church_leadership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals believe the divine command for plural marriage remains valid and that the Manifesto was a temporary, prudential suspension. They derive theological legitimacy and identity from this interpretation, even if they do not actively practice polygamy due to legal constraints. They benefit from the doctrinal continuity.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, continuationist_adherents, beneficiary,
    moderate, generational, identity_locked, local).

% These groups actively practice and preach plural marriage, claiming direct continuity with the original revelation and rejecting the mainstream church's interpretation of the Manifesto as a doctrinal change. They enforce their own internal rules based on this continuationist reading, often at significant legal risk.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, fundamentalist_splinter_groups, agenda_setter,
    organized, generational, constrained, regional).

% Families who, due to historical or personal reasons, continue to practice polygamy while attempting to remain within the broader cultural orbit of the mainstream church. They face social ostracization, legal risks, and internal conflict due to the church's official stance, yet their theological identity is tied to the continuationist reading.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, polygamous_families_in_mainstream_church, payer,
    powerless, biographical, trapped, local).

% The leadership of the mainstream church, which officially disavows polygamy and excommunicates practitioners, yet must contend with the historical and doctrinal legacy of the continuationist reading. They pay the cost of managing internal dissent and external scrutiny, while also setting and enforcing the current (monogamous) policy.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, mainstream_church_leadership, payer,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__continuationist_reading, mainstream_church_leadership, agenda_setter).

% The legal system that criminalizes polygamy, creating the external duress that led to the Manifesto. It acts as an external constraint on the religious practice, forcing the church to adapt its public stance and enforcement mechanisms.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, federal_legal_system, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the theological understanding of marriage and divine command for adherents who believe in the enduring validity of plural marriage, providing a framework for their identity and community, even if not actively practiced.
% TRANSFER_FUNCTION: Transfers theological legitimacy and a sense of continuity with foundational revelation to continuationist adherents and splinter groups. It imposes social and legal costs on those who practice polygamy within or adjacent to the mainstream church.
% ABSENT_VOICES: Those who left the mainstream church due to its perceived abandonment of core doctrines, or those who were excommunicated for practicing polygamy, are absent from the mainstream discourse. They would argue that the church has compromised divine command for worldly acceptance.
% DISAPPEARANCE_RATIONALE: If this continuationist reading vanished, the theological basis for fundamentalist splinter groups would collapse, and the internal tension within the mainstream church regarding its past and present doctrines would resolve. The identity of many adherents would be fundamentally altered, requiring a complete re-evaluation of their faith's history.
% FOUNDING_PROBLEM: The problem of reconciling a divine command for plural marriage with external legal and social pressures that criminalized and condemned it, while maintaining the integrity of the religious community and its core beliefs.
% FOUNDING_PROBLEM_CORROBORATION: The problem is live for continuationist adherents and splinter groups, who continue to face legal and social challenges. For the mainstream church, the problem is 'contested' as they claim it is resolved, but the persistence of splinter groups and internal historical debates suggests otherwise. Independent historians and sociologists corroborate the ongoing tension and the historical context of duress.
narrative_ontology:disappearance_verdict(divine_marriage_command__continuationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__continuationist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__continuationist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(divine_marriage_command__continuationist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__continuationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__continuationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) is substantial, as adherents who follow this reading (especially splinter groups) face legal penalties and social ostracization, while the mainstream church pays costs in managing this historical and doctrinal tension. Suppression (0.7) is high due to the active enforcement of anti-polygamy laws by the federal government and the mainstream church's internal disciplinary actions against practitioners. The theater ratio (0.4) reflects the performative aspect of the mainstream church's disavowal of polygamy, which is partly a response to external pressure while the underlying doctrinal validity remains contested by some adherents. The accessibility collapse (0.4) is moderate, as alternatives (like joining splinter groups or leaving the church) exist but come with high costs. Resistance (0.5) is also moderate, manifested in the continued existence of fundamentalist groups and ongoing internal debates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of continuationist adherents, this reading is a faithful preservation of divine command, a 'rope' of theological continuity. From the perspective of the mainstream church leadership, it is a 'snare' of historical baggage that complicates their modern identity and mission. The federal legal system views it as a 'mountain' of settled law. The engine's classification as a 'tangled_rope' captures the hybrid nature: a coordination function for those who identify with the original command, but with significant asymmetric extraction and active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Continuationist adherents and fundamentalist splinter groups are beneficiaries of this reading, as it provides their theological grounding and identity. However, splinter groups also act as agenda-setters by actively enforcing polygamy. Polygamous families within the mainstream church are victims, bearing the direct costs of legal and social pressure. The mainstream church leadership is also a victim, paying the cost of managing the internal conflict and external perception, while also acting as an agenda-setter for its current (monogamous) policy. The federal legal system is an external agenda-setter, enforcing the constraint through law.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preserving the divine command for plural marriage) is still 'live' for continuationist adherents, preventing a full mandatrophy resolution. However, for the mainstream church, the original problem (practicing polygamy under duress) has shifted to managing the legacy of that practice. The 'tangled_rope' classification prevents mislabeling it as a 'snare' (ignoring the coordination for adherents) or a 'rope' (ignoring the extraction and enforcement). The persistence of the continuationist reading, despite external pressure, highlights the deep identity-lock for its adherents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    manifesto_nature_ambiguity,
    'Was the 1890 Manifesto a prudential suspension of practice under duress (continuationist reading) or a new revelation superseding prior command (substitutionist reading)?',
    'Further theological scholarship, discovery of new historical documents, or a definitive pronouncement from a universally recognized religious authority that clarifies the doctrinal status of the Manifesto.',
    'If resolved as a new revelation, the continuationist reading would be foreclosed, and the constraint would shift towards a ''substitutionist'' interpretation, likely reducing internal conflict for the mainstream church. If resolved as purely prudential, the continuationist reading gains stronger internal legitimacy, potentially increasing external conflict.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(manifesto_nature_ambiguity, conceptual, 'Ambiguity regarding the doctrinal nature of the 1890 Manifesto.').

omega_variable(
    identity_lock_strength,
    'To what extent is the identity of continuationist adherents truly ''identity_locked'' versus ''constrained'' by social and familial ties?',
    'Longitudinal studies of individuals who leave continuationist communities: if identity crisis persists after social ties are severed, identity-lock is stronger.',
    'If identity-lock is weaker, exit options are more ''constrained'' than ''identity_locked'', potentially lowering effective extraction for some individuals. If stronger, the constraint''s hold is deeper than structural factors alone suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'Strength of identity-lock for continuationist adherents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__continuationist_reading, 1890, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1890, divine_marriage_command__continuationist_reading, theater_ratio, 1890, 0.1).
narrative_ontology:measurement_basis(divi_tr_t1890, observed).
narrative_ontology:measurement(divi_tr_t1920, divine_marriage_command__continuationist_reading, theater_ratio, 1920, 0.2).
narrative_ontology:measurement_basis(divi_tr_t1920, observed).
narrative_ontology:measurement(divi_tr_t1950, divine_marriage_command__continuationist_reading, theater_ratio, 1950, 0.3).
narrative_ontology:measurement_basis(divi_tr_t1950, observed).
narrative_ontology:measurement(divi_tr_t1980, divine_marriage_command__continuationist_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement_basis(divi_tr_t1980, observed).
narrative_ontology:measurement(divi_tr_t2010, divine_marriage_command__continuationist_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement_basis(divi_tr_t2010, observed).
narrative_ontology:measurement(divi_tr_t2024, divine_marriage_command__continuationist_reading, theater_ratio, 2024, 0.4).
narrative_ontology:measurement_basis(divi_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(divi_be_t1890, divine_marriage_command__continuationist_reading, base_extractiveness, 1890, 0.8).
narrative_ontology:measurement_basis(divi_be_t1890, observed).
narrative_ontology:measurement(divi_be_t1920, divine_marriage_command__continuationist_reading, base_extractiveness, 1920, 0.75).
narrative_ontology:measurement_basis(divi_be_t1920, observed).
narrative_ontology:measurement(divi_be_t1950, divine_marriage_command__continuationist_reading, base_extractiveness, 1950, 0.7).
narrative_ontology:measurement_basis(divi_be_t1950, observed).
narrative_ontology:measurement(divi_be_t1980, divine_marriage_command__continuationist_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement_basis(divi_be_t1980, observed).
narrative_ontology:measurement(divi_be_t2010, divine_marriage_command__continuationist_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement_basis(divi_be_t2010, observed).
narrative_ontology:measurement(divi_be_t2024, divine_marriage_command__continuationist_reading, base_extractiveness, 2024, 0.6).
narrative_ontology:measurement_basis(divi_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1890, divine_marriage_command__continuationist_reading, suppression_requirement, 1890, 0.9).
narrative_ontology:measurement_basis(divi_su_t1890, observed).
narrative_ontology:measurement(divi_su_t1920, divine_marriage_command__continuationist_reading, suppression_requirement, 1920, 0.85).
narrative_ontology:measurement_basis(divi_su_t1920, observed).
narrative_ontology:measurement(divi_su_t1950, divine_marriage_command__continuationist_reading, suppression_requirement, 1950, 0.8).
narrative_ontology:measurement_basis(divi_su_t1950, observed).
narrative_ontology:measurement(divi_su_t1980, divine_marriage_command__continuationist_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement_basis(divi_su_t1980, observed).
narrative_ontology:measurement(divi_su_t2010, divine_marriage_command__continuationist_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement_basis(divi_su_t2010, observed).
narrative_ontology:measurement(divi_su_t2024, divine_marriage_command__continuationist_reading, suppression_requirement, 2024, 0.7).
narrative_ontology:measurement_basis(divi_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__continuationist_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, divine_marriage_command__substitutionist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, divine_marriage_command__coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'divine_marriage_command' kernel, each representing a distinct structural interpretation of the same foundational religious text and historical events. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
