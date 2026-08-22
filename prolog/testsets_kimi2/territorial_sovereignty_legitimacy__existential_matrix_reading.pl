% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__existential_matrix_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__existential_matrix_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: territorial_sovereignty_legitimacy__existential_matrix_reading
 *   human_readable: Existential Territorial Sovereignty Matrix
 *   domain: political/international_relations
 *
 * SUMMARY:
 *   This constraint story instantiates the existential_matrix_reading of the
 *   contested kernel territorial_sovereignty_legitimacy. Under this reading,
 *   the standing arrangement is that sovereignty over the contested territory
 *   is treated not as a juridical question resolvable by law or history, but
 *   as an existential precondition for collective survival and identity
 *   expression. This framing produces a structurally zero-sum conflict:
 *   whichever people achieves military and demographic dominance secures
 *   survival, while the other bears the costs of dispossession and
 *   statelessness. Legal settlements and compromise frameworks are
 *   epiphenomenal because neither side can accept the vulnerability of shared
 *   or divided sovereignty. The constraint is actively enforced through
 *   territorial control, demographic consolidation, and military dominance,
 *   and it suppresses alternatives that would treat the territory as
 *   divisible or sovereignty as non-exclusive.
 *
 * KEY AGENTS:
 *   - dominant_territorial_actor (institutional/identity_locked) â agenda-setter and beneficiary; controls territory and enforces exclusivity
 *   - subordinate_territorial_community (organized/trapped) â primary payer; bears costs of dispossession and denied sovereignty
 *   - international_juridical_order (institutional/analytical) â excluded; provides legal frameworks overridden by security logic
 *   - great_power_arbiters (institutional/analytical) â observer; sponsors compromise frameworks that fail at the existential threshold
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.88).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.92).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__existential_matrix_reading, snare).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__existential_matrix_reading, "Existential Territorial Sovereignty Matrix").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__existential_matrix_reading, "political/international_relations").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__existential_matrix_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__existential_matrix_reading, 'a4134451-062e-4f19-86c9-844ad84c836c').
narrative_ontology:cs_kernel_codification('a4134451-062e-4f19-86c9-844ad84c836c', implicit).
narrative_ontology:cs_authority_grounding('a4134451-062e-4f19-86c9-844ad84c836c', self_enforcing).
narrative_ontology:cs_reading_relation('a4134451-062e-4f19-86c9-844ad84c836c', territorial_sovereignty_legitimacy__covenant_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('a4134451-062e-4f19-86c9-844ad84c836c', territorial_sovereignty_legitimacy__self_determination_reading, coexists_with).
narrative_ontology:cs_axiom('a4134451-062e-4f19-86c9-844ad84c836c', foundational, territorial_control_existential_prerequisite).
narrative_ontology:cs_axiom_status(territorial_control_existential_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('a4134451-062e-4f19-86c9-844ad84c836c', territorial_control_existential_prerequisite, empirically_contingent).
narrative_ontology:cs_axiom('a4134451-062e-4f19-86c9-844ad84c836c', foundational, juridical_legitimacy_epiphenomenal).
narrative_ontology:cs_axiom_status(juridical_legitimacy_epiphenomenal, holdable).
narrative_ontology:cs_axiom_grounding('a4134451-062e-4f19-86c9-844ad84c836c', juridical_legitimacy_epiphenomenal, empirically_contingent).
narrative_ontology:cs_reference_frame('a4134451-062e-4f19-86c9-844ad84c836c', existential_territorial_primacy).
narrative_ontology:cs_drift_state('a4134451-062e-4f19-86c9-844ad84c836c', contemporary_peace_process_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a4134451-062e-4f19-86c9-844ad84c836c', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, dominant_territorial_actor).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, subordinate_territorial_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the contested territory through military dominance and demographic consolidation. Justifies control as existential necessity for collective survival and identity expression. Cannot accept territorial compromise without violating its core security narrative. Administers the enforcement apparatus that maintains exclusive control.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, dominant_territorial_actor, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__existential_matrix_reading, dominant_territorial_actor, beneficiary).

% Bears the costs of denied sovereignty, territorial dispossession, and statelessness. Subject to military and administrative control that prevents autonomous territorial governance. Exit options are limited to displacement or subjugation; sovereignty claims are structurally overridden by the dominant actor's existential security framing.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, subordinate_territorial_community, payer,
    organized, generational, trapped, national).

% Provides legal frameworks for territorial partition and shared sovereignty, including UN resolutions and international legal opinions. Rendered epiphenomenal by the existential matrix; its instruments are acknowledged but overridden whenever they conflict with the security imperatives of the territorial actors.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, international_juridical_order, excluded,
    institutional, civilizational, analytical, global).

% Mediate and sponsor compromise frameworks such as two-state solutions and security guarantees. Their proposals repeatedly fail when they encounter the existential threshold of the territorial actors. They observe and document the constraint but lack enforcement capacity to alter its zero-sum structure.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, great_power_arbiters, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__existential_matrix_reading, dominant_territorial_actor).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__existential_matrix_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to coordinate collective survival and identity security by assigning exclusive territorial control to one people, thereby preventing the existential vulnerability that would follow from shared or divided sovereignty.
% TRANSFER_FUNCTION: Transfers territorial control, security, and demographic dominance from the subordinate territorial community to the dominant territorial actor; transfers existential risk and statelessness from the dominant actor to the subordinate community.
% ABSENT_VOICES: International juridical actors advocating shared sovereignty and binational frameworks are structurally excluded; they would argue for territorial compromise but are overridden by the security threshold. The subordinate territorial community's juridical claims are formally heard but operationally excluded.
% DISAPPEARANCE_RATIONALE: If the existential matrix vanished overnight, territorial compromise frameworks would become structurally viable; the conflict would shift from a zero-sum survival contest to a divisible juridical dispute over borders, rights, and governance. The dominant actor would lose its monopoly on existential justification.
% FOUNDING_PROBLEM: The absence of a higher enforcement authority capable of guaranteeing collective security to two competing national movements claiming the same territory, creating a security dilemma where each fears annihilation by the other.
% FOUNDING_PROBLEM_CORROBORATION: Security studies scholars and regional historians attest to the persistence of existential security dilemmas in the territory. Neutral analysts outside the beneficiary set document the existential framing, though they dispute its irreducibility and point to power-sharing models elsewhere as evidence that the problem admits non-zero-sum solutions.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__existential_matrix_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__existential_matrix_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__existential_matrix_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) because the constraint denies sovereignty, territory, and security to the subordinate community regardless of juridical merit. Suppression is higher (0.92) because compromise frameworks are actively suppressed by military and administrative enforcement. Theater_ratio is substantial (0.65): legal and historical arguments are deployed as cover, but the constraint's persistence is driven by existential fear rather than juridical legitimacy. Accessibility_collapse is high (0.88) because once the existential frame is adopted, power-sharing alternatives become unthinkable. Resistance is high (0.78) from the subordinate community and from international actors whose frameworks are overridden. The measurement series track a steady intensification as enforcement capacity and territorial facts on the ground hardened over the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the dominant actor's seat, the constraint appears as necessary survival architecture without which collective annihilation is imminent; from the subordinate community's seat, it appears as enforced dispossession and denial of national existence; from the observer seats, it appears as a zero-sum trap generated by mutual existential dread. The engine computes this divergence from the same structural data rather than resolving it.
 *
 * DIRECTIONALITY LOGIC:
 *   The dominant_territorial_actor sits near the full-beneficiary end (low d): the constraint subsidizes its security through territorial monopoly and demographic dominance. The subordinate_territorial_community sits near the full-target end (high d): the constraint extracts sovereignty, territory, and existential security from it. The international_juridical_order and great_power_arbiters occupy analytical seats with no directional extraction. The engine will compute asymmetric effective extraction from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling as coordination by showing that the survival narrative, while grounded in a genuine founding security dilemma, has become a cover for pure extraction. The coordination story (exclusive territorial control as mutual security) would require symmetric benefit; instead, security is delivered asymmetrically to the dominant actor while the subordinate community pays the existential cost. The founding problem remains live, but the constraint's form has become extractive because it actively suppresses power-sharing alternatives that could also address the security dilemma.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_reading_kernel_location,
    'This constraint instantiates the existential_matrix_reading of kernel territorial_sovereignty_legitimacy; would the sibling readings covenant_continuity_reading or self_determination_reading reassign the beneficiary and victim roles, or only the legitimating narrative?',
    'Author and compile sibling constraint stories for the same kernel; compare base_properties.beneficiaries, victims, and per-seat directionalities across the family.',
    'If beneficiaries swap across readings, the extraction is narrative-dependent and the kernel contest is structural; if beneficiaries are stable, the kernel contest is purely doctrinal cover for an underlying fixed extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(existential_reading_kernel_location, conceptual, 'Kernel reading committer ambiguity for territorial sovereignty legitimacy').

omega_variable(
    existential_claim_empirical_status,
    'Is territorial control actually an irreducible precondition for collective survival and identity, or have stateless peoples, diasporic communities, and binational arrangements demonstrated viable alternatives?',
    'Comparative historical analysis of stateless peoples'' survival rates, diaspora identity persistence across generations, and binational or consociational models.',
    'If viable alternatives exist, the constraint''s base_extractiveness is inflated by a false premise and its theater_ratio rises; if the claim is empirically robust, the extraction is structurally necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(existential_claim_empirical_status, empirical, 'Whether territorial control is truly existentially necessary').

omega_variable(
    compromise_suppression_source,
    'Is the suppression of territorial compromise driven by structural enforcement (military control, settlement facts on the ground) or by internalized existential fear that would persist even after structural barriers fell?',
    'Post-settlement or post-withdrawal referendum behavior and population movement patterns: if populations reject compromise even after leadership signatures, suppression is partially internalized.',
    'If internalized, effective suppression exceeds the structural measure and the constraint is more deeply embedded than institutional analysis suggests; if purely structural, removal of enforcement could permit rapid rearrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compromise_suppression_source, empirical, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__existential_matrix_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_tr_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_tr_t10, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_tr_t20, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_tr_t30, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 30, 0.6).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_tr_t40, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 40, 0.63).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_tr_t50, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 50, 0.65).

% Extraction over time
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_be_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_be_t10, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_be_t20, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_be_t30, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_be_t40, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 40, 0.86).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_be_t50, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 50, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_su_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_su_t10, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_su_t20, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 20, 0.84).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_su_t30, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 30, 0.87).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_su_t40, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 40, 0.9).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_su_t50, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 50, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, self_determination_reading).

% DUAL FORMULATION NOTE:
% The kernel territorial_sovereignty_legitimacy decomposes into three structurally distinct readings. This existential reading treats legitimacy claims as epiphenomenal to security dynamics, while siblings treat legitimacy as genuinely grounded in covenantal or democratic principles. Each reading carries its own epsilon, beneficiaries, and victim structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
