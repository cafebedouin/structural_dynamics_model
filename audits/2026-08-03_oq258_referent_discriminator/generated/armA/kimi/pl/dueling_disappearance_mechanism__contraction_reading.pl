% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__contraction_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: dueling_disappearance_mechanism__contraction_reading
 *   human_readable: Dueling Unthinkability via Dignity-Culture Displacement (Contraction Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint story instantiates the contraction_reading of the
 *   dueling_disappearance_mechanism kernel: the claim that dueling became
 *   culturally unthinkable because dignity-culture axioms morally and
 *   cognitively displaced honor-culture axioms, rendering the latter
 *   illegible rather than merely illegal. In this reading, the disappearance
 *   is not primarily institutional substitution (courts, libel law, banking)
 *   nor overdetermined causation, but a contraction of the imaginable:
 *   honor-culture practitioners became unable to articulate their own norms
 *   within the new moral vocabulary. The constraint is claimed as mountain
 *   because dignity culture operates as an irreversible substrate — no party
 *   enforces it, no party collects from it, and it persists by cultural
 *   naturalization. The victim set is limited to honor-culture practitioners
 *   whose framework became unintelligible.
 *
 * KEY AGENTS:
 *   - honor_culture_practitioners: Primary target (moderate/identity_locked) — bears cultural illegibility and framework erasure
 *   - cultural_historians: Analytical observer (analytical/analytical) — traces the structural displacement and its historiographic contestation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, 0.15).
domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, 0.18).
domain_priors:theater_ratio(dueling_disappearance_mechanism__contraction_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(dueling_disappearance_mechanism__contraction_reading, "Dueling Unthinkability via Dignity-Culture Displacement (Contraction Reading)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__contraction_reading, '55e27342-ef85-4f68-af2b-88fe75dd4bf0').
narrative_ontology:cs_kernel_codification('55e27342-ef85-4f68-af2b-88fe75dd4bf0', distributed).
narrative_ontology:cs_authority_grounding('55e27342-ef85-4f68-af2b-88fe75dd4bf0', distributed).
narrative_ontology:cs_reading_relation('55e27342-ef85-4f68-af2b-88fe75dd4bf0', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('55e27342-ef85-4f68-af2b-88fe75dd4bf0', dueling_disappearance_mechanism__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('55e27342-ef85-4f68-af2b-88fe75dd4bf0', foundational, dignity_universalism).
narrative_ontology:cs_axiom_status(dignity_universalism, holdable).
narrative_ontology:cs_axiom_grounding('55e27342-ef85-4f68-af2b-88fe75dd4bf0', dignity_universalism, deontological).
narrative_ontology:cs_axiom('55e27342-ef85-4f68-af2b-88fe75dd4bf0', foundational, honor_modern_incompatibility).
narrative_ontology:cs_axiom_status(honor_modern_incompatibility, holdable).
narrative_ontology:cs_axiom_grounding('55e27342-ef85-4f68-af2b-88fe75dd4bf0', honor_modern_incompatibility, empirically_contingent).
narrative_ontology:cs_reference_frame('55e27342-ef85-4f68-af2b-88fe75dd4bf0', dignity_culture_substrate).
narrative_ontology:cs_drift_state('55e27342-ef85-4f68-af2b-88fe75dd4bf0', post_displacement_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('55e27342-ef85-4f68-af2b-88fe75dd4bf0', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Military officers, Southern gentry, and others whose social identity is constituted through honor-based status and personal redress. Their normative framework for managing insult through armed combat became socially illegible, pathologized, and legally penalized under emerging dignity-culture norms. They cannot practice their customs without facing ostracism, criminal prosecution, or irreversible loss of standing. Exit would require abandoning an identity-fusion relationship to honor that constitutes their self-concept and social position.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners, payer,
    moderate, biographical, identity_locked, national).

% Analyze the transition from honor to dignity culture as a historiographic and sociological structure. They observe that dueling's disappearance is read by contraction theorists as a naturalized cultural substrate and by institutionalists as active substitution, tracing how honor-culture practitioners were rendered voiceless within the new normative framework.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, cultural_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social order by rendering interpersonal violence culturally unthinkable, replacing honor-based redress with dignity-based forbearance and institutional dispute processing.
% TRANSFER_FUNCTION: Transfers the right and capacity to resolve grave insults from individual armed redress to institutional and legal processing; transfers social legitimacy from honor-based status hierarchies to egalitarian dignity frameworks.
% ABSENT_VOICES: Honor-culture practitioners themselves are absent from dignity-culture historiography except as objects of ridicule, pathology, or romantic nostalgia; their own normative vocabulary for insult, redress, and masculine honor is rendered unintelligible within dignity-culture discourse, and they hold no seat in the conversation that classified their practices as barbaric.
% DISAPPEARANCE_RATIONALE: If the dignity-culture taboo on dueling vanished overnight, interpersonal violence as a legitimate redress mechanism would regain cultural intelligibility, the state's monopoly on legitimate violence would weaken, and social status mechanisms would shift back toward honor-based frameworks — the social world would rearrange around a renewed pluralism of dispute cultures.
% FOUNDING_PROBLEM: Chronic social instability, extra-legal killing, and blood feuds arising from honor-based insult cultures that lacked a monopoly on legitimate violence; the need to establish bourgeois social order and state jurisdiction over interpersonal violence.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and state-building theorists outside the honor-culture tradition attest that the state successfully monopolized violence; however, these same observers note that the cultural mechanism of displacement is contested, that the honor framework was rendered unintelligible rather than outcompeted, and that no corroboration exists from within the honor-culture tradition itself, which experienced the transition as erasure rather than problem-solving.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__contraction_reading, 0.15, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(dueling_disappearance_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the constraint does not actively transfer resources; it operates by rendering honor-culture practices unintelligible rather than by taxing them. Suppression is low (0.18) because the prohibition requires no visible enforcement machinery once internalized. Accessibility collapse is very high (0.92) because dignity-culture norms make the honor alternative not merely illegal but cognitively unthinkable. Resistance is negligible (0.05) — the constraint meets no organized opposition because its targets are identity-locked into a framework that dignity culture has pathologized. The temporal series show a life-cycle peak during the transition (mid-interval) when active social sanction was highest, declining toward naturalization by interval end.
 *
 * PERSPECTIVAL GAP:
 *   The honor-culture practitioner seat experiences the constraint as active erasure of their normative framework (high effective extraction due to identity lock), while from the dignity-culture adherent position the same arrangement appears as simply the way civilized society naturally operates (zero perceived extraction). The analytical observer seat sees the historical contingency of the shift. The engine will compute divergent per-seat classifications: the payer seat may read as snare or tangled_rope due to its high derived chi, while the absent dignity-culture beneficiary seat (if modeled) would read as mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary seat is declared, consistent with the mountain claim that no party collects from this constraint. The honor_culture_practitioners are declared in the victims array and carry identity_locked exit options, which drives their derived directionality toward the full-target end (d ≈ 1.0). The engine will compute high effective extraction for this seat despite the low base epsilon, because the identity lock amplifies the structural cost of the constraint. No directionality override is needed — the derivation chain produces the correct asymmetry automatically.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unregulated honor violence) is dead, and the arrangement has persisted as cultural substrate rather than active coordination. This creates a mandatrophy-like profile (dead founding problem + world_rearranges if removed) that risks misclassifying the constraint as a piton or snare. However, the key distinction from piton is the absence of theatrical maintenance: the theater_ratio is low (0.10) and declining, there is no agenda_setter administering the constraint, and the suppression_requirement trends toward zero as the norm naturalizes. The constraint persists not by institutional inertia but by complete cultural naturalization. The low resistance and high accessibility collapse differentiate it from a snare, which would require active suppression of alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_mountain_or_construct,
    'Is the dignity-culture prohibition on dueling a genuine irreversible cultural substrate, or a constructed normative order maintained by diffuse social sanction that benefits dignity-culture adherents?',
    'Cross-cultural and historical comparison examining whether societies without Western dignity-culture frameworks maintain dueling or analogous honor practices, and whether dignity-culture frameworks can reverse or erode under crisis conditions.',
    'If the taboo is reversible or absent in comparable societies, the constraint is likely a constructed snare or tangled rope rather than a mountain; if universally stable once established, the mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_mountain_or_construct, empirical, 'Whether dignity-culture dominance is natural law or constructed norm.').

omega_variable(
    sibling_reading_structural_delta,
    'How would this constraint''s classification change if the institutional_displacement_reading or overdetermined_composite_reading were adopted instead of the contraction_reading?',
    'Comparison with sibling constraint stories: the institutional reading would identify courts, bankers, and legal professionals as concentrated beneficiaries with higher extractiveness and active enforcement; the overdetermined reading would distribute causation across multiple mechanisms, likely yielding a tangled_rope or scaffold classification rather than mountain.',
    'Adopting a sibling reading would shift the constraint from mountain to rope, tangled_rope, or scaffold, with higher suppression, theater_ratio, and identifiable agenda_setters.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Structural delta between this reading and sibling readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__contraction_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t0, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(duel_tr_t16, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(duel_tr_t32, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(duel_tr_t48, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 48, 0.32).
narrative_ontology:measurement(duel_tr_t64, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 64, 0.18).
narrative_ontology:measurement(duel_tr_t80, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 80, 0.1).

% Extraction over time
narrative_ontology:measurement(duel_be_t0, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(duel_be_t16, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 16, 0.3).
narrative_ontology:measurement(duel_be_t32, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 32, 0.35).
narrative_ontology:measurement(duel_be_t48, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 48, 0.28).
narrative_ontology:measurement(duel_be_t64, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 64, 0.2).
narrative_ontology:measurement(duel_be_t80, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 80, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t0, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(duel_su_t16, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 16, 0.38).
narrative_ontology:measurement(duel_su_t32, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 32, 0.55).
narrative_ontology:measurement(duel_su_t48, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 48, 0.42).
narrative_ontology:measurement(duel_su_t64, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 64, 0.28).
narrative_ontology:measurement(duel_su_t80, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 80, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, institutional_displacement_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is the contraction_reading of kernel dueling_disappearance_mechanism, instantiating the dignity-culture displacement explanation. Sibling readings include institutional_displacement_reading (institutional substitution outcompeting dueling) and overdetermined_composite_reading (multiple independent sufficient causes). The epsilon values differ across readings: this reading treats the outcome as naturalized cultural substrate with low extraction, while sibling readings treat it as actively enforced or institutionally substituted with higher extraction and identifiable beneficiaries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
