% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__composite_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__composite_reading
 *   human_readable: Honor Settlement Legitimacy — Composite Reading
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   The composite reading of honor_settlement_legitimacy models dueling's
 *   decline (c. 1750–1900) as overdetermined by converging causal pathways.
 *   The dominant pathway is cultural contraction: the cognitive framework
 *   that made dueling intelligible as a legitimate honor-settlement mechanism
 *   became unthinkable across elite European and American societies. This
 *   contraction is reinforced by independent material and institutional
 *   mechanisms that would have suppressed the practice even absent cultural
 *   transformation: state legal monopolies criminalizing private violence,
 *   bourgeois professional codes replacing aristocratic honor with
 *   contractual reputation, clerical moral campaigns reframing dueling as
 *   murder, and military institutionalization replacing officer honor with
 *   disciplinary codes. The constraint's extraction operates by denying the
 *   honor-settlement pathway to those still embedded in the old framework
 *   while the new framework's beneficiaries (state, bourgeoisie, clergy)
 *   collect legitimacy and monopoly rents. The constraint is classified as
 *   piton because the primary coordination function (honor-based conflict
 *   resolution among status-equals) has atrophied, but the suppression
 *   machinery persists through institutional inertia and the
 *   theological/moral cover story that private violence is inherently
 *   illegitimate.
 *
 * KEY AGENTS:
 *   - aristocratic_officer_corps: Primary target (organized/identity_locked) — bears extraction as honor-settlement pathway closes
 *   - state_legal_monopoly: Primary beneficiary (institutional/arbitrage) — collects monopoly on legitimate violence
 *   - bourgeois_professional_classes: Beneficiary (organized/mobile) — replaces honor with contractual reputation
 *   - clerical_moral_authority: Beneficiary (institutional/constrained) — collects moral authority over violence
 *   - provincial_gentry_honor_culture: Victim (powerless/identity_locked) — loses honor-settlement mechanism with no replacement
 *   - military_academy_traditionalists: Victim (organized/identity_locked) — institutional honor codes suppressed by state discipline
 *   - legal_formalist_observers: Observer (analytical/analytical) — sees full structural transition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__composite_reading, 0.68).
domain_priors:suppression_score(honor_settlement_legitimacy__composite_reading, 0.82).
domain_priors:theater_ratio(honor_settlement_legitimacy__composite_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__composite_reading, piton).
narrative_ontology:human_readable(honor_settlement_legitimacy__composite_reading, "Honor Settlement Legitimacy — Composite Reading").
narrative_ontology:topic_domain(honor_settlement_legitimacy__composite_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__composite_reading, '1116ce39-d892-45f6-88b9-1dae98bb7ac7').
narrative_ontology:cs_kernel_codification('1116ce39-d892-45f6-88b9-1dae98bb7ac7', distributed).
narrative_ontology:cs_authority_grounding('1116ce39-d892-45f6-88b9-1dae98bb7ac7', extraction).
narrative_ontology:cs_interpretation_layer_present('1116ce39-d892-45f6-88b9-1dae98bb7ac7').
narrative_ontology:cs_reading_relation('1116ce39-d892-45f6-88b9-1dae98bb7ac7', honor_settlement_legitimacy__contraction_reading, influences).
narrative_ontology:cs_reading_relation('1116ce39-d892-45f6-88b9-1dae98bb7ac7', honor_settlement_legitimacy__drop_reading, influences).
narrative_ontology:cs_axiom('1116ce39-d892-45f6-88b9-1dae98bb7ac7', foundational, decline_overdetermined_by_converging_pathways).
narrative_ontology:cs_axiom_status(decline_overdetermined_by_converging_pathways, holdable).
narrative_ontology:cs_axiom_grounding('1116ce39-d892-45f6-88b9-1dae98bb7ac7', decline_overdetermined_by_converging_pathways, empirically_contingent).
narrative_ontology:cs_axiom('1116ce39-d892-45f6-88b9-1dae98bb7ac7', foundational, cultural_contraction_dominates_but_requires_reinforcement).
narrative_ontology:cs_axiom_status(cultural_contraction_dominates_but_requires_reinforcement, holdable).
narrative_ontology:cs_axiom_grounding('1116ce39-d892-45f6-88b9-1dae98bb7ac7', cultural_contraction_dominates_but_requires_reinforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('1116ce39-d892-45f6-88b9-1dae98bb7ac7', honor_settlement_as_legitimate_violence_monopoly).
narrative_ontology:cs_drift_state('1116ce39-d892-45f6-88b9-1dae98bb7ac7', long_nineteenth_century_transition, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1116ce39-d892-45f6-88b9-1dae98bb7ac7', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, state_legal_monopoly).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, bourgeois_professional_classes).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, clerical_moral_authority).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, aristocratic_officer_corps).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, provincial_gentry_honor_culture).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, military_academy_traditionalists).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__composite_reading, state_monopoly_on_violence).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__composite_reading, legal_formalism_over_private_violence).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__composite_reading, bourgeois_civic_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Officers whose professional identity and status hierarchy are constituted through the honor-settlement mechanism. They cannot exit the honor framework without ceasing to be officers in the cultural sense. The constraint extracts their primary conflict-resolution pathway and replaces it with state military justice that does not recognize honor as a legitimate domain.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, aristocratic_officer_corps, payer,
    organized, biographical, identity_locked, continental).

% The state apparatus (courts, police, penal codes) that administers the prohibition on private violence. It sets the legal agenda, enforces the suppression, and collects the monopoly rent on legitimate violence. It can arbitrage across jurisdictions and historical periods — the constraint is its instrument, not its master.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, state_legal_monopoly, agenda_setter,
    institutional, generational, arbitrage, national).

% Lawyers, merchants, civil servants, journalists whose status derives from contractual reputation and professional credentials rather than honor. They benefit from the constraint because it eliminates a rival status hierarchy and secures the legal-contractual framework their professions depend on. They can exit the honor framework easily — they were never fully in it.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, bourgeois_professional_classes, beneficiary,
    organized, biographical, mobile, continental).

% Church hierarchies (Catholic, Protestant) that campaigned against dueling as murder and sin. They collect moral authority over the definition of legitimate violence. Their exit is constrained because their doctrinal commitment to non-violence is identity-constitutive, but they are not targets of the constraint.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, clerical_moral_authority, beneficiary,
    institutional, generational, constrained, continental).

% Landowning gentry in peripheral regions (American South, Eastern Europe, rural France/Italy) whose local status hierarchy operates entirely through honor-settlement. They have no access to the new legal-contractual framework and cannot exit the honor framework without social death. They bear the full extraction with no replacement mechanism.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, provincial_gentry_honor_culture, payer,
    powerless, biographical, identity_locked, regional).

% Academy instructors and alumni who maintain the honor-code tradition (e.g., West Point, Saint-Cyr, Prussian cadet schools) against state-imposed disciplinary codes. Their institutional identity is fused with the honor-settlement mechanism. They cannot exit without abandoning the institutional tradition that defines them.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, military_academy_traditionalists, payer,
    organized, biographical, identity_locked, national).

% Jurists, historians, sociologists who analyze the transition from private to public violence monopoly. They neither collect nor pay; they observe the structural shift from outside the honor framework.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, legal_formalist_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Dueling provided a decentralized, status-sensitive mechanism for resolving honor disputes among equals without state intervention — coordinating expectations about retaliation, apology, and satisfaction in a world where courts were inaccessible or inadequate for honor questions.
% TRANSFER_FUNCTION: The constraint transfers the monopoly on legitimate violence from private honor-settlement to the state legal apparatus, transfers status-determination from honor-performance to contractual/professional reputation, and transfers moral authority over violence from the duelist's conscience to clerical doctrine. The transfer is from aristocratic/gentry honor-culture participants to state, bourgeois, and clerical beneficiaries.
% ABSENT_VOICES: The provincial gentry and military traditionalists (payers) were never adequately represented in the legislative, judicial, or professional bodies that suppressed dueling. Their objection would have been that the replacement mechanisms (courts, contracts) do not address honor questions — but they lacked the vocabulary to articulate this in the new framework's terms.
% DISAPPEARANCE_RATIONALE: If the prohibition on dueling and its cultural unthinkability vanished overnight, the honor-settlement mechanism would not spontaneously revive — the cultural framework is gone. But the state's violence monopoly would face a legitimacy challenge in honor-dispute contexts, and residual honor cultures (military academies, Southern gentry descendents, European aristocratic associations) would likely attempt to reconstruct the practice. The world would rearrange around the vacuum.
% FOUNDING_PROBLEM: In early modern Europe and its colonies, aristocratic and gentry men lacked reliable state courts for disputes involving honor, reputation, and status among equals. Dueling emerged as a self-enforcing mechanism to settle these disputes without state intervention, coordinating expectations about insult, challenge, and satisfaction.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's death is attested by state legal historians (e.g., Elias on civilizing process, Foucault on discipline), not by the constraint's beneficiaries. The state, bourgeoisie, and clergy all benefited from the problem's death and the constraint's persistence. No beneficiary-attested source claims the honor-dispute problem remains live in its original form.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(honor_settlement_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__composite_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_settlement_legitimacy__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_settlement_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness 0.68: the constraint extracts the honor-settlement pathway from honor-culture participants without providing a functional equivalent, while beneficiaries collect monopoly rents. Suppression 0.82: high because the constraint's persistence depends on active legal prohibition, professional sanctions, and moral condemnation — not merely cultural drift. Theater ratio 0.45: substantial performative maintenance (dueling codes, seconds' rituals, courtroom performances of honor) persists after the functional core has atrophied. Accessibility collapse 0.78: once the cultural framework shifts, the honor-settlement pathway becomes cognitively inaccessible — alternatives (courts, contracts) are structurally available but culturally unintelligible to the honor-bound. Resistance 0.35: modest because resistance is fractured — aristocratic resistance is identity-locked and cannot articulate a counter-framework; bourgeois and clerical actors actively support the new order.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute strong seat divergence: from the state/bourgeois/clerical beneficiary seats, the constraint appears as scaffold (transitional coordination toward legal order) or even rope (genuine coordination of violence monopoly). From the aristocratic officer/gentry payer seats, it appears as snare (pure extraction of honor-settlement with no exit). The composite reading's structural claim is that this divergence IS the constraint — the overdetermined convergence means no single seat's experience captures the whole.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: state_legal_monopoly (collects violence monopoly rents), bourgeois_professional_classes (collects contractual reputation market), clerical_moral_authority (collects moral authority). Victims: aristocratic_officer_corps (loses honor-settlement, identity-locked into officer corps), provincial_gentry_honor_culture (loses honor-settlement, trapped in declining social formation), military_academy_traditionalists (loses institutional honor codes, identity-locked into academy tradition). Directionality derives from beneficiary/victim declarations + exit options: identity_locked victims have no exit from the honor framework, so d approaches 1.0; institutional beneficiaries with arbitrage/mobile exit have d near 0.0.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits classic mandatrophy: the founding problem (honor-based conflict resolution among status-equals in a world without reliable courts) is dead (courts, contracts, police exist), but the suppression machinery persists. The mandate has outlived its function — the cultural contraction made the practice unthinkable, yet legal prohibitions and professional sanctions remain active. This is not a snare because no concentrated beneficiary actively maintains it for extraction; it is a piton because the constraint persists through institutional inertia and the moral cover story that private violence is inherently illegitimate (a vindicated proposition that collects no rents but legitimizes the suppression).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading (composite_reading) of the contested kernel honor_settlement_legitimacy. What structural elements distinguish it from sibling readings contraction_reading and drop_reading?',
    'Comparative analysis of causal weight assignments across readings: composite assigns primary weight to cultural contraction reinforced by independent material/institutional mechanisms; contraction_reading assigns near-total weight to cultural unthinkability; drop_reading assigns weight to persistent fringe practice.',
    'If the composite reading''s multi-pathway causal structure is empirically supported, it predicts different temporal dynamics and residual practice patterns than either sibling reading alone. The engine''s cs_structure.forecloses/influences relations depend on this distinction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commiter-frame identity: this constraint instantiates the composite reading of honor_settlement_legitimacy kernel').

omega_variable(
    contraction_vs_reinforcement_boundary,
    'Where does the cultural contraction mechanism end and the reinforcing material/institutional mechanisms begin? Are they analytically separable or fused?',
    'Counterfactual decomposition: would dueling have declined at the same rate if only one pathway operated? Historical natural experiments (e.g., jurisdictions with strong legal suppression but persistent honor culture vs. weak legal suppression with cultural transformation).',
    'If separable, the constraint is a tangled_rope of overlapping mechanisms; if fused, it is a single piton where cultural unthinkability has absorbed the institutional scaffolding. Affects cs_structure.reading_relations: separable mechanisms support ''influences'' between readings; fused mechanisms support ''forecloses''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contraction_vs_reinforcement_boundary, conceptual, 'Boundary between primary contraction driver and reinforcing pathways in composite reading').

omega_variable(
    residual_practice_ontology,
    'Do the late-19th/early-20th century residual duels (e.g., French academic duels, German Mensur, Southern US affairs of honor) represent the drop_reading''s persistent fringe, or are they structurally distinct phenomena that the composite reading misclassifies?',
    'Structural comparison of residual practices: do they operate under the same honor-settlement logic, or have they mutated into ritualized sport, institutional hazing, or performative tradition?',
    'If residual practices are structurally distinct, the composite reading''s ''overdetermined decline'' claim holds for the core practice but requires a separate constraint story for the residual forms. If they are the same practice persisting, the drop_reading captures a real structural remainder that the composite reading understates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(residual_practice_ontology, empirical, 'Ontological status of residual dueling practices relative to composite vs. drop readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__composite_reading, 1750, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_settlement_legitimacy__composite_reading_tr_t1750, honor_settlement_legitimacy__composite_reading, theater_ratio, 1750, 0.15).
narrative_ontology:measurement(honor_settlement_legitimacy__composite_reading_tr_t1780, honor_settlement_legitimacy__composite_reading, theater_ratio, 1780, 0.22).
narrative_ontology:measurement(honor_settlement_legitimacy__composite_reading_tr_t1810, honor_settlement_legitimacy__composite_reading, theater_ratio, 1810, 0.32).
narrative_ontology:measurement(honor_settlement_legitimacy__composite_reading_tr_t1840, honor_settlement_legitimacy__composite_reading, theater_ratio, 1840, 0.41).
narrative_ontology:measurement(honor_settlement_legitimacy__composite_reading_tr_t1870, honor_settlement_legitimacy__composite_reading, theater_ratio, 1870, 0.45).
narrative_ontology:measurement(honor_settlement_legitimacy__composite_reading_tr_t1900, honor_settlement_legitimacy__composite_reading, theater_ratio, 1900, 0.45).

% Extraction over time
narrative_ontology:measurement(honor_settlement_legitimacy__composite_reading_be_t1750, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1750, 0.35).
narrative_ontology:measurement(honor_settlement_legitimacy__composite_reading_be_t1780, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1780, 0.42).
narrative_ontology:measurement(honor_settlement_legitimacy__composite_reading_be_t1810, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1810, 0.55).
narrative_ontology:measurement(honor_settlement_legitimacy__composite_reading_be_t1840, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1840, 0.62).
narrative_ontology:measurement(honor_settlement_legitimacy__composite_reading_be_t1870, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1870, 0.68).
narrative_ontology:measurement(honor_settlement_legitimacy__composite_reading_be_t1900, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1900, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(honor_settlement_legitimacy__composite_reading_su_t1750, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1750, 0.45).
narrative_ontology:measurement(honor_settlement_legitimacy__composite_reading_su_t1780, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1780, 0.55).
narrative_ontology:measurement(honor_settlement_legitimacy__composite_reading_su_t1810, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1810, 0.68).
narrative_ontology:measurement(honor_settlement_legitimacy__composite_reading_su_t1840, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1840, 0.75).
narrative_ontology:measurement(honor_settlement_legitimacy__composite_reading_su_t1870, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1870, 0.82).
narrative_ontology:measurement(honor_settlement_legitimacy__composite_reading_su_t1900, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1900, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__composite_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(honor_settlement_legitimacy__composite_reading, 0.12).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, state_violence_monopoly_consolidation).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, bourgeois_contractual_reputation_formation).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, clerical_moral_campaign_anti_dueling).

% DUAL FORMULATION NOTE:
% honor_settlement_legitimacy kernel decomposes into three constraint stories: composite_reading (this file), contraction_reading, drop_reading. The composite reading assigns causal weight to cultural contraction reinforced by independent material/institutional mechanisms. The contraction_reading assigns near-total weight to cultural framework transformation. The drop_reading assigns weight to persistent fringe practice. They form a constraint family linked by affects_constraints. The composite reading influences both siblings (creates structural pressure on their causal claims) but does not foreclose them — all three remain live historiographical positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_settlement_legitimacy__composite_reading, organized, 0.75).
constraint_indexing:directionality_override(honor_settlement_legitimacy__composite_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
