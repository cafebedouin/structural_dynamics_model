% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__conditional_reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__conditional_reciprocity_reading, []).

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
 *   constraint_id: geneva_conventions_1949__conditional_reciprocity_reading
 *   human_readable: Geneva Conventions (1949) Conditional Reciprocity Reading
 *   domain: legal/international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint instantiates the conditional_reciprocity_reading of the
 *   geneva_conventions_1949 kernel. Under this reading, the 1949 Geneva
 *   Conventions function as reciprocal interstate bargains: full POW
 *   protections and civilian immunity are guaranteed only when adversaries
 *   comply with Article 4 criteria for lawful combatancy. Non-compliance by
 *   irregular forces permits states to degrade protections proportionally.
 *   The constraint coordinates genuine restraint among regular militaries in
 *   interstate wars while asymmetrically extracting legal protections from
 *   irregular combatants and narrowing civilian immunity through
 *   proportionality calculations in asymmetric conflicts.
 *
 * KEY AGENTS:
 *   - state_parties: Primary agenda-setter and beneficiary (institutional/constrained) â administers classification and gains legal flexibility
 *   - regular_militaries: Primary beneficiary (organized/constrained) â receives full POW protections reciprocally
 *   - irregular_combatants: Primary target (powerless/trapped) â excluded from full protections by Article 4 failure
 *   - civilians_in_asymmetric_conflict: Secondary target (powerless/trapped) â immunity narrowed by proportionality
 *   - international_courts: Analytical observer (institutional/analytical) â reviews but cannot compel
 *   - human_rights_ngos: Excluded voice (organized/constrained) â objects without structural seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, 0.68).
domain_priors:suppression_score(geneva_conventions_1949__conditional_reciprocity_reading, 0.7).
domain_priors:theater_ratio(geneva_conventions_1949__conditional_reciprocity_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__conditional_reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__conditional_reciprocity_reading, "Geneva Conventions (1949) Conditional Reciprocity Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__conditional_reciprocity_reading, "legal/international_humanitarian_law").

domain_priors:requires_active_enforcement(geneva_conventions_1949__conditional_reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__conditional_reciprocity_reading, '4f8ef4ee-a4ad-4bcb-bc7b-2c2e5b501ec4').
narrative_ontology:cs_kernel_codification('4f8ef4ee-a4ad-4bcb-bc7b-2c2e5b501ec4', formalized).
narrative_ontology:cs_authority_grounding('4f8ef4ee-a4ad-4bcb-bc7b-2c2e5b501ec4', lineage).
narrative_ontology:cs_interpretation_layer_present('4f8ef4ee-a4ad-4bcb-bc7b-2c2e5b501ec4').
narrative_ontology:cs_reading_relation('4f8ef4ee-a4ad-4bcb-bc7b-2c2e5b501ec4', geneva_conventions_1949__humanitarian_ceiling_reading, forecloses).
narrative_ontology:cs_reading_relation('4f8ef4ee-a4ad-4bcb-bc7b-2c2e5b501ec4', geneva_conventions_1949__security_maximization_reading, influences).
narrative_ontology:cs_axiom('4f8ef4ee-a4ad-4bcb-bc7b-2c2e5b501ec4', foundational, reciprocity_as_legal_condition).
narrative_ontology:cs_axiom_status(reciprocity_as_legal_condition, holdable).
narrative_ontology:cs_axiom_grounding('4f8ef4ee-a4ad-4bcb-bc7b-2c2e5b501ec4', reciprocity_as_legal_condition, conventional).
narrative_ontology:cs_axiom('4f8ef4ee-a4ad-4bcb-bc7b-2c2e5b501ec4', foundational, proportionality_permits_collateral_narrowing).
narrative_ontology:cs_axiom_status(proportionality_permits_collateral_narrowing, holdable).
narrative_ontology:cs_axiom_grounding('4f8ef4ee-a4ad-4bcb-bc7b-2c2e5b501ec4', proportionality_permits_collateral_narrowing, conventional).
narrative_ontology:cs_reference_frame('4f8ef4ee-a4ad-4bcb-bc7b-2c2e5b501ec4', reciprocal_interstate_restraint).
narrative_ontology:cs_drift_state('4f8ef4ee-a4ad-4bcb-bc7b-2c2e5b501ec4', asymmetric_warfare_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4f8ef4ee-a4ad-4bcb-bc7b-2c2e5b501ec4', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, state_parties).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, regular_militaries).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, irregular_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, civilians_in_asymmetric_conflict).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__conditional_reciprocity_reading, state_reciprocity_doctrine).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__conditional_reciprocity_reading, regular_warrior_privilege).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sovereign states that ratified the 1949 Conventions and administer their interpretation through military legal manuals, diplomatic statements, and detention policies. They retain discretion to classify detainees and to invoke proportionality when adversaries fail to meet Article 4 criteria.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, state_parties, agenda_setter,
    institutional, generational, constrained, global).

% Professional armed forces operating under organized command, distinctive insignia, and open carriage of arms. They receive full POW protections upon capture and legal combatant immunity for lawful acts of war, provided their adversary reciprocates.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, regular_militaries, beneficiary,
    organized, biographical, constrained, global).

% Armed group members who fail Article 4 criteria due to lack of visible command structure, failure to wear fixed distinctive signs, or failure to carry arms openly. They are classified as unlawful combatants, denied full POW status, and may be held indefinitely without the procedural timelines granted to regular prisoners of war.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, irregular_combatants, payer,
    powerless, immediate, trapped, local).

% Populations residing in areas where irregular forces operate. Their legal immunity against direct attack is preserved in principle but narrowed by proportionality calculations that permit foreseeable collateral harm when irregulars are targeted in populated areas.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, civilians_in_asymmetric_conflict, payer,
    powerless, immediate, trapped, local).

% International criminal tribunals and the ICJ that review state conduct against humanitarian law. They periodically challenge narrow reciprocity interpretations but lack enforcement power to compel states to abandon the conditional framework.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, international_courts, observer,
    institutional, generational, analytical, global).

% Organizations advocating for absolute humanitarian minimums and opposing the unlawful-combatant category. They are consulted by states only at discretion and hold no seat in treaty interpretation or military targeting decisions.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, human_rights_ngos, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__conditional_reciprocity_reading, state_parties).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__conditional_reciprocity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes reciprocal restraint among sovereign states to limit the destructiveness of interstate war by guaranteeing POW protections and baseline civilian immunity, contingent on adversary compliance with defined criteria for lawful combatancy.
% TRANSFER_FUNCTION: Moves legal protections, procedural guarantees, and combatant immunity from irregular combatants and civilians in asymmetric conflicts to state parties and regular militaries, permitting proportional degradation when adversaries fail to meet Article 4 criteria.
% ABSENT_VOICES: Irregular combatants have no seat at the drafting or interpretation table; human rights NGOs advocating absolute humanitarian minimums are consulted only at state discretion and hold no veto; affected civilian populations in conflict zones are represented only through state proxies.
% DISAPPEARANCE_RATIONALE: If the conditional reciprocity framework disappeared overnight, states would lose the legal architecture for classifying detainees as unlawful combatants, detention regimes would require immediate relegalization under broader protections, and the current equilibrium permitting proportional degradation would collapse toward either absolute protections or total state discretion.
% FOUNDING_PROBLEM: Mid-20th century interstate warfare produced mass atrocities against prisoners of war and civilian populations; states sought reciprocal incentives for compliance with humanitarian norms while preserving military necessity.
% FOUNDING_PROBLEM_CORROBORATION: State parties and regular militaries attest the problem remains live in interstate contexts. Human rights NGOs, ICRC commentaries, and ICTY jurisprudence attest the founding problem has shifted to asymmetric conflicts where the conditional framework itself enables abuse; independent judicial opinions from outside the benefiting parties corroborate that absolute minimums are needed regardless of reciprocity.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__conditional_reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__conditional_reciprocity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__conditional_reciprocity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_1949__conditional_reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__conditional_reciprocity_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as tangled_rope because it possesses both a genuine coordination function (reciprocal POW protections and civilian safeguards among states) and asymmetric extraction (denial of protections to irregulars, proportional narrowing of civilian immunity). Extractiveness (0.68) is substantial because the Article 4 criteria create a legal void that concentrates harm on non-state actors. Suppression (0.70) is high because the framework persists through active state enforcement: military legal manuals, detention classifications, and diplomatic insistence on reciprocity. Theater ratio (0.50) reflects significant performative maintenance â states invoke Geneva rhetoric while operating legal black holes such as unlawful-combatant designations and targeted-killing frameworks that depart from the Conventions' original interstate logic. Resistance (0.60) captures sustained pushback from international courts and human rights organizations.
 *
 * PERSPECTIVAL GAP:
 *   State parties and regular militaries experience the constraint as a necessary coordination mechanism that incentivizes compliance with humanitarian norms. Irregular combatants and affected civilians experience it as a legalized structure of exclusion and collateral risk. The engine computes this divergence from the structural data: beneficiaries with constrained interstate exit options sit at low directionality, while trapped non-state actors sit at high directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   State parties and regular militaries are structural beneficiaries of the conditional framework (low d): they gain legal predictability in interstate wars and retain discretionary latitude in asymmetric ones. Irregular combatants and civilians in asymmetric conflicts are structural targets (high d): they bear the costs of degraded protections and proportionality trade-offs. International courts and human rights NGOs occupy analytical and excluded positions respectively, with no extractive flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination function â reciprocal restraint among regular state militaries â is genuine and partially live, preventing reclassification as pure snare. However, the constraint has drifted substantially from its founding interstate context toward asymmetric warfare, where the reciprocity mechanism rarely binds. This drift risks mandatrophy: if interstate war becomes rare and the constraint persists primarily to license extraction from irregulars, it would approach snare. The current contested status of the founding problem (live for states, dead for human rights advocates) reflects precisely this ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_4_extraction_vs_incentive,
    'Does the Article 4 criteria gap genuinely incentivize irregular forces to regularize, or does it primarily create a legal void enabling state extraction?',
    'Comparative empirical analysis of state detention classifications versus actual insurgent organizational behavior across conflicts where the Conventions apply.',
    'If regularization incentives are ineffective, the constraint is extraction-dominant and the coordination story is cover; if effective, the extraction is partially justified as coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_4_extraction_vs_incentive, empirical, 'Whether Article 4 operates as incentive or extraction mechanism').

omega_variable(
    proportionality_as_extraction,
    'Do proportionality calculations in asymmetric conflicts structurally protect civilians, or do they systematically permit greater harm when irregulars are present?',
    'Empirical comparison of civilian casualty rates in asymmetric versus symmetric conflicts under IHL, controlling for intensity and terrain.',
    'Systematically higher civilian harm in asymmetric conflicts would indicate proportionality functions as extraction from civilians rather than genuine protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_as_extraction, empirical, 'Proportionality calculus as protective or extractive').

omega_variable(
    reading_stability,
    'Is the conditional reciprocity reading stabilizing or destabilizing the broader Geneva kernel?',
    'Longitudinal tracking of state reservations, judicial challenges, and protocol ratification rates over the measurement interval.',
    'If the reading destabilizes the kernel by encouraging non-compliance or non-ratification, the constraint family may drift toward security_maximization or humanitarian_ceiling attractors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_stability, conceptual, 'Conditional reciprocity reading''s effect on kernel stability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__conditional_reciprocity_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gene_tr_t15, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(gene_tr_t30, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(gene_tr_t45, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 45, 0.4).
narrative_ontology:measurement(gene_tr_t60, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 60, 0.55).
narrative_ontology:measurement(gene_tr_t75, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 75, 0.5).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(gene_be_t15, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(gene_be_t30, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(gene_be_t45, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 45, 0.62).
narrative_ontology:measurement(gene_be_t60, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 60, 0.72).
narrative_ontology:measurement(gene_be_t75, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 75, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(gene_su_t15, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement(gene_su_t30, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(gene_su_t45, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 45, 0.62).
narrative_ontology:measurement(gene_su_t60, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 60, 0.75).
narrative_ontology:measurement(gene_su_t75, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 75, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__conditional_reciprocity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, security_maximization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the geneva_conventions_1949 kernel, decomposed per the epsilon-invariance principle. The conditional_reciprocity_reading, humanitarian_ceiling_reading, and security_maximization_reading instantiate structurally distinct claims from the same treaty text, with different epsilon values, beneficiary/victim structures, and failure modes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
