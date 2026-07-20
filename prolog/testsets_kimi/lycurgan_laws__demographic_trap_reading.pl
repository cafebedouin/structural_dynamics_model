% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__demographic_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__demographic_trap_reading, []).

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
 *   constraint_id: lycurgan_laws__demographic_trap_reading
 *   human_readable: Lycurgan Constitutional Immutability as Demographic Trap
 *   domain: political/constitutional/social
 *
 * SUMMARY:
 *   This constraint story instantiates the demographic_trap_reading of the
 *   lycurgan_laws kernel. It treats the constitutional immutability of the
 *   Spartan rhetra not as sacred fidelity or adaptive fiction, but as a
 *   structurally extractive snare: the prohibition on revision prevented
 *   demographic and economic adaptation, producing a declining Spartiate
 *   population and systemic brittleness. The constraint is the
 *   institutionalized unrevisability itself, enforced by the gerousia and
 *   ephors, with the kleros and syssitia systems as its material
 *   infrastructure.
 *
 * KEY AGENTS:
 *   - gerousia: Agenda-setter (institutional/arbitrage) â guards the immutable tradition and interprets the rhetra; authority derives from Lycurgan lineage.
 *   - ephors: Agenda-setter/beneficiary (institutional/arbitrage) â enforces daily discipline; power contingent on the immutability frame.
 *   - spartiate_citizens: Primary payer (organized/identity_locked) â bears the demographic cost of a system that fuses their identity with unrevisable obligations.
 *   - perioikoi: Excluded victim (moderate/constrained) â economically active but politically barred from citizenship or reform.
 *   - helots: Payer (powerless/trapped) â surplus-extracted laborers maintained by the same enforcement apparatus.
 *   - reformist_kings: Excluded (powerful/constrained) â attempted revision and were crushed, revealing the suppression mechanism.
 *   - historical_analyst: Observer (analytical) â modern seat seeing the structural trap.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, 0.82).
domain_priors:suppression_score(lycurgan_laws__demographic_trap_reading, 0.88).
domain_priors:theater_ratio(lycurgan_laws__demographic_trap_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__demographic_trap_reading, snare).
narrative_ontology:human_readable(lycurgan_laws__demographic_trap_reading, "Lycurgan Constitutional Immutability as Demographic Trap").
narrative_ontology:topic_domain(lycurgan_laws__demographic_trap_reading, "political/constitutional/social").

domain_priors:requires_active_enforcement(lycurgan_laws__demographic_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__demographic_trap_reading, '7b24ff06-82bd-4ba9-98e1-5f37082013c6').
narrative_ontology:cs_kernel_codification('7b24ff06-82bd-4ba9-98e1-5f37082013c6', fixed_text).
narrative_ontology:cs_authority_grounding('7b24ff06-82bd-4ba9-98e1-5f37082013c6', lineage).
narrative_ontology:cs_interpretation_layer_present('7b24ff06-82bd-4ba9-98e1-5f37082013c6').
narrative_ontology:cs_reading_relation('7b24ff06-82bd-4ba9-98e1-5f37082013c6', lycurgan_laws__sacral_fidelity_reading, influences).
narrative_ontology:cs_reading_relation('7b24ff06-82bd-4ba9-98e1-5f37082013c6', lycurgan_laws__adaptive_fiction_reading, coexists_with).
narrative_ontology:cs_axiom('7b24ff06-82bd-4ba9-98e1-5f37082013c6', foundational, unrevisable_constitution_demographic_death_spiral).
narrative_ontology:cs_axiom_status(unrevisable_constitution_demographic_death_spiral, holdable).
narrative_ontology:cs_axiom_grounding('7b24ff06-82bd-4ba9-98e1-5f37082013c6', unrevisable_constitution_demographic_death_spiral, empirically_contingent).
narrative_ontology:cs_reference_frame('7b24ff06-82bd-4ba9-98e1-5f37082013c6', lycurgan_constitutional_order).
narrative_ontology:cs_drift_state('7b24ff06-82bd-4ba9-98e1-5f37082013c6', classical_decline, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('7b24ff06-82bd-4ba9-98e1-5f37082013c6', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__demographic_trap_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, gerousia).
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, ephors).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, spartiate_citizens).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, perioikoi).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, helots).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, spartiate_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Council of elders whose authority derives from guarding the immutable Lycurgan tradition. They interpret the rhetra and block any constitutional revision, preserving their institutional position as the sole legitimate transmitters of the ancestral order.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, gerousia, agenda_setter,
    institutional, generational, arbitrage, national).

% Five elected magistrates enforcing daily Lycurgan discipline. Their power and prestige depend on the immutability frame; they actively suppress reform proposals and police citizen behavior, benefiting from the authority the unrevisable constitution grants them.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, ephors, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__demographic_trap_reading, ephors, beneficiary).

% Full citizens bound by the agoge, syssitia, and kleros obligations. Their identity is fused with Spartan exceptionalism; they cannot abandon the system without losing status, yet the immutable citizenship and property rules cause their class to shrink through debt, inability to pay mess dues, and restricted marriage practices.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, spartiate_citizens, payer,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__demographic_trap_reading, spartiate_citizens, beneficiary).

% Free non-citizen inhabitants of Laconia and Messenia who are economically active but politically disenfranchised. The immutable citizenship rules structurally bar them from upward mobility or constitutional reform that would grant them Spartiate status.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, perioikoi, excluded,
    moderate, biographical, constrained, national).

% State-bound agricultural laborers whose surplus is extracted to free Spartiates for military training. They are held in place by military terror and the same constitutional order that prevents reform of the labor regime.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, helots, payer,
    powerless, biographical, trapped, national).

% Hereditary monarchs such as Agis IV and Cleomenes III who attempted land and constitutional reform to reverse demographic decline. They were blocked, exiled, or executed by the gerousia and ephors, revealing that the suppression machinery exists to prevent revision.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, reformist_kings, excluded,
    powerful, biographical, constrained, national).

% Modern political theorist analyzing the Lycurgan system as a case study in constitutional brittleness. This seat sees the structural trap: immutability enforced by a small elite producing demographic collapse across the citizen body.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, historical_analyst, observer,
    analytical, generational, analytical, global).

narrative_ontology:fixing_cost_class(lycurgan_laws__demographic_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally solved archaic internal instability and Messenian subjugation by freezing property relations, homogenizing the citizen-warrior class, and securing helot labor.
% TRANSFER_FUNCTION: Moves agricultural surplus from helots to Spartiate citizens; moves demographic risk and military obligation onto the Spartiate class; moves political authority to gerousia and ephors by constitutionalizing an immutable order.
% ABSENT_VOICES: Indebted Spartiates falling out of the syssitia, perioikoi seeking citizenship, helots seeking emancipation, and reformist kings were structurally excluded from constitutional revision. Their objections were suppressed by the religious sanctity attached to the Lycurgan frame.
% DISAPPEARANCE_RATIONALE: If the rhetra's immutability vanished, property and citizenship rules could have adjusted to demographic pressureâdebt relief, new enrollments, economic diversificationâhalting the Spartiate death spiral and reorganizing Laconian society around adaptable institutions.
% FOUNDING_PROBLEM: Post-conquest Messenia and internal unrest in archaic Sparta required a framework to homogenize the citizen-warrior class, prevent elite accumulation, and secure helot subjugation.
% FOUNDING_PROBLEM_CORROBORATION: Ancient historians Plutarch and Xenophon attest the founding crisis from outside the gerousia's direct interest. Modern demographic and land-tenure studies corroborate that the original conditions of archaic instability had disappeared by the classical period, while the constraint persisted into destructive obsolescence.
narrative_ontology:disappearance_verdict(lycurgan_laws__demographic_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__demographic_trap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__demographic_trap_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lycurgan_laws__demographic_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__demographic_trap_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__demographic_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lycurgan_laws__demographic_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.82) because the immutable framework extracted citizen numbers, labor, and adaptive capacity without reciprocal adjustment. Suppression is higher (0.88) because the system actively crushed reformers and sacralized the prohibition on revision. Theater_ratio (0.45) reflects that early military coordination became increasingly performative as Spartiate manpower dwindled: the agoge and syssitia continued as ritual even as the citizen body hollowed out. Accessibility_collapse (0.85) is high because alternative political arrangements were ideologically barred by the sanctity of the rhetra. Resistance (0.35) is moderate-to-low because identity-lock and terror suppressed open dissent until late, desperate reform attempts emerged.
 *
 * PERSPECTIVAL GAP:
 *   The gerousia and ephors experience the constraint as legitimate authority preserving a necessary order. The Spartiate citizen experiences it as an inexorable identity they cannot revise without annihilating their social self. The helot experiences it as bare coercion. The reformist king experiences it as a rigid wall that kills reformers. The historical analyst sees all seats simultaneously and recognizes the trap.
 *
 * DIRECTIONALITY LOGIC:
 *   Gerousia and ephors are structural beneficiaries (low d) because the immutability is the source of their authority and enforcement role. Spartiate citizens, perioikoi, and helots are structural targets (high d) because they bear the costs of demographic rigidity, exclusion, and labor extraction. Reformist kings are excluded (high d) because their very proposals threaten the immutable frame. The analyst sits outside the directionality calculation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was originally built to solve a genuine coordination problem: archaic instability and Messenian subjugation. By the classical period that founding problem was dead, yet the arrangement persisted without a sunset clause. This profile might suggest piton, but the active enforcement of immutability against reformersâAgis IV executed, Cleomenes III exiledâdemonstrates ongoing extraction and suppression rather than mere inertia. The constraint is therefore a snare: a once-functional coordination mechanism converted into pure extraction maintained by coercion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_internalization_depth,
    'Was the prohibition on constitutional revision internalized as core Spartan identity, or maintained purely by gerousia and ephors coercion?',
    'Compare the fate of reform attempts: if reformers faced popular backlash rooted in identity violation, suppression is internalized; if they faced only institutional punishment, suppression is structural.',
    'If internalized, effective suppression exceeds structural measures; the constraint operates as identity_coordination with high extraction. If purely structural, reform was institutionally blocked but culturally possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_depth, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    collapse_monocausality,
    'Can the demographic collapse be attributed primarily to constitutional immutability, or did external shocks (earthquakes, war casualties, helot revolts) dominate?',
    'Counterfactual demographic modeling controlling for external shocks; compare Sparta''s trajectory with other Greek poleis facing similar shocks but permitting reform.',
    'If external shocks dominated, base_extractiveness should be revised downward; if immutability was the binding constraint, the snare classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collapse_monocausality, empirical, 'Primary causal driver of Spartiate demographic decline').

omega_variable(
    kernel_reading_underdetermination,
    'Does the demographic trap reading capture the full normative structure of the Lycurgan kernel, or does it require the sacral and adaptive readings to account for the laws'' persistence?',
    'Cross-reference with the sacral_fidelity_reading and adaptive_fiction_reading constraints; evaluate whether materialist causation alone explains institutional stability over centuries.',
    'If underdetermined, the classification is stable but incomplete; if the sacral reading is required to explain persistence, the authority_grounding may need additional lineage weighting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Materialist reading sufficiency for kernel classification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__demographic_trap_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycurgan_demo_tr_t0, lycurgan_laws__demographic_trap_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(lycurgan_demo_tr_t100, lycurgan_laws__demographic_trap_reading, theater_ratio, 100, 0.25).
narrative_ontology:measurement(lycurgan_demo_tr_t200, lycurgan_laws__demographic_trap_reading, theater_ratio, 200, 0.32).
narrative_ontology:measurement(lycurgan_demo_tr_t300, lycurgan_laws__demographic_trap_reading, theater_ratio, 300, 0.38).
narrative_ontology:measurement(lycurgan_demo_tr_t400, lycurgan_laws__demographic_trap_reading, theater_ratio, 400, 0.42).
narrative_ontology:measurement(lycurgan_demo_tr_t500, lycurgan_laws__demographic_trap_reading, theater_ratio, 500, 0.45).

% Extraction over time
narrative_ontology:measurement(lycurgan_demo_be_t0, lycurgan_laws__demographic_trap_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(lycurgan_demo_be_t100, lycurgan_laws__demographic_trap_reading, base_extractiveness, 100, 0.52).
narrative_ontology:measurement(lycurgan_demo_be_t200, lycurgan_laws__demographic_trap_reading, base_extractiveness, 200, 0.61).
narrative_ontology:measurement(lycurgan_demo_be_t300, lycurgan_laws__demographic_trap_reading, base_extractiveness, 300, 0.7).
narrative_ontology:measurement(lycurgan_demo_be_t400, lycurgan_laws__demographic_trap_reading, base_extractiveness, 400, 0.78).
narrative_ontology:measurement(lycurgan_demo_be_t500, lycurgan_laws__demographic_trap_reading, base_extractiveness, 500, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(lycurgan_demo_su_t0, lycurgan_laws__demographic_trap_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(lycurgan_demo_su_t100, lycurgan_laws__demographic_trap_reading, suppression_requirement, 100, 0.65).
narrative_ontology:measurement(lycurgan_demo_su_t200, lycurgan_laws__demographic_trap_reading, suppression_requirement, 200, 0.72).
narrative_ontology:measurement(lycurgan_demo_su_t300, lycurgan_laws__demographic_trap_reading, suppression_requirement, 300, 0.8).
narrative_ontology:measurement(lycurgan_demo_su_t400, lycurgan_laws__demographic_trap_reading, suppression_requirement, 400, 0.85).
narrative_ontology:measurement(lycurgan_demo_su_t500, lycurgan_laws__demographic_trap_reading, suppression_requirement, 500, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, adaptive_fiction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the lycurgan_laws kernel, decomposed from the colloquial label 'Lycurgan laws' into three structurally distinct claims: sacral_fidelity_reading (divine ordinance), adaptive_fiction_reading (noble lie masking covert adaptation), and demographic_trap_reading (structural snare causing demographic collapse). Each reading has its own epsilon, beneficiary/victim structure, and classification, linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
