% ============================================================================
% CONSTRAINT STORY: udhr_article_3__procedural_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__procedural_hybrid_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: udhr_article_3__procedural_hybrid_reading
 *   human_readable: Article 3 Procedural Hybrid Reading (Due Process without Substantive Resolution)
 *   domain: constitutional_law/human_rights
 *
 * SUMMARY:
 *   Article 3 of the UDHR ('Everyone has the right to life, liberty and
 *   security of person') is read here as guaranteeing procedural due process
 *   protections—habeas corpus, prohibition of torture, access to judicial
 *   review—without resolving whether 'security of person' entails positive
 *   welfare entitlements or only negative liberty from state interference.
 *   This reading is instantiated in many constitutional systems and
 *   international jurisprudence (e.g., ICCPR Arts. 6, 7, 9). It functions as
 *   a coordination mechanism that enables states and individuals to manage
 *   the liberty/security tension procedurally, while the substantive contest
 *   remains open. The constraint extracts moderate resources from the state
 *   (courts, oversight, compliance) and suppresses certain state actions
 *   (arbitrary detention, torture), but does not settle the deeper
 *   distributive question.
 *
 * KEY AGENTS:
 *   - individuals: Primary beneficiaries (organized/constrained) — receive procedural protections
 *   - marginalized_groups: Primary beneficiaries (powerless/trapped) — depend on procedural floor
 *   - state_executive: Agenda setter and payer (institutional/arbitrage) — administers and funds the framework
 *   - judiciary: Agenda setter (institutional/analytical) — enforces procedural guarantees
 *   - security_apparatus: Payer (organized/constrained) — operationally constrained by prohibitions
 *   - international_human_rights_observers: Observer (analytical/analytical) — monitors compliance globally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__procedural_hybrid_reading, 0.45).
domain_priors:suppression_score(udhr_article_3__procedural_hybrid_reading, 0.4).
domain_priors:theater_ratio(udhr_article_3__procedural_hybrid_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__procedural_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__procedural_hybrid_reading, "Article 3 Procedural Hybrid Reading (Due Process without Substantive Resolution)").
narrative_ontology:topic_domain(udhr_article_3__procedural_hybrid_reading, "constitutional_law/human_rights").

domain_priors:requires_active_enforcement(udhr_article_3__procedural_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__procedural_hybrid_reading, '9ab6f475-0873-4e91-9387-4211c5130937').
narrative_ontology:cs_kernel_codification('9ab6f475-0873-4e91-9387-4211c5130937', formalized).
narrative_ontology:cs_authority_grounding('9ab6f475-0873-4e91-9387-4211c5130937', lineage).
narrative_ontology:cs_interpretation_layer_present('9ab6f475-0873-4e91-9387-4211c5130937').
narrative_ontology:cs_reading_relation('9ab6f475-0873-4e91-9387-4211c5130937', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ab6f475-0873-4e91-9387-4211c5130937', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_axiom('9ab6f475-0873-4e91-9387-4211c5130937', foundational, procedural_due_process_sufficient).
narrative_ontology:cs_axiom_status(procedural_due_process_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('9ab6f475-0873-4e91-9387-4211c5130937', procedural_due_process_sufficient, conventional).
narrative_ontology:cs_reference_frame('9ab6f475-0873-4e91-9387-4211c5130937', udhr_1948_procedural_compromise).
narrative_ontology:cs_drift_state('9ab6f475-0873-4e91-9387-4211c5130937', contemporary_counterterrorism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9ab6f475-0873-4e91-9387-4211c5130937', '').
narrative_ontology:cs_kernel_id(udhr_article_3__procedural_hybrid_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, individuals).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, marginalized_groups).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, state_executive).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, security_apparatus).
narrative_ontology:constraint_vindicates(udhr_article_3__procedural_hybrid_reading, due_process).
narrative_ontology:constraint_vindicates(udhr_article_3__procedural_hybrid_reading, prohibition_of_torture).
narrative_ontology:constraint_vindicates(udhr_article_3__procedural_hybrid_reading, habeas_corpus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive due process protections (habeas corpus, torture prohibition, judicial review) that constrain arbitrary state action. Their exit from the state's jurisdiction is difficult, so they rely on procedural guarantees. They benefit from the constraint's coordination function without bearing its implementation costs.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, individuals, beneficiary,
    organized, biographical, constrained, national).

% Disproportionately subject to arbitrary detention and abuse; the procedural guarantees are their primary shield. They lack political power to secure substantive welfare rights, making the procedural floor critical. Exit is effectively unavailable.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, marginalized_groups, beneficiary,
    powerless, biographical, trapped, national).

% Designs and administers the legal framework that implements Article 3. Bears the fiscal and operational costs of courts, detention oversight, and compliance with torture prohibition. Gains legitimacy from adherence but is constrained in security operations. Cannot exit the constraint without losing sovereign legitimacy.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, state_executive, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__procedural_hybrid_reading, state_executive, payer).

% Enforces the procedural guarantees through habeas corpus petitions, judicial review of detention, and torture prosecutions. Their institutional independence is both a condition for and a product of the constraint. They do not bear the fiscal costs but their docket and authority are shaped by it.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Police, intelligence, and military actors whose operational flexibility is limited by habeas corpus and torture prohibition. They bear the direct behavioral constraint and compliance costs. Some units resist through procedural evasion; others internalize the norms. Exit means leaving the service.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, security_apparatus, payer,
    organized, biographical, constrained, national).

% Monitor state compliance with Article 3's procedural guarantees through treaty bodies, special rapporteurs, and NGO reporting. They have no enforcement power but shape the reputational costs of violation. Their analytical seat sees the full structural field across jurisdictions.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, international_human_rights_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a procedural framework for protecting life and liberty through due process, habeas corpus, and torture prohibition, without prejudging the substantive scope of welfare or liberty entitlements.
% TRANSFER_FUNCTION: Transfers the burden of proof and procedural costs to the state (funding courts, restraining security forces, providing legal remedies) in exchange for legitimacy and orderly dispute resolution.
% ABSENT_VOICES: Future generations (not present to claim rights), non-citizens and stateless persons (often excluded from full due process protections), and advocates of purely negative or purely positive readings who are not represented in the procedural compromise.
% DISAPPEARANCE_RATIONALE: If procedural guarantees vanished overnight, arbitrary detention and torture would increase, executive power would expand unchecked, and the legal order would rearrange around security logic without procedural mediation. The substantive liberty/welfare contest would be resolved de facto by raw power.
% FOUNDING_PROBLEM: The need to constrain arbitrary state power over life and liberty after WWII, while avoiding commitment to either a purely negative (liberty as non-interference) or purely positive (security as material provision) conception of rights.
% FOUNDING_PROBLEM_CORROBORATION: The drafting history of the UDHR (Morsink, 1999; Glendon, 2001) shows deliberate ambiguity between negative and positive conceptions. The procedural hybrid reading reflects the actual compromise text. Legal historians and treaty body jurisprudence attest that the substantive contest remains unresolved.
narrative_ontology:disappearance_verdict(udhr_article_3__procedural_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__procedural_hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__procedural_hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(udhr_article_3__procedural_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__procedural_hybrid_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__procedural_hybrid_reading_tests).
:- end_tests(udhr_article_3__procedural_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects the state's ongoing resource commitment to courts, detention review, and torture prevention, plus the opportunity cost of forswearing certain security tactics. Suppression (0.4) captures the constraint on executive discretion—real but not total, as emergencies and national security exceptions persist. Theater ratio (0.2) is low because habeas corpus and torture prohibition are genuinely enforced in many jurisdictions, though performative compliance exists in others. Accessibility collapse (0.5) is moderate: arbitrary detention is largely illegitimized, but substantive welfare claims remain accessible as political demands. Resistance (0.5) reflects state pushback via emergency powers, counter-terrorism exceptions, and non-compliance in practice. The claimed_type tangled_rope captures the dual coordination (procedural order) and extraction (state bears costs, security apparatus constrained) structure.
 *
 * PERSPECTIVAL GAP:
 *   From the individual/marginalized seat, the constraint appears as a rope or even mountain—a vital procedural floor that makes life and liberty non-arbitrary. From the state_executive/security_apparatus seat, it appears as a snare or tangled_rope—costly, actively enforced, and constraining operational choices. The judiciary sees it as rope (coordination of dispute resolution). Observers see the full tangled_rope structure. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (individuals, marginalized_groups) are declared because they receive procedural protections without bearing implementation costs. Victims (state_executive, security_apparatus) are declared because they bear fiscal/operational costs and behavioral constraints. The state_executive is also agenda_setter—it authors the legal framework—but the constraint binds it. Directionality derivation: beneficiaries get low d (subsidy), victims get high d (extraction). The judiciary and observers sit near symmetric (d ~0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (constraining arbitrary power without resolving the liberty/welfare split) remains contested—the procedural compromise persists because neither substantive reading has achieved dominance. The constraint has not atrophied into a piton; its enforcement machinery (courts, treaty bodies) is active and expanding. Mandatrophy is not resolved; the procedural hybrid reading is the live institutional form of the unresolved contest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedural_substantive_boundary,
    'Where does the procedural guarantee end and the substantive entitlement begin? Does ''security of person'' procedurally require any material conditions (e.g., prison healthcare, protection from private violence)?',
    'Treaty body jurisprudence (HRC General Comments, ECtHR case law) progressively extends procedural obligations into substantive territory. Track the doctrinal boundary over time.',
    'If the boundary shifts toward substantive requirements, extractiveness increases and the constraint migrates toward positive_entitlement_reading. If held at pure procedure, the hybrid reading stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_substantive_boundary, conceptual, 'The irreducible ambiguity between procedural due process and substantive welfare obligations in Article 3.').

omega_variable(
    state_dual_role_agenda_setter_payer,
    'Does the state''s role as both agenda_setter (author of the legal framework) and payer (bearer of costs) create a structural conflict that distorts enforcement?',
    'Compare enforcement intensity in domains where state is sole payer (domestic detention) vs. where costs are shared (international missions, privatized detention).',
    'If the state systematically underfunds or evades the constraint it authored, effective suppression rises and the constraint degrades toward piton or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_dual_role_agenda_setter_payer, empirical, 'The state as both architect and constrained party of the procedural guarantee.').

omega_variable(
    coexistence_stability_with_siblings,
    'Can the procedural hybrid reading stably coexist with negative_liberty and positive_entitlement readings, or does doctrinal pressure force convergence toward one pole?',
    'Longitudinal study of constitutional courts and treaty bodies: track whether jurisprudence converges on a substantive theory or maintains procedural minimalism.',
    'If convergence occurs, the kernel collapses into a single reading (false summit for the hybrid). If stable coexistence persists, the hybrid reading is a genuine structural equilibrium.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coexistence_stability_with_siblings, conceptual, 'Whether the three readings form a stable triad or an unstable transitional state.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__procedural_hybrid_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t0, udhr_article_3__procedural_hybrid_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(udhr_tr_t0, observed).
narrative_ontology:measurement(udhr_tr_t15, udhr_article_3__procedural_hybrid_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement_basis(udhr_tr_t15, observed).
narrative_ontology:measurement(udhr_tr_t30, udhr_article_3__procedural_hybrid_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement_basis(udhr_tr_t30, observed).
narrative_ontology:measurement(udhr_tr_t45, udhr_article_3__procedural_hybrid_reading, theater_ratio, 45, 0.18).
narrative_ontology:measurement_basis(udhr_tr_t45, observed).
narrative_ontology:measurement(udhr_tr_t60, udhr_article_3__procedural_hybrid_reading, theater_ratio, 60, 0.19).
narrative_ontology:measurement_basis(udhr_tr_t60, observed).
narrative_ontology:measurement(udhr_tr_t75, udhr_article_3__procedural_hybrid_reading, theater_ratio, 75, 0.2).
narrative_ontology:measurement_basis(udhr_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(udhr_be_t0, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(udhr_be_t0, observed).
narrative_ontology:measurement(udhr_be_t15, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement_basis(udhr_be_t15, observed).
narrative_ontology:measurement(udhr_be_t30, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement_basis(udhr_be_t30, observed).
narrative_ontology:measurement(udhr_be_t45, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 45, 0.42).
narrative_ontology:measurement_basis(udhr_be_t45, observed).
narrative_ontology:measurement(udhr_be_t60, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 60, 0.44).
narrative_ontology:measurement_basis(udhr_be_t60, observed).
narrative_ontology:measurement(udhr_be_t75, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 75, 0.45).
narrative_ontology:measurement_basis(udhr_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t0, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(udhr_su_t0, observed).
narrative_ontology:measurement(udhr_su_t15, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement_basis(udhr_su_t15, observed).
narrative_ontology:measurement(udhr_su_t30, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(udhr_su_t30, observed).
narrative_ontology:measurement(udhr_su_t45, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 45, 0.45).
narrative_ontology:measurement_basis(udhr_su_t45, observed).
narrative_ontology:measurement(udhr_su_t60, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 60, 0.48).
narrative_ontology:measurement_basis(udhr_su_t60, observed).
narrative_ontology:measurement(udhr_su_t75, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 75, 0.4).
narrative_ontology:measurement_basis(udhr_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__procedural_hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(udhr_article_3__procedural_hybrid_reading, 0.1).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, iccpr_article_6_life).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, iccpr_article_7_torture).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, iccpr_article_9_liberty).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, echr_article_2_life).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, echr_article_3_torture).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, echr_article_5_liberty).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the udhr_article_3 kernel. The negative_liberty_reading (constraint_id: udhr_article_3__negative_liberty_reading) and positive_entitlement_reading (constraint_id: udhr_article_3__positive_entitlement_reading) are sibling constraints. All three share the same kernel text but instantiate different constraints with different ε, beneficiary/victim structures, and classifications. This reading's procedural minimums are cited as evidence by both siblings, creating the network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(udhr_article_3__procedural_hybrid_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
