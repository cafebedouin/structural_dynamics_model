% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__contraction_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: honor_violence_legitimacy__contraction_reading
 *   human_readable: Honor Redefined to Exclude Violence (Contraction Reading)
 *   domain: historical_sociology/legal_anthropology
 *
 * SUMMARY:
 *   This constraint story models the contraction reading of the
 *   honor-violence legitimacy kernel: the historical process by which dueling
 *   became structurally unthinkable through a redefinition of honor itself to
 *   exclude violent response. Rather than treating dueling as merely costly
 *   or legally penalized, this reading asserts that the conceptual space of
 *   legitimate honor responses contracted—violence was expelled from the
 *   semantic field of honorable conduct. The constraint is the stabilized
 *   social arrangement that enforces this redefinition: a new honor code that
 *   coordinates masculine elites away from private violence while
 *   asymmetrically extracting from those whose status depended on the old
 *   martial code. The claim is tangled_rope because the arrangement genuinely
 *   solves a collective-action problem (the dueling trap) but also enforces a
 *   unilateral devaluation of martial cultural capital.
 *
 * KEY AGENTS:
 *   - state_legal_apparatus: Agenda-setter (institutional/national) — codifies and enforces the legal prohibition on dueling, claims monopoly on legitimate violence.
 *   - bourgeois_gentry: Primary beneficiary (powerful/national) — their honor based on propriety and commerce is elevated as violent honor is delegitimized.
 *   - professional_mediators: Secondary beneficiary (moderate/national) — lawyers, clergy, physicians absorb disputes formerly resolved through violence.
 *   - martial_aristocracy: Primary payer/victim (powerful but identity-locked/national) — their cultural capital is devalued and their customary responses criminalized.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, 0.4).
domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, 0.58).
domain_priors:theater_ratio(honor_violence_legitimacy__contraction_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, 0.84).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__contraction_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__contraction_reading, "Honor Redefined to Exclude Violence (Contraction Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__contraction_reading, "historical_sociology/legal_anthropology").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__contraction_reading, 'c96211ac-f4b7-42e8-ab51-cb0da8289327').
narrative_ontology:cs_kernel_codification('c96211ac-f4b7-42e8-ab51-cb0da8289327', distributed).
narrative_ontology:cs_authority_grounding('c96211ac-f4b7-42e8-ab51-cb0da8289327', practice).
narrative_ontology:cs_interpretation_layer_present('c96211ac-f4b7-42e8-ab51-cb0da8289327').
narrative_ontology:cs_reading_relation('c96211ac-f4b7-42e8-ab51-cb0da8289327', honor_violence_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_reading_relation('c96211ac-f4b7-42e8-ab51-cb0da8289327', honor_violence_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('c96211ac-f4b7-42e8-ab51-cb0da8289327', foundational, honor_excludes_violence).
narrative_ontology:cs_axiom_status(honor_excludes_violence, holdable).
narrative_ontology:cs_axiom_grounding('c96211ac-f4b7-42e8-ab51-cb0da8289327', honor_excludes_violence, conventional).
narrative_ontology:cs_reference_frame('c96211ac-f4b7-42e8-ab51-cb0da8289327', honor_as_moral_integrity).
narrative_ontology:cs_drift_state('c96211ac-f4b7-42e8-ab51-cb0da8289327', eighteenth_century_court_society, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('c96211ac-f4b7-42e8-ab51-cb0da8289327', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, bourgeois_gentry).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, professional_mediators).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, martial_aristocracy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, state_legal_apparatus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Criminalizes dueling through statutes and court rulings, claims the monopoly on legitimate violence, and punishes duelists with professional and civil disabilities. Maintains the legal architecture that substitutes state process for private violent redress.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, state_legal_apparatus, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__contraction_reading, state_legal_apparatus, beneficiary).

% Their social standing rests on commercial credit, propriety, and legal dispute resolution rather than martial valor. The redefinition of honor to exclude violence elevates their cultural capital and protects their persons and estates from the dueling trap.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, bourgeois_gentry, beneficiary,
    powerful, biographical, mobile, national).

% Lawyers, clergy, and physicians who absorb honor disputes into legal, moral, and medical forums. Their professional authority and income expand as violent self-help is delegitimized and channeled into institutional process.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, professional_mediators, beneficiary,
    moderate, biographical, mobile, national).

% Traditional military and landed elite whose honor was constituted by readiness to fight and willingness to accept challenges. The new honor code renders their customary responses dishonorable, devaluing their cultural capital and forcing costly adaptation to norms of restraint that undermine their social identity.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, martial_aristocracy, payer,
    powerful, biographical, identity_locked, national).

narrative_ontology:fixing_cost_class(honor_violence_legitimacy__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collective-action trap of honor-based violence, in which individual incentives compel men to duel or accept challenges to avoid dishonor even though all participants would prefer a nonviolent equilibrium.
% TRANSFER_FUNCTION: Moves the right to dispute resolution and status defense from private violent encounter to public institutional process, transferring symbolic and material capital from martial elites to civilian professionals and legal institutions.
% ABSENT_VOICES: Women and non-elite men are structurally absent from the honor discourse; they experience the violence and its suppression but have no seat in defining what honor means. Provincial gentry and military subcultures who maintain the old code are heard only as criminal defendants, not as legitimate interlocutors in the redefinition.
% DISAPPEARANCE_RATIONALE: If the constraint vanished and violent honor were restored as structurally legitimate, social disputes would reorganize around personal combat, legal institutions would lose a major jurisdiction, the distribution of masculine status would shift back toward martial capacity, and the professional mediator class would contract.
% FOUNDING_PROBLEM: The dueling trap: in an honor-based society where insult must be answered with violence to maintain standing, all participants face mortal risk and social waste, but individual defection from the code invites dishonor and exclusion.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the civilizing process (Elias) and legal historians document the dueling trap from outside the benefiting bourgeois classes. Contemporary moralists and conduct-manual authors attested the problem as live, though their testimony is from within the reform movement; state legal records and mortality data provide independent corroboration of dueling's social costs.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_violence_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__contraction_reading, 0.4, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__contraction_reading_tests).
:- end_tests(honor_violence_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.40) reflects the asymmetric cost borne by martial elites whose honor code is rendered socially worthless, not a monetary extraction. Suppression (0.58) captures both legal penalties and social ostracism directed at duelists during the transition. Theater_ratio (0.30) acknowledges the performative dimensions of Victorian civility but weights the functional coordination higher. Accessibility_collapse (0.84) records the degree to which dueling became structurally unthinkable within the new honor framework—high, but not a natural law because the framework is historically contingent and met resistance. Resistance (0.40) reflects persistent adherence to the old code in military and provincial circles. The temporal series show extraction peaking during the transition (t=75) and modestly declining as internalization proceeds, consistent with a coordination mechanism maturing.
 *
 * PERSPECTIVAL GAP:
 *   The state and civilian gentry experience the constraint as progress toward a more civilized and predictable social order. The martial aristocracy experiences it as cultural dispossession. The engine computes this divergence from the structural data: identical power levels (powerful) do not produce identical seats because exit_options differ—the gentry is mobile within the new code while the martial aristocracy is identity_locked to the old one.
 *
 * DIRECTIONALITY LOGIC:
 *   The state_legal_apparatus sits low-to-mid directionality: it enforces and benefits from the monopoly on violence, but also bears the cost of maintaining enforcement infrastructure. The bourgeois_gentry and professional_mediators are beneficiaries (low d). The martial_aristocracy is the primary victim: identity_locked to a devalued honor code with no viable exit that preserves their social standing (high d).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by maintaining the coordination function (solving the dueling trap) as distinct from the extraction function (devaluing martial capital). A snare reading would ignore the genuine coordination benefit; a pure rope reading would ignore the asymmetric extraction from identity-locked martial elites. The temporal measurements show extraction peaking during the transition and modestly declining as internalization proceeds, which is consistent with a coordination mechanism maturing rather than a snare deepening.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contraction_vs_external_cost,
    'Did dueling decline primarily because honor was redefined to exclude violence, or because external costs (legal penalty, professional exclusion) rose while honor remained structurally legitimate?',
    'Comparative historical analysis of regions with divergent legal enforcement but shared honor cultures; if dueling persists where honor still sanctions it despite high costs, contraction is the driver.',
    'If external costs alone drove the decline, the constraint is a snare or tangled rope of state enforcement rather than a conceptual rope; if conceptual contraction dominated, it is a genuine coordination shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contraction_vs_external_cost, empirical, 'Competing causal accounts of dueling''s decline').

omega_variable(
    violence_displacement,
    'Did the redefinition of elite honor to exclude violence reduce total violence, or displace it onto colonial, domestic, and lower-class arenas excluded from the new honor regime?',
    'Aggregate violence statistics across social domains and imperial peripheries before and after the contraction; if elite pacification coincided with increased extra-elite violence, the constraint redistributed rather than coordinated.',
    'If violence was displaced, the constraint''s coordination benefit is narrower than claimed and its extractiveness from excluded groups is higher; if total violence fell, the coordination claim is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(violence_displacement, empirical, 'Whether elite honor contraction reduced or redistributed violence').

omega_variable(
    suppression_internalization,
    'Was the suppression of dueling achieved through internalized shame or external legal and professional enforcement?',
    'Post-legalization trajectory in jurisdictions that decriminalized dueling: if dueling revived, suppression was primarily external; if it remained absent, internalization was dominant.',
    'If primarily internalized, effective extraction is higher than structural measures suggest and the constraint approaches a rope; if primarily external, it remains a tangled rope requiring active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Internalized versus external suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__contraction_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hvlc_tr_t0, honor_violence_legitimacy__contraction_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(hvlc_tr_t25, honor_violence_legitimacy__contraction_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement(hvlc_tr_t50, honor_violence_legitimacy__contraction_reading, theater_ratio, 50, 0.25).
narrative_ontology:measurement(hvlc_tr_t75, honor_violence_legitimacy__contraction_reading, theater_ratio, 75, 0.3).
narrative_ontology:measurement(hvlc_tr_t100, honor_violence_legitimacy__contraction_reading, theater_ratio, 100, 0.32).
narrative_ontology:measurement(hvlc_tr_t125, honor_violence_legitimacy__contraction_reading, theater_ratio, 125, 0.31).
narrative_ontology:measurement(hvlc_tr_t150, honor_violence_legitimacy__contraction_reading, theater_ratio, 150, 0.3).

% Extraction over time
narrative_ontology:measurement(hvlc_be_t0, honor_violence_legitimacy__contraction_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(hvlc_be_t25, honor_violence_legitimacy__contraction_reading, base_extractiveness, 25, 0.3).
narrative_ontology:measurement(hvlc_be_t50, honor_violence_legitimacy__contraction_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(hvlc_be_t75, honor_violence_legitimacy__contraction_reading, base_extractiveness, 75, 0.45).
narrative_ontology:measurement(hvlc_be_t100, honor_violence_legitimacy__contraction_reading, base_extractiveness, 100, 0.44).
narrative_ontology:measurement(hvlc_be_t125, honor_violence_legitimacy__contraction_reading, base_extractiveness, 125, 0.42).
narrative_ontology:measurement(hvlc_be_t150, honor_violence_legitimacy__contraction_reading, base_extractiveness, 150, 0.4).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(honor_violence_legitimacy__contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__contraction_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the honor_violence_legitimacy kernel. The contraction reading (this file) asserts conceptual redefinition; the drop reading asserts practical cost-driven decline; the composite reading synthesizes both. Each reading carries a distinct epsilon and stakeholder structure because they are structurally distinct empirical claims about the same historical transition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
