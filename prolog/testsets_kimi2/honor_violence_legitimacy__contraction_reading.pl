% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   human_readable: Honor Redefinition Excluding Violent Redress (Contraction Reading)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the contraction reading of the
 *   honor_violence_legitimacy kernel: the historical transition in which
 *   dueling became structurally unthinkable because the concept of honor
 *   itself was redefined to exclude private violent redress. The reading
 *   asserts that conceptual space contracted—dueling exited the set of
 *   legitimate honor responses—not merely because external costs rose (the
 *   drop reading), but because the kernel of honor was internally
 *   reconstituted. This is a commitment-system constraint where the kernel
 *   (honor) is distributed across social practice, and the authority
 *   grounding is the practice of the new civil society. The claim is
 *   tangled_rope: the redefinition genuinely coordinates society around
 *   non-violent dispute resolution, while asymmetrically extracting
 *   autonomous status privileges from the aristocratic warrior class and
 *   transferring them to the state and bourgeois norm entrepreneurs.
 *
 * KEY AGENTS:
 *   - bourgeois_norm_entrepreneurs: Agenda-setter (organized/mobile) — redefines honor concepts and sets new civility standards
 *   - state_legal_order: Beneficiary (institutional/mobile) — inherits monopoly on violence and enforces the new legal framework
 *   - aristocratic_warrior_class: Primary target (powerful/identity_locked) — bears the loss of violent redress as a constitutive status practice
 *   - historical_sociologist: Analytical observer (analytical/analytical) — sees the full structural transition without cost or benefit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, 0.42).
domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, 0.48).
domain_priors:theater_ratio(honor_violence_legitimacy__contraction_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__contraction_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__contraction_reading, "Honor Redefinition Excluding Violent Redress (Contraction Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__contraction_reading, "historical_sociology/legal_anthropology/commitment_systems").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__contraction_reading, '9fc3a170-89f1-4d73-8bbd-3275e58fde6b').
narrative_ontology:cs_kernel_codification('9fc3a170-89f1-4d73-8bbd-3275e58fde6b', distributed).
narrative_ontology:cs_authority_grounding('9fc3a170-89f1-4d73-8bbd-3275e58fde6b', practice).
narrative_ontology:cs_interpretation_layer_present('9fc3a170-89f1-4d73-8bbd-3275e58fde6b').
narrative_ontology:cs_reading_relation('9fc3a170-89f1-4d73-8bbd-3275e58fde6b', honor_violence_legitimacy__drop_reading, forecloses).
narrative_ontology:cs_reading_relation('9fc3a170-89f1-4d73-8bbd-3275e58fde6b', honor_violence_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('9fc3a170-89f1-4d73-8bbd-3275e58fde6b', foundational, honor_excludes_violent_redress).
narrative_ontology:cs_axiom_status(honor_excludes_violent_redress, holdable).
narrative_ontology:cs_axiom_grounding('9fc3a170-89f1-4d73-8bbd-3275e58fde6b', honor_excludes_violent_redress, conventional).
narrative_ontology:cs_reference_frame('9fc3a170-89f1-4d73-8bbd-3275e58fde6b', honor_includes_violent_redress).
narrative_ontology:cs_drift_state('9fc3a170-89f1-4d73-8bbd-3275e58fde6b', modern_civil_society_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('9fc3a170-89f1-4d73-8bbd-3275e58fde6b', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, bourgeois_norm_entrepreneurs).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, state_legal_order).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, aristocratic_warrior_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors and disseminates conduct manuals, journalism, and salon discourse that redefines honorable behavior to exclude private lethal violence; shapes the conceptual boundaries of legitimate honor without holding formal state power, but benefits from the cultural hegemony of the new civil code.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, bourgeois_norm_entrepreneurs, agenda_setter,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__contraction_reading, bourgeois_norm_entrepreneurs, beneficiary).

% Claims and exercises a monopoly on legitimate violence; prosecutes duels as ordinary crimes or breaches of public order as private redress is culturally delegitimized, consolidating jurisdiction over interpersonal conflicts previously governed by aristocratic self-help.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, state_legal_order, beneficiary,
    institutional, generational, mobile, national).

% Traditionally derived social status from the willingness and socially recognized right to violent redress for insults; faces the erosion of a core identity practice as society redefines honor around restraint, legal process, and commercial reliability. Resistance is structurally impossible because challenging the new code reads as dishonorable under the redefined norm.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, aristocratic_warrior_class, payer,
    powerful, generational, identity_locked, national).

% Studies the structural transition from violent honor cultures to restrained civil societies; observes how the unthinkability of dueling indexes a deeper shift in habitus and the monopoly on legitimate violence.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, historical_sociologist, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__contraction_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collective-action problem of escalating lethal interpersonal violence and blood feuds by removing private redress from the set of legitimate honor responses, centralizing dispute resolution under state law and reputational mechanisms within commercial society.
% TRANSFER_FUNCTION: Moves the socially recognized right to legitimate violent redress from the aristocratic warrior class to the state legal order, and shifts the cultural currency of honor from martial prowess and lethal daring to civil self-restraint and legal compliance.
% ABSENT_VOICES: Traditional seconds, military aristocrats in peripheral regions, and subcultural enclaves where dueling persisted would object that honor without violent redress is mere cowardice; they are excluded from polite discourse by being redefined as barbaric or anachronistic, their objections treated as evidence of failed socialization rather than as debate.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, aristocratic violence would regain conceptual legitimacy, the state's monopoly on violence would fracture into competing private jurisdictions, and the bourgeois commercial order would face destabilizing private feuds — the social world would rearrange around re-legitimated dueling and the collapse of civil restraint norms.
% FOUNDING_PROBLEM: Private lethal violence as a legitimate honor response produced uncontrolled mortality, blood feuds, and chronic instability in emerging commercial and administrative social orders that required predictable, non-lethal interpersonal relations.
% FOUNDING_PROBLEM_CORROBORATION: Historical sociologists such as Elias and Bourdieu, writing from outside the benefiting bourgeoisie, attest that the founding problem of aristocratic violence has been substantially transformed; legal historians corroborate that the state now holds a near-complete monopoly on legitimate violence, though they debate whether the current arrangement is stable coordination or continued suppression of aristocratic identity.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_violence_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__contraction_reading, 0.42, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate (0.42) because the aristocracy loses a constitutive status practice and the state captures a monopoly, not because of monetary rent. Suppression is moderate (0.48) because the new honor code requires both legal prosecution and social ostracism to hold against residual aristocratic identity. Theater is low-moderate (0.22) because performative civility partly replaces functional violence but the coordination remains substantive. Accessibility collapse is very high (0.9) because dueling becomes cognitively unavailable as an honorable option. Resistance is low (0.25) because the unthinkability of the practice suppresses open resistance; what resistance exists is subcultural and delegitimized.
 *
 * PERSPECTIVAL GAP:
 *   The aristocratic payer seat experiences the constraint as severe identity loss and structural dispossession (identity_locked exit amplifies effective extraction toward the full-target end), while the state and bourgeois seats experience it as legitimate order and civilizational progress (mobile exit and beneficiary position dampen directionality toward the beneficiary end). The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (state_legal_order, bourgeois_norm_entrepreneurs) have mobile or organized exit and collect the gains of monopoly and cultural hegemony. The payer (aristocratic_warrior_class) has identity_locked exit—its social self-concept is fused with the old honor code—so directionality sits near the full-target end. The observer seat sees the full structure without cost or benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the decline of dueling as pure extraction (snare) by acknowledging the genuine coordination function: society-wide reduction in lethal violence and the stabilization of commercial relations. It prevents mislabeling it as pure coordination (rope) by naming the asymmetric cost borne by the aristocracy and the active enforcement required to maintain the new honor code against residual aristocratic identity. The founding problem (private lethal violence) is dead, but the arrangement has not atrophied into a piton because the new honor code remains functional and actively maintained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contraction_vs_enforcement_causality,
    'Does the structural unthinkability of dueling derive primarily from internalized conceptual redefinition of honor, or from external enforcement costs that made dueling practically untenable regardless of conceptual change?',
    'Comparative historical analysis of regions with varying state enforcement capacity: if dueling declines equally in weak-state and strong-state contexts, conceptual redefinition is primary; if decline tracks state capacity, external enforcement is primary.',
    'If external enforcement is primary, the constraint is better classified as enforcement_mechanism rather than identity_coordination, and the extraction/coordination balance shifts toward state power rather than normative consensus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contraction_vs_enforcement_causality, empirical, 'Causal ambiguity between conceptual and enforcement mechanisms in dueling''s decline').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of dueling structural (external legal barriers and state prosecution) or internalized (cognitive patterns of shame and unthinkability that persist after barrier removal)?',
    'Post-legalization trajectory analysis: if dueling remains unthinkable in jurisdictions that decriminalize it, suppression is largely internalized; if dueling resumes when legal barriers fall, suppression was structural.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest, and the directionality for aristocratic payers is more severe (identity_locked).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in honor norm transition').

omega_variable(
    composite_reading_validity,
    'Can the contraction and drop mechanisms be held as genuinely separable historical causes, or does the historical record require the composite overdetermined reading?',
    'Process-tracing studies that establish the temporal sequence of normative change versus legal enforcement intensification across multiple national cases.',
    'If composite is required, this constraint should be linked more tightly to a composite_reading constraint in the network; if separable, the contraction reading stands as an independent ε-invariant constraint with distinct empirical entailments.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(composite_reading_validity, conceptual, 'Whether contraction and drop readings are empirically separable or necessarily combined').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__contraction_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_violence_legitimacy__contraction_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(hono_tr_t30, honor_violence_legitimacy__contraction_reading, theater_ratio, 30, 0.11).
narrative_ontology:measurement(hono_tr_t60, honor_violence_legitimacy__contraction_reading, theater_ratio, 60, 0.14).
narrative_ontology:measurement(hono_tr_t90, honor_violence_legitimacy__contraction_reading, theater_ratio, 90, 0.17).
narrative_ontology:measurement(hono_tr_t120, honor_violence_legitimacy__contraction_reading, theater_ratio, 120, 0.2).
narrative_ontology:measurement(hono_tr_t150, honor_violence_legitimacy__contraction_reading, theater_ratio, 150, 0.22).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_violence_legitimacy__contraction_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(hono_be_t30, honor_violence_legitimacy__contraction_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement(hono_be_t60, honor_violence_legitimacy__contraction_reading, base_extractiveness, 60, 0.34).
narrative_ontology:measurement(hono_be_t90, honor_violence_legitimacy__contraction_reading, base_extractiveness, 90, 0.38).
narrative_ontology:measurement(hono_be_t120, honor_violence_legitimacy__contraction_reading, base_extractiveness, 120, 0.4).
narrative_ontology:measurement(hono_be_t150, honor_violence_legitimacy__contraction_reading, base_extractiveness, 150, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_violence_legitimacy__contraction_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(hono_su_t30, honor_violence_legitimacy__contraction_reading, suppression_requirement, 30, 0.3).
narrative_ontology:measurement(hono_su_t60, honor_violence_legitimacy__contraction_reading, suppression_requirement, 60, 0.36).
narrative_ontology:measurement(hono_su_t90, honor_violence_legitimacy__contraction_reading, suppression_requirement, 90, 0.41).
narrative_ontology:measurement(hono_su_t120, honor_violence_legitimacy__contraction_reading, suppression_requirement, 120, 0.45).
narrative_ontology:measurement(hono_su_t150, honor_violence_legitimacy__contraction_reading, suppression_requirement, 150, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is the contraction reading of the honor_violence_legitimacy kernel. It is structurally distinct from the drop reading (external cost mechanism) and the composite reading (overdetermined synthesis). They are linked as a constraint family under the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
