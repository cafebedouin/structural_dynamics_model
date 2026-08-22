% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Dignity-Culture Displacement of Honor Culture (Contraction Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   The contraction reading of dueling's disappearance holds that
 *   honor-culture axiomsâwhere personal reputation is paramount and
 *   affronts demand violent redressâwere displaced by dignity-culture
 *   axioms (inherent human worth, state-monopolized justice, equality before
 *   the law), rendering private violence culturally unthinkable. This reading
 *   treats the resulting dignity-culture framework not as a contingent
 *   institutional choice but as an irreversible cultural substrateâa
 *   mountainâthat forecloses the return of dueling not through active
 *   enforcement but through cognitive and moral unthinkability. Honor-culture
 *   practitioners are identified as victims of this displacement: their
 *   framework became illegible, and their status system collapsed not by
 *   direct defeat but by structural irrelevance.
 *
 * KEY AGENTS:
 *   - Bourgeois professional class: Primary beneficiary (organized/mobile) â gains cultural dominance as honor-based status markers are delegitimized.
 *   - State legal institutions: Agenda-setter and beneficiary (institutional/constrained) â gains monopoly on legitimate violence and dispute resolution.
 *   - Honor-culture practitioners: Primary payer (moderate/identity_locked) â bear the cost of cultural illegibility as their status framework becomes unthinkable.
 *   - Academic historians: Analytical observer (analytical/analytical) â dispute whether the mechanism was cultural, institutional, or overdetermined.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, 0.4).
domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, 0.2).
domain_priors:theater_ratio(dueling_disappearance_mechanism__contraction_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(dueling_disappearance_mechanism__contraction_reading, "Dignity-Culture Displacement of Honor Culture (Contraction Reading)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__contraction_reading, '5aa1168e-1d10-4dc6-866c-dd498beab806').
narrative_ontology:cs_kernel_codification('5aa1168e-1d10-4dc6-866c-dd498beab806', distributed).
narrative_ontology:cs_authority_grounding('5aa1168e-1d10-4dc6-866c-dd498beab806', distributed).
narrative_ontology:cs_reading_relation('5aa1168e-1d10-4dc6-866c-dd498beab806', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('5aa1168e-1d10-4dc6-866c-dd498beab806', dueling_disappearance_mechanism__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('5aa1168e-1d10-4dc6-866c-dd498beab806', foundational, cultural_axiom_constitution).
narrative_ontology:cs_axiom_status(cultural_axiom_constitution, holdable).
narrative_ontology:cs_axiom_grounding('5aa1168e-1d10-4dc6-866c-dd498beab806', cultural_axiom_constitution, empirically_contingent).
narrative_ontology:cs_axiom('5aa1168e-1d10-4dc6-866c-dd498beab806', secondary, dignity_as_irreversible_substrate).
narrative_ontology:cs_axiom_status(dignity_as_irreversible_substrate, holdable).
narrative_ontology:cs_axiom_grounding('5aa1168e-1d10-4dc6-866c-dd498beab806', dignity_as_irreversible_substrate, empirically_contingent).
narrative_ontology:cs_reference_frame('5aa1168e-1d10-4dc6-866c-dd498beab806', honor_culture_equilibrium).
narrative_ontology:cs_drift_state('5aa1168e-1d10-4dc6-866c-dd498beab806', dignity_culture_ascendant, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('5aa1168e-1d10-4dc6-866c-dd498beab806', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, bourgeois_professional_class).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, state_legal_institutions).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gains cultural dominance as honor-based status markersâdueling, aristocratic lineage, violent reputationâare delegitimized. Their status system, based on contract, professional credential, and nonviolent dispute resolution, becomes the default social grammar without needing to compete with honor-culture alternatives.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, bourgeois_professional_class, beneficiary,
    organized, generational, mobile, global).

% Sets and enforces the legal framework that criminalizes dueling and channels dispute resolution into courts. Collects the monopoly on legitimate violence and the expansion of jurisdiction that follows from privatizing honor-based conflict. Exit is constrained because abandoning the dignity-culture framework would undermine the state's own legitimacy claims.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, state_legal_institutions, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__contraction_reading, state_legal_institutions, beneficiary).

% Bear the cost of cultural illegibility as their entire status frameworkâpersonal honor, blood vengeance, aristocratic precedenceâbecomes unthinkable within dignity culture. Cannot exit to dignity culture without abandoning their constitutive identity; their responses to insult are read as pathology or crime rather than rightful conduct.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners, payer,
    moderate, biographical, identity_locked, national).

% Analyze and dispute the mechanism of dueling's disappearance, weighing cultural against institutional and overdetermined explanations. They occupy an analytical seat outside the benefit-and-cost structure of the historical constraint itself.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, academic_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Dignity culture coordinates social order by delegitimizing personal violence and channeling grievances into procedural, state-monopolized legal frameworks where equality before the law replaces hierarchical honor.
% TRANSFER_FUNCTION: Moves the locus of dispute resolution from private, status-based encounters to public, procedural institutions; transfers cultural dominance from honor-based reputation systems to dignity-based equality frameworks, rendering honor-culture practitioners structurally illegible.
% ABSENT_VOICES: Honor-culture practitioners whose framework became unintelligible within dignity culture would object that their status system was not inferior but differently structured; they are excluded from discourse because their premises are ruled out as unthinkable before argument can begin.
% DISAPPEARANCE_RATIONALE: If dignity-culture axioms disappeared overnight, honor-based practices including dueling would become thinkable again, dispute resolution would privatize, and the bourgeois state's monopoly on legitimate violence would face direct cultural challenge. The social world would reorganize around honor-based status competition.
% FOUNDING_PROBLEM: The cycle of aristocratic violence and blood feud endemic to honor-based societies, and the need for a stable, predictable dispute-resolution mechanism compatible with emerging capitalist social relations and centralized state authority.
% FOUNDING_PROBLEM_CORROBORATION: Beneficiaries (bourgeoisie, state institutions) attest the founding problem remains live in sublimated forms. Academic historians and critical theorists from outside the benefiting parties attest the founding problem of aristocratic blood-feud is largely solved in dignity-culture contexts, and the constraint persists as cultural hegemony rather than functional necessity; this corroboration is partial and disputed.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__contraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__contraction_reading, 0.4, 'kimi-k2.6', 'none', direct).

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
 *   The reading claims mountain status because dignity-culture axioms, once dominant, operate with near-zero enforcement cost and render alternatives cognitively inaccessible (accessibility_collapse 0.88, resistance 0.08). However, the constraint emerged from specific historical processes benefiting identifiable classes, and it structurally disadvantages honor-culture practitioners, making it a false-summit mountain candidate. Metrics are authored independently: extractiveness is moderate (0.40 at interval end) because the displacement transfers cultural dominance rather than material rents; suppression is low (0.20) because the mechanism is hegemonic rather than coercive; theater is low (0.12) because the unthinkability is genuinely internalized, not performed. The temporal series show extraction rising as honor culture is marginalized, suppression declining as cultural internalization replaces legal enforcement, and theater remaining flat.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (honor-culture practitioners) experiences the constraint as total cultural erasureâan existential threat to identity. The beneficiary seats (bourgeoisie, state) experience it as the natural, civilized order. The analytical seat sees a historical contingency reified as substrate. The engine should compute divergent classifications: beneficiaries near coordination or mountain, payers near snare or tangled_rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The bourgeois professional class and state institutions are structural beneficiaries of dignity-culture dominance (low d, low effective extraction). Honor-culture practitioners are structural targets (high d, amplified chi) because their entire framework is rendered illegible. Directionality is near-full-target for honor-culture practitioners because their exit is identity_lockedâthey cannot adopt dignity-culture premises without ceasing to be who they are. State institutions sit slightly above pure beneficiary because they must actively maintain the legal apparatus that reinforces the cultural shift.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâaristocratic blood-feud and social instabilityâis arguably solved by dignity culture, but the constraint persists as a comprehensive worldview rather than a temporary scaffold. The tension between founding_problem_status (contested) and disappearance_verdict (world_rearranges) flags potential mandatrophy if the problem is dead but the arrangement persists purely by inertia. However, this reading claims the arrangement is not inertial but substrate, so mandatrophy_resolved is omitted. The false-summit detection handles the risk that a culturally constructed hegemony is misclassified as natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dignity_culture_naturality,
    'Is dignity-culture dominance a genuine natural-law-like irreversible substrate, or a historically contingent cultural hegemony constructed by and benefiting specific social classes?',
    'Comparative historical analysis testing whether dignity-culture axioms persist across diverse civilizations independently of Western modernity and capitalist development, or are specific to that trajectory.',
    'If contingent, the mountain claim is a false summit and the constraint reclassifies as tangled_rope or snare; if universal and irreversible, the mountain claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_culture_naturality, conceptual, 'Whether dignity-culture dominance is natural or constructed hegemony').

omega_variable(
    enforcement_vs_internalization,
    'Did dueling disappear primarily through internalized cultural unthinkability, or through active legal and institutional suppression?',
    'Historical comparison of dueling rates in jurisdictions with strong anti-dueling laws versus weak laws but strong dignity-culture norms; analysis of post-exit behavior among honor-culture practitioners emigrating to contexts without legal prohibition.',
    'If legal suppression was primary, the mountain claim fails and the constraint is enforcement-dependent; if internalization was primary, the low suppression metric is accurate and the mountain claim is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vs_internalization, empirical, 'Structural versus internalized suppression mechanism for dueling''s disappearance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__contraction_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dueling_contraction_tr_t0, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(dueling_contraction_tr_t15, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 15, 0.09).
narrative_ontology:measurement(dueling_contraction_tr_t30, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(dueling_contraction_tr_t45, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 45, 0.11).
narrative_ontology:measurement(dueling_contraction_tr_t60, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 60, 0.12).

% Extraction over time
narrative_ontology:measurement(dueling_contraction_be_t0, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(dueling_contraction_be_t15, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 15, 0.22).
narrative_ontology:measurement(dueling_contraction_be_t30, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 30, 0.3).
narrative_ontology:measurement(dueling_contraction_be_t45, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 45, 0.36).
narrative_ontology:measurement(dueling_contraction_be_t60, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 60, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(dueling_contraction_su_t0, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(dueling_contraction_su_t15, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(dueling_contraction_su_t30, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 30, 0.32).
narrative_ontology:measurement(dueling_contraction_su_t45, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 45, 0.25).
narrative_ontology:measurement(dueling_contraction_su_t60, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 60, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
