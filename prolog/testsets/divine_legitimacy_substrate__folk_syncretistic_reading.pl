% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__folk_syncretistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__folk_syncretistic_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: divine_legitimacy_substrate__folk_syncretistic_reading
 *   human_readable: Divine Legitimacy via Folk Syncretistic Household/Village Ritual
 *   domain: religious/political/social
 *
 * SUMMARY:
 *   In ancient Egypt, divine legitimacy was contested across three reading
 *   frames. This story captures the FOLK SYNCRETISTIC READING: the view that
 *   legitimate access to divine agency flows through household and village
 *   ritual practice, with pragmatic polytheism (invoking multiple deities
 *   based on immediate needs rather than hierarchical cosmology) and
 *   distributed authority (local ritual specialists and household heads
 *   rather than centralized priesthood or pharaonic mediation). This reading
 *   coexists with the priestly Amun-polytheistic reading (which claims
 *   legitimacy flows through established temple priesthood and their
 *   interpretation of cosmic order) and the pharaonic Atenist reading (which
 *   asserts divine legitimacy flows only through pharaonic revelation of the
 *   one true deity). The three readings are structurally distinct constraints
 *   with different beneficiaries, different authority structures, and
 *   different ε values. Folk syncretism is substantially non-extractive (low
 *   suppression, low theater) and genuinely coordinates household and village
 *   cohesion; the priestly and pharaonic readings are more extractive and
 *   more enforced. This story captures only the folk reading, as a clean
 *   ε-invariant constraint.
 *
 * KEY AGENTS:
 *   - household_heads: moderate power, ritual decision-makers at household scale, treat central priesthood as distant
 *   - village_ceremonial_leaders: organized power, coordinate village rituals, gain prestige and resource claims from ritual leadership
 *   - central_priesthood: institutional power, excluded from household practice, claims interpretive authority that is structurally not invoked
 *   - pharaonic_authority: institutional power, claims cosmic mediation, treated by folk as distant and not binding on local practice
 *   - folk_practitioners: powerless, participate in rituals, gain spiritual security and social cohesion
 *   - competing_cosmological_frameworks: analytical observer seat for comparing the three readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__folk_syncretistic_reading, 0.31).
domain_priors:suppression_score(divine_legitimacy_substrate__folk_syncretistic_reading, 0.28).
domain_priors:theater_ratio(divine_legitimacy_substrate__folk_syncretistic_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, resistance, 0.56).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__folk_syncretistic_reading, rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__folk_syncretistic_reading, "Divine Legitimacy via Folk Syncretistic Household/Village Ritual").
narrative_ontology:topic_domain(divine_legitimacy_substrate__folk_syncretistic_reading, "religious/political/social").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__folk_syncretistic_reading, '92a79e14-7833-4507-b58d-729e4848060f').
narrative_ontology:cs_kernel_codification('92a79e14-7833-4507-b58d-729e4848060f', distributed).
narrative_ontology:cs_authority_grounding('92a79e14-7833-4507-b58d-729e4848060f', practice).
narrative_ontology:cs_interpretation_layer_present('92a79e14-7833-4507-b58d-729e4848060f').
narrative_ontology:cs_reading_relation('92a79e14-7833-4507-b58d-729e4848060f', divine_legitimacy_substrate__amun_polytheistic_reading, coexists_with).
narrative_ontology:cs_reading_relation('92a79e14-7833-4507-b58d-729e4848060f', divine_legitimacy_substrate__atenist_monotheistic_reading, coexists_with).
narrative_ontology:cs_axiom('92a79e14-7833-4507-b58d-729e4848060f', foundational, pragmatic_polytheism_legitimate).
narrative_ontology:cs_axiom_status(pragmatic_polytheism_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('92a79e14-7833-4507-b58d-729e4848060f', pragmatic_polytheism_legitimate, conventional).
narrative_ontology:cs_axiom('92a79e14-7833-4507-b58d-729e4848060f', foundational, distributed_authority_legitimate).
narrative_ontology:cs_axiom_status(distributed_authority_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('92a79e14-7833-4507-b58d-729e4848060f', distributed_authority_legitimate, conventional).
narrative_ontology:cs_reference_frame('92a79e14-7833-4507-b58d-729e4848060f', distributed_household_village_ritual_autonomy).
narrative_ontology:cs_drift_state('92a79e14-7833-4507-b58d-729e4848060f', atenist_reform_period, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('92a79e14-7833-4507-b58d-729e4848060f', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, household_ritual_practitioners).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, village_ceremonial_leaders).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__folk_syncretistic_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).
:- end_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.31 at interval end) because the constraint coordinates genuine benefits (spiritual security, life-cycle marking, village cohesion) without substantial coercive overhead or confiscatory transfer. The beneficiaries (household heads, village leaders) collect prestige and modest offerings, but the flows emerge through reciprocal gift-exchange and community obligation, not through coercive exaction or legal title. Suppression is low (0.28) because folk practice requires minimal active enforcement—it persists through cultural transmission, immediate practical benefit, and social integration rather than through prohibition of alternatives. Theater is minimal (0.18) because ritual practice is functionally embedded in subsistence, life-cycle events, and seasonal coordination; the performative component is inseparable from the practical function. Accessibility collapse is moderate (0.42) because alternatives exist (central priesthood, pharaonic ritual, or no ritual) but are experienced as inadequate, distant, or spiritually ineffective rather than legally or practically unavailable. Resistance is moderate-high (0.56) because folk practitioners resist centralized reform attempts (particularly visible under Akhenaten's Atenist imposition) and reassert their practical polytheism once top-down pressure relaxes. The measurement series show flat profiles: folk practice is stable across the interval, neither rising nor decaying in its fundamental extractiveness or suppression. This stability is a sign of genuine coordination without extraction layering.
 *
 * PERSPECTIVAL GAP:
 *   The folk-practitioner seat and the central-priesthood seat should compute very differently. From the household/village perspective, the folk syncretistic reading is non-extractive coordination with distributed legitimacy—legitimate because it works, because ancestors practiced it, because community consents to it. From the central priesthood's perspective, folk practice is heretical deviation from correct cosmic order, and the priesthood treats itself as the site of true legitimacy. The engine computes these divergences from the structural data: household heads and village leaders have moderate power and gain modest prestige; central priesthood has institutional power but is excluded from the actual decision-making that matters at household and village scale. The measurement series shows that folk practice generates low effective extraction from folk practitioners themselves (the benefits and costs are roughly aligned within the folk frame). By contrast, the priestly reading (if authored separately) would show the priesthood claiming authority it does not exercise over folk practice, generating extractive tension between the priestly claim and folk autonomy.
 *
 * DIRECTIONALITY LOGIC:
 *   Household heads are agenda-setters with moderate power; they benefit from ritual leadership prestige and are not extracted from—their directionality is low (near beneficiary). Village ceremonial leaders similarly benefit and are not substantially extracted from—they are beneficiaries with modest transfer inflows. Folk practitioners (powerless) participate in ritual with genuine benefit (spiritual security, social cohesion) and modest contribution (offerings, labor service at village scale); their directionality is near symmetric (around 0.5) because coordination benefits and reciprocal costs roughly balance. Central priesthood and pharaonic authority are excluded: they have institutional power but structurally no leverage over folk practice. The folk syncretistic reading treats them as present but irrelevant, not as beneficiaries or extractors—they are the 'excluded' stakeholder role because their authority claims are not invoked by folk practitioners. If this story were authored from the priestly perspective (as a separate constraint story under the amun_polytheistic_reading), the priesthood would be the agenda-setter and the folk would be targets whose extraction funds priestly authority; the ε and suppression would be substantially higher, and the constraint would compute as snare or tangled_rope rather than rope. The divergence is the point of the constraint family: the same kernel (divine legitimacy substrate) instantiates different constraints depending on which reading—which authority frame—is active.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (households and villages needing spiritual security, life-cycle marking, and community coordination) is live and unsolved by any alternative the folk frame considers adequate. Central priesthood claims the founding problem is now solved by their authority and that folk practice is obsolete superstition; this claim is not corroborated by archaeological evidence or by the persistence of folk practice across multiple dynasties even under pharaonic reform pressure. The constraint shows no mandatrophy—the founding problem remains the motivation for the arrangement. Pharaonic attempts to impose the Atenist reading (a single exclusive deity replacing practical polytheism) temporarily suppress folk practice but do not eliminate it; after pharaonic pressure relaxes, folk syncretism reasserts itself. This resilience against top-down reform is evidence that folk practice solves a real problem and is not a zombie constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    folk_syncretism_vs_priestly_suppression,
    'To what extent does folk syncretistic practice persist because it is genuinely non-extractive and beneficial, versus persisting despite active priestly suppression efforts that have limited enforcement reach at household and village scales?',
    'Historical textual analysis of priestly injunctions against folk practice combined with archaeological evidence of household shrine practice continuity across periods of priestly reform pressure. Comparative ethnography of contemporary non-centralized societies to test whether pragmatic polytheism is resilient under pressure from formal religious authority.',
    'If folk practice persists mainly due to genuine benefit and community preference, the constraint is robustly a rope (low suppression, non-extractive). If priestly suppression is substantial but ineffective at the household level, the constraint might shift toward tangled_rope (suppression present despite low extractiveness). The measurement profile (low suppression, flat trajectory) currently supports the former interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(folk_syncretism_vs_priestly_suppression, empirical, 'Whether folk practice''s stability reflects genuine coordination or resilience against suppression.').

omega_variable(
    authority_distributed_or_diffuse,
    'Is the folk syncretistic reading''s authority structure genuinely ''distributed'' (intentionally delegated to multiple local agents with clear decision authority) or merely ''diffuse'' (authority is unclear and emerges from practice without intentional design)?',
    'Ethnographic observation of actual household and village decision-making processes around ritual choices. Analysis of household shrine practices to determine whether families make deliberate choices based on understood legitimating principles, or whether practice is habitual and unreflective.',
    'If distributed, the folk reading''s coordination function is more robust—households understand the legitimacy frame and consciously participate. If merely diffuse, the coordination is fragile and vulnerable to top-down revision if enforcement improves. This affects whether the constraint would compute as rope (intentional coordination) or scaffold (emergent but impermanent arrangement).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_distributed_or_diffuse, conceptual, 'Whether authority is intentionally distributed or emerges unreflectively from practice.').

omega_variable(
    kernel_reading_contest_live_or_historical,
    'Is the contest between these three readings (folk, priestly, Atenist) a live, unresolved dispute in the actual historical moment this constraint story describes, or is it a retrospective reading imposed by modern analysis on what was experienced by ancient actors as simpler domination?',
    'Close reading of period texts (administrative records, mortuary texts, graffiti) for evidence of explicit awareness of the three readings as competing frameworks. Absence of meta-commentary on the contest does not resolve it—the ancient actors may have experienced the tension without naming it. Presence of reform rhetoric (as in Akhenaten) provides evidence of explicit challenge to existing frameworks.',
    'If the contest is live (ancient actors experienced it as a real dispute over legitimate authority), the three-reading decomposition is faithful to actual structure and the omega itself is methodological. If the contest is retrospective, the stories should be authored with different time horizons or with omegas documenting the anachronism. The prompt''s kernel context assumes the contest is live at some historical period (likely New Kingdom, with Atenist reform as the explicit challenge phase).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_live_or_historical, conceptual, 'Whether the three-reading contest is an ancient dispute or a modern analytical imposition.').

omega_variable(
    folk_beneficiary_clarity,
    'Who exactly benefits from the folk syncretistic arrangement, and how much? Are household heads and village leaders collecting genuine economic rents (extracted from folk practitioners), or is the transfer primarily social prestige with minimal material value?',
    'Quantitative study of offerings and labor service flows in household and village contexts. Analysis of whether ritual leaders can convert prestige into material goods or political leverage. Comparison with priestly and pharaonic extracted value to establish relative scales.',
    'If leaders collect substantial material rents, the constraint might compute as snare or tangled_rope (extraction masked as coordination). If transfers are primarily prestige with minimal material value, the constraint is robustly rope (genuinely reciprocal coordination). The low extractiveness metric (0.31) currently assumes prestige-dominant transfer, but this should be verified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(folk_beneficiary_clarity, empirical, 'Whether folk ritual leadership generates material extraction or primarily social prestige.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__folk_syncretistic_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement(divi_tr_t5, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement(divi_tr_t10, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(divi_tr_t15, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(divi_tr_t20, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(divi_tr_t25, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 25, 0.18).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(divi_be_t5, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 5, 0.29).
narrative_ontology:measurement(divi_be_t10, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(divi_be_t15, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 15, 0.31).
narrative_ontology:measurement(divi_be_t20, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement(divi_be_t25, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 25, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(divi_su_t5, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 5, 0.26).
narrative_ontology:measurement(divi_su_t10, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 10, 0.27).
narrative_ontology:measurement(divi_su_t15, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 15, 0.28).
narrative_ontology:measurement(divi_su_t20, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(divi_su_t25, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 25, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__folk_syncretistic_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(divine_legitimacy_substrate__folk_syncretistic_reading, 0.1).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate__amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate__atenist_monotheistic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel 'divine_legitimacy_substrate'. All three readings share a kernel (the importance of divine legitimacy for political and social order in ancient Egypt) but differ in the authority structure, beneficiary configuration, and ε-value. The folk syncretistic reading presents legitimacy as distributed through household/village practice; the Amun polytheistic reading presents it as centralized through temple priesthood; the Atenist reading presents it as flowing solely from pharaonic revelation. Each reading instantiates a different constraint because the ε-invariance test shows that measuring legitimacy via folk practice yields a different extraction profile than measuring it via priestly interpretation or pharaonic revelation. Sibling constraint stories are linked here via network.affects_constraints. This reading influences both siblings by providing the substrate against which priestly and pharaonic authority claims must assert themselves—folk practice must be actively suppressed or incorporated for either alternative reading to gain dominance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
