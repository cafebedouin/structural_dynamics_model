% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__contraction_reading, []).

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
 *   constraint_id: honor_satisfaction_mechanism__contraction_reading
 *   human_readable: Honor Satisfaction Mechanism — Contraction Reading (Cognitive Evacuation of Dueling)
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   The honor satisfaction mechanism — the requirement that aristocratic
 *   honor be vindicated through the duel — underwent a cognitive contraction
 *   between 1750 and 1914 such that dueling became not merely illegal or
 *   unfashionable but literally unthinkable as a category of action. This
 *   reading (contraction_reading) asserts that the constraint evacuated the
 *   possibility space: by 1914, an aristocrat could no more conceive of
 *   fighting a duel over honor than a modern citizen could conceive of trial
 *   by combat. The mechanism did not fade; it collapsed cognitively. The
 *   claim/metric independence is deliberate: the constraint is CLAIMED as
 *   mountain (it presents as cognitive natural law — 'one simply does not do
 *   that') while the authored metrics describe a historically extractive,
 *   suppressive mechanism that became a mountain only in its terminal phase
 *   through cognitive evacuation. The engine measures this divergence; do not
 *   reconcile the claim to the metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__contraction_reading, 0.78).
domain_priors:suppression_score(honor_satisfaction_mechanism__contraction_reading, 0.85).
domain_priors:theater_ratio(honor_satisfaction_mechanism__contraction_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(honor_satisfaction_mechanism__contraction_reading, "Honor Satisfaction Mechanism — Contraction Reading (Cognitive Evacuation of Dueling)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__contraction_reading, "historical_sociology/legal_history/normative_systems").

domain_priors:emerges_naturally(honor_satisfaction_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__contraction_reading, 'f66883cf-f49e-466d-88cf-a7895d9a8d27').
narrative_ontology:cs_kernel_codification('f66883cf-f49e-466d-88cf-a7895d9a8d27', distributed).
narrative_ontology:cs_authority_grounding('f66883cf-f49e-466d-88cf-a7895d9a8d27', practice).
narrative_ontology:cs_reading_relation('f66883cf-f49e-466d-88cf-a7895d9a8d27', honor_satisfaction_mechanism__decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('f66883cf-f49e-466d-88cf-a7895d9a8d27', honor_satisfaction_mechanism__composite_reading, influences).
narrative_ontology:cs_axiom('f66883cf-f49e-466d-88cf-a7895d9a8d27', foundational, honor_satisfaction_requires_cognitive_possibility).
narrative_ontology:cs_axiom_status(honor_satisfaction_requires_cognitive_possibility, holdable).
narrative_ontology:cs_axiom_grounding('f66883cf-f49e-466d-88cf-a7895d9a8d27', honor_satisfaction_requires_cognitive_possibility, deontological).
narrative_ontology:cs_axiom('f66883cf-f49e-466d-88cf-a7895d9a8d27', foundational, category_evacuation_is_irreversible).
narrative_ontology:cs_axiom_status(category_evacuation_is_irreversible, holdable).
narrative_ontology:cs_axiom_grounding('f66883cf-f49e-466d-88cf-a7895d9a8d27', category_evacuation_is_irreversible, empirically_contingent).
narrative_ontology:cs_reference_frame('f66883cf-f49e-466d-88cf-a7895d9a8d27', aristocratic_honor_practice).
narrative_ontology:cs_drift_state('f66883cf-f49e-466d-88cf-a7895d9a8d27', bourgeois_legal_transition, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('f66883cf-f49e-466d-88cf-a7895d9a8d27', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, state_legal_monopoly).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, bourgeois_honor_code).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, religious_moral_authority).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__contraction_reading, aristocratic_dueling_class).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__contraction_reading, military_officer_corps).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, military_officer_corps).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__contraction_reading, state_monopoly_on_legitimate_violence).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__contraction_reading, bourgeois_legal_personhood).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__contraction_reading, moral_conscience_as_honor_substitute).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Aristocratic men for whom dueling was the only cognitively available mode of honor satisfaction. The mechanism extracted life, limb, and legal standing from them. As the cognitive shift occurred, they could not conceive of honor without the duel — their professional and social identity was fused to the practice. Exit meant abandoning the very framework of their self-understanding.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, aristocratic_dueling_class, payer,
    moderate, biographical, identity_locked, national).

% Officers were both primary practitioners and institutional enforcers of the dueling code. The mechanism gave them status coherence but extracted fatal risk. The corps' collective identity was constituted through the duel; when the practice became cognitively impossible, the officer corps experienced a structural identity crisis rather than a mere policy change.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, military_officer_corps, payer,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__contraction_reading, military_officer_corps, beneficiary).

% The state progressively criminalized dueling while offering courts as the alternative honor-satisfaction venue. It benefited by consolidating the monopoly on legitimate violence and by channeling status disputes into state-controlled legal procedures. The state did not merely suppress dueling — it cultivated the cognitive conditions (legal education, professional honor codes) that made dueling unthinkable.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, state_legal_monopoly, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__contraction_reading, state_legal_monopoly, beneficiary).

% The rising bourgeoisie developed reputation-based honor (creditworthiness, professional standing, domestic virtue) that replaced violent satisfaction. They benefited by making honor compatible with commercial life and state law. Their honor code was cognitively portable — it survived the transition because it never required the duel as its satisfaction condition.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, bourgeois_honor_code, beneficiary,
    powerful, generational, mobile, national).

% Church authorities consistently condemned dueling as sinful (suicide/murder) and promoted conscience, confession, and forgiveness as the true satisfaction of honor. They benefited by expanding moral jurisdiction over the aristocratic soul. The religious frame provided the semantic resources for the cognitive evacuation — 'honor' was reinterpreted as 'conscience.'
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, religious_moral_authority, beneficiary,
    institutional, civilizational, arbitrage, continental).

% The vast majority were never parties to the aristocratic honor code — they had no standing to duel, no honor to satisfy in this register. They were structurally excluded from both the practice and its cognitive evacuation. Their honor disputes (if any) were handled through communal roughness or magistrate courts, never through the duel.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, common_populace, excluded,
    powerless, immediate, trapped, local).

% Analysts (Elias, Frevert, Kiernan, Nye) who reconstruct the constraint's operation from archives. They see the full structural arc: the mechanism's coordination function, its extraction from duelists, its beneficiaries, and its cognitive collapse. They are not subject to the constraint but their accounts shape how the constraint is understood retroactively.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinated status disputes among armed aristocrats without endless vendetta or premature state intervention. The duel provided a bounded, ritualized channel for violence that preserved social order among the elite while limiting collateral damage.
% TRANSFER_FUNCTION: Moved the risk of death, maiming, and legal prosecution onto individual duelists; moved the monopoly on legitimate violence and the authority to define honor onto the state, the bourgeoisie, and the church. The transfer was not negotiated — it was enacted through the cognitive evacuation of the duel as a thinkable option.
% ABSENT_VOICES: Women of the aristocracy (who bore the social consequences of dueling deaths but had no voice in the code), colonized subjects (for whom European honor codes were imposed violently), and the urban poor (whose honor disputes were criminalized while aristocratic duels were tolerated). These groups would have objected to the entire honor framework but were never in the room.
% DISAPPEARANCE_RATIONALE: When dueling became cognitively impossible, the entire architecture of aristocratic honor rearranged: status disputes migrated to courts, newspapers, and parliamentary privilege; military honor was sublimated into professional ethics and state service; bourgeois reputation markets absorbed the coordination function. The world of honor did not stay the same — it was rebuilt on new cognitive foundations.
% FOUNDING_PROBLEM: How to resolve status disputes among armed, touchy aristocrats without triggering endless blood feuds or inviting the state to dismantle aristocratic autonomy entirely. The duel was a self-limiting violence technology that kept the state at bay.
% FOUNDING_PROBLEM_CORROBORATION: Norbert Elias (The Civilizing Process), Ute Frevert (Men of Honour), Robert Nye (Masculinity and Male Codes of Honour) — historians outside the beneficiary classes — document that the armed aristocrat class vanished, the state monopoly on violence was consolidated, and the founding problem of 'private violence as aristocratic privilege' is historically extinct. No living aristocratic corps claims the duel as its honor mechanism.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__contraction_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__contraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_mechanism__contraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_satisfaction_mechanism__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_satisfaction_mechanism__contraction_reading),
    narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_satisfaction_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts very high (0.85) because the duel extracted life and liberty from its practitioners as the price of honor. It declines as the practice becomes rarer and the cognitive shift advances — by 1914 the mechanism is effectively gone (extractiveness ~0). Suppression rises monotonically (0.6→0.95) as legal penalties, professional sanctions, and moral condemnation accumulate. Theater ratio rises late (0.1→0.4) as the few remaining duels become performative relics. Accessibility collapse is extreme (0.92) — the cognitive evacuation means alternatives didn't just win; the old option became inconceivable. Resistance is low (0.25) by the end because cognitive evacuation preempts resistance — you cannot resist a category you cannot form. The measurement series run on one shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   From the aristocratic_dueling_class seat (early period), the constraint felt like a mountain — honor *required* the duel, no alternative was thinkable. From the state_legal_monopoly seat, it was a tangled rope — coordination (limiting vendetta) mixed with extraction (aristocratic autonomy). From the bourgeois_honor_code seat, it was a snare to be escaped. From the historical_sociologists seat, it is a false summit mountain: it presents as cognitive natural law but benefited identifiable agents. The engine will compute these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   The aristocratic_dueling_class and military_officer_corps are structural targets (payers) — they bore the fatal risk and legal exposure. Their identity_locked exit reflects the fusion of self-concept with the practice: leaving the duel meant leaving the self. The state_legal_monopoly is the agenda_setter and primary beneficiary — it wrote the laws, controlled the courts, and harvested the monopoly on violence. The bourgeois_honor_code and religious_moral_authority are beneficiaries — they provided the replacement cognitive frameworks. The common_populace were excluded from the entire system. Historical_sociologists are analytical observers. The engine computes per-seat effective extraction from these structural declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (containing aristocratic violence without state overreach) is dead — the armed aristocracy is gone, the state monopoly is total. The constraint persists only as a cognitive fossil: the unthinkability of the duel. This is mandatrophy resolved — the mechanism's function vanished, but its cognitive form remains as a mountain. The contraction reading captures this: the constraint did not persist by inertia (piton) or theater; it became a cognitive boundary. The mandate atrophied, but the category evacuated rather than decayed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_evacuation_mechanism,
    'Was the cognitive evacuation of dueling driven by internalized suppression (aristocrats policing their own minds) or by genuine category restructuring (the concept ''honor'' lost the duel as a semantic parameter)?',
    'Analyze diary, correspondence, and memoir corpora for the period 1840-1890: track whether aristocrats *struggled* against the unthinkability (internalized suppression) or simply ceased to generate the thought (category loss). Linguistic analysis of ''honor'' collocates over time.',
    'If internalized suppression, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression after exit. If category restructuring, the mountain classification is more secure — the constraint became a genuine cognitive boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_evacuation_mechanism, empirical, 'Mechanism of the cognitive evacuation: suppression vs. category loss.').

omega_variable(
    beneficiary_engineering_vs_passive_capture,
    'Did the state, bourgeoisie, and church actively engineer the cognitive shift (propaganda, education, legal framing) or did they passively capture the benefits of a shift driven by broader civilizational forces?',
    'Trace policy documents, pedagogical texts, and ecclesiastical directives for explicit anti-dueling cognitive campaigns. Compare timing of state/bourgeois/church interventions against the measured cognitive evacuation trajectory.',
    'If active engineering, the beneficiaries are stronger candidates for agenda_setter roles and the extraction is more deliberate. If passive capture, the constraint''s extraction was incidental to a larger civilizational drift — the mountain claim is stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_engineering_vs_passive_capture, conceptual, 'Whether beneficiaries actively produced the cognitive evacuation or merely inherited it.').

omega_variable(
    kernel_reading_boundary,
    'Does the contraction reading genuinely foreclose the decline and composite readings, or do they coexist as descriptions of different temporal phases or social strata?',
    'Test whether a single historical actor could hold both the contraction view (for their class) and the decline view (for another class) without contradiction. If yes, they coexist. If the contraction reading''s core premise (cognitive evacuation as total) logically excludes the decline reading''s core premise (gradual frequency decline), they foreclose.',
    'Determines reading_relations in cs_structure. Foreclosure would mean the kernel has mutually exclusive readings; coexistence means the kernel hosts a live multi-reading dispute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Structural relationship between the three kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__contraction_reading, 1750, 1914).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1750, 0.1).
narrative_ontology:measurement(hono_tr_t1780, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1780, 0.12).
narrative_ontology:measurement(hono_tr_t1810, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1810, 0.15).
narrative_ontology:measurement(hono_tr_t1840, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1840, 0.18).
narrative_ontology:measurement(hono_tr_t1870, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1870, 0.22).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1900, 0.35).
narrative_ontology:measurement(hono_tr_t1914, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1914, 0.4).

% Extraction over time
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1750, 0.85).
narrative_ontology:measurement(hono_be_t1780, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1780, 0.82).
narrative_ontology:measurement(hono_be_t1810, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1810, 0.75).
narrative_ontology:measurement(hono_be_t1840, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1840, 0.65).
narrative_ontology:measurement(hono_be_t1870, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1870, 0.45).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1900, 0.15).
narrative_ontology:measurement(hono_be_t1914, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1914, 0.02).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1750, 0.6).
narrative_ontology:measurement(hono_su_t1780, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1780, 0.65).
narrative_ontology:measurement(hono_su_t1810, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1810, 0.75).
narrative_ontology:measurement(hono_su_t1840, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1840, 0.82).
narrative_ontology:measurement(hono_su_t1870, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1870, 0.88).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1900, 0.92).
narrative_ontology:measurement(hono_su_t1914, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1914, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_mechanism__contraction_reading, 0.08).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, state_violence_monopoly).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, bourgeois_reputation_market).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, military_professional_ethics).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism__decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism__composite_reading).

% DUAL FORMULATION NOTE:
% This is the contraction_reading of the honor_satisfaction_mechanism kernel. The decline_reading models dueling as a practice that faded gradually; the composite_reading models multiple concurrent mechanisms. The contraction reading claims cognitive evacuation was the primary dynamic. The three readings differ in ε: contraction sees high historical extraction followed by cognitive collapse; decline sees declining extraction with persistent residuals; composite sees segmented extraction across parallel mechanisms. They are linked as a constraint family via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_mechanism__contraction_reading, moderate, 0.9).
constraint_indexing:directionality_override(honor_satisfaction_mechanism__contraction_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
