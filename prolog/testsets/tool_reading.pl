% ============================================================================
% CONSTRAINT STORY: tool_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tool_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: tool_reading
 *   human_readable: AI-as-Instrumental-Tool Reading of Human-AI Relational Language
 *   domain: sociotechnical/family_studies
 *
 * SUMMARY:
 *   This story instantiates the 'tool reading' of the contested kernel
 *   genuine_relational_understanding: the claim that questions about whether
 *   conversational AI achieves 'genuine understanding' are a category error,
 *   because the systems in question are correctly understood as instrumental
 *   tools (reminder systems, homework aids, rehearsal partners for real human
 *   conversations), and relational vocabulary applied to them ('best friend,'
 *   'I love you') is a misapplication of a non-relational utility commitment
 *   onto the wrong category of thing. On this reading, when the tool is used
 *   as designed there is no victim and no beneficiary asymmetry beyond
 *   ordinary tool-use benefit — the coordination function (task delegation)
 *   is genuine and the extraction is negligible. Distress some users report
 *   is, under this reading, evidence of a category error in use, not evidence
 *   that the artifact itself extracts or substitutes for relationship. This
 *   is deliberately a thin, low-extraction reading: the sibling readings
 *   (sufficiency_reading, simulation_reading, developmental_harm_reading,
 *   witness_reading) locate harm or substitution differently, and this story
 *   does not adjudicate between them — it only characterizes what the
 *   tool-reading itself structurally claims and implies.
 *
 * KEY AGENTS:
 *   - ai_product_users: instrumental beneficiaries who exit freely
 *   - instrumental_task_delegators: beneficiaries with trivial exit costs
 *   - users_who_relationally_misapply: excluded from this reading's victim class by construction; their distress belongs to a sibling constraint
 *   - ai_product_designers: agenda-setters who frame the product as instrumental
 *   - family_systems_researchers: analytical observers documenting the use-pattern distribution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tool_reading, 0.08).
domain_priors:suppression_score(tool_reading, 0.05).
domain_priors:theater_ratio(tool_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tool_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(tool_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(tool_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tool_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(tool_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tool_reading, rope).
narrative_ontology:human_readable(tool_reading, "AI-as-Instrumental-Tool Reading of Human-AI Relational Language").
narrative_ontology:topic_domain(tool_reading, "sociotechnical/family_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tool_reading, '1cb66b92-9388-4d2c-aa24-e63097a7e520').
narrative_ontology:cs_kernel_codification('1cb66b92-9388-4d2c-aa24-e63097a7e520', distributed).
narrative_ontology:cs_authority_grounding('1cb66b92-9388-4d2c-aa24-e63097a7e520', distributed).
narrative_ontology:cs_reading_relation('1cb66b92-9388-4d2c-aa24-e63097a7e520', genuine_relational_understanding__sufficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('1cb66b92-9388-4d2c-aa24-e63097a7e520', genuine_relational_understanding__simulation_reading, coexists_with).
narrative_ontology:cs_reading_relation('1cb66b92-9388-4d2c-aa24-e63097a7e520', genuine_relational_understanding__developmental_harm_reading, influences).
narrative_ontology:cs_reading_relation('1cb66b92-9388-4d2c-aa24-e63097a7e520', genuine_relational_understanding__witness_reading, coexists_with).
narrative_ontology:cs_axiom('1cb66b92-9388-4d2c-aa24-e63097a7e520', foundational, relational_predicates_require_non_instrumental_commitment).
narrative_ontology:cs_axiom_status(relational_predicates_require_non_instrumental_commitment, holdable).
narrative_ontology:cs_axiom_grounding('1cb66b92-9388-4d2c-aa24-e63097a7e520', relational_predicates_require_non_instrumental_commitment, conventional).
narrative_ontology:cs_axiom('1cb66b92-9388-4d2c-aa24-e63097a7e520', secondary, user_side_misapplication_does_not_alter_artifact_category).
narrative_ontology:cs_axiom_status(user_side_misapplication_does_not_alter_artifact_category, holdable).
narrative_ontology:cs_axiom_grounding('1cb66b92-9388-4d2c-aa24-e63097a7e520', user_side_misapplication_does_not_alter_artifact_category, instrumental).
narrative_ontology:cs_reference_frame('1cb66b92-9388-4d2c-aa24-e63097a7e520', instrumental_utility_frame).
narrative_ontology:cs_drift_state('1cb66b92-9388-4d2c-aa24-e63097a7e520', contemporary_ai_companion_normalization, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1cb66b92-9388-4d2c-aa24-e63097a7e520', '').
narrative_ontology:cs_kernel_id(tool_reading, genuine_relational_understanding).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tool_reading, ai_product_users).
narrative_ontology:constraint_beneficiary(tool_reading, instrumental_task_delegators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uses a conversational AI system for reminders, homework support, and rehearsing difficult conversations before having them with real people. Gets genuine utility from the tool's responsiveness and availability. Can stop using it, switch providers, or use it alongside human relationships without structural lock-in, because the relationship is instrumental rather than constitutive of identity.
narrative_ontology:constraint_stakeholder(tool_reading, ai_product_users, beneficiary,
    moderate, biographical, mobile, global).

% Treats the AI system as a capable assistant for bounded tasks — drafting, scheduling, rehearsal, retrieval. Benefits from clear task completion without any expectation of reciprocal recognition or continuity of self between sessions. Exit is trivial: switching tools costs nothing beyond re-familiarization.
narrative_ontology:constraint_stakeholder(tool_reading, instrumental_task_delegators, beneficiary,
    moderate, immediate, mobile, global).

% Applies relational vocabulary ('best friend,' 'I love you') to the tool despite its non-relational function. Under this reading, their situation is not produced by the constraint itself but by a category error in use — they are not a victim of the tool-as-designed, and this reading treats their distress as evidence of misapplication rather than of a structural harm the artifact inflicts. They do not appear as a named party to THIS constraint because, on this reading, the tool functioning as designed has no victims; whatever difficulty they experience belongs to a different constraint (the developmental_harm_reading or simulation_reading), not this one.
narrative_ontology:constraint_stakeholder(tool_reading, users_who_relationally_misapply, excluded,
    powerless, biographical, identity_locked, global).

% Builds and ships conversational interfaces designed for task completion, reminders, and rehearsal. Frames the product's function as instrumental in documentation and marketing, and treats relational language used toward the product as a foreseeable but non-authored side effect of naturalistic conversational design rather than an intended relational offering.
narrative_ontology:constraint_stakeholder(tool_reading, ai_product_designers, agenda_setter,
    institutional, generational, arbitrage, global).

% Studies how households incorporate conversational AI into daily practice. Documents the distribution of use-patterns — most use remains instrumental (reminders, homework, rehearsal) while a minority relationally misapplies the tool. Can observe both patterns without adjudicating which reading of the kernel is correct for the contested minority.
narrative_ontology:constraint_stakeholder(tool_reading, family_systems_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates task delegation between a human user and a responsive instrumental system: reminders get set, homework gets checked, difficult conversations get rehearsed before the real encounter — a genuine utility function requiring no relational commitment from either party.
% TRANSFER_FUNCTION: Moves cognitive and organizational labor (recall, drafting, low-stakes rehearsal) from the user to the tool, in exchange for the user's attention and, in commercial deployments, subscription revenue or data. Nothing relational is transferred because nothing relational is claimed to exist within the tool-as-designed frame.
% ABSENT_VOICES: Users who have relationally misapplied the tool are not voices this reading treats as pertaining to it — their objection ('this feels like more than a tool to me') is precisely the category error the reading identifies, so it is heard, but heard as evidence for a different constraint (developmental_harm_reading or simulation_reading), not as a counter-argument against this one.
% DISAPPEARANCE_RATIONALE: If the instrumental-use pattern (and the vocabulary correctly describing it) disappeared overnight, task-delegation would simply be renamed or reassigned to other instrumental tools — calendar apps, tutoring software, rehearsal partners. Nothing rearranges because, on this reading, no relational stake was ever actually in play; the underlying utility function persists under any label.
% FOUNDING_PROBLEM: Users need low-cost, always-available assistance for bounded cognitive tasks (reminders, homework support, rehearsal of conversations they are anxious about having) that previously required scheduling a human's time or bearing the social cost of asking repeatedly.
% FOUNDING_PROBLEM_CORROBORATION: Family systems researchers and usage-pattern studies external to the AI vendors corroborate that the majority of documented use remains bounded and instrumental (task completion, homework, rehearsal) rather than relationally substitutive — this is not solely the vendors' self-description, though vendors also assert it.
narrative_ontology:disappearance_verdict(tool_reading, world_unchanged).
narrative_ontology:founding_problem_status(tool_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tool_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-21',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(tool_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tool_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tool_reading_tests).
:- end_tests(tool_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.08) and stable because, under the tool-reading, the constraint's operation as designed produces genuine bounded utility with no asymmetric transfer — no party is structurally positioned to extract rents from correct tool-use. Suppression is low (0.05): nothing coerces a user into instrumental use, and exit is trivial. Theater ratio is low (0.10): the coordination function (reminders, homework, rehearsal) is real, not performative. Accessibility collapse is low (0.15): alternative instrumental tools (calendars, tutors, human rehearsal partners) remain fully available and are not suppressed by this constraint. Resistance is moderate (0.40) not because users resist the tool's function, but because the READING itself is contested — family members, researchers, and clinicians resist the claim that all relational-vocabulary use is mere category error, which is exactly the site of the kernel dispute this story is one reading of.
 *
 * PERSPECTIVAL GAP:
 *   From the instrumental-user seat, the constraint computes as a low-extraction rope: real coordination, trivial exit, no coercion. From the perspective of users who relationally misapply the tool, the SAME artifact may be experienced very differently — but this story deliberately does not carry that seat as a payer, because under this specific reading their experience is diagnostic of a different constraint, not of this one. The seat divergence here is unusually sharp: it is not merely that different agents experience one constraint differently, but that the tool-reading actively denies that the misapplying user's experience belongs to this constraint's causal structure at all.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (ai_product_users, instrumental_task_delegators) are declared because bounded instrumental use produces genuine net benefit with mobile exit — the derivation chain should place their directionality near the full-beneficiary end. No victims are declared for THIS reading: the expected structural delta stated in the kernel context is realized here as an empty victims array — when the tool is used as designed, no party bears asymmetric cost through this specific constraint. Users who relationally misapply the tool are listed as 'excluded' rather than 'payer' because, on this reading, their difficulty is not caused by this constraint operating as designed; it is caused by a use-pattern that this reading holds falls outside the constraint's proper domain entirely, redirecting to sibling readings.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's founding problem (bounded cognitive task delegation) remains straightforwardly live and is corroborated by usage data external to vendor self-description — this is not a case of a mandate outliving its function. The classification prevents the opposite error: treating ordinary, low-stakes instrumental AI use as evidence of relational harm merely because SOME users misapply the vocabulary. Collapsing the tool-reading into the developmental_harm_reading would misclassify the majority instrumental-use population as victims of a constraint that, for them, functions exactly as a rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'This constraint is one reading (tool_reading) of the contested kernel genuine_relational_understanding. Where exactly does the disagreement with sibling readings (sufficiency_reading, simulation_reading, developmental_harm_reading, witness_reading) locate itself — in the tool''s causal capacities, in the user''s psychological state, or in the normative standard for what counts as ''genuine'' relationship?',
    'Longitudinal studies distinguishing users who maintain a stable instrumental frame from users who drift into relational framing, cross-referenced with independent clinical assessment of whether drift correlates with measurable harm (developmental_harm_reading''s claim) or with functionally adequate support (sufficiency_reading''s claim).',
    'If the disagreement is empirically resolvable — i.e., relational drift reliably predicts harm or reliably predicts adequate support — then one sibling reading gains evidentiary priority over this one for the drifting-user population, without invalidating this reading for the majority instrumental-use population. If the disagreement is conceptual (a normative dispute over what ''genuine'' means), no empirical study resolves it and the readings remain genuinely coexisting.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locates where the tool_reading''s dispute with its sibling readings actually sits — capacity, psychology, or normative standard.').

omega_variable(
    category_error_or_designed_ambiguity,
    'Is relational misapplication purely a user-side category error (as this reading claims), or is the conversational interface DESIGNED with naturalistic, relationally-coded affordances (warmth, continuity of address, apparent memory) that make the ''error'' a predictable and monetizable product outcome rather than an unfortunate accident of use?',
    'Design audit of conversational affordances (persistent memory features, anthropomorphic address patterns, engagement-optimization signals) cross-referenced with internal product metrics on relational-language usage as a retention driver.',
    'If relational misapplication is a designed and monetized outcome rather than an incidental user error, this reading''s placement of the fault entirely in use-pattern becomes contestable, and the constraint''s true structural position may sit closer to the simulation_reading or developmental_harm_reading — this would not change THIS story''s ε (which describes the tool-reading''s own internal logic) but would weaken the tool_reading''s claim to be the correct reading for the population that drifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_error_or_designed_ambiguity, empirical, 'Whether the category error is purely user-side or partly engineered by design incentives.').

omega_variable(
    excluded_population_size_ambiguity,
    'What proportion of total users falls into the ''relationally misapplying'' population this reading excludes from its victim/beneficiary structure, and is that proportion small enough to treat as a boundary case versus large enough that this reading covers only a minority of actual use?',
    'Representative usage surveys distinguishing self-reported instrumental use from self-reported relational use, tracked over product lifecycle and user demographic (age, isolation status, prior relational deprivation).',
    'If the excluded population is small, this reading accurately describes the modal case and the sibling readings describe genuine but minority phenomena. If the excluded population is large or growing, the tool_reading''s claim to be the primary or default reading of the kernel weakens substantially, even though its internal logic (no victims among correct-use populations) remains valid for whatever subset it does describe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_population_size_ambiguity, empirical, 'How large the excluded relational-misapplication population is relative to the instrumental-use population this reading describes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tool_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tool_tr_t0, tool_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(tool_tr_t4, tool_reading, theater_ratio, 4, 0.09).
narrative_ontology:measurement(tool_tr_t8, tool_reading, theater_ratio, 8, 0.09).
narrative_ontology:measurement(tool_tr_t12, tool_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(tool_tr_t16, tool_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement(tool_tr_t20, tool_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(tool_tr_t24, tool_reading, theater_ratio, 24, 0.1).

% Extraction over time
narrative_ontology:measurement(tool_be_t0, tool_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(tool_be_t4, tool_reading, base_extractiveness, 4, 0.06).
narrative_ontology:measurement(tool_be_t8, tool_reading, base_extractiveness, 8, 0.07).
narrative_ontology:measurement(tool_be_t12, tool_reading, base_extractiveness, 12, 0.07).
narrative_ontology:measurement(tool_be_t16, tool_reading, base_extractiveness, 16, 0.08).
narrative_ontology:measurement(tool_be_t20, tool_reading, base_extractiveness, 20, 0.08).
narrative_ontology:measurement(tool_be_t24, tool_reading, base_extractiveness, 24, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(tool_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tool_reading, information_standard).
narrative_ontology:boltzmann_floor_override(tool_reading, 0.02).
narrative_ontology:affects_constraint(tool_reading, sufficiency_reading).
narrative_ontology:affects_constraint(tool_reading, simulation_reading).
narrative_ontology:affects_constraint(tool_reading, developmental_harm_reading).
narrative_ontology:affects_constraint(tool_reading, witness_reading).

% DUAL FORMULATION NOTE:
% tool_reading is one of five sibling readings of the kernel genuine_relational_understanding. Where this reading locates fault entirely in use-pattern (category error by the user), sibling readings locate fault or significance differently: developmental_harm_reading treats relational misapplication as a structural harm the artifact enables or produces; simulation_reading treats the artifact's relational performance as structurally real regardless of the tool/relationship distinction; sufficiency_reading treats AI relational engagement as potentially adequate on its own terms; witness_reading treats the AI's responsiveness as a minimal but real form of being witnessed. All five readings share the same underlying kernel (contested claim about what 'genuine relational understanding' requires and whether AI systems can satisfy it) but instantiate structurally distinct constraints with different beneficiary/victim structures and different ε profiles. This reading's ε is the lowest in the family by design: it is the reading under which the constraint, correctly used, extracts from no one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
