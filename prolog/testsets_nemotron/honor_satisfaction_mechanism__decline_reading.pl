% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__decline_reading, []).

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
 *   constraint_id: honor_satisfaction_mechanism__decline_reading
 *   human_readable: Dueling as Honor Satisfaction Mechanism (Decline Reading)
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   This constraint story models the decline_reading of the
 *   honor_satisfaction_mechanism kernel: dueling as a practice that persisted
 *   at declining frequency until fringe status. The constraint weakens across
 *   1750-1900 as state monopoly on violence expands, alternative dispute
 *   resolution (courts, press, politics) matures, and the gentry/officer
 *   class migrates to lower-cost status defense. By 1900 the arrangement is
 *   largely theatrical — performed in military academies and aristocratic
 *   codes but rarely enacted. The claimed type is piton: a former
 *   rope/tangled_rope whose coordination function has atrophied but persists
 *   through institutional inertia and identity performance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__decline_reading, 0.35).
domain_priors:suppression_score(honor_satisfaction_mechanism__decline_reading, 0.45).
domain_priors:theater_ratio(honor_satisfaction_mechanism__decline_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__decline_reading, piton).
narrative_ontology:human_readable(honor_satisfaction_mechanism__decline_reading, "Dueling as Honor Satisfaction Mechanism (Decline Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__decline_reading, "historical_sociology/legal_history/normative_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__decline_reading, '46ba8ad0-779d-476b-aa0e-9709d0cd37e5').
narrative_ontology:cs_kernel_codification('46ba8ad0-779d-476b-aa0e-9709d0cd37e5', implicit).
narrative_ontology:cs_authority_grounding('46ba8ad0-779d-476b-aa0e-9709d0cd37e5', practice).
narrative_ontology:cs_interpretation_layer_present('46ba8ad0-779d-476b-aa0e-9709d0cd37e5').
narrative_ontology:cs_reading_relation('46ba8ad0-779d-476b-aa0e-9709d0cd37e5', honor_satisfaction_mechanism__contraction_reading, influences).
narrative_ontology:cs_reading_relation('46ba8ad0-779d-476b-aa0e-9709d0cd37e5', honor_satisfaction_mechanism__composite_reading, coexists_with).
narrative_ontology:cs_axiom('46ba8ad0-779d-476b-aa0e-9709d0cd37e5', foundational, honor_practice_declines_continuously).
narrative_ontology:cs_axiom_status(honor_practice_declines_continuously, holdable).
narrative_ontology:cs_axiom_grounding('46ba8ad0-779d-476b-aa0e-9709d0cd37e5', honor_practice_declines_continuously, empirically_contingent).
narrative_ontology:cs_axiom('46ba8ad0-779d-476b-aa0e-9709d0cd37e5', secondary, normative_vocabulary_persists_post_practice).
narrative_ontology:cs_axiom_status(normative_vocabulary_persists_post_practice, holdable).
narrative_ontology:cs_axiom_grounding('46ba8ad0-779d-476b-aa0e-9709d0cd37e5', normative_vocabulary_persists_post_practice, conventional).
narrative_ontology:cs_reference_frame('46ba8ad0-779d-476b-aa0e-9709d0cd37e5', pre_state_honor_practice).
narrative_ontology:cs_drift_state('46ba8ad0-779d-476b-aa0e-9709d0cd37e5', late_19th_century, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('46ba8ad0-779d-476b-aa0e-9709d0cd37e5', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, gentry_elite).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, military_officer_corps).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, non_commissioned_officers).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, civilian_males_defending_reputation).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, women_honor_dependents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintained honor satisfaction through dueling as a class monopoly on legitimate violence for status defense. Declining participation reduced personal risk while preserving the normative framework that validated their status. Could substitute legal, political, or social mechanisms when dueling became inconvenient.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, gentry_elite, beneficiary,
    institutional, generational, arbitrage, national).

% Administered and enforced dueling codes within the officer corps as a matter of professional honor and unit cohesion. The practice reinforced officer identity and hierarchy. As state monopoly on violence strengthened, the corps faced pressure to suppress dueling officially while tolerating it informally — creating a dual standard that persisted longest in this group.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, military_officer_corps, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__decline_reading, military_officer_corps, beneficiary).

% Excluded from the honor code that authorized dueling; an insult to an NCO required no satisfaction, while an NCO insulting an officer triggered disproportionate consequences. Bore the extractive asymmetry of a system that recognized honor only above a rank threshold. No exit — the military hierarchy was their livelihood and identity.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, non_commissioned_officers, payer,
    powerless, immediate, trapped, local).

% Could theoretically access dueling for honor defense but faced escalating legal prosecution, social sanction, and professional ruin. The declining frequency meant fewer precedents, less institutional knowledge, and higher personal cost per encounter. Exit options: legal courts (slow, unsatisfying for honor), emigration, or submission to insult.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, civilian_males_defending_reputation, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__decline_reading, civilian_males_defending_reputation, payer).

% Honor satisfaction for women was mediated entirely through male relatives — fathers, husbands, brothers. A woman's reputation could trigger a duel she had no standing to initiate, refuse, or influence. Bore consequences (widowhood, scandal, destitution) without agency in the mechanism. Structurally excluded from both the coordination and extraction sides.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, women_honor_dependents, excluded,
    powerless, biographical, trapped, local).

% Progressively criminalized dueling while struggling to enforce bans against elite resistance. The constraint's decline correlates with state capacity to monopolize violence and provide alternative dispute resolution. Their enforcement effort is the primary driver of rising suppression costs over the interval.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, state_legal_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a bounded, ritualized mechanism for elite males to resolve status disputes without escalating to feud or assassination, while signaling class membership through willingness to risk death.
% TRANSFER_FUNCTION: Transferred the risk of death and legal consequence from the elite class (who gained status protection) to subordinate males and dependents (who bore the asymmetry of exclusion and mediation). Also transferred enforcement cost to the state as it criminalized the practice.
% ABSENT_VOICES: Women, non-commissioned enlisted men, and civilian males without social standing — who would object to the honor monopoly and its gendered/class mediation — were structurally excluded from the code's authorship and its dispute-resolution benefits.
% DISAPPEARANCE_RATIONALE: By 1900, dueling had already become a fringe practice; its formal disappearance would not rearrange social arrangements because the coordination function had already migrated to courts, press, and political institutions. The residual constraint was theatrical — the world had already rearranged around its absence.
% FOUNDING_PROBLEM: In pre-state or weak-state societies, elite males needed a credible, private mechanism to defend status and resolve insults without triggering uncontrolled violence or relying on unreliable courts.
% FOUNDING_PROBLEM_CORROBORATION: Historical sociologists (e.g., Stephen Banks, Ute Frevert, Robert Nye) document the migration of honor disputes from dueling to libel law, parliamentary privilege, and press duels across 1750-1900. The gentry and officer corps themselves abandoned the practice voluntarily as state alternatives matured — corroborated by the decline in duel frequency preceding legal suppression.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__decline_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__decline_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__decline_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(honor_satisfaction_mechanism__decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__decline_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__decline_reading_tests).
:- end_tests(honor_satisfaction_mechanism__decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate (0.55) when dueling is a live coordination mechanism with real asymmetric extraction (elites benefit, subordinates excluded) and declines to 0.35 as the practice becomes marginal and fewer encounters occur. Theater ratio rises from 0.25 to 0.65 — the coordination function (dispute resolution) is increasingly performed rather than used. Suppression requirement rises from 0.2 to 0.45 as state criminalization intensifies but enforcement remains selective against non-elites. The constraint never fully disappears because the normative vocabulary of honor persists even after the practice fades.
 *
 * PERSPECTIVAL GAP:
 *   From the gentry seat, the constraint appears as a fading but dignified tradition (rope-like coordination). From the NCO/woman seat, it appears as a persistent structural exclusion (snare-like extraction) even in decline. The officer corps sits in tension — administering a code they officially suppress. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Gentry elite and officer corps are beneficiaries (d low) — they hold the honor monopoly and can exit to alternative status mechanisms. NCOs and women are trapped victims (d high) — structural exclusion with no exit. Civilian males are constrained payers (d ~0.6) — theoretical access but escalating cost. State authorities are analytical observers (d ~0.5) — they bear enforcement cost but gain monopoly on violence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (private honor defense in weak-state contexts) is dead — state courts, libel law, and political institutions now handle status disputes. The constraint persists as piton because the military officer corps maintains the code as identity theater (theater_ratio 0.65) and the gentry elite retain the normative vocabulary. No concentrated beneficiary captures the residual extraction — it is diffuse institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contraction_vs_decline_boundary,
    'At what point does declining frequency become conceptual unthinkability — is there a sharp boundary or a continuous gradient between the decline_reading and contraction_reading?',
    'Lexical and cultural analysis: track when ''duel'' shifts from a live verb (''to duel'') to a historical noun (''the duel'') in elite correspondence, military manuals, and press. A sharp transition supports contraction_reading; a gradual shift supports decline_reading.',
    'If contraction_reading is correct, accessibility_collapse should be higher (near 1.0) and resistance near 0 — the constraint becomes a mountain of culture. If decline_reading holds, the moderate accessibility_collapse (0.4) and resistance (0.55) authored here are accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contraction_vs_decline_boundary, conceptual, 'Whether the decline reading and contraction reading are structurally distinct or gradient endpoints').

omega_variable(
    composite_mechanism_separability,
    'Does the composite_reading''s claim of multiple distinct mechanisms (state monopoly, bourgeois norms, insurance, category-shift) represent separable constraints, or are these facets of a single declining practice?',
    'Decompose the interval: if each mechanism has independent temporal dynamics (different start/end points, different beneficiary/victim sets), they are separate constraints linked by affects_constraints. If they move in lockstep, a single decline constraint suffices.',
    'If mechanisms are separable, this story should be split into a constraint family (per ε-invariance). If not, the composite_reading is an analytical lens on one constraint, not a distinct structural claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(composite_mechanism_separability, conceptual, 'Whether the composite reading describes one constraint or a constraint family').

omega_variable(
    officer_corps_identity_lock,
    'Is the officer corps'' persistence in dueling codes driven by genuine identity fusion (identity_locked exit) or by institutional inertia with available exit (constrained exit)?',
    'Compare officer corps dueling codes to civilian gentry codes over the same period. If officer codes persist longer and with more ritual intensity despite identical state pressure, identity_lock is supported. If they track civilian decline, it is institutional inertia.',
    'Identity_locked would raise effective extraction for officers (they cannot exit the honor frame) and lower theater_ratio (the performance is genuinely constitutive). Constrained exit supports the piton classification with high theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(officer_corps_identity_lock, empirical, 'Whether military dueling persistence reflects identity fusion or institutional lag').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__decline_reading, 1750, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1750, 0.25).
narrative_ontology:measurement(hono_tr_t1775, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1775, 0.3).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1800, 0.4).
narrative_ontology:measurement(hono_tr_t1825, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1825, 0.5).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1850, 0.58).
narrative_ontology:measurement(hono_tr_t1875, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1875, 0.62).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1900, 0.65).

% Extraction over time
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1750, 0.55).
narrative_ontology:measurement(hono_be_t1775, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1775, 0.52).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1800, 0.48).
narrative_ontology:measurement(hono_be_t1825, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1825, 0.42).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1850, 0.38).
narrative_ontology:measurement(hono_be_t1875, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1875, 0.36).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1900, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1750, 0.2).
narrative_ontology:measurement(hono_su_t1775, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1775, 0.25).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1800, 0.35).
narrative_ontology:measurement(hono_su_t1825, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1825, 0.4).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1850, 0.45).
narrative_ontology:measurement(hono_su_t1875, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1875, 0.45).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1900, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__decline_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_mechanism__decline_reading, 0.08).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism__contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism__composite_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, state_monopoly_on_violence).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, libel_law_as_honor_substitute).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, military_justice_codes).

% DUAL FORMULATION NOTE:
% Part of the honor_satisfaction_mechanism constraint family. This reading (decline) treats the mechanism as a single practice weakening continuously. The contraction_reading treats it as a cognitive category foreclosure. The composite_reading treats it as multiple mechanisms with different temporalities. All three share the kernel but instantiate different constraints with different ε trajectories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_mechanism__decline_reading, organized, 0.35).
constraint_indexing:directionality_override(honor_satisfaction_mechanism__decline_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
