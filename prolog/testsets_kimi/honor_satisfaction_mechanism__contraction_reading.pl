% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: honor_satisfaction_mechanism__contraction_reading
 *   human_readable: Honor Satisfaction Mechanism â Contraction Reading
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   The honor satisfaction mechanismâmost visibly instantiated as
 *   aristocratic duelingâwas the primary extralegal institution for
 *   resolving grave insults among European elites from the early modern
 *   period through the nineteenth century. Under the contraction reading,
 *   this mechanism did not merely decline in frequency but underwent a
 *   structural evacuation: by the early twentieth century, dueling had become
 *   cognitively unthinkable as a response to insult, effectively removed from
 *   the normative category space rather than merely suppressed or
 *   marginalized. This constraint story models the mechanism across its
 *   lifecycle, from active enforcement to disappearance, treating the
 *   terminal metrics as post-contraction state and the temporal series as the
 *   lifecycle trace.
 *
 * KEY AGENTS:
 *   - aristocratic_elite_collective: Primary beneficiary (powerful/continental/constrained) â captures diffuse social order and class autonomy
 *   - gentleman_duellists: Primary target (moderate/national/identity_locked) â bears concentrated mortal, financial, and psychological costs
 *   - military_officer_corps: Agenda-setter (institutional/national/constrained) â administers and enforces the code; professional identity fused with the mechanism
 *   - bourgeois_reformers: Excluded voice (moderate/national/mobile) â pushes legal alternatives from outside the honor sphere
 *   - state_legal_apparatus: Analytical observer (institutional/national/analytical) â criminalizes but cannot penetrate elite jurisdiction for most of the interval
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__contraction_reading, 0.02).
domain_priors:suppression_score(honor_satisfaction_mechanism__contraction_reading, 0.02).
domain_priors:theater_ratio(honor_satisfaction_mechanism__contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__contraction_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__contraction_reading, "Honor Satisfaction Mechanism â Contraction Reading").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__contraction_reading, "historical_sociology/legal_history/normative_systems").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__contraction_reading, 'f7990191-fdf5-4f75-8ebc-1e5cabd2412a').
narrative_ontology:cs_kernel_codification('f7990191-fdf5-4f75-8ebc-1e5cabd2412a', distributed).
narrative_ontology:cs_authority_grounding('f7990191-fdf5-4f75-8ebc-1e5cabd2412a', practice).
narrative_ontology:cs_interpretation_layer_present('f7990191-fdf5-4f75-8ebc-1e5cabd2412a').
narrative_ontology:cs_reading_relation('f7990191-fdf5-4f75-8ebc-1e5cabd2412a', honor_satisfaction_mechanism__decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('f7990191-fdf5-4f75-8ebc-1e5cabd2412a', honor_satisfaction_mechanism__composite_reading, coexists_with).
narrative_ontology:cs_axiom('f7990191-fdf5-4f75-8ebc-1e5cabd2412a', foundational, personal_combat_obligatory_for_honor_vindication).
narrative_ontology:cs_axiom_status(personal_combat_obligatory_for_honor_vindication, overridden).
narrative_ontology:cs_axiom_grounding('f7990191-fdf5-4f75-8ebc-1e5cabd2412a', personal_combat_obligatory_for_honor_vindication, conventional).
narrative_ontology:cs_axiom('f7990191-fdf5-4f75-8ebc-1e5cabd2412a', foundational, aristocratic_jurisdiction_autonomous_from_state).
narrative_ontology:cs_axiom_status(aristocratic_jurisdiction_autonomous_from_state, overridden).
narrative_ontology:cs_axiom_grounding('f7990191-fdf5-4f75-8ebc-1e5cabd2412a', aristocratic_jurisdiction_autonomous_from_state, conventional).
narrative_ontology:cs_reference_frame('f7990191-fdf5-4f75-8ebc-1e5cabd2412a', aristocratic_honor_autonomy).
narrative_ontology:cs_drift_state('f7990191-fdf5-4f75-8ebc-1e5cabd2412a', bourgeois_legal_hegemony, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('f7990191-fdf5-4f75-8ebc-1e5cabd2412a', '2026-06-20T12:00:00Z').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, aristocratic_elite_collective).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__contraction_reading, gentleman_duellists).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__contraction_reading, aristocratic_jurisdictional_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collectively maintained a transnational honor culture that resolved elite disputes internally through personal combat, preserving class autonomy and preventing bourgeois or state interference in aristocratic affairs. The group captured the diffuse good of social order and jurisdictional independence, though individual members were sometimes subject to the same code.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, aristocratic_elite_collective, beneficiary,
    powerful, generational, constrained, continental).

% Individual gentlemen who, upon receiving a challenge or grave insult, were structurally compelled to participate in single combat under threat of total social death and ostracism. They bore the concentrated mortal, financial, and psychological costs of a mechanism that primarily benefited the collective aristocratic order.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, gentleman_duellists, payer,
    moderate, biographical, identity_locked, national).

% Administered and enforced the dueling code within military institutions, reviewing disputes, regulating seconds, and ensuring ritualized combat. Professional identity and promotion prospects were fused with the maintenance of the honor code, making exit costly even when personal objections existed.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, military_officer_corps, agenda_setter,
    institutional, generational, constrained, national).

% Advocated for state monopoly on violence and legal-bureaucratic resolution of disputes. They were structurally excluded from aristocratic honor adjudication throughout most of the interval, but their normative frameworks gradually displaced the dueling code as bourgeois hegemony advanced.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, bourgeois_reformers, excluded,
    moderate, generational, mobile, national).

% Criminalized dueling and asserted jurisdiction over interpersonal violence, but lacked effective enforcement capacity within the aristocratic sphere for most of the interval. Observed the mechanism from the outside, prosecuting cases only when they spilled into public view or involved commoners.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, state_legal_apparatus, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_mechanism__contraction_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a standardized, extralegal mechanism for resolving grave honor disputes among armed elites, preventing the escalation to blood feuds, vendetta cycles, or intra-class wars that would destabilize aristocratic society.
% TRANSFER_FUNCTION: Moved the physical, financial, and mortal costs of honor vindication from the aristocratic collective onto individual gentlemen, while transferring social credit and class standing from non-participants (who faced ostracism) to participants and their seconds.
% ABSENT_VOICES: Women, commoners, and bourgeois legal reformers were structurally excluded from the honor code's adjudication; they would have objected to the violence, lawlessness, and gendered exclusivity but had no standing in the aristocratic sphere until the late nineteenth century.
% DISAPPEARANCE_RATIONALE: If the mechanism had vanished overnight at its peak, aristocratic society would have lost its internal dispute-resolution apparatus and likely faced a surge of unreconciled honor conflicts or state intervention; by the interval's end the world had already rearranged around state courts and bourgeois civility, but the historical dependence is what makes the verdict world_rearranges.
% FOUNDING_PROBLEM: How to resolve grave insults and honor disputes among armed, status-conscious elites without triggering blood feuds, vendettas, or intra-class violence that would destroy the internal coherence and autonomy of the aristocratic order.
% FOUNDING_PROBLEM_CORROBORATION: Weberian sociologists and legal historians outside the aristocratic beneficiary class attest that the blood-feud problem was ultimately resolved through state centralization and legal rationalization, not through aristocratic self-help; the aristocratic claim that dueling was necessary is treated as a self-serving origin myth by contemporary scholarship.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__contraction_reading, 0.02, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__contraction_reading_tests).
:- end_tests(honor_satisfaction_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base properties at interval end (T=1920) reflect the post-contraction state: near-zero extraction and suppression because the constraint has been evacuated from possibility space. Temporal measurements trace the lifecycle from robust enforcement (T=1750) through theatrical late-phase maintenance (T=1870) to cognitive unthinkability. Accessibility_collapse is low at interval end because alternativesâstate courts, bourgeois civility, legal procedureâare fully available. Resistance is low because the constraint no longer meets meaningful opposition; it is simply absent. The divergence between claimed_type (tangled_rope, reflecting the mechanism's historical structure) and terminal metrics is intentional: it captures the structural contraction the reading posits, producing a mandatrophy-resolved signal rather than a current extraction profile.
 *
 * PERSPECTIVAL GAP:
 *   From the aristocratic collective seat, the mechanism was a necessary coordination device preserving class autonomy and preventing blood feuds; from the individual duelist seat, it was an identity-locked extraction that risked life, limb, and fortune on behalf of a diffuse collective good. The engine computes this asymmetry from the beneficiary/payer split and the differentiated exit options (constrained vs. identity_locked). The military officer corps experiences a hybrid seat: administratively powerful but individually constrained by professional identity fusion.
 *
 * DIRECTIONALITY LOGIC:
 *   The aristocratic elite collective is declared beneficiary because it captured the diffuse good of class-wide feud prevention and jurisdictional autonomy; gentleman_duellists are declared victims because they bore the concentrated mortal and financial costs. The officer corps sits as agenda_setter with constrained exit (professional identity fused with the code). Bourgeois reformers and the state legal apparatus were excluded or analytical observers with mobile/analytical exit options, yielding lower directionality despite their structural opposition to the mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope rather than snare preserves the genuine coordination function (preventing elite blood feuds and stabilizing aristocratic society) while acknowledging asymmetric extraction onto individual duelists. The mandatrophy is resolved: the founding problem (aristocratic honor disputes) was solved instead by state legal centralization and bourgeois normative hegemony, and the mechanism persisted as theatrical inertia before total evacuation. The R5 mismatch (founding_problem_status=dead paired with disappearance_verdict=world_rearranges) flags the zombie/capture phase that the contraction reading says ended in category collapse rather than infinite piton decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contraction_vs_decline_reading,
    'Did the honor satisfaction mechanism undergo category-level cognitive evacuation, or did it merely decline in frequency while remaining structurally available as a live option?',
    'Comparative historical analysis of elite correspondence, legal records, and military manuals from 1850-1920 to determine whether dueling remained a conceivable, if rare, response to insult or was genuinely evacuated from normative cognition.',
    'If decline, the terminal phase is inertial decay (piton-flavored); if contraction, it is structural dissolution with distinct implications for how normative systems terminate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contraction_vs_decline_reading, conceptual, 'Whether the mechanism was evacuated from cognitive space or simply fell into disuse').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the constraint''s persistence driven by active social enforcement (ostracism, social death) or by internalized honor ideology that became self-enforcing?',
    'Examine whether dueling persisted in contexts where external enforcement was weak; post-exit suppression trajectoryâif social death for refusers persisted after legal prohibition, suppression was partially internalized.',
    'Internalized suppression implies higher effective extraction than structural measures suggest and shifts the coordination type from enforcement_mechanism toward identity_coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression in honor culture').

omega_variable(
    category_evacuation_verification,
    'Is the ''cognitive unthinkability'' of dueling a genuine historical category-level shift, or a retrospective narrative projection constructed by modern observers?',
    'Discourse analysis of early twentieth-century elite texts, memoirs, and legal commentary for the total absence of dueling as a conceivable response to insult, as opposed to its treatment as archaic but comprehensible.',
    'If unthinkability is projection, the contraction reading overstates the structural delta and the constraint is better modeled as decline; if genuine, it validates the category-collapse thesis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(category_evacuation_verification, conceptual, 'Whether cognitive evacuation is a real historical structure or modern reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__contraction_reading, 0, 170).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hono_tr_t30, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(hono_tr_t60, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(hono_tr_t90, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 90, 0.55).
narrative_ontology:measurement(hono_tr_t120, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 120, 0.45).
narrative_ontology:measurement(hono_tr_t150, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 150, 0.15).
narrative_ontology:measurement(hono_tr_t170, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 170, 0.05).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(hono_be_t30, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(hono_be_t60, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 60, 0.5).
narrative_ontology:measurement(hono_be_t90, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 90, 0.38).
narrative_ontology:measurement(hono_be_t120, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 120, 0.22).
narrative_ontology:measurement(hono_be_t150, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 150, 0.08).
narrative_ontology:measurement(hono_be_t170, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 170, 0.02).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(hono_su_t30, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(hono_su_t60, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement(hono_su_t90, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 90, 0.4).
narrative_ontology:measurement(hono_su_t120, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 120, 0.25).
narrative_ontology:measurement(hono_su_t150, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 150, 0.08).
narrative_ontology:measurement(hono_su_t170, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 170, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism__decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the honor_satisfaction_mechanism kernel, which decomposes into three structurally distinct interpretations of the same historical institution: contraction (category-level evacuation), decline (marginal frequency reduction), and composite (multiple overlapping causes). Each reading carries a distinct epsilon and stakeholder configuration; they are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
