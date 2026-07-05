% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Honor-Satisfaction Duel — Category Evacuation (Contraction Reading)
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   This story instantiates the CONTRACTION reading of the
 *   honor-satisfaction-mechanism kernel: the claim that dueling did not
 *   merely decline in frequency (decline_reading) nor result from a bundle of
 *   independently operating causal mechanisms (composite_reading), but that
 *   the entire cognitive category making a duel a legible response to insult
 *   was evacuated from the possibility space of liberal-professional
 *   modernity. Under this reading, ε is measured as very low and falling
 *   further not because a coercive suppression apparatus successfully drove
 *   down an otherwise-persistent practice, but because there is structurally
 *   nothing left to extract from or enforce against — a category, once alive
 *   with real stakes (life, honor, standing), has become unthinkable rather
 *   than merely illegal or unfashionable. This is offered as one of three
 *   sibling constraints on the same kernel; the other two (decline_reading,
 *   composite_reading) are separate stories with their own ε trajectories and
 *   structural claims, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - post_duel_professional_gentry: beneficiary (institutional/analytical) — inherits a status economy that no longer risks lives
 *   - modern_state_legal_monopolists: beneficiary (institutional/analytical) — inherits sole jurisdiction over honor-harm adjudication as a downstream consequence, not a cause
 *   - historical_dueling_participants: excluded (powerless/trapped) — cannot corroborate or contest any reading; permanently unavailable as witnesses
 *   - social_historians_of_honor: observer (analytical/analytical) — reconstructs and adjudicates between competing readings of the kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__contraction_reading, 0.08).
domain_priors:suppression_score(honor_satisfaction_mechanism__contraction_reading, 0.12).
domain_priors:theater_ratio(honor_satisfaction_mechanism__contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, accessibility_collapse, 0.93).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(honor_satisfaction_mechanism__contraction_reading, "Honor-Satisfaction Duel — Category Evacuation (Contraction Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__contraction_reading, "historical_sociology/legal_history/normative_systems").

domain_priors:emerges_naturally(honor_satisfaction_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__contraction_reading, '60fb50cc-10d6-4e84-a8dc-83df8d5b6bb8').
narrative_ontology:cs_kernel_codification('60fb50cc-10d6-4e84-a8dc-83df8d5b6bb8', distributed).
narrative_ontology:cs_authority_grounding('60fb50cc-10d6-4e84-a8dc-83df8d5b6bb8', distributed).
narrative_ontology:cs_reading_relation('60fb50cc-10d6-4e84-a8dc-83df8d5b6bb8', honor_satisfaction_mechanism__decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('60fb50cc-10d6-4e84-a8dc-83df8d5b6bb8', honor_satisfaction_mechanism__composite_reading, influences).
narrative_ontology:cs_axiom('60fb50cc-10d6-4e84-a8dc-83df8d5b6bb8', foundational, honor_category_fully_evacuated_not_merely_rare).
narrative_ontology:cs_axiom_status(honor_category_fully_evacuated_not_merely_rare, holdable).
narrative_ontology:cs_axiom_grounding('60fb50cc-10d6-4e84-a8dc-83df8d5b6bb8', honor_category_fully_evacuated_not_merely_rare, empirically_contingent).
narrative_ontology:cs_axiom('60fb50cc-10d6-4e84-a8dc-83df8d5b6bb8', secondary, suppression_apparatus_not_the_operative_causal_mechanism).
narrative_ontology:cs_axiom_status(suppression_apparatus_not_the_operative_causal_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('60fb50cc-10d6-4e84-a8dc-83df8d5b6bb8', suppression_apparatus_not_the_operative_causal_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('60fb50cc-10d6-4e84-a8dc-83df8d5b6bb8', honor_as_zero_sum_publicly_defensible_possession).
narrative_ontology:cs_drift_state('60fb50cc-10d6-4e84-a8dc-83df8d5b6bb8', post_professionalization_liberal_order, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('60fb50cc-10d6-4e84-a8dc-83df8d5b6bb8', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, post_duel_professional_gentry).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, modern_state_legal_monopolists).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__contraction_reading, cognitive_category_evacuation_thesis).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__contraction_reading, honor_semantics_incommensurability_with_liberal_personhood).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The descendant class of the dueling gentry now operates inside a professional-bureaucratic status economy (careers, credentials, litigation, reputation management through institutions). They do not defend dueling and do not experience its absence as a loss; the category of 'satisfaction by combat' is simply not part of their available repertoire for resolving affronts. They benefit incidentally from a status system that no longer risks their lives to maintain rank.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, post_duel_professional_gentry, beneficiary,
    institutional, generational, analytical, national).

% Contemporary state legal systems inherit a jurisdiction over interpersonal harm and honor-vindication that dueling once contested. They benefit from the disappearance of a rival adjudicative category, but under this reading that benefit is a downstream consequence of category evacuation, not the mechanism that produced it — the state did not need to suppress dueling because the cognitive slot for it no longer existed to suppress.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, modern_state_legal_monopolists, beneficiary,
    institutional, civilizational, analytical, national).

% The historical gentlemen who once dueled are not present to attest whether their own practice was suppressed, declined, or became literally unthinkable to their successors. They cannot corroborate or contest this reading; their absence is total and permanent, not merely an information gap.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, historical_dueling_participants, excluded,
    powerless, civilizational, trapped, national).

% Historians reconstruct honor codes from correspondence, dueling codes (codes duello), newspaper accounts, and legal records. They debate whether the end of dueling reflects decline, suppression, composite causation, or a genuine categorical evacuation of the concept-space in which dueling was intelligible as a response to insult. This story's authoring seat is this reading's analytical position.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, social_historians_of_honor, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None, under this reading, in the present tense — there is no coordination problem being solved because there is no longer a live category ('affront requiring blood satisfaction') for any mechanism to coordinate around. Historically, dueling coordinated status-claims among status-equals without appeal to a common judge; that coordination function did not decline or get suppressed, it lost its referent.
% TRANSFER_FUNCTION: None presently active. Historically dueling transferred risk of death/injury and reputational stakes between social equals as the price of honor-restoration; under the contraction reading this transfer mechanism is not weakened or attenuated today, it is categorically absent — there is nothing left to transfer because the concept it transferred (satisfaction-by-combat) is not cognitively available.
% ABSENT_VOICES: The historical participants themselves cannot testify to whether they experienced a rising cost of dueling (decline reading) or would find the entire question unintelligible if resurrected today (contraction reading). Modern honor-code revivalists and libertarian dueling advocates occasionally object that dueling remains a coherent option merely made illegal — their objection is itself evidence for the decline/suppression readings and is a live rebuttal to this reading that this story does not adjudicate.
% DISAPPEARANCE_RATIONALE: Under the contraction reading, there is nothing left to disappear: the claim IS that dueling has already fully evacuated the space of live options. Nothing would rearrange today if a formal duel-suppression law were repealed tomorrow, because the cognitive category that made dueling a legible response to insult no longer exists for any party to reactivate. This is the diagnostic signature distinguishing contraction from decline: a declining practice could theoretically be revived by removing suppression; an evacuated category cannot be revived by removing suppression because suppression was never the operative mechanism.
% FOUNDING_PROBLEM: Dueling was built to solve status-equals' need to resolve affronts to honor without appeal to a common judge, in a social order where honor was a zero-sum, publicly legible, and violently defensible possession constitutive of one's standing as a gentleman.
% FOUNDING_PROBLEM_CORROBORATION: Social and cultural historians outside any dueling-adjacent interest group (e.g., historians of the professionalization of the officer corps and the bourgeoisie, working from correspondence and legal archives with no stake in honor culture's survival) attest that the underlying concept of honor as a public, zero-sum, blood-defensible possession has itself dissolved in liberal-professional societies — not merely that enforcement against dueling intensified. No living party benefits from dueling's absence in a way that would motivate misrepresenting this as evacuation rather than suppression, which is itself a data point supporting rather than undermining the corroboration.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__contraction_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__contraction_reading_tests).

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
 *   Extractiveness and suppression are authored low and falling because, under contraction, there is no active extraction relationship and no active suppression apparatus doing ongoing work — the category is gone, not policed. Accessibility_collapse is authored very high (0.93) because this is the central empirical claim of the contraction reading: alternatives (i.e., dueling as a live option) have not merely become costly, they have become unavailable as a cognitively legible move at all, much as one cannot 'choose' to resolve a dispute via trial-by-combat in a modern courtroom — the option is not suppressed, it is outside the category system. Resistance is authored near-zero (0.04) because a genuinely evacuated category meets no active resistance from anyone attempting to revive it through ordinary means; what little revivalist rhetoric exists is itself evidence for the rival decline/suppression readings, not evidence against evacuation, since true revivalists treat dueling as merely illegal rather than as literally unthinkable.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries here (professional gentry, state legal monopolists) are authored with very weak/incidental directionality — they collect ambient benefits of a category's disappearance but are not agents actively producing or maintaining that disappearance under this reading, which is precisely what distinguishes contraction from the decline/composite readings where suppression or bourgeois-norm-enforcement would be active, ongoing, extractive work performed by identifiable agents against identifiable victims. There are no victims declared under this reading because a category evacuation, by construction, has no one currently paying an extraction cost — the historical participants who might have been 'victims' of, e.g., legal suppression are not victims of evacuation; they are simply unavailable as a class to this reading's present tense.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy mislabeling in the opposite direction from the usual case: rather than a persisting arrangement whose founding function has died while extraction continues (the classic piton/mandatrophy pattern), contraction claims the founding problem AND the arrangement AND the extraction have all died together, evacuated as a single unit. The founding_problem_status is authored 'dead' with outside corroboration, and disappearance_verdict is 'world_unchanged' — this is the correct signature for a Mountain-like natural-seeming absence rather than a suppressed-but-latent Snare. The risk this reading must guard against is treating evacuation as self-evidently true merely because no one currently duels; the sibling decline_reading exists precisely to hold open the alternative that dueling is merely rare and illegal, not unthinkable, and that a sufficiently destabilized legal order could see its cognitive return.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contraction_vs_decline_discriminator,
    'Is dueling''s disappearance genuinely a category evacuation (unthinkable) or an extreme case of decline (vanishingly rare but still cognitively available, as evidenced by occasional 20th/21st century revivalist duels and dueling scenes remaining fully intelligible in fiction and law-review hypotheticals)?',
    'Compare historical evidence of whether post-1900 individuals who experienced grave public insult ever seriously considered dueling as a live option (diary/letter evidence of the option being weighed and rejected on cost/legality grounds, which would support decline) versus evidence that the very concept of blood-satisfaction for insult ceased to arise as a candidate response at all (which would support contraction). The persistence of intelligible fictional and hypothetical dueling narratives is some evidence against pure contraction, since a truly evacuated category should be difficult even to represent coherently.',
    'If decline is correct, this story''s very low ε and near-total accessibility_collapse are overstated — the true structural picture would resemble decline_reading''s declining-but-recoverable extraction/suppression profile, and this story''s disappearance_verdict of world_unchanged would need revision toward contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contraction_vs_decline_discriminator, conceptual, 'Whether dueling''s end is genuine cognitive evacuation or extreme-tail decline that remains conceptually available.').

omega_variable(
    category_evacuation_is_underdetermined_by_absence,
    'Can any historical record ever positively establish that a category became unthinkable, as opposed to merely establishing that no instances occurred — i.e., is contraction empirically distinguishable from decline-to-zero at all?',
    'Would require evidence of cognitive processing, not just behavioral frequency — e.g., historical individuals'' explicit reasoning showing the option was not considered at all (contraction) versus considered and rejected (decline). Such evidence is sparse and interpretively contested by nature.',
    'If the two readings are not empirically distinguishable even in principle from the available historical record, the contraction reading''s claim to be a distinct structural fact (rather than a redescription of decline_reading''s endpoint) is weakened, and the kernel''s three-way split may collapse toward a two-way (decline vs. composite) split with contraction as a limiting case of decline rather than an independent mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(category_evacuation_is_underdetermined_by_absence, conceptual, 'Whether category-evacuation is a testable historical claim distinct from decline-to-vanishing-frequency.').

omega_variable(
    beneficiary_declaration_on_a_mountain_claim,
    'Does declaring beneficiaries (professional gentry, state legal monopolists) on a claimed Mountain constraint indicate the contraction reading is itself a constructed, self-serving narrative — i.e., is ''dueling became unthinkable'' a story told by the successor class precisely because it flatters their own moral and institutional position by making the transition seem natural and inevitable rather than the result of contested suppression from which they benefited?',
    'Cross-check against sources contemporaneous with the transition (e.g., anti-dueling societies, legal debates over dueling statutes) for explicit awareness that suppression was ongoing, contested political work rather than an already-completed cognitive fact — extensive contemporaneous suppression advocacy would favor the FSM (false-summit) hypothesis that this ''mountain'' is actually constructed.',
    'If FSM applies, this reading should reclassify toward tangled_rope or scaffold: dueling''s end would be better modeled as actively engineered institutional transition (state monopolization, class-interest-driven norm change) dressed retrospectively as inevitability, rather than as genuine category evacuation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_declaration_on_a_mountain_claim, conceptual, 'Whether the contraction reading''s naturalization of dueling''s end is itself a beneficiary-serving narrative construction (false summit candidate).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__contraction_reading, 1780, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1780, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1780, 0.1).
narrative_ontology:measurement(hono_tr_t1810, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1810, 0.09).
narrative_ontology:measurement(hono_tr_t1840, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1840, 0.08).
narrative_ontology:measurement(hono_tr_t1870, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1870, 0.07).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1900, 0.06).
narrative_ontology:measurement(hono_tr_t1930, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1930, 0.05).
narrative_ontology:measurement(hono_tr_t1950, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1950, 0.05).

% Extraction over time
narrative_ontology:measurement(hono_be_t1780, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1780, 0.15).
narrative_ontology:measurement(hono_be_t1810, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1810, 0.13).
narrative_ontology:measurement(hono_be_t1840, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1840, 0.11).
narrative_ontology:measurement(hono_be_t1870, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1870, 0.1).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1900, 0.09).
narrative_ontology:measurement(hono_be_t1930, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1930, 0.08).
narrative_ontology:measurement(hono_be_t1950, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1950, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(honor_satisfaction_mechanism__contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism__decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the honor_satisfaction_mechanism kernel. decline_reading models dueling's end as declining-but-still-intelligible practice (ε tracks enforcement/social-cost intensity, potentially recoverable). composite_reading models it as the joint product of several partially independent mechanisms (state monopoly on violence, bourgeois professional norms, insurance actuarial disincentives, category-shift), each separately weighted. This contraction_reading claims the strongest and least recoverable structural outcome: full evacuation of the cognitive category, with ε near-floor and non-restorable by mere deregulation. The three stories are linked bidirectionally in the network so that contamination/coupling analysis can compare how each reading's ε trajectory and classification diverge from a shared kernel and shared historical interval.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
