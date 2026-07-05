% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__cultural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__cultural_contraction_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: honor_satisfaction_substrate__cultural_contraction_reading
 *   human_readable: Erosion of the Honor Code as the Interpretive Substrate for Dueling (Cultural Contraction Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   In the eighteenth and early nineteenth centuries, dueling functioned as a
 *   socially legible mechanism for defending honor within aristocratic,
 *   military, and later commercial elites across Western Europe and the
 *   American South. By the early twentieth century, dueling had become not
 *   merely illegal but unintelligible — a practice that struck contemporaries
 *   as archaic or absurd rather than as a live option foreclosed by force.
 *   This reading holds that the honor code's own collapse as an interpretive
 *   substrate — the shift from honor (contingent, externally validated,
 *   violence-redeemable status) to dignity (unconditional, intrinsic
 *   personhood-based status) — is the mechanism of dueling's disappearance,
 *   prior to and independent of any legal suppression.
 *
 * KEY AGENTS:
 *   - urban_professional_classes: primary beneficiaries of the dignity-substrate shift — status portable without violent defense
 *   - commercial_bourgeoisie: beneficiaries whose reputational capital migrated to creditworthiness and legal standing
 *   - residual_aristocratic_honor_culture: bearers of the eroding substrate, losing their interpretive audience
 *   - historians_of_honor_culture: analytical observers tracing the substrate-collapse mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__cultural_contraction_reading, 0.08).
domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, 0.05).
domain_priors:theater_ratio(honor_satisfaction_substrate__cultural_contraction_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, 0.06).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__cultural_contraction_reading, mountain).
narrative_ontology:human_readable(honor_satisfaction_substrate__cultural_contraction_reading, "Erosion of the Honor Code as the Interpretive Substrate for Dueling (Cultural Contraction Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__cultural_contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(honor_satisfaction_substrate__cultural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__cultural_contraction_reading, '088d68fe-d750-4ff5-8ec6-456932a1e79b').
narrative_ontology:cs_kernel_codification('088d68fe-d750-4ff5-8ec6-456932a1e79b', distributed).
narrative_ontology:cs_authority_grounding('088d68fe-d750-4ff5-8ec6-456932a1e79b', practice).
narrative_ontology:cs_interpretation_layer_present('088d68fe-d750-4ff5-8ec6-456932a1e79b').
narrative_ontology:cs_reading_relation('088d68fe-d750-4ff5-8ec6-456932a1e79b', honor_satisfaction_substrate__practice_decline_reading, forecloses).
narrative_ontology:cs_reading_relation('088d68fe-d750-4ff5-8ec6-456932a1e79b', honor_satisfaction_substrate__composite_overdetermined_reading, influences).
narrative_ontology:cs_axiom('088d68fe-d750-4ff5-8ec6-456932a1e79b', foundational, honor_code_content_itself_transformed).
narrative_ontology:cs_axiom_status(honor_code_content_itself_transformed, holdable).
narrative_ontology:cs_axiom_grounding('088d68fe-d750-4ff5-8ec6-456932a1e79b', honor_code_content_itself_transformed, empirically_contingent).
narrative_ontology:cs_axiom('088d68fe-d750-4ff5-8ec6-456932a1e79b', secondary, dueling_exited_thinkable_action_set_prior_to_enforcement).
narrative_ontology:cs_axiom_status(dueling_exited_thinkable_action_set_prior_to_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('088d68fe-d750-4ff5-8ec6-456932a1e79b', dueling_exited_thinkable_action_set_prior_to_enforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('088d68fe-d750-4ff5-8ec6-456932a1e79b', honor_as_contingent_externally_defensible_status).
narrative_ontology:cs_drift_state('088d68fe-d750-4ff5-8ec6-456932a1e79b', early_twentieth_century_dignity_consolidation, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('088d68fe-d750-4ff5-8ec6-456932a1e79b', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__cultural_contraction_reading, urban_professional_classes).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__cultural_contraction_reading, commercial_bourgeoisie).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__cultural_contraction_reading, residual_aristocratic_honor_culture).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__cultural_contraction_reading, dignity_culture_supersession_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lawyers, physicians, clerks, and merchants whose social standing came to rest on credentialed competence and institutional reputation rather than on personal readiness to answer insult with the sword or pistol. As the honor code's underlying plausibility structure eroded within this class, dueling simply stopped registering as a live option for defending one's standing — it became a category error, not a suppressed impulse.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, urban_professional_classes, beneficiary,
    organized, generational, mobile, national).

% Merchants and industrialists whose reputational capital was increasingly measured in creditworthiness, contract-keeping, and legal standing rather than in personal courage demonstrated through ritual combat. The rise of dignity-based self-worth, portable and unconditional rather than honor's contingent, defensible status, removed the felt need to stake reputation on violent vindication.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, commercial_bourgeoisie, beneficiary,
    organized, generational, mobile, national).

% Pockets of the older aristocratic and military milieu for whom honor remained a lived, defensible category. As the surrounding interpretive substrate collapsed, they found dueling decreasingly recognized by peers, courts, or the public as meaningful rather than merely criminal or absurd — their vocabulary of offense-and-satisfaction lost its audience, not merely its legality.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, residual_aristocratic_honor_culture, payer,
    moderate, biographical, trapped, regional).

% Scholars who trace the disappearance of dueling to the deeper substrate question: did the practice die because the code beneath it changed, or merely because enforcement rose around an unchanged code? This reading argues the former — the code itself dissolved, taking the practice's intelligibility with it.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, historians_of_honor_culture, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the ordinary sense — this is not a coordination mechanism but a shared interpretive substrate (a cultural common-knowledge structure) that once made dueling a legible, socially readable response to insult. Its function was to render honor claims mutually intelligible across a status-conscious population, not to solve an allocation or enforcement problem.
% TRANSFER_FUNCTION: Nothing is transferred by this constraint in the extractive sense; it is a background condition of intelligibility. What moves, historically, is status-currency itself: from honor (contingent, publicly defensible, violence-redeemable) to dignity (unconditional, held by virtue of personhood, not defensible through combat). No party collects rents from the substrate's erosion — it is a structural transformation, not an extraction mechanism.
% ABSENT_VOICES: The residual honor-culture holdouts (declining aristocratic and military milieux) had no institutional voice capable of halting the substrate's dissolution — no court, legislature, or press organ existed whose ruling could have reversed a shift in what ordinary people found intelligible as a response to insult. Their objection, where recorded, appears as lament rather than argument, because the argument's audience had already left.
% DISAPPEARANCE_RATIONALE: This constraint names the erosion of a substrate, not a standing arrangement that could 'disappear' further — dueling is already unthinkable across the relevant population under this reading, and no active party depends on the substrate remaining intact for present arrangements to function. Nothing rearranges because nothing currently rests on the honor code's operative force; the world already reflects its absence.
% FOUNDING_PROBLEM: Before honor's own dissolution, the practice of dueling addressed a real coordination problem: how to adjudicate and settle competing claims to social standing and personal veracity in the absence of centralized courts capable of certifying reputation. This reading's constraint — the code's own transformation — was not 'built' to solve a problem; it is the erosion of the substrate that once solved one.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside any beneficiary group by historians of manners and legal historians (e.g., studies of the shift from Bertram Wyatt-Brown-style Southern honor culture and European dueling codes to twentieth-century dignity norms) who document, independent of any party benefiting from the change, that courts, newspapers, and dueling manuals themselves record contemporaries' growing inability to treat the ritual as meaningful rather than criminal or ridiculous.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__cultural_contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__cultural_contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__cultural_contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__cultural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__cultural_contraction_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_satisfaction_substrate__cultural_contraction_reading),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction and suppression are both authored low (0.08, 0.05) because under this reading no party actively enforces the code's disappearance or collects from its erosion — it is a mountain-erosion process, a natural-seeming shift in collectively held meaning, not an extractive mechanism. Accessibility collapse is authored high (0.88) because once the dignity-substrate takes hold, dueling as a meaningful response genuinely ceases to be available as a live option — not merely penalized, but literally unthinkable to those inside the new substrate. Resistance is authored low (0.06) because there is minimal active contestation of the shift itself (contrast with a suppression reading, where legal resistance to dueling would be highly visible); the residual honor-culture holdouts do not organize resistance to the substrate's collapse so much as fail to be heard by it.
 *
 * DIRECTIONALITY LOGIC:
 *   Urban professionals and the commercial bourgeoisie are declared beneficiaries because the dignity-substrate directly serves their status-interests — unconditional worth requires no violent defense and is compatible with commercial and professional life. The residual aristocratic/military honor culture is the structural payer: not extracted from by any agent, but bearing the cost of finding their entire vocabulary of offense-and-satisfaction suddenly untranslatable. No victim group is declared because this reading does not posit an extractive beneficiary — the shift is a genuine substrate transformation with diffuse winners, not a captured rent stream, consistent with the FSM check requiring an omega given declared beneficiaries on a mountain claim.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy mislabeling in a specific direction: because dueling's disappearance is so often narrated as successful *suppression* (law triumphing over barbarism), there is a temptation to read the whole phenomenon as a Tangled Rope or Snare (state power extracting compliance from a resistant honor culture). This reading blocks that collapse by insisting the code's own plausibility structure eroded first — the founding problem (adjudicating contested honor claims without central courts) went dead not because it was solved by force but because the class of claims it was designed to solve stopped being asked. Treating this as mountain erosion rather than suppression prevents mislabeling a substrate collapse as an extraction victory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substrate_vs_suppression_causal_priority,
    'Did the honor code''s collapse as an interpretive substrate causally precede and independently drive dueling''s disappearance, or did legal/institutional suppression (criminalization, professional sanctions, military discipline) do the causal work while the code''s apparent transformation was a post-hoc rationalization?',
    'Fine-grained dueling-frequency time series cross-referenced against enforcement-intensity time series by jurisdiction; if dueling rates fell in regions/periods with weak enforcement but strong dignity-norm diffusion (e.g., through print culture, professional associations), this reading is supported over practice_decline_reading.',
    'If suppression turns out to be causally prior and sufficient, this constraint collapses into the sibling practice_decline_reading and the mountain-erosion framing is displaced by an enforcement-mechanism framing (likely tangled_rope or snare at the level of the suppressing state).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_vs_suppression_causal_priority, empirical, 'Whether cultural substrate collapse or legal suppression was causally prior in dueling''s decline.').

omega_variable(
    natural_law_vs_constructed_substrate_shift,
    'Is the honor-to-dignity substrate shift a genuine mountain (an emergent, unengineered cultural transformation with no identifiable architect) given that identifiable classes (urban professionals, commercial bourgeoisie) benefited from it — or was the shift partly constructed/promoted by those classes as a status-competition strategy against aristocratic honor codes?',
    'Examine whether professional and commercial associations actively propagated dignity-norms as an explicit status strategy (e.g., through etiquette literature, professional codes of conduct) versus whether the shift shows no identifiable promotional agency and appears as diffuse emergent change across unrelated social sectors simultaneously.',
    'If deliberate promotion by beneficiary classes is found, this constraint should be reclassified via the false_summit_mountain signature toward tangled_rope — a constructed substrate shift serving class interests dressed as natural cultural evolution. If no promotional agency is found, the mountain classification holds despite declared beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_substrate_shift, conceptual, 'Whether declared beneficiaries indicate a constructed shift or incidental beneficiaries of a genuine natural-law-like cultural transformation.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does this reading''s premise diverge from the sibling practice_decline_reading and composite_overdetermined_reading — specifically, is the disagreement about WHETHER the code changed at all, or only about its causal WEIGHT relative to enforcement?',
    'Compare contemporaneous primary sources (dueling manuals, honor-code treatises, court records) across the interval for explicit evidence of changing definitions of honor/insult/satisfaction, versus records showing stable definitions with rising enforcement cost alone.',
    'If sources show the definitional content of honor remained stable while only enforcement intensified, this reading is wrong and practice_decline_reading is the accurate single-mechanism account. If sources show both dynamics with non-separable interaction effects, composite_overdetermined_reading is the accurate account and this reading and its sibling are both partial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locates the precise structural disagreement among the three kernel readings: content-change vs. enforcement-weight vs. non-independence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__cultural_contraction_reading, 1800, 1930).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1800, 0.05).
narrative_ontology:measurement(hono_tr_t1830, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1830, 0.08).
narrative_ontology:measurement(hono_tr_t1860, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1860, 0.14).
narrative_ontology:measurement(hono_tr_t1890, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1890, 0.11).
narrative_ontology:measurement(hono_tr_t1910, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1910, 0.1).
narrative_ontology:measurement(hono_tr_t1930, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1930, 0.1).

% Extraction over time
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1800, 0.1).
narrative_ontology:measurement(hono_be_t1830, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1830, 0.1).
narrative_ontology:measurement(hono_be_t1860, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1860, 0.09).
narrative_ontology:measurement(hono_be_t1890, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1890, 0.09).
narrative_ontology:measurement(hono_be_t1910, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1910, 0.08).
narrative_ontology:measurement(hono_be_t1930, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1930, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(honor_satisfaction_substrate__cultural_contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__cultural_contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate__practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate__composite_overdetermined_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the honor_satisfaction_substrate kernel. practice_decline_reading holds the code persisted and enforcement alone explains decline (this reading forecloses that premise directly — both cannot be true of the same historical process, since one asserts the code changed and the other asserts it did not). composite_overdetermined_reading holds both mechanisms operated jointly and non-independently; this reading influences that account by supplying the endogenous-delegitimation half of its composite causal story without foreclosing the joint-causation claim. Each story carries its own ε, stakeholders, and classification per the ε-invariance principle; this file must not be read as adjudicating the kernel contest on its own.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
