% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__drop_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: honor_violence_legitimacy__drop_reading
 *   human_readable: Honor Violence Legitimacy: Drop Reading
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the drop reading of the
 *   honor_violence_legitimacy kernel: the practice of dueling became
 *   practically rare due to rising external costs (state prosecution,
 *   mortality, economic burden), but the conceptual legitimacy of honor
 *   violence remained intact within the gentlemanly class. The constraint is
 *   the residual legitimacy structure — a piton in which the original
 *   coordination function (dispute resolution through personal combat) has
 *   atrophied, leaving mostly theatrical maintenance of the honor code. The
 *   agenda-setters (gentlemanly norm arbiters) could formally repudiate the
 *   code but do not because the diffuse cost of inertia is lower than the
 *   political cost of explicit abolition. There is no concentrated
 *   beneficiary capturing rent; the constraint persists because no party is
 *   hurt enough to fix it and no party profits enough to maintain it.
 *
 * KEY AGENTS:
 *   - Gentlemanly norm arbiters: Agenda-setter (organized/constrained) — maintain the conceptual frame without active enforcement, could repudiate it but gain nothing from doing so.
 *   - Honor-bound gentry: Primary payer (moderate/identity_locked) — bear diffuse costs of navigating a masculinity code that still structurally sanctions violence.
 *   - Challenged men: Direct payer (powerless/trapped) — when the rare challenge occurs, face the immediate existential and legal costs of a still-legitimate practice.
 *   - Legal state: Observer (institutional/analytical) — prosecutes actual duels but does not invest in dismantling the underlying honor culture.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__drop_reading, 0.35).
domain_priors:suppression_score(honor_violence_legitimacy__drop_reading, 0.38).
domain_priors:theater_ratio(honor_violence_legitimacy__drop_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__drop_reading, piton).
narrative_ontology:human_readable(honor_violence_legitimacy__drop_reading, "Honor Violence Legitimacy: Drop Reading").
narrative_ontology:topic_domain(honor_violence_legitimacy__drop_reading, "historical_sociology/legal_anthropology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__drop_reading, 'e1bc76e0-0177-40aa-ba84-402ed9cb6165').
narrative_ontology:cs_kernel_codification('e1bc76e0-0177-40aa-ba84-402ed9cb6165', distributed).
narrative_ontology:cs_authority_grounding('e1bc76e0-0177-40aa-ba84-402ed9cb6165', practice).
narrative_ontology:cs_interpretation_layer_present('e1bc76e0-0177-40aa-ba84-402ed9cb6165').
narrative_ontology:cs_reading_relation('e1bc76e0-0177-40aa-ba84-402ed9cb6165', honor_violence_legitimacy__contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('e1bc76e0-0177-40aa-ba84-402ed9cb6165', honor_violence_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('e1bc76e0-0177-40aa-ba84-402ed9cb6165', foundational, lethal_combat_remains_thinkable).
narrative_ontology:cs_axiom_status(lethal_combat_remains_thinkable, holdable).
narrative_ontology:cs_axiom_grounding('e1bc76e0-0177-40aa-ba84-402ed9cb6165', lethal_combat_remains_thinkable, empirically_contingent).
narrative_ontology:cs_axiom('e1bc76e0-0177-40aa-ba84-402ed9cb6165', secondary, costs_are_external_to_code).
narrative_ontology:cs_axiom_status(costs_are_external_to_code, holdable).
narrative_ontology:cs_axiom_grounding('e1bc76e0-0177-40aa-ba84-402ed9cb6165', costs_are_external_to_code, empirically_contingent).
narrative_ontology:cs_reference_frame('e1bc76e0-0177-40aa-ba84-402ed9cb6165', personal_combat_dispute_resolution).
narrative_ontology:cs_drift_state('e1bc76e0-0177-40aa-ba84-402ed9cb6165', post_centralization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e1bc76e0-0177-40aa-ba84-402ed9cb6165', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, honor_bound_gentry).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, challenged_men).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% They sit at the top of informal honor networks and judge whether challenges are proper and apologies sufficient. They could issue a formal repudiation of the dueling code but have not done so, because the issue has become rare enough that explicit abolition would draw more attention than quiet neglect. They do not collect material rents from the practice; their interest is in preserving the dignity of a fading tradition.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, gentlemanly_norm_arbiters, agenda_setter,
    organized, biographical, constrained, national).

% They organize their masculine identity around the code of honor and must continually signal willingness to fight lest they be read as cowards. They rarely duel in practice because of legal risk, but they still structure their social interactions around challenge, insult, and satisfaction. Leaving the gentlemanly class entirely is possible only by abandoning their social world.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, honor_bound_gentry, payer,
    moderate, biographical, identity_locked, national).

% When a challenge is issued, they face an immediate bind: accept and risk death or prosecution, or refuse and face social ruin within the only community they know. Because actual duels are rare, this bind seldom materializes, but the structural possibility shapes their deference to social superiors.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, challenged_men, payer,
    powerless, immediate, trapped, regional).

% It criminalizes dueling and prosecutes participants when cases come to its attention, but it does not mount a sustained cultural campaign against the gentlemanly honor code itself. It treats dueling as a residual crime of a social order that is fading on its own.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, legal_state, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__drop_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__drop_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its active phase, dueling provided a decentralized mechanism for resolving status disputes among social equals without recourse to centralized courts, stabilizing hierarchical peer relations through codified lethal ritual.
% TRANSFER_FUNCTION: Moves social standing, physical risk, and reputational capital between challenger and challenged; in the atrophied phase, transfers diffuse status-anxiety and identity-maintenance labor among the honor-bound class.
% ABSENT_VOICES: Women, commercial middle classes, and religious reformers who rejected honor-violence logic were structurally excluded from the gentlemanly public sphere in which dueling legitimacy was negotiated; they would have argued for formal abolition of the conceptual frame rather than its practical suppression.
% DISAPPEARANCE_RATIONALE: If the structural legitimacy of honor violence disappeared overnight, the gentlemanly code would lose its foundational sanction for masculine status assertion. Social relations among the honor-bound class would reorganize around apology, legal recourse, or commercial negotiation rather than the latent threat of lethal confrontation, while the arbiters would no longer need to perform indifference to state law.
% FOUNDING_PROBLEM: How to regulate status competition among armed social equals in the absence of a centralized honor court, preventing endless feuding while preserving masculine autonomy and peer equality.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and state institutions attest that centralized courts monopolized violence and expanded civil adjudication; the gentlemanly class itself no longer cites dueling as a necessary dispute-resolution mechanism, corroborating that the founding problem is solved. This attestation comes from outside the residual honor culture.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__drop_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__drop_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__drop_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_violence_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__drop_reading, 0.35, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__drop_reading_tests).
:- end_tests(honor_violence_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater ratio is high (0.72) because the constraint is now primarily performative: gentlemen maintain postures of willingness to fight, codes of honor are ritually invoked, and literary tropes sustain the frame, while actual combat is vanishingly rare. Base extractiveness is moderate-low (0.35) because the conceptual availability still structures social behavior (anxiety, status maintenance, occasional challenges) but the external costs have suppressed the highest-extraction phase. Suppression is moderate (0.38): the primary suppression is social ostracism for refusing the code's logic, reinforced by state prosecution of actual combat. Resistance is low (0.25) because the practice is withering; reformist energies have moved to other targets. Accessibility collapse is moderate-high (0.60) within the honor-bound class because alternatives like legal recourse or apology are coded as unmanly.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (gentlemanly norm arbiters), the constraint appears as a fading but dignified tradition that defines class identity; from the payer seats (honor-bound gentry and challenged men), it appears as a persistent social tax on masculine status that occasionally threatens lethal consequences. The engine computes this divergence from the structural data: arbiters have constrained exit (they could change the code but would lose standing), while payers have identity_locked or trapped exit.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation produces d near 1.0 for challenged_men (trapped, powerless, direct targets of the rare but operative challenge) and d near 0.7-0.8 for honor_bound_gentry (identity_locked, moderate power, diffuse targets). The arbiters have moderate power and constrained exit, which would place them near d ≈ 0.4, but because they are agenda_setters with no beneficiary capture, the structural derivation does not push them to the beneficiary end. No directionality override is needed because the absence of beneficiaries correctly signals that no seat is subsidized by the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as piton prevents mislabeling the residual legitimacy as a rope (there is no active coordination benefit) or a snare (there is no concentrated beneficiary enforcing extraction). The founding problem — decentralized honor regulation — is dead. The constraint persists by inertia because the cost of formal repudiation to the arbiters exceeds the diffuse cost the payers bear. If a concentrated beneficiary were discovered (e.g., a weapons trade or a class that actively profits from status differentiation), the constraint would reclassify as snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    drop_vs_contraction_ambiguity,
    'Is the practical rarity of dueling attributable primarily to rising external costs (legal prosecution, mortality risk) while conceptual legitimacy remains intact, or has the honor concept itself contracted to exclude violence?',
    'Comparative historical analysis of honor codes: if challenged parties retain social standing after non-violent resolution and the concept of ''satisfaction'' migrates to non-lethal forms, contraction is supported; if insulted parties still suffer social death for failing to issue challenges but are deterred by state punishment, the drop reading is supported.',
    'If contraction, the legitimacy kernel has shifted and the constraint is an atrophied snare or piton of a dead code; if drop, the legitimacy persists as a latent piton that could reactivate if external costs fell.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drop_vs_contraction_ambiguity, conceptual, 'Whether the decline is cost-driven or concept-driven.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers to dueling) or internalized (the honor-bound gentry believe they must maintain a posture of willingness to fight)?',
    'Post-exit suppression trajectory: observe whether men who leave the honor-bound class (through social mobility or geographic relocation) continue to organize their masculine identity around violence-backed status claims.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism.').

omega_variable(
    arbiter_inertia_or_interest,
    'Do gentlemanly norm arbiters maintain the conceptual legitimacy of dueling out of genuine identity inertia, or does some diffuse benefit (status differentiation, masculine boundary policing) concentrate enough to constitute a beneficiary?',
    'Examine whether arbiters who formally repudiate dueling suffer status loss within the peer group relative to those who maintain the old code.',
    'If concentrated benefit exists, the constraint is a snare masquerading as a piton; if pure inertia, the piton classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(arbiter_inertia_or_interest, empirical, 'Whether residual legitimacy serves a hidden beneficiary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__drop_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_drop_tr_t0, honor_violence_legitimacy__drop_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(honor_drop_tr_t12, honor_violence_legitimacy__drop_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(honor_drop_tr_t24, honor_violence_legitimacy__drop_reading, theater_ratio, 24, 0.44).
narrative_ontology:measurement(honor_drop_tr_t36, honor_violence_legitimacy__drop_reading, theater_ratio, 36, 0.54).
narrative_ontology:measurement(honor_drop_tr_t48, honor_violence_legitimacy__drop_reading, theater_ratio, 48, 0.64).
narrative_ontology:measurement(honor_drop_tr_t60, honor_violence_legitimacy__drop_reading, theater_ratio, 60, 0.72).

% Extraction over time
narrative_ontology:measurement(honor_drop_be_t0, honor_violence_legitimacy__drop_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(honor_drop_be_t12, honor_violence_legitimacy__drop_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(honor_drop_be_t24, honor_violence_legitimacy__drop_reading, base_extractiveness, 24, 0.46).
narrative_ontology:measurement(honor_drop_be_t36, honor_violence_legitimacy__drop_reading, base_extractiveness, 36, 0.42).
narrative_ontology:measurement(honor_drop_be_t48, honor_violence_legitimacy__drop_reading, base_extractiveness, 48, 0.38).
narrative_ontology:measurement(honor_drop_be_t60, honor_violence_legitimacy__drop_reading, base_extractiveness, 60, 0.35).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(honor_violence_legitimacy__drop_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% The natural-language concept 'honor violence legitimacy' decomposes into three structurally distinct readings: drop (this file), contraction, and composite. Each reading assigns a different causal mechanism to the decline of dueling and a different status to the kernel's conceptual content. They form a constraint family linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
