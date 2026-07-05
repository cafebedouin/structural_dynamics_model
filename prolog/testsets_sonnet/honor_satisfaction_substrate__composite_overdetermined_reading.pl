% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__composite_overdetermined_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__composite_overdetermined_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: honor_satisfaction_substrate__composite_overdetermined_reading
 *   human_readable: Honor-Satisfaction Substrate: Composite Overdetermined Decline Reading
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   The historical decline of dueling in Western elite societies (roughly
 *   1750-1900) is a canonical case for the honor-satisfaction substrate
 *   kernel. Rather than treat the decline as caused by law alone or by
 *   cultural change alone, this reading holds that the coordination function
 *   (rope: mutual recognition among duelists that the code settled disputes)
 *   collapsed under legal pressure AT THE SAME TIME AND THROUGH THE SAME
 *   INSTITUTIONS that were re-narrating honor itself (mountain-erosion:
 *   honor's substrate transformed from combat-readiness to legal/professional
 *   standing), and that neither mechanism can be cleanly subtracted from the
 *   other's effect size.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, 0.44).
domain_priors:suppression_score(honor_satisfaction_substrate__composite_overdetermined_reading, 0.58).
domain_priors:theater_ratio(honor_satisfaction_substrate__composite_overdetermined_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__composite_overdetermined_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__composite_overdetermined_reading, "Honor-Satisfaction Substrate: Composite Overdetermined Decline Reading").
narrative_ontology:topic_domain(honor_satisfaction_substrate__composite_overdetermined_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__composite_overdetermined_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__composite_overdetermined_reading, '500aa9cb-337c-466f-b103-059137b9fdb8').
narrative_ontology:cs_kernel_codification('500aa9cb-337c-466f-b103-059137b9fdb8', distributed).
narrative_ontology:cs_authority_grounding('500aa9cb-337c-466f-b103-059137b9fdb8', practice).
narrative_ontology:cs_interpretation_layer_present('500aa9cb-337c-466f-b103-059137b9fdb8').
narrative_ontology:cs_reading_relation('500aa9cb-337c-466f-b103-059137b9fdb8', honor_satisfaction_substrate__practice_decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('500aa9cb-337c-466f-b103-059137b9fdb8', honor_satisfaction_substrate__cultural_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('500aa9cb-337c-466f-b103-059137b9fdb8', foundational, causal_pathways_are_non_separable).
narrative_ontology:cs_axiom_status(causal_pathways_are_non_separable, holdable).
narrative_ontology:cs_axiom_grounding('500aa9cb-337c-466f-b103-059137b9fdb8', causal_pathways_are_non_separable, empirically_contingent).
narrative_ontology:cs_axiom('500aa9cb-337c-466f-b103-059137b9fdb8', secondary, shared_institutions_jointly_produce_legal_and_cultural_change).
narrative_ontology:cs_axiom_status(shared_institutions_jointly_produce_legal_and_cultural_change, holdable).
narrative_ontology:cs_axiom_grounding('500aa9cb-337c-466f-b103-059137b9fdb8', shared_institutions_jointly_produce_legal_and_cultural_change, empirically_contingent).
narrative_ontology:cs_reference_frame('500aa9cb-337c-466f-b103-059137b9fdb8', elite_private_honor_adjudication).
narrative_ontology:cs_drift_state('500aa9cb-337c-466f-b103-059137b9fdb8', post_professionalization_1900, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('500aa9cb-337c-466f-b103-059137b9fdb8', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, elite_male_status_incumbents).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, modern_state_monopoly_on_violence).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, dueling_participants_and_seconds).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, socially_pressured_non_combatants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Aristocratic and professional-class men whose social standing was historically secured through the code duello. As the century progresses, this same class begins administering the code's own transformation into non-lethal honor rituals and, ultimately, its abandonment — they hold both the coordination function (deciding who counts as a gentleman) and the levers that dismantle the practice's lethal enforcement mechanism when it becomes reputationally costly rather than beneficial.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, elite_male_status_incumbents, beneficiary,
    powerful, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__composite_overdetermined_reading, elite_male_status_incumbents, agenda_setter).

% Courts, legislatures, and prosecutors who criminalize dueling and, more importantly, stop protecting duelists from ordinary homicide law. The state benefits by consolidating exclusive legitimate authority over lethal violence, absorbing a function honor culture had previously performed privately. It sets the legal terms and enforces them through prosecution, court-martial exclusion, and loss of office for participants.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, modern_state_monopoly_on_violence, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__composite_overdetermined_reading, modern_state_monopoly_on_violence, agenda_setter).

% Individual men caught in the transition: still socially compelled to answer challenges to avoid being branded a coward, yet now facing prosecution, career ruin, or exclusion from professional bodies if they duel. They pay in blood if they duel, in status if they refuse, and increasingly in legal jeopardy either way — the substrate has not yet clearly told them which risk is worse.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, dueling_participants_and_seconds, payer,
    moderate, biographical, trapped, local).

% Men (and by extension their families) who wished to opt out of the honor-violence system entirely but could not, because refusal itself carried social death within elite circles for as long as the code retained force. Their exit only opens once the honor code's substrate itself shifts toward 'dignity' norms — before that shift, no legal reform alone frees them, since the shame of refusal was independent of legality.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, socially_pressured_non_combatants, payer,
    powerless, biographical, trapped, local).

% Religious and civic reform movements that campaign against dueling on moral grounds, reframing it as barbaric rather than honorable. They do not enforce law directly but reshape the semantic substrate — the honor code itself — making the practice culturally illegible to a rising middle class. Their pressure is entangled with, not separate from, the state's legal suppression.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, clergy_and_moral_reformers, agenda_setter,
    organized, generational, mobile, national).

% Later scholars attempting to disentangle whether dueling's disappearance was caused by law, by cultural change, or by both operating through shared channels (e.g., professionalization simultaneously producing legal codes of conduct AND redefining honor). They observe that the two causal stories cannot be cleanly separated because the same institutions (bar associations, officer corps, parliaments) were simultaneously legal enforcers and honor-code redefiners.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The honor-satisfaction substrate coordinated status disputes among social equals without appeal to a third-party authority, providing a mutually recognized procedure (challenge, seconds, code duello) for resolving affronts to reputation in contexts where no court could adjudicate 'insult.'
% TRANSFER_FUNCTION: Physical risk and reputational capital moved between disputants; more broadly, exclusive authority over legitimate violence and over the definition of honorable conduct moved from private elite networks to the state and to a professionalizing middle-class moral order.
% ABSENT_VOICES: Women, servants, and lower-class men had no standing to issue or receive challenges and are almost entirely absent from the code's own accounting of who bears its costs, despite bearing indirect costs (loss of providers, social instability) when duels occurred.
% DISAPPEARANCE_RATIONALE: The practice's disappearance did rearrange the world: professional and political classes reorganized status contests around litigation, dueling codes gave way to codes of professional conduct and libel law, and the entire vocabulary of 'satisfaction' migrated into courts and newspapers. This was not a return to a prior natural state but a genuine institutional substitution.
% FOUNDING_PROBLEM: Elite social orders needed a procedure for adjudicating insults to honor in a context where no legal remedy existed for reputational injury among social equals, and where deferring to state courts for such matters was itself considered dishonorable.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the dueling fraternity (e.g., court records showing prosecutors treating duelists as ordinary homicide defendants by the mid-to-late 19th century) and moral-reform society records corroborate that the problem the code solved — absence of a legitimate honor-adjudication forum — was independently displaced by expanding libel law, professional disciplinary bodies, and dueling's own re-description as criminal rather than noble; no surviving dueling-fraternity record attests the founding problem remains live.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__composite_overdetermined_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__composite_overdetermined_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__composite_overdetermined_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, 0.44, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).
:- end_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises through the middle of the interval (0.28 to 0.46) as the practice becomes costlier for participants relative to its coordination benefit — dueling increasingly extracts reputational and legal risk from participants while its coordination function erodes, then plateaus/slightly declines post-1840 as the practice becomes vestigial rather than actively extractive (fewer duels occur at all). Theater ratio rises steadily and plateaus at 0.5 by mid-century: an increasing share of remaining 'honor satisfaction' activity (formal apologies, published cards of honor, symbolic gestures) is performative substitute for the real coordination function, consistent with a practice hollowing out rather than vanishing instantly. Suppression rises sharply 1780-1840 (state criminalization intensifying) then plateaus — consistent with legal suppression reaching a stable enforcement equilibrium once the underlying honor substrate had also shifted, requiring less marginal enforcement effort to hold the decline in place.
 *
 * PERSPECTIVAL GAP:
 *   From the elite incumbent seat, the shift away from dueling looks like voluntary moral progress the class itself authored. From the trapped participant seat mid-transition, the same period looks like being caught between two lethal risks (death in the duel, ruin by prosecution) with no honorable exit visible yet. The engine should compute these as structurally different experiences of the same interval, not as disagreement about facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Elite status incumbents and the state both occupy beneficiary/agenda-setter seats because both gain from the practice's transformation — incumbents retain status-sorting function through new (legal, professional) channels, the state gains monopoly authority over legitimate violence. Individual duelists and socially pressured non-combatants are targets: they bear the transition costs (legal jeopardy, social shaming, actual death) without controlling the terms of transition. Clergy/reformers are agenda-setters without being direct beneficiaries in the extraction sense — they reshape legitimacy conditions rather than collect rents, which is why they are agenda_setter rather than beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (adjudicating honor-injury with no legal remedy) is dead by 1900 — libel law, professional disciplinary codes, and reformed honor codes now perform that function through non-lethal channels. Treating dueling's persistence into the 19th century as evidence the substrate was 'still needed' would be a mandatrophy error; the composite reading shows the substrate's function was already migrating to other institutions well before the practice's final legal death, meaning what remained was increasingly theater (rising theater_ratio) rather than live coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_independence_of_pathways,
    'Can the exogenous (legal suppression) and endogenous (honor code transformation) causal pathways be statistically or historically disentangled, or are they necessarily confounded because the same institutional actors (bar associations, officer corps, legislatures) executed both simultaneously?',
    'Comparative case analysis across jurisdictions where legal suppression occurred without parallel honor-code transformation (or vice versa) — e.g., regions with strong legal prohibition but persistent honor-culture norms (parts of the American South post-Civil War) versus regions with early cultural delegitimation but weak legal enforcement, to see if decline rates diverge as predicted by an additive vs. entangled model.',
    'If a clean natural experiment shows the pathways are separable and additive, this composite reading should be abandoned in favor of whichever sibling reading (practice_decline or cultural_contraction) better fits the disentangled data. If no such separation is found across any jurisdiction, the entangled composite reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_independence_of_pathways, empirical, 'Whether the two decline mechanisms are truly non-separable or only appear so due to data limitations.').

omega_variable(
    which_reading_the_historical_actors_themselves_held,
    'Did contemporaries (duelists, reformers, legislators) themselves experience the decline as one, two, or entangled processes — and does the composite reading impose a retrospective analytical synthesis that no historical actor actually held?',
    'Close reading of period pamphlets, legislative debate records, and honor-code revision documents (e.g., the successive editions of dueling codes like the Irish Code Duello) for explicit statements linking legal and cultural causation.',
    'If period actors explicitly linked the two (e.g., reformers citing both moral and legal arguments in the same breath, as many temperance-adjacent honor reformers did), the composite reading has strong period corroboration. If period actors treated them as wholly separate debates, the composite reading risks being an artifact of modern historiographical synthesis rather than a structural fact about the decline itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(which_reading_the_historical_actors_themselves_held, conceptual, 'Whether entanglement is a structural fact or a historiographical construction.').

omega_variable(
    beneficiary_status_of_the_state_as_fsm_signal,
    'Is the modern state''s monopoly-on-violence beneficiary status evidence that the entire decline narrative (in any reading) is a false-summit story where a ''natural'' cultural evolution is claimed but an identifiable institutional beneficiary (the state) actually drove and profited from the transition?',
    'Trace state revenue, judicial caseload, and military discipline records for measurable institutional gains attributable specifically to dueling''s suppression, distinct from general modernization trends.',
    'If the state shows disproportionate institutional gain from dueling''s suppression relative to other contemporaneous social changes, this strengthens a tangled_rope (not mountain) reading across all three sibling constraints and cautions against ever treating any of the three readings as naturalized cultural inevitability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_status_of_the_state_as_fsm_signal, empirical, 'Whether state institutional benefit is disproportionate enough to signal false-naturalization risk.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__composite_overdetermined_reading, 1750, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1750, 0.15).
narrative_ontology:measurement(hono_tr_t1780, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1780, 0.22).
narrative_ontology:measurement(hono_tr_t1810, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1810, 0.34).
narrative_ontology:measurement(hono_tr_t1840, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1840, 0.46).
narrative_ontology:measurement(hono_tr_t1870, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1870, 0.5).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1900, 0.5).

% Extraction over time
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1750, 0.28).
narrative_ontology:measurement(hono_be_t1780, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1780, 0.32).
narrative_ontology:measurement(hono_be_t1810, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1810, 0.4).
narrative_ontology:measurement(hono_be_t1840, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1840, 0.46).
narrative_ontology:measurement(hono_be_t1870, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1870, 0.42).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1900, 0.44).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1750, 0.2).
narrative_ontology:measurement(hono_su_t1780, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1780, 0.3).
narrative_ontology:measurement(hono_su_t1810, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1810, 0.48).
narrative_ontology:measurement(hono_su_t1840, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1840, 0.6).
narrative_ontology:measurement(hono_su_t1870, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1870, 0.58).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1900, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, cultural_contraction_reading).

% DUAL FORMULATION NOTE:
% This story is the composite/entangled member of a three-story kernel family (honor_satisfaction_substrate). practice_decline_reading isolates the exogenous legal-suppression mechanism holding the honor substrate constant; cultural_contraction_reading isolates the endogenous honor-code transformation holding legal enforcement constant. This story holds both simultaneously and treats their interaction as the structural object, rather than decomposing into either single-mechanism account. All three share the same underlying historical interval and stakeholder cast but differ in which causal claim each authors as primary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
