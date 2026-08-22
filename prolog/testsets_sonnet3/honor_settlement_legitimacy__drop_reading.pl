% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__drop_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: honor_settlement_legitimacy__drop_reading
 *   human_readable: Honor-Settlement Legitimacy: The Drop Reading (Dueling as Persisting Fringe Norm)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   This story instantiates the DROP reading of the
 *   honor_settlement_legitimacy kernel: dueling did not vanish through
 *   wholesale cultural transformation (the contraction reading) nor decline
 *   through a diffuse mixture of overdetermined mechanisms trending toward
 *   extinction (the composite reading). Instead, the drop reading holds that
 *   the practice simply dropped out of the mainstream normative repertoire
 *   while persisting, largely unchanged in its internal logic, as a fringe
 *   option retained by a residual honor-culture population (certain military
 *   and gentry subcultures, dueling fraternities) who continue to treat it as
 *   legitimate within their own bounded social world. The kernel here is
 *   'what makes an honor-settlement legitimate' — this reading asserts that
 *   legitimacy became geographically and socially localized rather than
 *   universally revoked or universally reframed as unthinkable.
 *
 * KEY AGENTS:
 *   - residual_honor_culture_elites: primary beneficiary and agenda-setter, retains status by credible invocation of the code
 *   - dueling_code_arbiters: administer the surviving ritual, identity- and livelihood-locked to its persistence
 *   - duel_participants_family_dependents: bear catastrophic risk with zero voice in the proceeding
 *   - rank_and_file_challenged_men: face a local binary between physical risk and social excommunication from their niche
 *   - state_legal_authorities: formally available but functionally excluded from the niche's operative logic
 *   - mainstream_society_observers: analytical seat from which the practice is visible as residual/fringe
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__drop_reading, 0.42).
domain_priors:suppression_score(honor_settlement_legitimacy__drop_reading, 0.55).
domain_priors:theater_ratio(honor_settlement_legitimacy__drop_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__drop_reading, piton).
narrative_ontology:human_readable(honor_settlement_legitimacy__drop_reading, "Honor-Settlement Legitimacy: The Drop Reading (Dueling as Persisting Fringe Norm)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__drop_reading, "historical_sociology/legal_history/cultural_anthropology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__drop_reading, '191c76ed-cbd7-49ce-8a50-69a3ef6c0847').
narrative_ontology:cs_kernel_codification('191c76ed-cbd7-49ce-8a50-69a3ef6c0847', distributed).
narrative_ontology:cs_authority_grounding('191c76ed-cbd7-49ce-8a50-69a3ef6c0847', practice).
narrative_ontology:cs_interpretation_layer_present('191c76ed-cbd7-49ce-8a50-69a3ef6c0847').
narrative_ontology:cs_reading_relation('191c76ed-cbd7-49ce-8a50-69a3ef6c0847', honor_settlement_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('191c76ed-cbd7-49ce-8a50-69a3ef6c0847', honor_settlement_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('191c76ed-cbd7-49ce-8a50-69a3ef6c0847', foundational, honor_code_retains_local_legitimacy_absent_universal_repudiation).
narrative_ontology:cs_axiom_status(honor_code_retains_local_legitimacy_absent_universal_repudiation, holdable).
narrative_ontology:cs_axiom_grounding('191c76ed-cbd7-49ce-8a50-69a3ef6c0847', honor_code_retains_local_legitimacy_absent_universal_repudiation, conventional).
narrative_ontology:cs_axiom('191c76ed-cbd7-49ce-8a50-69a3ef6c0847', secondary, framework_survival_does_not_require_majority_adherence).
narrative_ontology:cs_axiom_status(framework_survival_does_not_require_majority_adherence, holdable).
narrative_ontology:cs_axiom_grounding('191c76ed-cbd7-49ce-8a50-69a3ef6c0847', framework_survival_does_not_require_majority_adherence, conventional).
narrative_ontology:cs_reference_frame('191c76ed-cbd7-49ce-8a50-69a3ef6c0847', aristocratic_honor_code_supremacy).
narrative_ontology:cs_drift_state('191c76ed-cbd7-49ce-8a50-69a3ef6c0847', post_legal_criminalization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('191c76ed-cbd7-49ce-8a50-69a3ef6c0847', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, residual_honor_culture_elites).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, dueling_code_arbiters).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, duel_participants_family_dependents).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, rank_and_file_challenged_men).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__drop_reading, personal_honor_as_adjudicable_property).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Military officer corps, plantation gentry remnants, and certain professional-class subcultures (dueling fraternities, some officer messes) continue to treat the code duello as a live, if narrowing, mechanism for settling insult. They retain social standing by being seen as willing to invoke it, and they administer informal codes (seconds, degrees of satisfaction) that keep the practice legible within their circle even as the surrounding legal and cultural order has moved on.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, residual_honor_culture_elites, beneficiary,
    powerful, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__drop_reading, residual_honor_culture_elites, agenda_setter).

% Self-appointed seconds, code-of-honor pamphleteers, and informal arbiters who administer the surviving ritual — setting terms, verifying grievances, and legitimating outcomes within the niche. Their social function and sometimes livelihood (as duty instructors, fencing/pistol tutors) depends on the practice's continued, if diminished, legitimacy.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, dueling_code_arbiters, agenda_setter,
    moderate, biographical, identity_locked, local).

% Wives, children, and dependents of men who accept or issue challenges bear the risk of death, injury, disgrace, or destitution without having any voice in whether the challenge is issued or accepted. Honor-code logic treats their interests as subordinate to the male principal's standing; they have no standing within the ritual itself to object or intervene.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, duel_participants_family_dependents, payer,
    powerless, immediate, trapped, local).

% Men embedded in the residual honor subculture who receive a challenge face a binary: accept (risking death or injury) or decline (accepting social death within that specific community). Because the surrounding legal and cultural mainstream no longer enforces or requires dueling, formal exit exists nationally, but exit from the specific niche community that still enforces the code is costly — reputational excommunication from a social world they remain embedded in.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, rank_and_file_challenged_men, payer,
    moderate, immediate, constrained, local).

% Courts and legislatures had, by the period this reading concerns, criminalized dueling and offered civil/criminal remedies for insult and injury as substitutes. Within the residual honor-culture niche their jurisdiction is nominally present but functionally sidelined — participants and arbiters treat resort to law as itself dishonorable, so the state's alternative dispute mechanism is excluded from the niche's actual practice even though it is formally available.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, state_legal_authorities, excluded,
    institutional, generational, analytical, national).

% The broader public and press by this period regard dueling as anachronistic, criminal, or scandalous rather than normal — they document, satirize, and occasionally prosecute surviving duels, providing the outside vantage from which the practice's persistence is visible as a residual fringe phenomenon rather than the norm.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, mainstream_society_observers, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_settlement_legitimacy__drop_reading, residual_honor_culture_elites).
narrative_ontology:fixing_cost_class(honor_settlement_legitimacy__drop_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Within the residual honor-culture niche, the code duello still coordinates status disputes among men who share the code: it provides an agreed procedure (challenge, seconds, terms, satisfaction) for adjudicating insult that both sides of a dispute recognize as legitimate, avoiding open-ended feud or unresolved humiliation within that specific social world.
% TRANSFER_FUNCTION: Moves physical risk, potential fatality, and reputational stakes from the initiating insult onto the bodies of the principals, while moving social costs (loss of provider, disgrace, financial ruin from injury) onto dependents who have no voice in the proceeding; simultaneously transfers social capital and authority to the arbiters and elites who administer and can invoke the code credibly.
% ABSENT_VOICES: Dependents of duelists (wives, children) are structurally absent from the ritual's decision procedure entirely. State legal authorities are formally present in the wider society but excluded from the niche's operative logic — their remedies are available but treated as dishonorable to invoke, which silences the very authority that would otherwise adjudicate the underlying insult through non-lethal means.
% DISAPPEARANCE_RATIONALE: From the mainstream observer's seat, if the residual practice vanished overnight almost nothing would change — it is already marginal, already illegal, already treated as scandal. From the residual honor-culture elite's seat, the disappearance of even this fringe practice would mean the final loss of a distinctive status-adjudication mechanism that still marks their subculture as separate from and above ordinary legal recourse; for the arbiters specifically, an identity- and sometimes livelihood-bearing role would end. The verdict is genuinely contested between these seats rather than resolvable to one answer.
% FOUNDING_PROBLEM: Dueling as an institution was originally built to provide an extralegal, honor-preserving mechanism for resolving insults among social equals in societies where courts either could not adjudicate matters of personal honor/reputation or where resort to courts was itself considered dishonorable for a gentleman.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and contemporaneous jurists (outside the dueling subculture) attest that by the period in question, civil defamation and assault law, alongside criminal statutes against dueling itself, had substituted adequate legal remedies for insult and injury; newspaper accounts and government prosecution records from the era corroborate that mainstream institutions regarded the underlying problem as already solved by law. Only the residual honor-culture elites and arbiters themselves continue to assert the problem is still live within their niche — this is the one seat that has not corroborated the founding problem's obsolescence, which is itself the signal the drop reading is built to explain.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__drop_reading, contested).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__drop_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__drop_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__drop_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__drop_reading_tests).
:- end_tests(honor_settlement_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.42, declining slightly over the interval, because within the niche the code still delivers a real (if narrow) coordination benefit to its own participants — this is not simple predation, it is a legitimacy structure that persists for a shrinking population who still find it functional. Suppression (0.55) is moderate: the mechanism holding rank-and-file challenged men inside the practice is social excommunication from a specific community they remain embedded in, not state coercion — the state has in fact withdrawn its coercive backing entirely by this period, which is precisely why theater_ratio climbs (0.15→0.40): as the practice's original function (settling disputes where no legal alternative existed) becomes obsolete elsewhere, an increasing share of what remains is performative honor-maintenance for its own sake within an ever-smaller, more self-referential niche. Accessibility_collapse is low (0.35) and resistance is comparatively high (0.6) because — unlike a genuine mountain — alternatives (courts, informal reconciliation, simply declining and accepting the social cost) are readily available and are in fact exercised by the surrounding mainstream; the practice's persistence is a choice by a residual population, not an inescapable structural fact.
 *
 * PERSPECTIVAL GAP:
 *   The elite/arbiter seats and the payer seats (dependents, challenged men) would compute this constraint very differently: from the arbiter and elite seat, the code still performs live coordination work and confers status — it looks like a shrinking rope. From the dependent and challenged-man seat, the same structure extracts catastrophic, involuntary risk with no exit that doesn't cost their place in a community they are still embedded in — it looks like a snare wearing a coordination costume. The claimed_type of piton reflects the outside/mainstream-observer vantage: a formerly functional institution (settling honor disputes where law offered no adequate remedy) that has lost its founding justification everywhere except within a shrinking, self-maintaining enclave, and persists there mostly through inertia and identity investment rather than active nationwide enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Residual honor-culture elites and code arbiters are declared beneficiaries: they derive status, livelihood, or social function directly from the practice's continued legitimacy within the niche, giving them low directionality (subsidized by the arrangement). Duel participants' dependents and rank-and-file challenged men are declared victims/payers: they bear the risk, injury, or death, or the threat of social excommunication, without commensurate benefit or voice, giving them high directionality (targeted by the arrangement). The identity_locked exit option for elites and arbiters is deliberate — their exit is blocked not by external force but by the fact that their social standing and self-conception are partly constituted by their credible relationship to the code; a mobile/arbitrage exit option would misrepresent how bound they are to maintaining the fiction that the code still matters.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored as dead — the mainstream legal and social order solved the underlying problem (adjudicating personal insult) generations before this reading's observation window closes — yet the arrangement persists within the niche. This is a textbook mandatrophy signature: the mandate (settle otherwise-unadjudicable honor disputes) has expired everywhere except inside the population that benefits from pretending it has not. The disappearance_verdict of 'contested' rather than 'world_unchanged' is what keeps this from being simple mislabeling: for the vast majority of the society this constraint touches, its disappearance would change nothing (supporting a piton/near-mountain-of-custom read from outside), but for the small identity-locked population still inside it, its disappearance would be a real rupture — the classification must hold both facts rather than collapsing to either extreme.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    drop_vs_contraction_boundary,
    'Is the residual honor-culture population''s continued use of dueling evidence that the honor framework itself survives intact in a niche (drop reading), or is their persistence better explained as a degraded, already-partially-transformed vestige that the contraction reading would also predict at its margins?',
    'Comparative analysis of participants'' own stated justifications: if residual duelists articulate the classical honor-framework reasoning (insult must be answered in kind, courts are dishonorable) unchanged from earlier generations, this supports drop; if their justifications show internal hedging, irony, or partial adoption of legal/psychological framing, this supports contraction having reached even the residual population.',
    'If contraction is found to have penetrated even the residual population''s own self-justification, this constraint should be merged into or subsumed by contraction_reading rather than standing as an independent drop reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drop_vs_contraction_boundary, conceptual, 'Whether the residual population represents a true retained framework or an already-degraded transitional form.').

omega_variable(
    sibling_reading_disagreement_location,
    'Where exactly do drop, contraction, and composite readings locate their disagreement — is it about the MECHANISM of decline (single-cause unthinkability vs. multi-cause overdetermination) or about the ENDPOINT (total elimination vs. persistent fringe)?',
    'Cross-reading comparison document specifying, for each reading, its claim about (a) mechanism and (b) endpoint state as of the observation window''s close.',
    'If the disagreement is purely about mechanism with all three readings agreeing on a near-total-elimination endpoint, drop_reading''s distinct beneficiary/victim structure (which requires a genuinely surviving niche) would be undermined and the reading would need revision or retirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_location, conceptual, 'Locating whether kernel siblings disagree on mechanism, endpoint, or both.').

omega_variable(
    niche_persistence_causal_basis,
    'Does the residual honor-culture niche persist because it retains genuine coordination value for a bounded population (a live Rope-like function within the niche), or because of identity-lock inertia among arbiters and elites with no remaining functional payoff (pure Piton)?',
    'Track whether new entrants join the residual honor-culture niche (suggesting live functional value) versus whether the population is purely aging incumbents with no replacement (suggesting inertial persistence only).',
    'If new entrants are found, the coordination_function claim strengthens and the classification should weight toward tangled_rope-within-niche rather than piton; if no replacement population exists, piton is confirmed and the practice is purely inertial.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(niche_persistence_causal_basis, empirical, 'Whether niche persistence reflects live function or pure inertia.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__drop_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_settlement_legitimacy__drop_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hono_tr_t10, honor_settlement_legitimacy__drop_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(hono_tr_t20, honor_settlement_legitimacy__drop_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(hono_tr_t30, honor_settlement_legitimacy__drop_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(hono_tr_t40, honor_settlement_legitimacy__drop_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(hono_tr_t50, honor_settlement_legitimacy__drop_reading, theater_ratio, 50, 0.39).
narrative_ontology:measurement(hono_tr_t60, honor_settlement_legitimacy__drop_reading, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_settlement_legitimacy__drop_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(hono_be_t10, honor_settlement_legitimacy__drop_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(hono_be_t20, honor_settlement_legitimacy__drop_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement(hono_be_t30, honor_settlement_legitimacy__drop_reading, base_extractiveness, 30, 0.44).
narrative_ontology:measurement(hono_be_t40, honor_settlement_legitimacy__drop_reading, base_extractiveness, 40, 0.43).
narrative_ontology:measurement(hono_be_t50, honor_settlement_legitimacy__drop_reading, base_extractiveness, 50, 0.42).
narrative_ontology:measurement(hono_be_t60, honor_settlement_legitimacy__drop_reading, base_extractiveness, 60, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(honor_settlement_legitimacy__drop_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__drop_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_settlement_legitimacy__drop_reading, 0.08).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the honor_settlement_legitimacy kernel (drop, contraction, composite), each authored as a separate ε-invariant story per the ε-invariance principle. The drop reading holds ε=0.42 with a moderate-piton profile reflecting a genuinely surviving but shrinking niche population; contraction_reading and composite_reading author their own independent ε values reflecting their distinct mechanism/endpoint claims. All three link to each other via affects_constraints to preserve the family structure; none averages or hedges across the others' claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
