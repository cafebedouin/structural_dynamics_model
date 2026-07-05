% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__existential_matrix_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__existential_matrix_reading, []).

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
 *   constraint_id: territorial_sovereignty_legitimacy__existential_matrix_reading
 *   human_readable: Existential-Matrix Reading of Territorial Sovereignty Legitimacy
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This story instantiates the existential-matrix reading of the
 *   territorial-sovereignty-legitimacy kernel: legitimacy talk (covenantal,
 *   juridical, self-determination) is treated as epiphenomenal to the real
 *   driver, which is each national group's fear that ceding territorial
 *   control amounts to accepting an existential vulnerability it cannot
 *   survive. Under this reading, negotiated compromise frameworks (e.g.,
 *   two-state proposals) are structurally unstable not because their legal
 *   terms are unsound but because neither side's security establishment can
 *   accept the vulnerability compromise requires, regardless of the legal
 *   merits. The beneficiary at any moment is whichever side currently holds
 *   demographic or military dominance; the arrangement inverts rather than
 *   resolves if dominance shifts. This is a distinct constraint from its
 *   sibling readings (covenant_continuity_reading grounds legitimacy in
 *   continuous presence plus international recognition;
 *   self_determination_reading grounds it in modern self-determination
 *   doctrine applied to demographic majority) — each sibling has its own
 *   epsilon, its own beneficiary structure, and its own persistence logic,
 *   and none of that structure is folded into this file.
 *
 * KEY AGENTS:
 *   - demographically_dominant_national_group: shifting occupant of structural advantage, treats control as survival precondition
 *   - militarily_dominant_state_apparatus: administers enforcement, cannot de-escalate without domestic legitimacy cost
 *   - displaced_and_stateless_populations: bear the concrete transfer, no meaningful exit
 *   - moderate_political_factions_on_both_sides: excluded from setting terms, marginalized as the existential framing hardens
 *   - international_mediators_and_legal_bodies: analytical observers whose juridical frameworks are treated as secondary by both dominant parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.71).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.78).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__existential_matrix_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__existential_matrix_reading, "Existential-Matrix Reading of Territorial Sovereignty Legitimacy").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__existential_matrix_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__existential_matrix_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__existential_matrix_reading, '36575e46-b769-4d56-a309-6a928d5c3b0a').
narrative_ontology:cs_kernel_codification('36575e46-b769-4d56-a309-6a928d5c3b0a', distributed).
narrative_ontology:cs_authority_grounding('36575e46-b769-4d56-a309-6a928d5c3b0a', distributed).
narrative_ontology:cs_reading_relation('36575e46-b769-4d56-a309-6a928d5c3b0a', territorial_sovereignty_legitimacy__covenant_continuity_reading, influences).
narrative_ontology:cs_reading_relation('36575e46-b769-4d56-a309-6a928d5c3b0a', territorial_sovereignty_legitimacy__self_determination_reading, influences).
narrative_ontology:cs_axiom('36575e46-b769-4d56-a309-6a928d5c3b0a', foundational, legitimacy_claims_are_epiphenomenal_to_existential_fear).
narrative_ontology:cs_axiom_status(legitimacy_claims_are_epiphenomenal_to_existential_fear, holdable).
narrative_ontology:cs_axiom_grounding('36575e46-b769-4d56-a309-6a928d5c3b0a', legitimacy_claims_are_epiphenomenal_to_existential_fear, empirically_contingent).
narrative_ontology:cs_axiom('36575e46-b769-4d56-a309-6a928d5c3b0a', foundational, territorial_control_is_precondition_for_collective_survival).
narrative_ontology:cs_axiom_status(territorial_control_is_precondition_for_collective_survival, holdable).
narrative_ontology:cs_axiom_grounding('36575e46-b769-4d56-a309-6a928d5c3b0a', territorial_control_is_precondition_for_collective_survival, empirically_contingent).
narrative_ontology:cs_axiom('36575e46-b769-4d56-a309-6a928d5c3b0a', secondary, negotiated_compromise_frameworks_are_structurally_unstable_under_symmetric_vulnerability).
narrative_ontology:cs_axiom_status(negotiated_compromise_frameworks_are_structurally_unstable_under_symmetric_vulnerability, holdable).
narrative_ontology:cs_axiom_grounding('36575e46-b769-4d56-a309-6a928d5c3b0a', negotiated_compromise_frameworks_are_structurally_unstable_under_symmetric_vulnerability, instrumental).
narrative_ontology:cs_reference_frame('36575e46-b769-4d56-a309-6a928d5c3b0a', pre_existential_framing_juridical_contest).
narrative_ontology:cs_drift_state('36575e46-b769-4d56-a309-6a928d5c3b0a', post_repeated_settlement_collapse_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('36575e46-b769-4d56-a309-6a928d5c3b0a', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, demographically_dominant_national_group).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, militarily_dominant_state_apparatus).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, security_establishment_on_both_sides).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, displaced_and_stateless_populations).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, border_zone_civilians).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, moderate_political_factions_on_both_sides).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, future_generations_inheriting_the_conflict).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, rival_national_group_across_the_boundary).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, rival_national_group_across_the_boundary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Whichever national community currently holds demographic weight and territorial control in a given zone treats that control as existential necessity, not merely policy preference. It mobilizes settlement, security architecture, and political institutions to lock in present advantage, framing any territorial concession as an existential risk rather than a negotiable interest. It cannot easily exit the framing because its own security discourse now depends on treating the other side's presence as a survival threat.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, demographically_dominant_national_group, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__existential_matrix_reading, demographically_dominant_national_group, agenda_setter).

% The state or para-state security apparatus with superior military capacity administers checkpoints, borders, and territorial control, justifying this administration as the precondition for its population's collective survival. It sets the operative rules of engagement and can, in principle, alter enforcement posture, but its own legitimacy is now bound to maintaining the existential framing — de-escalation is read domestically as weakness that invites destruction.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, militarily_dominant_state_apparatus, agenda_setter,
    institutional, generational, constrained, regional).

% Refugees, residents of contested zones, and those without secure citizenship bear the concrete costs of a conflict framed as existential and therefore non-negotiable: displacement, statelessness, generational transmission of grievance. They have no meaningful exit — neither absorption into a neighboring state, nor return, nor durable local settlement is available to them under either side's current existential posture.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, displaced_and_stateless_populations, payer,
    powerless, civilizational, trapped, regional).

% Civilians living along contested borders or in mixed zones absorb the immediate physical costs of a framework in which compromise is treated as suicidal by both sides — recurring violence, movement restriction, home demolition or requisition. Their day-to-day survival is treated as instrumentally acceptable collateral to the two dominant groups' respective existential projects.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, border_zone_civilians, payer,
    powerless, immediate, trapped, local).

% Political actors who argue that compromise frameworks are viable pay a direct political cost: they are marginalized within their own communities as naive or treasonous once the existential framing dominates public discourse, because any acceptance of vulnerability is read as endangering collective survival. Their voice is structurally crowded out by the security establishments on both sides that benefit from the zero-sum framing.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, moderate_political_factions_on_both_sides, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__existential_matrix_reading, moderate_political_factions_on_both_sides, excluded).

% Children born into either community inherit a conflict structure premised on permanent zero-sum survival competition; they have no say in the founding framing and no mechanism to renegotiate it, since the existential logic reproduces itself institutionally (education, military service, memorial culture) across generations.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, future_generations_inheriting_the_conflict, payer,
    powerless, civilizational, trapped, regional).

% UN bodies, treaty negotiators, and international courts attempt to adjudicate the conflict using juridical and historical frameworks (partition plans, self-determination doctrine, prior sovereignty claims). Under the existential-matrix reading, their frameworks are treated by both dominant parties as secondary to survival calculus, which is why negotiated settlements repeatedly fail to hold even when formally signed.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, international_mediators_and_legal_bodies, observer,
    institutional, generational, analytical, global).

% Mirrors the demographically dominant group's structural position from the other side of the boundary: it experiences the other's territorial control as its own existential threat, and pursues its own control as its own survival precondition. Whichever side is currently subordinate in demographic or military terms is a comprehensive payer; if the balance shifts, the roles invert rather than resolve.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, rival_national_group_across_the_boundary, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__existential_matrix_reading, rival_national_group_across_the_boundary, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__existential_matrix_reading, diffuse).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__existential_matrix_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading there is no genuine coordination function at the level of the conflict itself — each side's territorial control coordinates the survival and internal cohesion of its own national group, but the two coordination projects are structurally incompatible over the same territory, so what looks like coordination internally is extraction externally.
% TRANSFER_FUNCTION: The arrangement moves security, land, demographic weight, and political voice from whichever population currently lacks military or demographic dominance to whichever population currently holds it; the direction of transfer is not fixed to an ethnicity or state but tracks the balance of power at a given moment, inverting if the balance inverts.
% ABSENT_VOICES: Displaced populations, stateless residents, border-zone civilians, and moderate factions on both sides would object that the existential framing forecloses negotiated coexistence, but they are structurally absent from the security-establishment negotiations that set the terms of engagement on both sides.
% DISAPPEARANCE_RATIONALE: If the existential-matrix framing itself disappeared overnight, the security establishments and demographically dominant factions on both sides would lose their primary justification for non-compromise, and negotiated territorial frameworks would gain political viability — but the underlying material scarcity (land, water, security infrastructure) would not disappear, so the parties dispute whether removing the framing would rearrange the conflict's substance or merely its rhetoric.
% FOUNDING_PROBLEM: The reading was constructed to explain repeated failure of juridically and historically framed settlement processes (partition plans, peace accords) by locating the actual driver of non-compromise in existential fear rather than in competing legal or historical claims — it was built to answer why legally sound settlements do not hold.
% FOUNDING_PROBLEM_CORROBORATION: Conflict-resolution scholars and negotiators outside both national security establishments (e.g., political psychologists studying protracted ethnonational conflicts, and some retired diplomats who negotiated failed settlement attempts) corroborate that fear-driven non-compromise dynamics recur independent of the legal merits of specific proposals. However, no source entirely outside both benefiting security establishments can attest whether existential fear is the true irreducible driver or itself a constructed narrative that legitimizes territorial control already sought on other grounds — this is exactly the ambiguity the omega below is built to hold open.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__existential_matrix_reading, contested).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__existential_matrix_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__existential_matrix_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.71 at interval end) is authored high because territorial and security advantage is transferred from the currently-subordinate population to the currently-dominant one, and that transfer is justified by survival necessity rather than by a service rendered. Suppression (0.78) is authored higher than extraction because active enforcement — checkpoints, settlement expansion, military administration, and internal political marginalization of compromise advocates — is required continuously to hold the arrangement against resistance from displaced and border-zone populations. Theater ratio (0.42) reflects that a substantial share of activity under this framing (peace summits, negotiation processes, legal appeals) is genuinely attempted but structurally undermined by the existential logic beneath it, producing recurring performative negotiation cycles that do not resolve the underlying zero-sum structure. Accessibility collapse (0.68) and resistance (0.88) are both authored high: alternatives (federation, binational state, robust minority-rights frameworks) are treated by both dominant establishments as unthinkable given the existential framing, yet resistance from displaced populations, moderates, and international bodies remains persistently active rather than extinguished — this is not a settled mountain, it is a heavily defended construction.
 *
 * PERSPECTIVAL GAP:
 *   The dominant group's own establishment experiences its territorial administration as defensive coordination essential to its people's survival — a rope, in its own frame. The subordinate group and stateless populations experience the identical structure as enforced, non-negotiable extraction of land, security, and voice — a snare or tangled rope, in theirs. The engine computes these divergent seat classifications from the same structural data; the story does not adjudicate which seat is 'correct' because under this reading the divergence itself, not its resolution, is the structural fact requiring explanation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality tracks relative dominance rather than any fixed national identity: the demographically or militarily dominant group at a given moment sits near the beneficiary end (control, security, transferred land and resources flow to it), while the currently subordinate group and its stateless diaspora sit near the full-target end (bearing displacement, statelessness, and the enforcement apparatus's costs). Because dominance can shift, this is authored as a structural role, not a permanent identity assignment — the same population can occupy either seat at different points in the conflict's history, which is exactly the zero-sum structure the existential-matrix reading claims. Moderate factions and civilians in border zones are payers regardless of which side is currently dominant, since the mechanism that pays them (marginalization, physical exposure) operates on anyone who steps outside the existential framing or lives inside its blast radius.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — explaining why juridically sound settlements repeatedly fail — remains genuinely live in the sense that settlement failure continues to recur, but the arrangement built to explain it (treating existential fear as an irreducible, non-negotiable constant) has itself become a tool that security establishments on both sides use to foreclose negotiation regardless of whether fear is still the operative driver or has become a self-perpetuating institutional justification. This is why founding_problem_status is authored as 'contested' rather than 'live': the diagnostic claim (fear drives non-compromise) may have been true at some historical point and may now function partly as a legitimating myth for continued territorial control by whichever side is currently dominant — the corpus should not accept either the myth-status or the live-status uncritically, and the omega below is built to hold this open rather than resolve it by authorial fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_fear_versus_constructed_narrative,
    'Is existential fear genuinely the irreducible driver of non-compromise, or is the existential framing itself a constructed narrative that legitimizes territorial control sought for independent reasons (resource access, historical claim, demographic strategy)?',
    'Comparative historical analysis of cases where demonstrable existential threat receded (e.g., formal peace treaties, demilitarization agreements) while territorial control demands persisted or intensified; survey and discourse analysis tracking whether existential rhetoric tracks measurable threat levels or political convenience.',
    'If fear is genuinely irreducible, the tangled_rope classification with its structural instability is close to descriptively complete, and mandatrophy is not resolvable by settlement design alone. If the framing is substantially constructed, the constraint shifts toward snare — coordination language covering a legitimation project for whichever side currently holds the advantage.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_fear_versus_constructed_narrative, conceptual, 'Whether the existential-matrix reading describes an irreducible psychological constant or a legitimating narrative for material advantage.').

omega_variable(
    kernel_reading_indeterminacy,
    'This story is one reading (existential_matrix_reading) of the territorial_sovereignty_legitimacy kernel among three declared readings (covenant_continuity_reading, self_determination_reading, existential_matrix_reading). Which reading, if any, best accounts for the persistence of the conflict across legal and political changes?',
    'No single empirical test resolves this — it is a framing choice among readings held by different parties (religious-nationalist frameworks favor covenant_continuity, international law bodies and much post-colonial scholarship favor self_determination, conflict-resolution psychology favors existential_matrix). Cross-reading comparison of predictive power (which reading best predicts when negotiated settlements will or won''t hold) is the closest available resolution mechanism, but even that comparison is contested on methodological grounds.',
    'Choosing this reading over its siblings determines which agents are cast as structural beneficiaries (dominance-holders here, versus continuous-presence claimants under covenant_continuity, versus demographic-majority claimants under self_determination) and which type each reading computes to. The three readings are not competing measurements of one constraint; they are three distinct constraints per the epsilon-invariance principle, linked but not merged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Documents that this story is one of three sibling readings of a contested kernel, and that reading-choice is not empirically adjudicable in the way a single constraint''s epsilon is.').

omega_variable(
    power_balance_inversion_stability,
    'If demographic or military dominance shifted decisively to the currently-subordinate group, would the existential-matrix structure simply invert beneficiary and victim roles, or would the underlying dynamic dissolve because the new dominant group would have different institutional history and different exit options?',
    'Comparative case study of conflicts where dominance has historically inverted (e.g., shifting demographic majorities in contested regions elsewhere) to observe whether the zero-sum structure persists across the inversion or resolves once one side achieves durable security.',
    'If the structure is confirmed to simply invert, this substantially strengthens the reading''s zero-sum claim and its tangled_rope/high-suppression profile as a stable long-run description. If dominance-achievement tends to produce durable security and reduced suppression, the existential-matrix reading may overstate the conflict''s irreducibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(power_balance_inversion_stability, empirical, 'Whether the zero-sum structure is a stable attractor across power inversions or an artifact of the current unresolved balance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__existential_matrix_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(terr_tr_t8, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(terr_tr_t16, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(terr_tr_t24, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(terr_tr_t32, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(terr_tr_t40, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(terr_be_t8, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(terr_be_t16, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(terr_be_t24, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(terr_be_t32, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 32, 0.69).
narrative_ontology:measurement(terr_be_t40, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(terr_su_t8, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 8, 0.64).
narrative_ontology:measurement(terr_su_t16, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(terr_su_t24, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(terr_su_t32, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 32, 0.75).
narrative_ontology:measurement(terr_su_t40, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__existential_matrix_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.1).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy__covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy__self_determination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the territorial_sovereignty_legitimacy kernel, linked via affects_constraints. covenant_continuity_reading grounds legitimacy in continuous historical presence and international recognition instruments (Balfour Declaration, UN Partition Plan); self_determination_reading grounds it in modern self-determination doctrine applied to demographic majority and residence during the 19th-20th centuries; this existential_matrix_reading treats both juridical and historical legitimacy arguments as epiphenomenal to an underlying existential-fear dynamic that makes the conflict zero-sum and beneficiary-inverting with the power balance. Each reading has its own epsilon, its own stakeholder structure, and its own computed classification; none averages or defers to the others. The readings coexist as live positions held by different parties in ongoing political and scholarly discourse rather than being resolved into a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
