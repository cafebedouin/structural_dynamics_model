% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__narrow_armed_attack_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__narrow_armed_attack_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: article_51_self_defense__narrow_armed_attack_reading
 *   human_readable: Article 51 Narrow Armed-Attack Reading (State-Attribution Trigger)
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Article 51 self-defense
 *   kernel: the narrow armed-attack reading, under which a state may lawfully
 *   use force in self-defense only in response to an actual or imminent armed
 *   attack, mounted by a state whose responsibility is attributable under
 *   international law. Consolidated doctrinally by the ICJ's Nicaragua
 *   judgment (1986), the reading renders non-state actor attacks legally
 *   inert as triggers unless host-state attribution is established. Its
 *   beneficiaries are weaker states and host states shielded from
 *   reclassification as strike zones, and the Security Council whose
 *   force-authorization monopoly the reading preserves; its payers are
 *   militarily powerful states whose strategic freedom is constrained
 *   precisely where their capabilities point. Per the epsilon-invariance
 *   principle, epsilon here is authored for THIS reading's arrangement only —
 *   the sibling readings (expansive_preventive_reading,
 *   unable_unwilling_doctrine_reading) are separate constraint stories with
 *   their own epsilon, linked through network.affects_constraints. The claim
 *   and the metrics are authored independently: the story claims
 *   tangled_rope; the metrics describe a moderately extractive, actively
 *   enforced, increasingly theatrical arrangement.
 *
 * KEY AGENTS:
 *   - militarily_powerful_states: Primary target (powerful/constrained) — bears the strategic-freedom extraction
 *   - weaker_states: Primary beneficiary (organized/trapped) — sovereignty shield, collectively defended
 *   - un_security_council: Institutional beneficiary (institutional/trapped) — force-authorization monopoly preserved
 *   - international_legal_community: Agenda-setter (institutional/identity_locked) — maintains the doctrinal boundary
 *   - host_states_of_nonstate_groups: Secondary beneficiary (moderate/constrained) — shielded from cross-border strikes absent attribution
 *   - populations_exposed_to_nonstate_threats: Dual-positioned (powerless/trapped) — spared intervention campaigns, bear residual exposure
 *   - transnational_armed_groups: Excluded party (organized/mobile) — the disputed trigger events, no seat in the order
 *   - security_studies_analysts: Analytical observer — codes practice against professed doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__narrow_armed_attack_reading, 0.48).
domain_priors:suppression_score(article_51_self_defense__narrow_armed_attack_reading, 0.55).
domain_priors:theater_ratio(article_51_self_defense__narrow_armed_attack_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__narrow_armed_attack_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__narrow_armed_attack_reading, "Article 51 Narrow Armed-Attack Reading (State-Attribution Trigger)").
narrative_ontology:topic_domain(article_51_self_defense__narrow_armed_attack_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__narrow_armed_attack_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__narrow_armed_attack_reading, 'ce4fb39b-a1d1-4307-a6c6-a30cba55d719').
narrative_ontology:cs_kernel_codification('ce4fb39b-a1d1-4307-a6c6-a30cba55d719', fixed_text).
narrative_ontology:cs_authority_grounding('ce4fb39b-a1d1-4307-a6c6-a30cba55d719', lineage).
narrative_ontology:cs_interpretation_layer_present('ce4fb39b-a1d1-4307-a6c6-a30cba55d719').
narrative_ontology:cs_reading_relation('ce4fb39b-a1d1-4307-a6c6-a30cba55d719', article_51_self_defense__expansive_preventive_reading, forecloses).
narrative_ontology:cs_reading_relation('ce4fb39b-a1d1-4307-a6c6-a30cba55d719', article_51_self_defense__unable_unwilling_doctrine_reading, coexists_with).
narrative_ontology:cs_axiom('ce4fb39b-a1d1-4307-a6c6-a30cba55d719', foundational, defensive_force_requires_attributable_state_attack).
narrative_ontology:cs_axiom_status(defensive_force_requires_attributable_state_attack, holdable).
narrative_ontology:cs_axiom_grounding('ce4fb39b-a1d1-4307-a6c6-a30cba55d719', defensive_force_requires_attributable_state_attack, conventional).
narrative_ontology:cs_axiom('ce4fb39b-a1d1-4307-a6c6-a30cba55d719', secondary, imminence_is_caroline_last_window).
narrative_ontology:cs_axiom_status(imminence_is_caroline_last_window, holdable).
narrative_ontology:cs_axiom_grounding('ce4fb39b-a1d1-4307-a6c6-a30cba55d719', imminence_is_caroline_last_window, conventional).
narrative_ontology:cs_reference_frame('ce4fb39b-a1d1-4307-a6c6-a30cba55d719', charter_narrow_trigger_paradigm).
narrative_ontology:cs_drift_state('ce4fb39b-a1d1-4307-a6c6-a30cba55d719', post_2001_transnational_conflict_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ce4fb39b-a1d1-4307-a6c6-a30cba55d719', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, weaker_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, host_states_of_nonstate_groups).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, un_security_council).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, militarily_powerful_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, populations_exposed_to_nonstate_threats).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, populations_exposed_to_nonstate_threats).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, charter_force_authorization_monopoly).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, sovereign_equality_principle).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, caroline_imminence_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with global power-projection capacity and the most frequent targets of transnational armed-group attacks. Under this reading they may not treat such attacks as lawful triggers for cross-border force unless the host state's responsibility is established; their lawful paths run through attribution evidence or Security Council authorization, which rivals can veto. They bear the arrangement's operative burden: foregone response options in exactly the situations their militaries are built for, and they cannot exit the normative order without paying systemic reputational and relational costs.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, militarily_powerful_states, payer,
    powerful, generational, constrained, global).

% Small and middle powers without expeditionary militaries. The reading shields their territory from being reclassified as a permissible strike zone whenever a stronger state suffers an attack traceable to people on their soil. Their protection depends on the reading holding, and they defend it collectively through General Assembly majorities and the Non-Aligned Movement; they have no outside option if it fails, since their security is constituted by the norm rather than by their own capabilities.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, weaker_states, beneficiary,
    organized, generational, trapped, regional).

% The fifteen-member body that receives every Article 51 report and holds the monopoly on authorizing force. Each invocation routed through it renews its procedural centrality; each invocation bypassing it erodes that centrality. Its authority is the pool into which the reading channels defensive-force decisions, and it cannot abandon that role without dissolving its own reason for existence.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, un_security_council, beneficiary,
    institutional, civilizational, trapped, global).

% Foreign-ministry legal advisers, International Court of Justice judges, and the academic canon that define what counts as an armed attack and whose conduct is attributable. Their interpretive authority rests on the Charter paradigm remaining the governing frame; a decisive shift to rival readings would devalue interpretive capital built over eight decades. They publish, advise, and adjudicate in defense of the narrow trigger, and their career paths and institutional identities are bound to its maintenance.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, international_legal_community, agenda_setter,
    institutional, generational, identity_locked, global).

% States on whose territory transnational armed groups operate, whether by tolerance, incapacity, or design. The reading bars cross-border strikes against them unless their responsibility for the specific attack is established. They gain a shield over their sovereignty; they also carry the counterparty risk of hosting groups that provoke crises they cannot control and that invite political pressure short of force.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, host_states_of_nonstate_groups, beneficiary,
    moderate, generational, constrained, regional).

% Civilians living under the threat of transnational armed-group attacks and in the regions where retaliatory or preventive operations would occur. They are spared the large-scale intervention campaigns an unconstrained regime would produce, and they bear the residual exposure that persists when their government cannot lawfully strike a group staging attacks from across a border. They have no seat and no exit; their interests are voiced only indirectly by governments that may prefer silence.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, populations_exposed_to_nonstate_threats, beneficiary,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__narrow_armed_attack_reading, populations_exposed_to_nonstate_threats, payer).

% Non-state armed networks whose attacks are the disputed trigger events. They hold no standing anywhere in the state-centric legal order that classifies them; their existence and cross-border mobility are what generate the pressure each reading of the trigger must answer. They adapt to whichever reading governs, relocating and dispersing to exploit attribution gaps.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, transnational_armed_groups, excluded,
    organized, immediate, mobile, global).

% Academic and think-tank analysts who code state practice against professed doctrine. They take no position in the legal dispute; their datasets on cross-border operations, invoked justifications, and Council outcomes form the external record the other seats argue over.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, security_studies_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__narrow_armed_attack_reading, un_security_council).
narrative_ontology:fixing_cost_class(article_51_self_defense__narrow_armed_attack_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, publicly verifiable trigger for lawful defensive force, so that no state can convert its own military discretion into a legal pretext; routes all defensive-force claims through a common reporting and authorization channel centered on the Security Council, addressing the collective-action problem of unilateral war initiation.
% TRANSFER_FUNCTION: Moves decision rights over cross-border force from militarily powerful states acting alone to the Security Council's pooled authority, and moves security to weaker states and host states whose territory is removed from the set of permissible strike zones; moves legitimacy from unilateral action to channeled action.
% ABSENT_VOICES: Transnational armed groups have no standing anywhere in the conversation that classifies them. Civilian populations in prospective strike zones are represented only indirectly, through governments that may prefer silence. Legal traditions outside the Charter paradigm, and scholarship critical of Council veto politics, enter only through academic channels with no vote.
% DISAPPEARANCE_RATIONALE: If the narrow trigger vanished overnight — if self-defense became whatever a powerful state declared necessary — the sovereignty shield over weaker states and host states would evaporate, the Council's reporting-and-authorization channel would lose its function, and the eighty-year expectation that defense requires an identifiable attacker would collapse into bilateral capability contests. Interstate crisis behavior would reorganize around deterrence and fait accompli rather than law.
% FOUNDING_PROBLEM: After the League's failure, the drafters faced the problem that had produced Manchuria and Abyssinia: great powers manufacturing legal pretexts for aggression. Article 51 was written to preserve a genuine right of defense against actual aggression while closing the pretext space — permitting defense only where an armed attack has actually occurred, or is unmistakably imminent, and is mounted by an identifiable state.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set by the constraint's principal payers: militarily powerful states argue in official doctrine and Security Council debate that the founding problem has mutated — transnational armed groups now stage attacks no state launched — which is itself an attestation that the underlying problem of separating genuine defense from pretext remains unsolved. The ICJ's Nicaragua judgment corroborates the original formulation from the judicial seat. No party to the dispute claims the founding problem is dead.
narrative_ontology:disappearance_verdict(article_51_self_defense__narrow_armed_attack_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__narrow_armed_attack_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__narrow_armed_attack_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_51_self_defense__narrow_armed_attack_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__narrow_armed_attack_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__narrow_armed_attack_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__narrow_armed_attack_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__narrow_armed_attack_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48): the burden falls asymmetrically on powerful states, but it is bounded — attribution evidence and Council authorization remain open paths, and permanent-member veto power blunts enforcement against the strongest payers. Suppression (0.55) reflects institutional rather than physical coercion: reputational cost, adjudicative exposure, and diplomatic isolation. The enforcement series shows a ratchet: as major-power defections accumulated after 2001, the reading's defenders escalated doctrinal, judicial, and diplomatic enforcement activity even as behavioral compliance thinned — rising suppression_requirement models intensifying enforcement effort, not effectiveness. Theater (0.33, rising from 0.15) tracks the widening gap between professed doctrine and operational practice: a growing share of the reading's maintenance is argumentation that participants expect not to change outcomes. Accessibility collapse is deliberately LOW (0.28): the reading's defining structural feature is that its rivals remain fully live — expansive and unable-or-unwilling practices operate openly — which is what distinguishes this construct from a natural law or a sealed trap. Resistance is high (0.70): the payers are the most capable resisters in the system and resist continuously. All three series share one time grid (1986-2026, six points); mild oscillation around crisis episodes (each major defection produces an enforcement surge followed by tacit accommodation) appears as slope variation rather than full cycles, and the end-state values correspond to the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   From the payer seat, the arrangement computes as imposed extraction: a rule that forecloses exactly the responses their security environment demands, administered by institutions they partly fund and partly defy. From the beneficiary seats, the same structure computes as the price of order: the only reason weaker states' borders are not provisional is that powerful states accepted a trigger they did not write. From the agenda-setter seat, it is authoritative law under siege, and defending it is professional obligation. The engine derives these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation: militarily_powerful_states (victim, powerful, constrained exit) derive near the full-target end — the constraint taxes their core capability. weaker_states (beneficiary, trapped) derive near the full-beneficiary end — their subsidy is locked in, since they cannot exit the territorial order the reading protects. host_states_of_nonstate_groups (beneficiary, moderate) sit low but above weaker_states, since their shield is contingent on attribution findings. The two institutional seats are handled by an explicit override (institutional -> 0.18): the Security Council and the international legal community both sit nearer the beneficiary end than a generic institutional derivation would place them, because the reading's operation directly subsidizes the former's procedural monopoly and the latter's interpretive authority — a relationship the derivation chain cannot distinguish from neutral administration. populations_exposed_to_nonstate_threats carry a dual declaration and sit mid-range. transnational_armed_groups are excluded and feed no derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents two mislabels. Calling the reading a pure rope ignores that its costs land on a specific, capable class that never consented to the trade and actively resists it. Calling it a snare ignores the genuine coordination achievement — the pretext-closing function the Nicaragua-era record shows operated against real incentives. Mandatrophy is NOT resolved: the founding problem of separating defense from pretext is live, mutated by transnational threats, and the arrangement's fate turns on whether the narrow trigger can answer the mutation. The forward risk is piton drift: if the practice-theater gap widens until the reading is maintained purely as rhetoric while operations proceed under rival justifications, the structure becomes performance — tracked by the theater series and the practice_theater_decoupling omega rather than asserted here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the article_51_self_defense kernel. What would the sibling readings change structurally if adopted as the governing frame?',
    'Comparative classification across the three reading stories in the constraint family; convergence or divergence of computed types identifies which structural elements (trigger timing, attribution standard, beneficiary polarity) carry the classification.',
    'Under expansive_preventive_reading the beneficiary/victim polarity reverses (powerful states gain, weaker states and host states pay); under unable_unwilling_doctrine_reading the victim set shifts toward host states and the attribution boundary dissolves. This story''s classification holds only for the narrow reading''s arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed identity: the classification is valid for the narrow armed-attack reading of the Article 51 kernel, not for the kernel label.').

omega_variable(
    attribution_standard_boundary,
    'What degree of host-state involvement renders a non-state attack ''attributable'' — effective control, substantial involvement (Nicaragua), harboring, financing, or mere toleration?',
    'Evolution of ICJ jurisprudence and state practice in attribution disputes; application of the ILC articles on state responsibility to armed-attack cases.',
    'A stricter standard keeps the reading''s boundary where Nicaragua placed it; a looser standard collapses the practical difference between this reading and unable_unwilling_doctrine_reading, migrating victims toward host states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attribution_standard_boundary, conceptual, 'The reading''s load-bearing boundary is an attribution standard that is itself unsettled.').

omega_variable(
    imminence_standard_drift,
    'Does ''imminent'' retain the Caroline standard (instant, overwhelming, leaving no moment for deliberation) or drift toward a ''last feasible window'' reading that widens the permissible window?',
    'Doctrinal tracking of the Chatham House principles process, state pleadings before the Court, and Security Council debate over anticipatory-force claims.',
    'A widened imminence standard expands this reading''s effective scope without textual change, reducing the extraction borne by powerful states and moving the computed type toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imminence_standard_drift, conceptual, 'Imminence interpretation is the second load-bearing boundary of the narrow trigger.').

omega_variable(
    practice_theater_decoupling,
    'Is the reading''s persistence genuine constraint on state behavior, or rhetorical cover beneath operations conducted under rival justifications?',
    'Behavioral coding of cross-border force episodes against invoked justifications: if operations proceed regardless of attribution findings while narrow language is retained, decoupling is confirmed.',
    'Confirmed decoupling would push the structure toward piton dynamics (form maintained, function atrophied) and would date the type transition from the theater-series inflection around 2002-2010.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practice_theater_decoupling, empirical, 'Whether professed doctrine still binds behavior or has become performance.').

omega_variable(
    weak_state_shield_effectiveness,
    'Does the sovereignty shield actually protect weaker and host states when tested, or does it fail precisely in the cases that matter most?',
    'Case comparison of episodes where powerful states struck despite absent attribution versus episodes where the reading deterred action or channeled it through the Council.',
    'If the shield fails under test, the beneficiary declarations overstate protection, the security interests of the nominal beneficiaries absorb hidden extraction, and the coordination-function half of the tangled_rope claim weakens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(weak_state_shield_effectiveness, empirical, 'Whether the reading''s protection benefit is delivered or nominal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__narrow_armed_attack_reading, 1986, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(art51_narrow_tr_t1986, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1986, 0.15).
narrative_ontology:measurement_basis(art51_narrow_tr_t1986, observed).
narrative_ontology:measurement(art51_narrow_tr_t1994, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1994, 0.13).
narrative_ontology:measurement_basis(art51_narrow_tr_t1994, observed).
narrative_ontology:measurement(art51_narrow_tr_t2002, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2002, 0.2).
narrative_ontology:measurement_basis(art51_narrow_tr_t2002, observed).
narrative_ontology:measurement(art51_narrow_tr_t2010, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement_basis(art51_narrow_tr_t2010, observed).
narrative_ontology:measurement(art51_narrow_tr_t2018, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2018, 0.29).
narrative_ontology:measurement_basis(art51_narrow_tr_t2018, observed).
narrative_ontology:measurement(art51_narrow_tr_t2026, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2026, 0.33).
narrative_ontology:measurement_basis(art51_narrow_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(art51_narrow_be_t1986, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1986, 0.38).
narrative_ontology:measurement_basis(art51_narrow_be_t1986, observed).
narrative_ontology:measurement(art51_narrow_be_t1994, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1994, 0.35).
narrative_ontology:measurement_basis(art51_narrow_be_t1994, observed).
narrative_ontology:measurement(art51_narrow_be_t2002, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2002, 0.42).
narrative_ontology:measurement_basis(art51_narrow_be_t2002, observed).
narrative_ontology:measurement(art51_narrow_be_t2010, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2010, 0.45).
narrative_ontology:measurement_basis(art51_narrow_be_t2010, observed).
narrative_ontology:measurement(art51_narrow_be_t2018, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2018, 0.47).
narrative_ontology:measurement_basis(art51_narrow_be_t2018, observed).
narrative_ontology:measurement(art51_narrow_be_t2026, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2026, 0.48).
narrative_ontology:measurement_basis(art51_narrow_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(art51_narrow_su_t1986, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1986, 0.44).
narrative_ontology:measurement_basis(art51_narrow_su_t1986, observed).
narrative_ontology:measurement(art51_narrow_su_t1994, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1994, 0.4).
narrative_ontology:measurement_basis(art51_narrow_su_t1994, observed).
narrative_ontology:measurement(art51_narrow_su_t2002, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2002, 0.47).
narrative_ontology:measurement_basis(art51_narrow_su_t2002, observed).
narrative_ontology:measurement(art51_narrow_su_t2010, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2010, 0.51).
narrative_ontology:measurement_basis(art51_narrow_su_t2010, observed).
narrative_ontology:measurement(art51_narrow_su_t2018, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2018, 0.54).
narrative_ontology:measurement_basis(art51_narrow_su_t2018, observed).
narrative_ontology:measurement(art51_narrow_su_t2026, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2026, 0.55).
narrative_ontology:measurement_basis(art51_narrow_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__narrow_armed_attack_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense__expansive_preventive_reading).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense__unable_unwilling_doctrine_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'Article 51 self-defense' covers three structurally distinct trigger regimes that cannot share one epsilon. This story is the upstream member — the textual baseline whose attribution and imminence standards the siblings modify. The expansive reading supplies the necessity-pressure that erodes the narrow trigger; the unable-unwilling reading is the negotiated hybrid between them. Each member authors its own epsilon, beneficiaries, and victims; the edges here propagate contamination analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_51_self_defense__narrow_armed_attack_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
