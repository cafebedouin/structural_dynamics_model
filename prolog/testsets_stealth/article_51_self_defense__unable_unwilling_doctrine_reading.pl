% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__unable_unwilling_doctrine_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__unable_unwilling_doctrine_reading, []).

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
 *   constraint_id: article_51_self_defense__unable_unwilling_doctrine_reading
 *   human_readable: Article 51 Self-Defense: Unable-or-Unwilling Doctrine (Reading)
 *   domain: international law/security studies/constitutional interpretation
 *
 * SUMMARY:
 *   The unwilling-or-unable doctrine is the claimed legal standard under
 *   which a state may use force in self-defense against a non-state armed
 *   actor located on another state's territory when that territorial state is
 *   unwilling or unable to suppress the actor. This story instantiates the
 *   doctrine as a standing arrangement: a rule of decision that intervening
 *   states articulate and apply, host states absorb, and the Charter's
 *   collective organs watch without controlling. The arrangement has a
 *   genuine coordination function — it fills the attribution gap the
 *   Charter's state-centric self-defense trigger leaves open against
 *   transnational armed groups — and it carries an asymmetric cost structure:
 *   the intervening state acquires a lawful channel it did not previously
 *   have, while the host state's territorial inviolability is overridden
 *   without its consent and host civilians bear the operational harm. This
 *   story is one reading of the Article 51 self-defense kernel; the sibling
 *   readings (the narrow armed-attack reading and the expansive preventive
 *   reading) are separate constraints with their own ε, victim sets, and
 *   enforcement structures, linked through the network section. The
 *   claim/metric gap is deliberate: the constraint is CLAIMED as a lawful
 *   conditional channel (tangled_rope, the reading's own framing) while the
 *   authored metrics describe its actual operation — unilateral
 *   self-certification of the trigger, rising boilerplate justification, and
 *   accumulating precedent — and the engine measures the divergence rather
 *   than the author reconciling it.
 *
 * KEY AGENTS:
 *   - intervening_counterterror_states: primary beneficiary and agenda-setter (powerful/arbitrage) — articulates the standard in justification letters and executive legal opinions, collects the operational license
 *   - host_states_bypassed: primary target (moderate/constrained) — territorial control overridden without consent; recourse limited to protest and slow litigation
 *   - civilian_populations_in_host_states: diffuse target (powerless/trapped) — bear kinetic and displacement costs with no procedural voice
 *   - transnational_armed_groups: excluded non-party (moderate/trapped) — the doctrine's enforcement object, no standing in the framework that governs responses to it
 *   - un_security_council: bypassed collective organ (institutional/analytical) — receives notifications, divided along permanent-member lines
 *   - international_court_of_justice: analytical observer (institutional/analytical) — formal jurisprudential counterweight whose case law the practice routes around
 *   - doctrinal_objector_states: excluded interpretive bloc (organized/constrained) — GA majority objection with no enforcement effect
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, 0.62).
domain_priors:suppression_score(article_51_self_defense__unable_unwilling_doctrine_reading, 0.5).
domain_priors:theater_ratio(article_51_self_defense__unable_unwilling_doctrine_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__unable_unwilling_doctrine_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__unable_unwilling_doctrine_reading, "Article 51 Self-Defense: Unable-or-Unwilling Doctrine (Reading)").
narrative_ontology:topic_domain(article_51_self_defense__unable_unwilling_doctrine_reading, "international law/security studies/constitutional interpretation").

domain_priors:requires_active_enforcement(article_51_self_defense__unable_unwilling_doctrine_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__unable_unwilling_doctrine_reading, '06e1ca3c-32ae-4b3b-955d-74703f48db51').
narrative_ontology:cs_kernel_codification('06e1ca3c-32ae-4b3b-955d-74703f48db51', fixed_text).
narrative_ontology:cs_authority_grounding('06e1ca3c-32ae-4b3b-955d-74703f48db51', practice).
narrative_ontology:cs_interpretation_layer_present('06e1ca3c-32ae-4b3b-955d-74703f48db51').
narrative_ontology:cs_reading_relation('06e1ca3c-32ae-4b3b-955d-74703f48db51', article_51_self_defense__narrow_armed_attack_reading, forecloses).
narrative_ontology:cs_reading_relation('06e1ca3c-32ae-4b3b-955d-74703f48db51', article_51_self_defense__expansive_preventive_reading, influences).
narrative_ontology:cs_axiom('06e1ca3c-32ae-4b3b-955d-74703f48db51', foundational, non_state_attack_triggers_when_host_fails).
narrative_ontology:cs_axiom_status(non_state_attack_triggers_when_host_fails, holdable).
narrative_ontology:cs_axiom_grounding('06e1ca3c-32ae-4b3b-955d-74703f48db51', non_state_attack_triggers_when_host_fails, instrumental).
narrative_ontology:cs_axiom('06e1ca3c-32ae-4b3b-955d-74703f48db51', secondary, sovereignty_conditional_on_territorial_control).
narrative_ontology:cs_axiom_status(sovereignty_conditional_on_territorial_control, holdable).
narrative_ontology:cs_axiom_grounding('06e1ca3c-32ae-4b3b-955d-74703f48db51', sovereignty_conditional_on_territorial_control, conventional).
narrative_ontology:cs_reference_frame('06e1ca3c-32ae-4b3b-955d-74703f48db51', inherent_right_gap_filling_framework).
narrative_ontology:cs_drift_state('06e1ca3c-32ae-4b3b-955d-74703f48db51', contemporary_state_practice_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('06e1ca3c-32ae-4b3b-955d-74703f48db51', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_counterterror_states).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_bypassed).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, civilian_populations_in_host_states).
narrative_ontology:constraint_vindicates(article_51_self_defense__unable_unwilling_doctrine_reading, inherent_right_of_self_defense).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States that have suffered or anticipate armed attacks by non-state actors and maintain the military capacity to strike across borders. They articulate the unwilling-or-unable standard in letters to the Security Council, executive legal opinions, and military manuals, and they decide in each case whether the standard is met. Because they can alternatively invoke host-state consent, Council authorization, or the narrower attribution rule depending on the operation, their legal position is selected per case rather than fixed.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_counterterror_states, agenda_setter,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_counterterror_states, beneficiary).

% States on whose territory the armed groups operate. When they decline to act against the groups, or lack the capacity to do so, an intervening state may declare the standard met and conduct operations on their territory without their consent. Their formal recourse — General Assembly protest, Security Council complaint, occasional international litigation — has not stopped an operation to date. Their territory cannot be relocated, so the exposure persists as long as the groups remain.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_bypassed, payer,
    moderate, generational, constrained, national).

% People living in the districts where armed groups are embedded. They bear the strikes, the displacement, and the collateral harm of operations they had no part in authorizing, and their protection depends on targeting choices made by the intervening state and on diplomacy conducted by their own government.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, civilian_populations_in_host_states, payer,
    powerless, immediate, trapped, local).

% The non-state organizations whose attacks trigger the standard and whose presence in host territory supplies its condition. They are the object of the operations the standard licenses and have no standing in any forum where the standard's content is argued. Their realistic responses are dispersal, relocation, or embedding more deeply among civilians.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, transnational_armed_groups, excluded,
    moderate, biographical, trapped, regional).

% The collective organ the Charter assigns primary responsibility for international peace and security. It receives the intervening states' notifications, has on occasion endorsed the underlying right, and is otherwise divided along permanent-member lines; it neither authorizes nor stops the operations conducted under the standard.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, un_security_council, observer,
    institutional, generational, analytical, global).

% The principal judicial organ. Its case law requires attribution of an armed attack to a state and has expressed doubt about extending self-defense to non-state actors whose hosts are merely unwilling or unable to act. It rules only when contentious cases reach it, and compliance with its rulings in this area has been voluntary.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, international_court_of_justice, observer,
    institutional, generational, analytical, global).

% The bloc of states — a General Assembly majority at various points since 2014 — that rejects the standard as an erosion of the Charter's limits on the use of force. They state their position in debates and resolutions; the position does not alter the conduct of operations carried out under the standard, and several members of the bloc have themselves been hosts or intervenors in specific cases.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, doctrinal_objector_states, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_counterterror_states).
narrative_ontology:fixing_cost_class(article_51_self_defense__unable_unwilling_doctrine_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the attribution gap: the Charter's self-defense rule, read to require state-attributable armed attacks, leaves a victim state with no lawful unilateral remedy when an armed attack is launched by a non-state actor operating from a state that will not or cannot suppress it. The doctrine supplies a conditional lawful channel — force in response to an actual armed attack, lawful only while the territorial state's failure persists — and communicates to all states the consequence of tolerating armed groups on their territory.
% TRANSFER_FUNCTION: Moves the decision over cross-border force from the Security Council and the host state's consent to the intervening state's unilateral legal determination; moves the operational and collateral costs of counterterror force into the host state's territory; moves legal exposure from the intervening state, which acquires a public justification, to the host state, which loses the inviolability of its territory for the duration of the determination.
% ABSENT_VOICES: Host states and the civilians in the affected territories have no seat where the doctrine's content is made — the standard is articulated in intervening states' justification letters, executive legal memos, and military manuals. Transnational armed groups are outside the legal order entirely. The majority bloc of objecting states speaks in General Assembly debate, but its position registers no enforcement effect on ongoing operations.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, every ongoing cross-border operation justified by it would lose its claimed legal basis: intervening states would have to obtain host consent, seek Council authorization, or act without justification; host states would regain a stronger territorial shield; and the precedent structure supporting further unilateral operations would stop accumulating. The underlying military operations might continue under other claimed bases, but the specific legal arrangement — and the sovereignty cost it allocates — would be gone.
% FOUNDING_PROBLEM: After the 2001 attacks, the Charter framework appeared to leave a victim state of a large-scale non-state armed attack without a lawful unilateral remedy when the territorial state would not suppress the attackers — the drafters' state-attribution trigger did not map onto transnational terrorism.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the intervening-state beneficiary set: Security Council resolutions 1368 and 1373 (2001) reaffirmed the inherent right of self-defense after a non-state attack without limiting it to state-attributable attacks; the ICJ's jurisprudence engages the attribution problem even while declining the proposed solution; and host states' objections concede the reality of cross-border armed-group attacks while disputing the remedy. No body outside the beneficiary set attests that unilateral sovereignty bypass specifically is the necessary solution — the corroboration covers the problem, not this reading's remedy.
narrative_ontology:disappearance_verdict(article_51_self_defense__unable_unwilling_doctrine_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__unable_unwilling_doctrine_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_51_self_defense__unable_unwilling_doctrine_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__unable_unwilling_doctrine_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__unable_unwilling_doctrine_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate and rising (0.62 at interval end): the arrangement transfers a real, non-recoverable good — territorial control — from host states without consent, but the transfer is bounded by the actual-attack requirement, the necessity and proportionality limits that still formally govern, and the fact that a host state can end the condition by acting against the armed groups itself. Suppression is authored at 0.50 as a raw structural property (it is not scaled by power or scope; only extractiveness is scaled in the engine's computation): the arrangement holds against contrary ICJ case law and a General Assembly majority not through formal coercion but through the continuous accumulation of great-power practice and the acquiescence that practice extracts. The suppression_requirement series shows the enforcement machinery maturing through the mid-2010s — legal advocacy infrastructure, routinized justification practice, acquiescence management — and then partially normalizing as the practice base thickened and other powers adopted or tolerated the standard. Theater is 0.48: early justifications were genuine, contested legal argument, while contemporary notification practice is increasingly boilerplate — the same paragraph structure citing the same standard regardless of the host state's actual capacity or intent. Accessibility collapse is 0.55: the formal alternatives (Council authorization, host consent, restraint under the narrow reading) remain open, but intervening states treat the doctrine as the default available pathway, and the practical cost of the alternatives rises with each accumulated precedent. Resistance is 0.60: sustained objection from a GA majority, contrary ICJ jurisprudence, and host-state protest that has not altered conduct. All three series share one time grid (2001–2025, seven points) so no metric's row is backfilled from another's.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by structure. From the intervening state's position the arrangement is a lawful, conditional channel it built to close a gap the Charter left — the trigger conditions look like constraints it voluntarily accepts. From the host state's position the same structure is a unilateral override of territorial control it never consented to and cannot terminate except by doing the intervening state's military work for it. The Council experiences erosion of its assigned role; the ICJ experiences its case law being routed around by practice; host civilians experience the arrangement only as incoming fire. Intervening states and host states are formally equal sovereigns — the same nominal level of the state system — and what differentiates them is not legal status but military capacity and the ability to choose among legal theories per operation; the doctrine's differentiation of formally equal actors is the same-level dynamic this story turns on.
 *
 * DIRECTIONALITY LOGIC:
 *   The intervening states are the structural beneficiaries: they collect the operational license and shift legal risk outward, and their exit is arbitrage-grade because they can select among consent, Council authorization, the narrow rule, or this doctrine per operation — d sits near the beneficiary end for them. Host states are the targets: they pay in sovereignty and cannot relocate territory; their exit is constrained to protest and slow, voluntary-compliance litigation — d sits near the target end. Host civilians are the most trapped seats: they bear kinetic and displacement costs with no procedural voice at all, placing them at the full-target end. Armed groups are excluded rather than coordinated — they are the enforcement object, and their exclusion is what part of the enforcement machinery maintains. The Council and the ICJ are analytical seats whose formal authority the arrangement bypasses; they collect no rents and bear no direct costs, which is why the arrangement can persist despite both.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the attribution gap for transnational armed attacks — is live, so the arrangement is not mandatrophy-resolved and the live-status × world-rearranges pairing raises no capture flag. The early-warning signal is the theater trajectory: justification practice is drifting from argued legal position toward boilerplate, and if the threat landscape changes (armed groups suppressed, consent-based frameworks covering most operations) while the notification practice continues unchanged, the arrangement degrades toward inertial maintenance — a standard invoked ritually by intervenors who no longer need it and objected to by hosts who cannot stop it. The classification prevents two symmetric misreadings: reading the arrangement as pure coordination (which would ignore that the same structure licensing the intervenor strips the host) and reading it as pure extraction (which would ignore the real gap it fills and the conditionality that bounds it).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_51_kernel_reading_location,
    'This story instantiates one reading of the Article 51 self-defense kernel — the unwilling-or-unable reading. How much of the measured structure is specific to this reading, versus an artifact of the kernel text itself that the sibling readings share?',
    'Generate the sibling readings (narrow_armed_attack_reading, expansive_preventive_reading) as separate constraint stories and compare per-seat classifications; structural elements constant across all three belong to the kernel, not to this reading.',
    'If the extraction profile is shared across all readings, the kernel text itself carries the extraction and this reading''s ε is overstated; if it is reading-specific, the decomposition stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_51_kernel_reading_location, conceptual, 'Which structural features belong to this reading versus the shared Article 51 kernel.').

omega_variable(
    unwillingness_evidentiary_standard,
    'What evidentiary standard actually governs ''unwilling or unable'' determinations in state practice, and is the determination effectively unilateral self-certification by the intervening state?',
    'Systematic comparison of intervention-justifying letters to the Security Council against independent assessments of host-state capacity and intent; code whether any determination has ever been rejected or reversed by a body outside the intervening state.',
    'If the standard is effectively unilateral self-certification, the doctrine''s constraint on intervenors is thinner than its terms and ε is understated (drifting toward snare); if determinations are genuinely contestable and occasionally withheld, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unwillingness_evidentiary_standard, empirical, 'Whether the trigger determination is a real constraint or a unilateral formality.').

omega_variable(
    consent_practice_base_contamination,
    'How much of the state practice cited to entrench the doctrine actually rests on host-state consent rather than on an unwilling-or-unable determination?',
    'Classify each cross-border counterterror operation cited in doctrine-defending scholarship by its actual legal basis: host consent, Council authorization, unable-or-unable determination, or no claimed basis.',
    'A thin genuine practice base means the doctrine is less entrenched than claimed and its accumulation trajectory is flatter than the measurement series suggests; a thick base confirms the trajectory and the rising accessibility collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_practice_base_contamination, empirical, 'Whether the doctrine''s practice base is thicker than its genuine applications.').

omega_variable(
    coordination_extraction_tradeoff,
    'Does the doctrine''s coordination benefit — a lawful channel for victim states facing transnational armed attacks when the Council is paralyzed — justify its cost to host-state sovereignty, as a matter of values?',
    'Not resolvable by data; depends on the relative weight parties assign to sovereign equality versus effective self-defense. Resolved politically only if a codification moment (General Assembly resolution, ICJ ruling, or consolidated treaty practice) forces an explicit trade.',
    'If sovereignty is weighted higher, the arrangement computes as closer to pure extraction; if effective self-defense is weighted higher, the coordination function dominates and the constraint sits closer to rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_tradeoff, preference, 'The values question underlying whether the sovereignty cost is a justified coordination price or an unjustified transfer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__unable_unwilling_doctrine_reading, 2001, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(a51_unable_unwilling_tr_t2001, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2001, 0.22).
narrative_ontology:measurement_basis(a51_unable_unwilling_tr_t2001, observed).
narrative_ontology:measurement(a51_unable_unwilling_tr_t2005, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement_basis(a51_unable_unwilling_tr_t2005, observed).
narrative_ontology:measurement(a51_unable_unwilling_tr_t2009, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2009, 0.33).
narrative_ontology:measurement_basis(a51_unable_unwilling_tr_t2009, observed).
narrative_ontology:measurement(a51_unable_unwilling_tr_t2013, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2013, 0.4).
narrative_ontology:measurement_basis(a51_unable_unwilling_tr_t2013, observed).
narrative_ontology:measurement(a51_unable_unwilling_tr_t2017, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2017, 0.45).
narrative_ontology:measurement_basis(a51_unable_unwilling_tr_t2017, observed).
narrative_ontology:measurement(a51_unable_unwilling_tr_t2021, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2021, 0.47).
narrative_ontology:measurement_basis(a51_unable_unwilling_tr_t2021, observed).
narrative_ontology:measurement(a51_unable_unwilling_tr_t2025, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2025, 0.48).
narrative_ontology:measurement_basis(a51_unable_unwilling_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(a51_unable_unwilling_be_t2001, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2001, 0.35).
narrative_ontology:measurement_basis(a51_unable_unwilling_be_t2001, observed).
narrative_ontology:measurement(a51_unable_unwilling_be_t2005, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2005, 0.4).
narrative_ontology:measurement_basis(a51_unable_unwilling_be_t2005, observed).
narrative_ontology:measurement(a51_unable_unwilling_be_t2009, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2009, 0.46).
narrative_ontology:measurement_basis(a51_unable_unwilling_be_t2009, observed).
narrative_ontology:measurement(a51_unable_unwilling_be_t2013, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2013, 0.54).
narrative_ontology:measurement_basis(a51_unable_unwilling_be_t2013, observed).
narrative_ontology:measurement(a51_unable_unwilling_be_t2017, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2017, 0.6).
narrative_ontology:measurement_basis(a51_unable_unwilling_be_t2017, observed).
narrative_ontology:measurement(a51_unable_unwilling_be_t2021, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2021, 0.62).
narrative_ontology:measurement_basis(a51_unable_unwilling_be_t2021, observed).
narrative_ontology:measurement(a51_unable_unwilling_be_t2025, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2025, 0.62).
narrative_ontology:measurement_basis(a51_unable_unwilling_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(a51_unable_unwilling_su_t2001, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2001, 0.42).
narrative_ontology:measurement_basis(a51_unable_unwilling_su_t2001, observed).
narrative_ontology:measurement(a51_unable_unwilling_su_t2005, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2005, 0.47).
narrative_ontology:measurement_basis(a51_unable_unwilling_su_t2005, observed).
narrative_ontology:measurement(a51_unable_unwilling_su_t2009, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2009, 0.53).
narrative_ontology:measurement_basis(a51_unable_unwilling_su_t2009, observed).
narrative_ontology:measurement(a51_unable_unwilling_su_t2013, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2013, 0.58).
narrative_ontology:measurement_basis(a51_unable_unwilling_su_t2013, observed).
narrative_ontology:measurement(a51_unable_unwilling_su_t2017, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2017, 0.56).
narrative_ontology:measurement_basis(a51_unable_unwilling_su_t2017, observed).
narrative_ontology:measurement(a51_unable_unwilling_su_t2021, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2021, 0.52).
narrative_ontology:measurement_basis(a51_unable_unwilling_su_t2021, observed).
narrative_ontology:measurement(a51_unable_unwilling_su_t2025, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2025, 0.5).
narrative_ontology:measurement_basis(a51_unable_unwilling_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__unable_unwilling_doctrine_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, expansive_preventive_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'self-defense under Article 51' covers three structurally distinct claims that decompose per the ε-invariance principle. The narrow armed-attack reading (formal-textual baseline: state attribution required) has near-negligible extraction from intervening states but leaves the attribution gap open; this unable-or-unwilling reading (moderate: actual attack required, unilateral trigger determination permitted) carries moderate extraction from host states; the expansive preventive reading (downstream: preventive force against emerging threats) would carry the highest extraction if adopted. The narrow reading is upstream (its case-law baseline is what this reading's practice argues against); the expansive reading is downstream (it cites this reading's accumulated practice as precedential material). Each story carries its own ε, beneficiaries, victims, and enforcement structure; they are linked here and via each sibling's own affects_constraints array.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
