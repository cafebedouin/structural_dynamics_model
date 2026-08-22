% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__national_liberation_reading, []).

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
 *   constraint_id: combatant_status_definition__national_liberation_reading
 *   human_readable: AP I Article 1(4) National Liberation Combatant Status Extension
 *   domain: legal/international_humanitarian
 *
 * SUMMARY:
 *   Additional Protocol I Article 1(4) extends the scope of international
 *   armed conflict to wars 'in which peoples are fighting against colonial
 *   domination and alien occupation and against racist régimes in the
 *   exercise of their right of self-determination,' and Article 96(3) lets
 *   the authority representing such a people undertake the Conventions by
 *   unilateral declaration — the route by which its fighters would obtain
 *   combatant status and prisoner-of-war protection on capture, conditional
 *   on organization under responsible command and humanitarian-law
 *   discipline. This file authors the national liberation reading of the
 *   combatant-status kernel as a clean constraint: the rule as enacted and as
 *   it actually operates. The striking structural fact is dormancy: in the
 *   five decades since adoption, no authority has ever lodged a 96(3)
 *   declaration, no state party has ever granted combatant status under the
 *   article, and major military powers declined to ratify the Protocol citing
 *   the article among their principal objections. The provision's daily life
 *   is diplomatic defense — General Assembly reaffirmations, scholarly
 *   citation, anniversary rhetoric — while its operative core has never fired
 *   once. The claim/metric gap is deliberate: the constraint is CLAIMED as a
 *   hybrid coordination-and-cost arrangement (a real protection-gap function
 *   entangled with asymmetric costs on state parties) while the authored
 *   metrics register high theatrical maintenance, decaying enforcement
 *   capacity, and heavy continuing resistance — the engine measures that
 *   divergence rather than the author reconciling it.
 *
 * KEY AGENTS:
 *   - national_liberation_movements: Primary intended beneficiary (organized/identity_locked) — holds the activation switch it has never pulled; the struggle constitutes the movement, so exit means the cause's dissolution
 *   - administering_and_occupying_state_parties: Primary target (institutional/constrained) — bears the obligation to grant combatant immunity to insurgents; ratification traps them, since denunciation forfeits the whole Protocol
 *   - captured_liberation_fighters: Designed beneficiary, operative cost-bearer (powerless/trapped) — bears criminalization in domestic courts while the promised status stays theoretical
 *   - peoples_under_self_determination_struggle: Collective beneficiary (powerless/trapped) — the right the provision operationalizes; bears the war's costs regardless of classification
 *   - champion_state_parties: Agenda-setter and secondary beneficiary (institutional/constrained) — maintains the provision diplomatically at low cost, has never sponsored an activation
 *   - non_party_military_powers: Excluded objectors (institutional/arbitrage) — outside the binding reach, anchor the state-only sibling account
 *   - icrc: Analytical observer (institutional/analytical) — custodian whose customary-law determinations gate the rule's reach beyond the treaty parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, 0.52).
domain_priors:suppression_score(combatant_status_definition__national_liberation_reading, 0.42).
domain_priors:theater_ratio(combatant_status_definition__national_liberation_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__national_liberation_reading, "AP I Article 1(4) National Liberation Combatant Status Extension").
narrative_ontology:topic_domain(combatant_status_definition__national_liberation_reading, "legal/international_humanitarian").

domain_priors:requires_active_enforcement(combatant_status_definition__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__national_liberation_reading, '8b4768d1-5656-454a-a115-c10d433b45cb').
narrative_ontology:cs_kernel_codification('8b4768d1-5656-454a-a115-c10d433b45cb', formalized).
narrative_ontology:cs_authority_grounding('8b4768d1-5656-454a-a115-c10d433b45cb', lineage).
narrative_ontology:cs_interpretation_layer_present('8b4768d1-5656-454a-a115-c10d433b45cb').
narrative_ontology:cs_reading_relation('8b4768d1-5656-454a-a115-c10d433b45cb', combatant_status_definition__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('8b4768d1-5656-454a-a115-c10d433b45cb', combatant_status_definition__functional_protection_reading, influences).
narrative_ontology:cs_axiom('8b4768d1-5656-454a-a115-c10d433b45cb', foundational, self_determination_cause_confers_combatant_status).
narrative_ontology:cs_axiom_status(self_determination_cause_confers_combatant_status, holdable).
narrative_ontology:cs_axiom_grounding('8b4768d1-5656-454a-a115-c10d433b45cb', self_determination_cause_confers_combatant_status, deontological).
narrative_ontology:cs_axiom('8b4768d1-5656-454a-a115-c10d433b45cb', secondary, status_extension_incentivizes_ihl_compliance).
narrative_ontology:cs_axiom_status(status_extension_incentivizes_ihl_compliance, holdable).
narrative_ontology:cs_axiom_grounding('8b4768d1-5656-454a-a115-c10d433b45cb', status_extension_incentivizes_ihl_compliance, instrumental).
narrative_ontology:cs_reference_frame('8b4768d1-5656-454a-a115-c10d433b45cb', self_determination_inclusive_combatant_regime).
narrative_ontology:cs_drift_state('8b4768d1-5656-454a-a115-c10d433b45cb', contemporary_post_decolonization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8b4768d1-5656-454a-a115-c10d433b45cb', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__national_liberation_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, national_liberation_movements).
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, captured_liberation_fighters).
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, peoples_under_self_determination_struggle).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, administering_and_occupying_state_parties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, champion_state_parties).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, captured_liberation_fighters).
narrative_ontology:constraint_vindicates(combatant_status_definition__national_liberation_reading, self_determination_right).
narrative_ontology:constraint_vindicates(combatant_status_definition__national_liberation_reading, equality_of_combatants_principle).
narrative_ontology:constraint_vindicates(combatant_status_definition__national_liberation_reading, geneva_law_universality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organized armed groups waging armed struggle against colonial administration, foreign occupation, or institutionalized racial rule on behalf of a people claiming self-determination. Under the provision they can obtain combatant status for their members if organized under responsible command and enforcing humanitarian-law discipline; activation runs through a unilateral declaration by the authority representing the people, lodged with the treaty depositary. The declaration commits them to apply the Geneva Conventions in full, exposes their command structures to reciprocal scrutiny, and requires them to characterize their conflict as international — a characterization their adversary disputes. In five decades no authority representing such a people has lodged the declaration, so members captured in action are processed under their captor's criminal law while the entitlement remains on paper.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, national_liberation_movements, beneficiary,
    organized, generational, identity_locked, regional).

% States party to the Protocol that govern territory whose inhabitants claim self-determination, or that face armed movements invoking the provision. The rule obliges them to treat captured members of a qualifying movement as combatants rather than criminals — surrendering the criminal jurisdiction they would otherwise exercise over insurgency and conferring legitimacy on armed challenge to their authority. They resist invocation by disputing that any live conflict meets the provision's categories, and their exit is costly: denouncing the Protocol to escape the article would forfeit the conduct-of-hostilities and civilian-protection rules that shield their own forces.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, administering_and_occupying_state_parties, payer,
    institutional, generational, constrained, global).

% Individual members of movements that qualify or claim to qualify, taken in combat and held by the adverse party. If the provision operated, they would hold combatant status on capture: immunity for their warlike acts and prisoner-of-war treatment. As the arrangement stands, with no declaration ever lodged, they face prosecution for murder, terrorism, or treason in domestic courts, with the Common Article 3 floor as their only treaty protection. They hold no exit at all: their status is decided entirely by the capturing power's legal position.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, captured_liberation_fighters, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__national_liberation_reading, captured_liberation_fighters, payer).

% The populations of non-self-governing, occupied, or racially-ruled territories whose right of self-determination the provision operationalizes. They bear the war's costs directly — displacement, siege, reprisal — regardless of how their fighters are classified. The provision's benefit to them is indirect: reciprocal treatment of their fighters and international legitimation of their struggle as a war rather than a criminal disorder.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, peoples_under_self_determination_struggle, beneficiary,
    powerless, generational, trapped, national).

% The bloc of states, largely from the Non-Aligned and Group of 77 caucuses, that carried the provision through the 1974–1977 diplomatic conference against Western objection and have defended it since in General Assembly resolutions and conference declarations. They collect diplomatic and normative capital from the provision's existence and treat it as settled progressive law. Defending it costs them little; sponsoring an actual declaration for any live movement would force them to choose among claimants and confront their trading partners, and none has ever done so.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, champion_state_parties, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__national_liberation_reading, champion_state_parties, beneficiary).

% Major military powers that declined to ratify the Protocol, citing the provision among their principal objections, and that remain outside its binding reach. They argue that extending combatant privilege to insurgents rewards unlawful violence and erodes state authority over non-state armed groups. Their exclusion is structural: they neither owe nor receive anything under the provision, while their practice and doctrine anchor the rival state-only account of combatant status.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, non_party_military_powers, excluded,
    institutional, generational, arbitrage, global).

% The custodial institution of international humanitarian law: it monitors compliance in armed conflicts, maintains the official commentary on the provision, and its 2005 customary-law study declined to list the combatant-status rule as customary, confining it to the treaty-bound parties. It neither pays nor collects under the arrangement; its determinations shape which accounts of the status question carry institutional force.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, icrc, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__national_liberation_reading, diffuse).
narrative_ontology:fixing_cost_class(combatant_status_definition__national_liberation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Extends the reciprocal combatant and prisoner-of-war framework of international humanitarian law to wars of self-determination: without it, fighters in colonial, occupation, and racially-ordered conflicts fall outside the combatant regime entirely and are governed only by domestic criminal law and the Common Article 3 minimums. The rule gives both sides a common status framework — organization, responsible command, and humanitarian-law compliance as conditions — so captured fighters are processed under a known regime rather than ad hoc criminal prosecution.
% TRANSFER_FUNCTION: Moves legal privilege and protection from state parties to qualifying non-state fighters: on activation, the state party must treat captured members of the movement as combatants — immunity for acts of war and prisoner-of-war status — rather than criminals. The corresponding flow to the state is the movement's reciprocal undertaking, lodged by declaration, to apply the Geneva Conventions in full and enforce humanitarian-law discipline within its ranks.
% ABSENT_VOICES: The peoples actually fighting and the captured fighters whose status the rule determines have never held seats in any invocation decision — activation is lodged by the authority representing the people and contested by the adverse state, with no procedural voice for the detainees whose treatment hangs on it. The non-party military powers object from outside the treaty's binding reach, structurally excluded from the arrangement they most actively contest.
% DISAPPEARANCE_RATIONALE: If the provision vanished overnight, the state-only account of combatant status would become the uncontested default in every legal order, liberation movements would lose their principal legal argument against criminalization of captured fighters, the treaty politics of 1977 would read differently (the major-power non-ratifications would lose their principal stated target), and the status-independent minimum-protection floor would become the only remaining protection track for detainees in self-determination conflicts. The beneficiaries, payers, and champions all hold positions that depend on the provision's existence.
% FOUNDING_PROBLEM: The decolonization-era protection gap: under the state-centric combatant-status framework, fighters in wars against colonial domination, alien occupation, and racist regimes were denied combatant status, their conflicts were often denied international character altogether, and captured fighters faced domestic prosecution or execution as common criminals. The provision was built to extend the combatant and prisoner-of-war regime to those struggles, operationalizing the recognized right of peoples to self-determination inside the law of armed conflict.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the 1974–1977 diplomatic conference records show the administering powers themselves acknowledging the historical protection gap while disputing the remedy's categories; ICRC commentary and the mainstream scholarly literature — neither a beneficiary seat — attest the gap's historical reality and the rule's contested present status. On whether the problem REMAINS live, attestation splits along the reading contest itself: UN decolonization-framework listings and the movements' sponsors attest live categories (occupied territories, residual non-self-governing situations), while target-facing state practice attests the categories are spent. No source outside the beneficiary set attests that the mechanism as operated has ever delivered the promised protection; that universal absence is itself the strongest corroborating datum about the arrangement's operative state.
narrative_ontology:disappearance_verdict(combatant_status_definition__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__national_liberation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__national_liberation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(combatant_status_definition__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__national_liberation_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__national_liberation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(combatant_status_definition__national_liberation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is authored at 0.52 (moderate): the arrangement's designed transfer — state parties surrender criminal jurisdiction over captured insurgents; fighters gain conditional privilege gated by organization, command, and compliance — is real and asymmetric, but criteria-gated and never executed, so the standing arrangement extracts nominally rather than operatively. Suppression is 0.42: the rule's operative coercive force (its bar on prosecuting qualifying fighters) is latent, and the enforcement machinery that would operate it has decayed across the interval. Theater ratio is 0.70 and rising across the series: the dominant activity around the provision is performative maintenance (reaffirmation resolutions, doctrinal defense, ratification politics) against zero functional activations in 49 years. Accessibility collapse is 0.50: alternatives partly persist — the customary-law argument, the status-independent minimum-protection floor for detainees, non-ratification for states — so understanding the arrangement does not foreclose every alternative route. Resistance is 0.80: the article drew the fiercest opposition of any provision in the 1974–1977 conference and remains the principal stated reason for major-power non-ratification. The three temporal series share one eight-point grid, every metric authored at every point. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity decay — the provision's operability, not merely its cost-shifting, is the dynamic: 0.62 at adoption, when administering powers faced live qualifying struggles and status-determination machinery was anticipated, falling to 0.42 as the decolonization wave completed, the qualifying-conflict population thinned, and the 2005 customary-law study declined to extend the rule beyond the treaty parties. Base extractiveness rises mildly (0.42 to 0.52) as the norm's nominal force is reaffirmed in UN fora while its delivery decays; theater rises steeply (0.25 to 0.70). The joint signature — stable nominal force, falling enforcement, rising theater — is the quantitative shadow of an arrangement drifting toward inertial maintenance while its claimed function stays formally live.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seats compute different constraints from the same text. From the administering-and-occupying state parties' position, the arrangement is an imposed obligation: it would disarm their criminal jurisdiction over insurgents, confer legitimacy on armed challenge, and do so under categories their adversaries define. From the movements' position, it is a conditional entitlement they have never been able to collect, gated by criteria requiring them to expose their command structures and characterize their conflict as international. From the champion states' position, it is settled progressive law whose defense costs resolutions and nothing operational. From the non-party powers' position, it is a dangerous precedent they declined to be bound by. The engine computes these per-seat classifications from the structural data — power, exit, directionality — and the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations map to low directionality for the movements, the captured fighters, and the peoples: the provision subsidizes them with conditional status and protection. The victim declaration maps to high directionality for the administering-and-occupying state parties: they bear the arrangement's cost — surrendered criminal jurisdiction and legitimized insurgency — and ratification constrains their exit, since denunciation would forfeit the Protocol's protections for their own forces. Two structural facts complicate the derivation and are handled explicitly. First, the movements' seat carries an override to 0.35 on the organized power atom: the structural derivation would read a pure beneficiary (low d) from the beneficiary declaration, but the standing arrangement's dormancy plus the activation switch's costs — full Convention undertakings, reciprocal scrutiny, the admission that the conflict is international — leave the movements bearing regime costs without collecting the benefit, nearer the target end than a beneficiary derivation alone suggests. Second, the captured fighters carry a secondary cost-bearing position: under the arrangement as operated they are the seat that actually bears criminalization; that fact is carried by the theater ratio and the dormancy omega rather than by flipping their primary declaration, preserving the expected per-seat profile (moderate burden on the movements, high burden on the state parties). The champion states sit low-mid as agenda-setting defenders; the non-party powers sit outside the transfer entirely; the ICRC is analytical. Global treaty scope modestly amplifies effective extraction per the engine's scope scaling; suppression is a raw structural property and is not scaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabels. As pure coordination it would erase the asymmetric cost structure: state parties surrender real sovereign prerogatives while the fighters' gain is criteria-gated — that asymmetry, riding a genuine protection-gap function, is why the claim is a hybrid rather than a pure coordination arrangement. As pure extraction it would erase the absence of a capturer: no seat collects the arrangement's gains — the mechanism has never paid out, and the gains are affirmatively diffuse — so there is no concentrated beneficiary whose interest explains the provision's persistence; what explains persistence is diplomatic sunk cost and the provision's value as a positional marker in the status contest. The R5 interview finds the founding problem contested-live rather than dead: the decolonization residue (occupied territories, residual non-self-governing situations, persisting racially-ordered rule where alleged) keeps the provision's categories nominally populated, and the parties dispute whether the problem persists. No dead-mandate flag fires — but the theater trajectory is the quantitative shadow of a remedy whose delivery mechanism has atrophied while its mandate is still argued live. If the founding problem is ultimately judged spent, the provision becomes a candidate for inertial-maintenance classification; that judgment is exactly what the dormancy omega holds open, and the fixing-cost entry (prohibitive) records why no seat has spent the political capital to resolve it either way: denunciation would forfeit the entire Protocol, and activation would force the champions to choose among claimants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the combatant_status_definition kernel — the national_liberation_reading, instantiating AP I Article 1(4). What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'A sibling reading is adopted by a legal order through ratification posture, military manuals, and judicial treatment of captured insurgents: adoption of the state_centric_reading would remove non-state fighters from the beneficiary set entirely and leave the state''s criminal jurisdiction unimpaired; adoption of the functional_protection_reading would dissolve the status question into status-independent minimums, emptying both the beneficiary set and the state''s obligation.',
    'Each reading instantiates a different constraint with a different epsilon and different victim/beneficiary structure: this reading authors moderate base extraction over an arrangement whose designed transfer never executes; the state-centric sibling authors high extraction from fully criminalized captured fighters; the functional sibling authors near-zero extraction at the minimum-protection floor. Per-seat classifications computed from this file are valid only for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: which reading of the status kernel this file instantiates and what the siblings would change.').

omega_variable(
    invocation_dormancy_ambiguity,
    'Is the provision''s fifty-year non-invocation evidence that its function has atrophied and what remains is inertial, performative maintenance — or evidence that activation conditions have simply never been met by a qualifying movement with both the standing and the incentive to declare?',
    'A qualifying authority lodging an Article 96(3) declaration and the state parties'' response to it — or a definitive determination in state practice and ICRC doctrine that the provision''s categories (colonial domination, alien occupation, racist regimes) are spent.',
    'If the function is dead, the arrangement reclassifies toward inertial maintenance (theater-dominant, no capturer); if it is live but unmet, the high theater ratio reads as latency rather than atrophy and the hybrid coordination reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(invocation_dormancy_ambiguity, empirical, 'Whether the provision is a live remedy awaiting conditions or a spent instrument maintained performatively.').

omega_variable(
    customary_status_ambiguity,
    'Does the combatant-status rule for liberation fighters bind states that never ratified the Protocol, as customary international law, or is it strictly treaty-bound to the roughly 170 parties?',
    'State practice and opinio juris analysis in a live qualifying conflict: military manuals, diplomatic protests, and judicial decisions of non-party states facing self-determination claims.',
    'If customary, the constraint''s reach extends to the non-party military powers and their effective extraction rises from outside-position to target; if treaty-only, the non-party powers sit outside and the state-centric sibling reading gains force in exactly the conflicts where this reading would matter most.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_status_ambiguity, empirical, 'Whether the rule''s reach is treaty-bound or customary.').

omega_variable(
    activation_cost_asymmetry,
    'Why has no authority representing a qualifying people ever lodged the Article 96(3) declaration — does the activation switch go unused because its costs (full Convention undertakings, reciprocal scrutiny, admission that the conflict is international) exceed the expected benefit, or because state parties would refuse to honor any declaration?',
    'Movement archives, legal-adviser testimony, and diplomatic correspondence surrounding candidate conflicts (occupied territories and residual non-self-governing situations).',
    'If activation costs deter, the beneficiary seat bears part of the arrangement''s burden and its directionality sits nearer the target end than the beneficiary declaration alone suggests (consistent with the authored override); if state refusal is certain, the arrangement''s performative character is structural and the theater ratio is design rather than drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(activation_cost_asymmetry, empirical, 'Whether the unused activation switch reflects beneficiary-side deterrence or target-side refusal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__national_liberation_reading, 1977, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t1977, combatant_status_definition__national_liberation_reading, theater_ratio, 1977, 0.25).
narrative_ontology:measurement_basis(comb_tr_t1977, observed).
narrative_ontology:measurement(comb_tr_t1984, combatant_status_definition__national_liberation_reading, theater_ratio, 1984, 0.35).
narrative_ontology:measurement_basis(comb_tr_t1984, observed).
narrative_ontology:measurement(comb_tr_t1991, combatant_status_definition__national_liberation_reading, theater_ratio, 1991, 0.45).
narrative_ontology:measurement_basis(comb_tr_t1991, observed).
narrative_ontology:measurement(comb_tr_t1998, combatant_status_definition__national_liberation_reading, theater_ratio, 1998, 0.55).
narrative_ontology:measurement_basis(comb_tr_t1998, observed).
narrative_ontology:measurement(comb_tr_t2005, combatant_status_definition__national_liberation_reading, theater_ratio, 2005, 0.62).
narrative_ontology:measurement_basis(comb_tr_t2005, observed).
narrative_ontology:measurement(comb_tr_t2012, combatant_status_definition__national_liberation_reading, theater_ratio, 2012, 0.66).
narrative_ontology:measurement_basis(comb_tr_t2012, observed).
narrative_ontology:measurement(comb_tr_t2019, combatant_status_definition__national_liberation_reading, theater_ratio, 2019, 0.68).
narrative_ontology:measurement_basis(comb_tr_t2019, observed).
narrative_ontology:measurement(comb_tr_t2026, combatant_status_definition__national_liberation_reading, theater_ratio, 2026, 0.7).
narrative_ontology:measurement_basis(comb_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(comb_be_t1977, combatant_status_definition__national_liberation_reading, base_extractiveness, 1977, 0.42).
narrative_ontology:measurement_basis(comb_be_t1977, observed).
narrative_ontology:measurement(comb_be_t1984, combatant_status_definition__national_liberation_reading, base_extractiveness, 1984, 0.5).
narrative_ontology:measurement_basis(comb_be_t1984, observed).
narrative_ontology:measurement(comb_be_t1991, combatant_status_definition__national_liberation_reading, base_extractiveness, 1991, 0.55).
narrative_ontology:measurement_basis(comb_be_t1991, observed).
narrative_ontology:measurement(comb_be_t1998, combatant_status_definition__national_liberation_reading, base_extractiveness, 1998, 0.5).
narrative_ontology:measurement_basis(comb_be_t1998, observed).
narrative_ontology:measurement(comb_be_t2005, combatant_status_definition__national_liberation_reading, base_extractiveness, 2005, 0.46).
narrative_ontology:measurement_basis(comb_be_t2005, observed).
narrative_ontology:measurement(comb_be_t2012, combatant_status_definition__national_liberation_reading, base_extractiveness, 2012, 0.48).
narrative_ontology:measurement_basis(comb_be_t2012, observed).
narrative_ontology:measurement(comb_be_t2019, combatant_status_definition__national_liberation_reading, base_extractiveness, 2019, 0.5).
narrative_ontology:measurement_basis(comb_be_t2019, observed).
narrative_ontology:measurement(comb_be_t2026, combatant_status_definition__national_liberation_reading, base_extractiveness, 2026, 0.52).
narrative_ontology:measurement_basis(comb_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1977, combatant_status_definition__national_liberation_reading, suppression_requirement, 1977, 0.62).
narrative_ontology:measurement_basis(comb_su_t1977, observed).
narrative_ontology:measurement(comb_su_t1984, combatant_status_definition__national_liberation_reading, suppression_requirement, 1984, 0.57).
narrative_ontology:measurement_basis(comb_su_t1984, observed).
narrative_ontology:measurement(comb_su_t1991, combatant_status_definition__national_liberation_reading, suppression_requirement, 1991, 0.52).
narrative_ontology:measurement_basis(comb_su_t1991, observed).
narrative_ontology:measurement(comb_su_t1998, combatant_status_definition__national_liberation_reading, suppression_requirement, 1998, 0.48).
narrative_ontology:measurement_basis(comb_su_t1998, observed).
narrative_ontology:measurement(comb_su_t2005, combatant_status_definition__national_liberation_reading, suppression_requirement, 2005, 0.45).
narrative_ontology:measurement_basis(comb_su_t2005, observed).
narrative_ontology:measurement(comb_su_t2012, combatant_status_definition__national_liberation_reading, suppression_requirement, 2012, 0.44).
narrative_ontology:measurement_basis(comb_su_t2012, observed).
narrative_ontology:measurement(comb_su_t2019, combatant_status_definition__national_liberation_reading, suppression_requirement, 2019, 0.43).
narrative_ontology:measurement_basis(comb_su_t2019, observed).
narrative_ontology:measurement(comb_su_t2026, combatant_status_definition__national_liberation_reading, suppression_requirement, 2026, 0.42).
narrative_ontology:measurement_basis(comb_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__national_liberation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__functional_protection_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'combatant status for non-state fighters' decomposes into three structurally distinct claims — the combatant_status_definition kernel family: the national_liberation_reading (this file: cause-based extension, conditional, treaty-bound), the state_centric_reading (categorical state-only exclusion), and the functional_protection_reading (status-independent minimum protections). Their epsilon values differ: this reading authors moderate base extraction over a standing arrangement whose designed transfer never executes; the state-centric sibling authors high extraction from fully criminalized captured fighters; the functional sibling authors near-zero extraction at the minimum-protection floor. The readings are linked because each is cited against the others: the state-centric reading's persistence in state practice is the proximate cause of this reading's dormancy, and this reading's dormancy is the proximate cause of the functional reading's practical salience as the operative fallback floor for detainees.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(combatant_status_definition__national_liberation_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
