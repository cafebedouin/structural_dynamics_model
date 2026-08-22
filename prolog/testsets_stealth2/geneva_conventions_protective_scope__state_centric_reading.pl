% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__state_centric_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__state_centric_reading
 *   human_readable: Geneva Protective Scope — State-Centric Reading (Article 4 Combatant Gate)
 *   domain: legal/international_humanitarian_law
 *
 * SUMMARY:
 *   This file instantiates ONE reading of a contested kernel. The kernel —
 *   the protective scope of the Geneva corpus — is read here in its
 *   state-centric form: full combatant protections (prisoner-of-war status
 *   under Geneva III, combatant immunity for lawful acts of war) attach only
 *   to fighters meeting Article 4 criteria — belonging to a party to the
 *   conflict under responsible command, wearing fixed distinctive signs
 *   visible at distance, carrying arms openly, conducting operations in
 *   accordance with the laws of war. Fighters failing those markers fall
 *   outside this reading's protective scope: they may be engaged under attack
 *   rules without combatant-immunity treatment, detained without POW
 *   registration, and prosecuted under domestic criminal law. The reading
 *   preserves a real interstate coordination function — reciprocal
 *   lawful-combatant recognition and prisoner exchange between conventional
 *   forces — while concentrating its costs on non-state fighters, which is
 *   why the claimed type is tangled_rope rather than rope or snare. Sibling
 *   readings (universal_rights_reading, hybrid_proportionality_reading) are
 *   separate constraint files linked through the network section; their
 *   structural deltas are carried in omega variables, never averaged into
 *   this file's epsilon. Claim and metrics are authored independently: the
 *   claimed type reflects the structure I believe true; the metrics describe
 *   the arrangement's actual operation as I assess it.
 *
 * KEY AGENTS:
 *   - conventional_state_militaries: Primary beneficiary and administrator (institutional/arbitrage) — their soldiers sit inside the protected class, and the same institutions run the status-determination and prosecution machinery that enforces the boundary
 *   - unprivileged_belligerents: Primary target class (moderate/trapped) — irregular fighters who cannot meet the Article 4 markers without abandoning the concealment their survival depends on
 *   - detained_unprivileged_fighters: Concentrated target (powerless/trapped) — captured fighters bearing the detention-side costs of exclusion from POW protections
 *   - affected_civilian_populations: Diffuse cost-bearer (powerless/constrained) — live where the blurred fighter/civilian line runs through their households and marketplaces
 *   - icrc_protection_directorate: Institutional observer (institutional/analytical) — monitors detention and advocates broader application; cannot compel status outcomes
 *   - human_rights_treaty_bodies: Excluded critics (institutional/analytical) — object from outside the status-determination conversation that classifies the fighters
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, 0.65).
domain_priors:suppression_score(geneva_conventions_protective_scope__state_centric_reading, 0.66).
domain_priors:theater_ratio(geneva_conventions_protective_scope__state_centric_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__state_centric_reading, "Geneva Protective Scope — State-Centric Reading (Article 4 Combatant Gate)").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__state_centric_reading, "legal/international_humanitarian_law").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__state_centric_reading, 'bcea4ff1-5709-47a1-801e-5bf2a43b0a0f').
narrative_ontology:cs_kernel_codification('bcea4ff1-5709-47a1-801e-5bf2a43b0a0f', fixed_text).
narrative_ontology:cs_authority_grounding('bcea4ff1-5709-47a1-801e-5bf2a43b0a0f', distributed).
narrative_ontology:cs_reading_relation('bcea4ff1-5709-47a1-801e-5bf2a43b0a0f', geneva_conventions_protective_scope__universal_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('bcea4ff1-5709-47a1-801e-5bf2a43b0a0f', geneva_conventions_protective_scope__hybrid_proportionality_reading, influences).
narrative_ontology:cs_axiom('bcea4ff1-5709-47a1-801e-5bf2a43b0a0f', foundational, combatant_immunity_requires_article_4_compliance).
narrative_ontology:cs_axiom_status(combatant_immunity_requires_article_4_compliance, holdable).
narrative_ontology:cs_axiom_grounding('bcea4ff1-5709-47a1-801e-5bf2a43b0a0f', combatant_immunity_requires_article_4_compliance, conventional).
narrative_ontology:cs_axiom('bcea4ff1-5709-47a1-801e-5bf2a43b0a0f', foundational, statehood_grounds_lawful_belligerency).
narrative_ontology:cs_axiom_status(statehood_grounds_lawful_belligerency, holdable).
narrative_ontology:cs_axiom_grounding('bcea4ff1-5709-47a1-801e-5bf2a43b0a0f', statehood_grounds_lawful_belligerency, conventional).
narrative_ontology:cs_axiom('bcea4ff1-5709-47a1-801e-5bf2a43b0a0f', secondary, reciprocity_preserves_humanitarian_incentives).
narrative_ontology:cs_axiom_status(reciprocity_preserves_humanitarian_incentives, holdable).
narrative_ontology:cs_axiom_grounding('bcea4ff1-5709-47a1-801e-5bf2a43b0a0f', reciprocity_preserves_humanitarian_incentives, instrumental).
narrative_ontology:cs_reference_frame('bcea4ff1-5709-47a1-801e-5bf2a43b0a0f', article_4_status_gated_protection).
narrative_ontology:cs_drift_state('bcea4ff1-5709-47a1-801e-5bf2a43b0a0f', contemporary_asymmetric_conflict_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bcea4ff1-5709-47a1-801e-5bf2a43b0a0f', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, unprivileged_belligerents).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, detained_unprivileged_fighters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, affected_civilian_populations).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, affected_civilian_populations).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__state_centric_reading, article_4_status_criteria).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__state_centric_reading, strict_combatant_reciprocity).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__state_centric_reading, westphalian_monopoly_on_lawful_force).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Field professional armies under national command. Their soldiers meet the Article 4 markers — uniform, responsible command, open carriage of arms — and rely on reciprocal prisoner-of-war treatment when fighting other signatory forces. The same institutions run the status-determination boards, draft the targeting policy applied to fighters who fail the markers, and staff the military commissions that prosecute them. Exit for them means rewriting doctrine, adopting protocols they have so far declined, or shifting operations to partners and proxies; each path is available at a price they can afford to weigh.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries, agenda_setter).

% Fighters in irregular formations — insurgents, resistance cells, militia volunteers — who wear no fixed distinctive sign, carry weapons concealed, and answer to command structures the opposing state may refuse to recognize. Meeting the Article 4 markers would require abandoning the concealment that keeps them alive against superior firepower and surveillance. When captured they are processed under domestic criminal law rather than registered as prisoners of war; when engaged they are targeted under attack rules without the capture incentives that combatant status creates. Some belong to organized movements controlling territory; others fight in loose networks; none can purchase status.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, unprivileged_belligerents, payer,
    moderate, biographical, trapped, regional).

% Captured fighters held in state custody while authorities decide — or indefinitely defer deciding — their status. They lack prisoner-of-war registration, repatriation-upon-hostilities-end guarantees, and the communication rights attached to them; access to independent review depends entirely on the detaining state's courts and policy. Some have been held for years at facilities whose existence the detaining state initially disputed.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, detained_unprivileged_fighters, payer,
    powerless, immediate, trapped, local).

% Live in the towns and valleys where irregular hostilities occur. Because the fighters among them carry no distinguishing marks, the line between combatant and civilian runs through their households and marketplaces; raids, strikes, and reprisal fears land on them directly, and flight is costly and often impossible. They also receive whatever restraint the framework secures when both sides field recognizable armies, and they petition home states and international bodies for protection they cannot obtain locally.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, affected_civilian_populations, payer,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__state_centric_reading, affected_civilian_populations, beneficiary).

% Visits detention facilities where governments admit it, registers prisoners, relays family messages, and publishes commentary urging that humane-treatment obligations reach every person in state hands regardless of status. It holds no vote in status determinations and depends on state consent for access; its leverage is confidentiality, accumulated precedent, and the threat of public reporting.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, icrc_protection_directorate, observer,
    institutional, generational, analytical, global).

% Treaty committees and special rapporteurs reviewing state compliance with human rights obligations. They issue findings that detention and targeting of fighters without recognized status must still satisfy due-process and humane-treatment floors, and they press states to account for every person in custody. Their conclusions carry no binding force inside military status-determination processes, which proceed under domestic and Geneva-law channels that do not seat them.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, human_rights_treaty_bodies, excluded,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the identification-and-reciprocity problem of war between organized forces: a shared test for who counts as a lawful combatant lets opposing conventional militaries grant quarter confidently, register and exchange prisoners, and separate war crimes from status offenses without renegotiating terms conflict by conflict.
% TRANSFER_FUNCTION: Moves combatant immunity, prisoner-of-war protections, and detention-and-prosecution discretion along organizational lines: away from fighters lacking the Article 4 markers, toward state militaries, which gain engagement latitude over the excluded class and control the forums that classify them.
% ABSENT_VOICES: Non-state armed groups had no seat at the 1949 Diplomatic Conference and have none in contemporary status-determination proceedings; the fighters this reading classifies as unprivileged, and the civilian populations among whom they fight, would object to a protective scope drawn without them. The ICRC attends as a non-voting observer; human rights bodies issue findings entirely outside the process.
% DISAPPEARANCE_RATIONALE: Detention regimes, targeting doctrine, military-commission statutes, and prisoner-exchange diplomacy all presuppose the status gate; if it vanished overnight, every detaining and engaging state would have to rebuild its rules around either universal application or explicit ad hoc discretion, and the POW channel between peer forces would lose its legal currency.
% FOUNDING_PROBLEM: After the World Wars, states needed a stable regime that would protect soldiers who fought under recognized discipline when captured, and would prevent irregular resistance from dissolving the laws of war into reprisal cycles — hence a status test separating privileged from unprivileged combatants.
% FOUNDING_PROBLEM_CORROBORATION: The 1949 drafting records and the ICRC's Pictet Commentary — sources outside any single belligerent's interest — corroborate the founding problem as stated. That the problem remains live is attested mainly by the benefiting militaries themselves; UN human rights bodies, tribunal jurisprudence, and post-2001 litigation records from outside the beneficiary set attest that the problem as originally framed no longer describes the dominant conflict types, which is why the status is contested rather than live.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__state_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__state_centric_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.65: for anyone caught by the gate the deprivation is total — no combatant immunity, no POW registration, exposure to prosecution — but the affected class is bounded (persons who take up arms without meeting the markers) and residual civilian-protection pathways survive in parts of the corpus this reading discounts, so epsilon sits high but short of a pure-exclusion profile. Suppression 0.66 is structural, not internalized: the Article 4 markers are tactically unreachable for guerrilla methods (a fixed distinctive sign visible at distance is a targeting beacon against modern surveillance and standoff fire), so the alternative of qualifying out of the excluded class is not realistically available to the target class, while states retain full interpretive discretion. Theater_ratio 0.30: status-determination machinery performs genuine adjudication in some national systems and operates as post-hoc ratification of targeting and detention decisions in others; the authored value averages that split. Accessibility_collapse 0.55: for non-state fighters alternatives largely collapse once the gate is understood, but states retain real exits (voluntary extension of protections, protocol adoption, doctrinal change), keeping the figure below the mountain range. Resistance 0.60: sustained doctrinal opposition — Additional Protocol I's extension of combatant status to self-determination struggles, tribunal jurisprudence blurring the interstate/non-international line, human-rights-body findings, ICRC advocacy, and litigation over detention practices. Coalition capacity among the target class is limited by fragmentation: organized movements exist, but the class as a whole lacks the coordination to bargain over scope, which is why their power atom is moderate despite real organizational islands. Boltzmann coordination type is identity_coordination: the gate's primary function is boundary maintenance — deciding which armed actors count as members of the lawful-combatant class — and the identity-framing gaming alert applies squarely, since requiring fighters to look like soldiers in order to count is precisely the identity framing that can serve as cover for exclusion; coupling data should be watched for power-by-scope concentration. Measurements run on one shared grid (t=0 to t=75, six points, every tracked metric authored at every point): base_extractiveness climbs through the decolonization and proxy wars, jumps across the 2001-2009 detention-and-targeting intensification, then eases slightly as litigation and policy revision restored some process substance; suppression_requirement traces the build-out of determination boards, military commissions, and detention facilities, peaking before partial normalization; theater_ratio follows the same arc, peaking when wholesale designation displaced individualized determination.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary/administrator seat and the payer seats should compute as different constraint types from the same text. From the conventional-military seat the Article 4 gate is order-preserving infrastructure: it tells their soldiers who must be given quarter, guarantees their own personnel POW treatment against peer forces, and channels prosecution toward genuine violations. From the unprivileged-fighter seat the identical gate is the mechanism that strips immunity, removes capture incentives from the opposing side's engagement decisions, and converts captivity into indeterminate detention. Affected civilians straddle the two: they bear the blurred-distinction costs the gate produces while receiving whatever restraint the reciprocity function delivers in conventional war. The engine computes these per-seat classifications from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared structure drives the derivation without overrides: conventional_state_militaries are the sole declared beneficiary with arbitrage-grade exit (doctrinal rewrite, partner and proxy shifts, protocol refusal), placing them near the beneficiary pole; unprivileged_belligerents and detained_unprivileged_fighters are declared victims with trapped exit — qualification is tactically unavailable and leaving the class means ceasing to fight — placing them near the full-target pole; affected_civilian_populations carry a dual payer/beneficiary position and derive mid-range; the observer and excluded seats are analytical and neutral. Spatial scope is global, and verification of status criteria is hardest in asymmetric theaters, so the engine's scope amplification lands disproportionately on the target side. No directionality overrides were needed: the beneficiary/victim declarations plus exit options reproduce the true relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what blocks both mislabels. Reading the gate as pure coordination would erase the asymmetric exclusion that is this reading's defining feature — a class of fighters stripped of protections the same instrument grants their adversaries. Reading it as pure extraction would erase the genuine interstate function: the POW regime and reciprocity machinery operate only because a shared status test exists, and dismantling the gate would not leave that function intact. On mandatrophy: the founding problem — protecting disciplined state soldiers and preserving reciprocity in wars between uniformed armies — is contested rather than dead, because peer interstate war remains possible and the regime demonstrably functions there; but the arrangement now spends most of its operating life in asymmetric conflicts its drafters did not design for, where its practical effect inverts from protection-delivery to exclusion-enforcement. The founding_problem_status by disappearance_verdict mismatch consumer should watch this file: status 'contested' with verdict 'world_rearranges' flags the zombie risk without asserting it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the state_centric_reading of kernel geneva_conventions_protective_scope; would instantiating a sibling reading instead change the structural classification?',
    'Author and evaluate the sibling files separately: universal_rights_reading expands the protected set to all persons in armed conflict and raises epsilon sharply over the same referent arrangement; hybrid_proportionality_reading splits application by conflict type, lowering effective burden for non-international conflicts covered by Common Article 3 floors.',
    'Classification is reading-indexed: under the universal reading the same standing arrangement computes as near-pure exclusion with reciprocity as cover; under the hybrid reading part of the measured burden redistributes to the interstate tier. This file''s verdict holds only for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one of three live readings of the Geneva protective-scope kernel; sibling deltas routed here rather than averaged into epsilon.').

omega_variable(
    reciprocity_equilibrium_vs_sovereignty_carveout,
    'Is the Article 4 gate a genuine reciprocity equilibrium that protects all disciplined fighters, or a sovereignty carve-out that states would maintain even where no reciprocal treatment is on offer?',
    'Compare state handling of captured irregulars in conflicts where the adversary could offer no reciprocal prisoner-of-war treatment (counterinsurgencies, expeditionary operations) against handling in peer interstate wars; persistent gate-keeping absent reciprocity prospects indicates the carve-out dominates.',
    'If carve-out, the coordination component is thinner than the reading claims and excess burden rises; if genuine equilibrium, a larger share of the measured burden is the operating price of the reciprocity function itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_equilibrium_vs_sovereignty_carveout, empirical, 'Whether the gate''s coordination function is real reciprocity maintenance or post-hoc justification.').

omega_variable(
    status_determination_adjudication_vs_rubber_stamp,
    'Do formal Article 4 status determinations in recent asymmetric conflicts function as genuine adjudication or as post-hoc ratification of targeting and detention decisions already made?',
    'Audit determination records across detaining states: timing relative to the underlying detention or engagement decision, reversal rates, evidentiary standards actually applied, and whether adverse findings ever block an operation.',
    'A high rubber-stamp share pushes theater_ratio well above the authored 0.30 and supports drift hypotheses for the determination machinery toward inertial performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_determination_adjudication_vs_rubber_stamp, empirical, 'Functional-versus-performative share of the status-determination apparatus.').

omega_variable(
    gate_incentive_effect_direction,
    'Does withholding combatant status from irregular fighters increase or decrease armed groups'' incentives to adopt discriminating, command-accountable conduct?',
    'Comparative study of group behavior where status prospects varied: arrangements offering pathways toward recognized status versus categorical denial, controlling for conflict intensity and external sponsorship.',
    'If categorical denial degrades incentives, the gate''s instrumental justification fails and its burden on the excluded class loses its coordination defense; if it improves incentives, part of the burden is functional price rather than extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gate_incentive_effect_direction, empirical, 'Direction of the gate''s behavioral incentive effect on non-state armed groups.').

omega_variable(
    api_extension_customary_status_dispute,
    'Does Additional Protocol I Article 1(4)''s extension of combatant status to anti-colonial and self-determination struggles bind this reading, or does major-power non-ratification leave the Article 4 gate intact?',
    'Track ratification practice and customary-law findings (ICJ, tribunal jurisprudence) on the AP I extension; a customary-status finding narrows the reading''s excluded class without any textual amendment.',
    'If customary, the victim set is smaller than authored and measured burden falls; if not, the gate stands as authored and the reading''s drift trajectory is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(api_extension_customary_status_dispute, empirical, 'Customary versus treaty-bound reach of the AP I status extension against this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__state_centric_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(gene_tr_t0, observed).
narrative_ontology:measurement(gene_tr_t15, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement_basis(gene_tr_t15, observed).
narrative_ontology:measurement(gene_tr_t30, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement_basis(gene_tr_t30, observed).
narrative_ontology:measurement(gene_tr_t45, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 45, 0.27).
narrative_ontology:measurement_basis(gene_tr_t45, observed).
narrative_ontology:measurement(gene_tr_t60, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 60, 0.32).
narrative_ontology:measurement_basis(gene_tr_t60, observed).
narrative_ontology:measurement(gene_tr_t75, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 75, 0.3).
narrative_ontology:measurement_basis(gene_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(gene_be_t0, observed).
narrative_ontology:measurement(gene_be_t15, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement_basis(gene_be_t15, observed).
narrative_ontology:measurement(gene_be_t30, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 30, 0.53).
narrative_ontology:measurement_basis(gene_be_t30, observed).
narrative_ontology:measurement(gene_be_t45, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 45, 0.58).
narrative_ontology:measurement_basis(gene_be_t45, observed).
narrative_ontology:measurement(gene_be_t60, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 60, 0.66).
narrative_ontology:measurement_basis(gene_be_t60, observed).
narrative_ontology:measurement(gene_be_t75, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 75, 0.65).
narrative_ontology:measurement_basis(gene_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(gene_su_t0, observed).
narrative_ontology:measurement(gene_su_t15, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement_basis(gene_su_t15, observed).
narrative_ontology:measurement(gene_su_t30, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 30, 0.46).
narrative_ontology:measurement_basis(gene_su_t30, observed).
narrative_ontology:measurement(gene_su_t45, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 45, 0.55).
narrative_ontology:measurement_basis(gene_su_t45, observed).
narrative_ontology:measurement(gene_su_t60, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement_basis(gene_su_t60, observed).
narrative_ontology:measurement(gene_su_t75, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 75, 0.66).
narrative_ontology:measurement_basis(gene_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__state_centric_reading, identity_coordination).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope__universal_rights_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope__hybrid_proportionality_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Geneva protections' decomposes into three readings of one kernel, each a separate file with its own stable epsilon, per the epsilon-invariance principle. This file authors the state-centric reading only. Direction of influence runs from this reading outward: it is the entrenched founding reading, and the universal and hybrid readings emerged as responses to its rigidity, so its practice shapes their legitimacy conditions. The sibling files must carry reciprocal links and their own dual-formulation notes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
