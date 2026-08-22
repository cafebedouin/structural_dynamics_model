% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__contraction_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__contraction_reading
 *   human_readable: Cognitive Unavailability of Honor Settlement (Contraction Reading)
 *   domain: historical sociology/legal history/cultural anthropology
 *
 * SUMMARY:
 *   This story instantiates the contraction_reading of the
 *   honor_settlement_legitimacy kernel: the claim that dueling's death was a
 *   cognitive event. Between the mid-nineteenth and mid-twentieth centuries
 *   the cultural framework that constituted honor settlement as a live
 *   obligation — the code duello, the economy of challenge and satisfaction,
 *   the gentleman's status stakes — dissolved, and with it the very
 *   conceivability of dueling as legitimate action. Legal prohibition had
 *   existed for centuries and failed; what ended the practice was the exit of
 *   the framework that made it thinkable. The standing arrangement under
 *   contest is therefore the post-transformation normative regime itself:
 *   disputes route to law, contract, and administrative channels; the state's
 *   violence monopoly is uncontested by private settlement; the honor
 *   vocabulary survives only as historical costume. Constraint family note
 *   (epsilon-invariance): the sibling readings instantiate different
 *   constraints with different referents and epsilon values — the
 *   drop_reading's standing arrangement includes a live fringe honor economy
 *   (extant adherents, higher epsilon), and the composite_reading distributes
 *   causation across legal, military, and economic mechanisms (a lower
 *   framework-transformation share). This file's epsilon is authored only for
 *   the contraction reading's referent and is not comparable to, or
 *   averageable with, the siblings'.
 *
 * KEY AGENTS:
 *   - - state_violence_monopoly: agenda-setter and beneficiary (institutional/arbitrage) — defines the normative space through law and schooling; collects the uncontested violence monopoly.
 *   - - commercial_professional_classes: primary beneficiary (organized/mobile) — reputation secured by credit and contract rather than challengeability.
 *   - - legal_professions: secondary beneficiary (organized/mobile) — inherited the dispute-resolution demand the honor channel vacated.
 *   - - residual_honor_adherents: payer (powerless/identity_locked) — hold settlement frameworks the mainstream renders unintelligible; exit means identity death.
 *   - - legally_marginalized_disputants: payer (powerless/trapped) — lost the one accessible redress channel without gaining an affordable formal one.
 *   - - historical_sociologists: analytical observer (analytical/analytical) — reconstruct the transformation and hold the competing kernel readings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__contraction_reading, 0.08).
domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, 0.1).
domain_priors:theater_ratio(honor_settlement_legitimacy__contraction_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__contraction_reading, rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__contraction_reading, "Cognitive Unavailability of Honor Settlement (Contraction Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__contraction_reading, "historical sociology/legal history/cultural anthropology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__contraction_reading, 'b1c29b42-e7de-419b-9e9a-bfcd3bfb87b6').
narrative_ontology:cs_kernel_codification('b1c29b42-e7de-419b-9e9a-bfcd3bfb87b6', distributed).
narrative_ontology:cs_authority_grounding('b1c29b42-e7de-419b-9e9a-bfcd3bfb87b6', distributed).
narrative_ontology:cs_reading_relation('b1c29b42-e7de-419b-9e9a-bfcd3bfb87b6', honor_settlement_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_reading_relation('b1c29b42-e7de-419b-9e9a-bfcd3bfb87b6', honor_settlement_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('b1c29b42-e7de-419b-9e9a-bfcd3bfb87b6', foundational, cognitive_unavailability_constitutes_decline).
narrative_ontology:cs_axiom_status(cognitive_unavailability_constitutes_decline, holdable).
narrative_ontology:cs_axiom_grounding('b1c29b42-e7de-419b-9e9a-bfcd3bfb87b6', cognitive_unavailability_constitutes_decline, empirically_contingent).
narrative_ontology:cs_axiom('b1c29b42-e7de-419b-9e9a-bfcd3bfb87b6', secondary, prohibition_without_framework_change_insufficient).
narrative_ontology:cs_axiom_status(prohibition_without_framework_change_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('b1c29b42-e7de-419b-9e9a-bfcd3bfb87b6', prohibition_without_framework_change_insufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('b1c29b42-e7de-419b-9e9a-bfcd3bfb87b6', framework_indexed_legitimacy).
narrative_ontology:cs_drift_state('b1c29b42-e7de-419b-9e9a-bfcd3bfb87b6', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b1c29b42-e7de-419b-9e9a-bfcd3bfb87b6', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, state_violence_monopoly).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, commercial_professional_classes).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, legal_professions).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__contraction_reading, residual_honor_adherents).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__contraction_reading, legally_marginalized_disputants).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__contraction_reading, state_violence_monopoly_doctrine).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__contraction_reading, juridified_dispute_resolution_norm).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__contraction_reading, civilizing_process_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines the normative space the arrangement operates in: through law, schooling, and the monopoly on legitimate violence, it establishes which dispute-settlement channels exist at all. It prosecuted duelists during the transformation and now maintains the dead-letter statutes; it collects the settlement's principal benefit — a violence monopoly that no honor challenge contests and that requires no enforcement to defend.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, state_violence_monopoly, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__contraction_reading, state_violence_monopoly, beneficiary).

% Status and creditworthiness rest on contract, credential, and credit reference rather than on challengeability. Their reputations cannot be violently impugned, and they bear none of the arrangement's costs; from their seat the transformation was narrated as moral progress.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, commercial_professional_classes, beneficiary,
    organized, biographical, mobile, continental).

% Inherited the dispute-resolution demand the honor channel vacated: courts, contract enforcement, professional mediation. Their market expanded as the framework exited; they hold no enforcement role in the cognitive constraint and collect through ordinary professional demand.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, legal_professions, beneficiary,
    organized, biographical, mobile, national).

% Descendants of the honor economy — some military and student subcultures, diaspora and street communities with live respect codes — still hold settlement frameworks the mainstream renders unintelligible rather than merely illegal. Their settlements carry criminal liability and social illegibility; leaving the framework would mean abandoning the identity it constitutes, so most do not leave, and the practice survives only where mainstream attention does not reach.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, residual_honor_adherents, payer,
    powerless, biographical, identity_locked, regional).

% Wronged parties without affordable access to courts. The honor framework once offered even the low-status a rough, accessible — if brutal and unequal — redress channel; its foreclosure left them the formal system's costs without its protection. Most grievances in this seat simply go unaddressed.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, legally_marginalized_disputants, payer,
    powerless, immediate, trapped, national).

% Reconstruct the transformation from court-martial records, challenge correspondence, and the atrophied honor vocabulary; they hold the competing readings of the kernel and can see the full structure — including that the arrangement's stability is an achievement, not a law.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_settlement_legitimacy__contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_settlement_legitimacy__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement coordinates dispute settlement society-wide: once the honor framework exited, all parties share the expectation that grievances route to law, contract, and administrative channels rather than personal violence, and the state's violence monopoly operates without contest from private settlement.
% TRANSFER_FUNCTION: During the transformation it moved status capital and dispute-resolution demand: from the honor classes, whose status system was expropriated, to the state, which consolidated the violence monopoly, and to the legal professions, which inherited the settlement market. In the standing arrangement it moves almost nothing — its operation is foreclosure: the informal challenge channel is withheld from those without affordable legal redress, and most of what they lose is simply unmet rather than transferred.
% ABSENT_VOICES: The residual honor adherents had no seat in the transformation's adjudication — the process was narrated entirely by its beneficiaries (bourgeois reformers, state prosecutors, military modernizers), and the adherents' frameworks were rendered unintelligible rather than argued down. The legally marginalized, for whom the honor channel had been the one accessible redress, were equally absent: no party at the table represented the cost the foreclosure would impose on them. Historiography reconstructs both positions from the outside.
% DISAPPEARANCE_RATIONALE: Overnight, little: no infrastructure, vocabulary, or disposition remains to enact a challenge. Over a generation, substantially: the cognitive foreclosure is the load-bearing element of the violence-monopoly settlement. With it lifted, honor frameworks would regrow wherever status competition is intense and legal redress weak — the conditions under which the honor economy originally formed — and the state's monopoly would again be contested by private settlement. The beneficiary seats' arrangements (juridified dispute resolution, credit-based reputation) all depend on the foreclosure holding.
% FOUNDING_PROBLEM: The honor economy was a coordination trap: in a status system where declining a challenge meant social and professional ruin, every gentleman faced a recurring obligation to risk his life over words, private violence contested public justice, and the classes below the honor line had no redress at all. The framework transformation dissolved the trap by dissolving the framework.
% FOUNDING_PROBLEM_CORROBORATION: Professional historiography corroborates both the problem and its death from outside the beneficiary set: Kiernan's and Frevert's accounts rest on court-martial records, challenge correspondence, and prosecution files showing the compulsion was real; the atrophied honor vocabulary — no mainstream actor can state what 'satisfaction' required — corroborates the death. The residual adherent communities dissent, attesting their settlement frameworks remain live; the contraction reading classifies that dissent as testimony from outside the mainstream possibility space rather than as counterexample.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__contraction_reading, 0.08, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__contraction_reading_tests).
:- end_tests(honor_settlement_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claim and the metrics are authored independently. The claimed type is rope: the standing arrangement solves a genuine collective-action problem (exit from the honor economy's escalation trap) with negligible extraction (0.08), negligible coercive overhead (0.10), and no active enforcement — its signature is precisely that it requires none, unlike the prohibitions that failed for three centuries. The metric profile is nonetheless mountain-shaped: accessibility_collapse 0.88 (the honor framework cannot be revived by choice — it required an entire social infrastructure: seconds, surgeons, codes, a status economy), resistance 0.05 (no movement seeks restoration; the failed revival attempts under regime power are the strongest evidence the framework cannot be rebuilt), and self-sustaining operation. Whether a constructed constraint can stably occupy mountain-shaped operating space without being a mountain is the measurement this story exists to take; emerges_naturally is authored false because the reading's own account is historicist — this state was produced by cultural framework transformation, not natural law. The suppression_requirement series is the story's core dynamic and follows the enforcement-decay pattern: active suppressive force (courts-martial, prosecutions, social compulsion) fell from 0.72 to 0.10 as socialization replaced coercion — the transformation's defining substitution. The theater series rises from 0.10 to 0.32 but stays sub-threshold: the statutory apparatus is increasingly dead letter, but the functional activity (socialization reproducing the framework) dominates the performative. The epsilon series falls as the contest resolves: mid-transformation the arrangement extracted from both sides (gentlemen compelled to duel, adherents compelled to abandon identity); the settled arrangement extracts almost nothing. All three series share one time grid. Suppression is authored as a raw structural property and is not scaled by scope or directionality; only extractiveness is engine-scaled. Receipt surface: gain_flow is authored 'diffuse' after checking every seat — the extraction itself (foreclosure costs, destroyed status capital) accrues to no seat; the beneficiary seats collect benefits of the arrangement's operation, not receipts of what it takes. fixing_cost is 'prohibitive': removing the constraint would require reconstructing the honor framework society-wide, and the failed revivals are direct evidence of that cost.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from one structure. From the state, commercial, and legal seats the arrangement is a settled civilizational achievement — coordination with near-zero extraction, plausibly computing rope or mountain-like. From the residual adherent seat the same structure operates as expropriation: a framework that constituted their identity was rendered unintelligible rather than argued down, with identity_locked exit — from that seat the arrangement computes as extraction with no exit. From the marginalized-disputant seat it operates as a foreclosed justice option: no transfer is taken, but the trapped exit atom amplifies effective extraction on the seat that bears the foreclosure. The engine computes this divergence from the structural data; the rope claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: the state (agenda-setter and beneficiary — it defines the space and collects the monopoly), the commercial professional classes, and the legal professions are subsidized by the constraint's operation; effective extraction is damped or inverted for them. Victim declarations map to high directionality: residual honor adherents bear identity expropriation and, being identity_locked, sit near the full-target end — the arrangement's cost concentrates on exactly the seat that cannot leave. Legally marginalized disputants are trapped and bear the foreclosure cost; they pay no transfer, but amplification for trapped targets correctly registers that the standing costs fall on the seat with the fewest exits. No directionality overrides are authored: the derivation from beneficiary/victim declarations plus exit atoms captures every seat's relationship, including the state's dual position. Spatial scopes are modest (national/continental), so scope amplification is mild.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is dead and the arrangement persists — the (dead × world_rearranges) mismatch flag is expected and is here the signature of a settled-success equilibrium, not a zombie: the founding problem is dead because the arrangement's operation holds it dead, theater is sub-threshold (0.32), nothing is degraded, and gain_flow is diffuse (no seat captures the extraction). The mandatrophy danger in this domain runs the opposite direction: naturalizing a contingent achievement — reading 'civilized people simply do not duel' as a law of human nature rather than as a constructed, self-maintaining settlement. The contraction reading's own historicism is the corrective, and emerges_naturally: false encodes it. A mandatrophy-resolved component does exist inside the family: the anti-dueling statutes are dead letters whose suppressive mandate was fulfilled — piton-like vestige — but they are not the constraint; the cognitive constraint's function is fully alive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    possibility_space_boundary,
    'Where is the boundary of the ''normative possibility space'' whose contraction this reading claims — mainstream society or society-wide? Honor-violence pockets (diaspora honor codes, street respect codes, residual military and student subcultures) persist as live settlement frameworks.',
    'Survey and ethnographic operationalization: can mainstream actors articulate honor settlement as a live legitimate option, and do pocket frameworks recruit beyond their enclaves? If the space is society-wide, the drop_reading''s persistence claim stands.',
    'If the space is mainstream-only, the contraction holds and the pockets are cognitively alien residue; if society-wide, this reading''s epsilon and victim structure are wrong and the drop_reading file becomes the operative constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(possibility_space_boundary, conceptual, 'Boundary ambiguity of the contracted possibility space — the located disagreement with the drop_reading.').

omega_variable(
    mechanism_attribution,
    'Was the framework transformation causally sufficient for dueling''s death, or did legal prohibition and military discipline do decisive independent work (the composite_reading''s overdetermination claim)?',
    'Comparative natural experiment: jurisdictions with lax enforcement (France, tolerant into the twentieth century) versus strict (the German army after 1918). If dueling died at comparable rates regardless of enforcement intensity, the cognitive mechanism dominates.',
    'If enforcement was decisive, this reading overstates the framework transformation; causal attribution shifts toward the composite account and the composite file''s weighting becomes operative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_attribution, empirical, 'Causal sufficiency of framework transformation versus parallel mechanisms — the located disagreement with the composite_reading.').

omega_variable(
    naturalization_stability,
    'Is the contraction self-sustaining — a constructed constraint operating with natural-law stability — or does it depend on continued socialization reproduction that could fail under status-competition shocks such as state collapse or legal breakdown?',
    'Post-state-collapse natural experiments: did honor-settlement frameworks regrow where the state''s violence monopoly failed (e.g., the Balkans in the 1990s)? The failed Nazi-era revival of dueling (Mensur rehabilitation, Wehrmacht honor courts) is the strongest existing test — reconstruction could not restore the framework even under regime power.',
    'If regrowth occurs under state collapse, the constraint is state-capacity-dependent and its mountain-like stability is contingent; if not, the naturalized-rope reading holds and accessibility_collapse is genuinely near-total.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naturalization_stability, empirical, 'Whether the cognitive constraint is self-sustaining or reproduction-dependent.').

omega_variable(
    foreclosure_cost_distribution,
    'How much of the standing arrangement''s cost falls on disputants without affordable legal redress, and does the honor channel''s foreclosure measurably worsen their access to justice?',
    'Comparative access-to-justice data in communities where informal challenge norms historically operated; grievance-outcome studies for low-income disputants.',
    'If substantial, the arrangement''s epsilon is understated and the marginalized-disputant seat computes as a genuine extraction target, pressuring the classification toward tangled_rope; if negligible, the rope classification holds cleanly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_cost_distribution, empirical, 'Whether the standing arrangement''s foreclosure costs concentrate on the legally marginalized.').

omega_variable(
    reading_instantiation_scope,
    'This file instantiates only the contraction_reading of the honor_settlement_legitimacy kernel — what structurally changes if a sibling reading is instantiated instead?',
    'The sibling files (honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__composite_reading) carry their own epsilon, beneficiaries, and victim sets; classification of this file must not average across readings.',
    'The drop_reading adds a live residual-adherent constituency (raising epsilon and persistence); the composite_reading distributes causation across mechanisms (lowering the framework-transformation share). Epsilon and type are stable only within this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_instantiation_scope, conceptual, 'Committer-frame scope: one reading, one constraint, one epsilon (DP-001).').

omega_variable(
    grid_t0_reconstruction,
    'The coercion grid''s 1850 endpoints are level-resolved historiographical reconstructions, not measurements — how much confidence do the level differentials (individual coercion high, structural foreclosure incomplete) deserve?',
    'Cross-check against court-martial records, prosecution rates, and duel-correspondence corpora for the individual and organizational levels; the structural-level values are interpretive and may never be directly resolvable.',
    'If individual-level coercion in 1850 was lower than authored (social compulsion doing the work rather than state force), the enforcement-decay trajectory flattens and the transformation reads as attrition-plus-socialization rather than coercion-replaced-by-socialization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(grid_t0_reconstruction, empirical, 'Reconstruction uncertainty in the grid''s t0 level differentials.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__contraction_reading, 1850, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1850, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1850, 0.1).
narrative_ontology:measurement(hono_tr_t1880, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1880, 0.14).
narrative_ontology:measurement(hono_tr_t1910, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1910, 0.2).
narrative_ontology:measurement(hono_tr_t1940, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1940, 0.26).
narrative_ontology:measurement(hono_tr_t1970, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(hono_tr_t2000, honor_settlement_legitimacy__contraction_reading, theater_ratio, 2000, 0.32).
narrative_ontology:measurement(hono_tr_t2020, honor_settlement_legitimacy__contraction_reading, theater_ratio, 2020, 0.32).

% Extraction over time
narrative_ontology:measurement(hono_be_t1850, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1850, 0.4).
narrative_ontology:measurement(hono_be_t1880, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1880, 0.33).
narrative_ontology:measurement(hono_be_t1910, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1910, 0.24).
narrative_ontology:measurement(hono_be_t1940, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1940, 0.16).
narrative_ontology:measurement(hono_be_t1970, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1970, 0.11).
narrative_ontology:measurement(hono_be_t2000, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 2000, 0.08).
narrative_ontology:measurement(hono_be_t2020, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 2020, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1850, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1850, 0.72).
narrative_ontology:measurement(hono_su_t1880, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1880, 0.58).
narrative_ontology:measurement(hono_su_t1910, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1910, 0.42).
narrative_ontology:measurement(hono_su_t1940, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1940, 0.28).
narrative_ontology:measurement(hono_su_t1970, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1970, 0.16).
narrative_ontology:measurement(hono_su_t2000, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement(hono_su_t2020, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 2020, 0.1).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1850, tn=2020
narrative_ontology:measurement(hono_grid_01, honor_settlement_legitimacy__contraction_reading, accessibility_collapse(class), 1850, 0.55).
narrative_ontology:measurement(hono_grid_02, honor_settlement_legitimacy__contraction_reading, accessibility_collapse(class), 2020, 0.88).
narrative_ontology:measurement(hono_grid_03, honor_settlement_legitimacy__contraction_reading, accessibility_collapse(individual), 1850, 0.45).
narrative_ontology:measurement(hono_grid_04, honor_settlement_legitimacy__contraction_reading, accessibility_collapse(individual), 2020, 0.9).
narrative_ontology:measurement(hono_grid_05, honor_settlement_legitimacy__contraction_reading, accessibility_collapse(organizational), 1850, 0.3).
narrative_ontology:measurement(hono_grid_06, honor_settlement_legitimacy__contraction_reading, accessibility_collapse(organizational), 2020, 0.85).
narrative_ontology:measurement(hono_grid_07, honor_settlement_legitimacy__contraction_reading, accessibility_collapse(structural), 1850, 0.25).
narrative_ontology:measurement(hono_grid_08, honor_settlement_legitimacy__contraction_reading, accessibility_collapse(structural), 2020, 0.85).
narrative_ontology:measurement(hono_grid_09, honor_settlement_legitimacy__contraction_reading, resistance(class), 1850, 0.45).
narrative_ontology:measurement(hono_grid_10, honor_settlement_legitimacy__contraction_reading, resistance(class), 2020, 0.02).
narrative_ontology:measurement(hono_grid_11, honor_settlement_legitimacy__contraction_reading, resistance(individual), 1850, 0.25).
narrative_ontology:measurement(hono_grid_12, honor_settlement_legitimacy__contraction_reading, resistance(individual), 2020, 0.03).
narrative_ontology:measurement(hono_grid_13, honor_settlement_legitimacy__contraction_reading, resistance(organizational), 1850, 0.3).
narrative_ontology:measurement(hono_grid_14, honor_settlement_legitimacy__contraction_reading, resistance(organizational), 2020, 0.02).
narrative_ontology:measurement(hono_grid_15, honor_settlement_legitimacy__contraction_reading, resistance(structural), 1850, 0.5).
narrative_ontology:measurement(hono_grid_16, honor_settlement_legitimacy__contraction_reading, resistance(structural), 2020, 0.03).
narrative_ontology:measurement(hono_grid_17, honor_settlement_legitimacy__contraction_reading, stakes_inflation(class), 1850, 0.6).
narrative_ontology:measurement(hono_grid_18, honor_settlement_legitimacy__contraction_reading, stakes_inflation(class), 2020, 0.7).
narrative_ontology:measurement(hono_grid_19, honor_settlement_legitimacy__contraction_reading, stakes_inflation(individual), 1850, 0.75).
narrative_ontology:measurement(hono_grid_20, honor_settlement_legitimacy__contraction_reading, stakes_inflation(individual), 2020, 0.65).
narrative_ontology:measurement(hono_grid_21, honor_settlement_legitimacy__contraction_reading, stakes_inflation(organizational), 1850, 0.55).
narrative_ontology:measurement(hono_grid_22, honor_settlement_legitimacy__contraction_reading, stakes_inflation(organizational), 2020, 0.75).
narrative_ontology:measurement(hono_grid_23, honor_settlement_legitimacy__contraction_reading, stakes_inflation(structural), 1850, 0.5).
narrative_ontology:measurement(hono_grid_24, honor_settlement_legitimacy__contraction_reading, stakes_inflation(structural), 2020, 0.6).
narrative_ontology:measurement(hono_grid_25, honor_settlement_legitimacy__contraction_reading, suppression(class), 1850, 0.55).
narrative_ontology:measurement(hono_grid_26, honor_settlement_legitimacy__contraction_reading, suppression(class), 2020, 0.05).
narrative_ontology:measurement(hono_grid_27, honor_settlement_legitimacy__contraction_reading, suppression(individual), 1850, 0.6).
narrative_ontology:measurement(hono_grid_28, honor_settlement_legitimacy__contraction_reading, suppression(individual), 2020, 0.05).
narrative_ontology:measurement(hono_grid_29, honor_settlement_legitimacy__contraction_reading, suppression(organizational), 1850, 0.65).
narrative_ontology:measurement(hono_grid_30, honor_settlement_legitimacy__contraction_reading, suppression(organizational), 2020, 0.05).
narrative_ontology:measurement(hono_grid_31, honor_settlement_legitimacy__contraction_reading, suppression(structural), 1850, 0.6).
narrative_ontology:measurement(hono_grid_32, honor_settlement_legitimacy__contraction_reading, suppression(structural), 2020, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy__composite_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, state_violence_monopoly_settlement).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, juridified_dispute_resolution_regime).

% DUAL FORMULATION NOTE:
% The colloquial label 'the end of dueling' decomposes into a constraint family under the honor_settlement_legitimacy kernel. This file instantiates the contraction_reading (framework exit — cognitive unavailability; epsilon 0.08; referent: the post-transformation normative regime). The sibling files instantiate the drop_reading (fringe persistence — a live residual honor economy with extant adherents; higher epsilon, different victim set) and the composite_reading (overdetermined decline — causation distributed across legal, military, and economic mechanisms, with the framework transformation one edge among several). The epsilon values differ because the referents differ: each reading fixes a different standing arrangement under contest. Structural relations: the composite account is the broader historiographical frame within which the contraction claim is isolated; the drop reading contests the contraction's endpoint rather than its mechanism. All three family members link via network.affects_constraints; this file additionally underpins the modern violence-monopoly settlement and the juridified dispute-resolution regime.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
