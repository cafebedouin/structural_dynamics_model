% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__hybrid_legitimation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__hybrid_legitimation_reading, []).

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
 *   constraint_id: imposition_mechanism_kernel__hybrid_legitimation_reading
 *   human_readable: Hybrid Legitimation Channel: Imperial Exemplar Plus Institutional Incentives (Constantinian-Theodosian Arc)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This story instantiates the hybrid_legitimation_reading of the
 *   imposition_mechanism_kernel: the claim that new norms in an imperial
 *   formation achieved regime-wide legitimacy through symbolic authority
 *   transfer from the emperor's person, propagated by a graded ladder of
 *   institutional incentives - neither by prior mass acceptance (the
 *   endogenous_climb_reading's claim) nor by monopoly violence (the
 *   exogenous_override_reading's claim). The concrete arc is the
 *   Constantinian-Theodosian Christianization of the Roman Empire (312-392):
 *   the emperor's visible adoption fixed the target practice; access to
 *   office, patronage, and episcopal honor recruited the elite stratum first;
 *   municipal curiae and lineage heads mediated adoption downward; mass
 *   adherence followed by status emulation, with legal penalties arriving
 *   only at the endgame. Per the epsilon-invariance principle, the colloquial
 *   question 'how did the new norm gain legitimacy?' is three structurally
 *   distinct constraints with different epsilon, victim sets, and enforcement
 *   signatures; this file authors only the hybrid reading and links its
 *   siblings through network.affects_constraints. The epsilon referent is the
 *   standing arrangement under contest - the exemplar-plus-incentive channel
 *   itself - assessed by this reading's own lights; the reading is
 *   descriptive and endorses no alternative arrangement.
 *
 * KEY AGENTS:
 *   - imperial_court: Agenda-setter and primary beneficiary (institutional/arbitrage) - anchors norms in the emperor's example and administers the incentive ladder
 *   - scholar_official_elite: Dual-positioned early adopter (organized/identity_locked) - converts conformity into career capital; bears first-mover adaptation costs
 *   - local_lineage_elders: Intermediary beneficiary (moderate/constrained) - exemplifies and enforces the norm locally, collects authority, absorbs enforcement friction
 *   - commoner_households: Late-adopting payers (powerless/trapped) - bear adaptation costs with no seat in adoption decisions
 *   - household_women: Payer and excluded voice (powerless/trapped) - absorb the domestic costs of re-tooled practice; absent from every decision layer
 *   - rival_norm_entrepreneurs: Delegitimized payers (moderate/constrained) - unanchored normative channels lose standing by design
 *   - comparative_historians: Analytical observer (analytical/analytical) - reconstruct the mechanism from the record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.72).
domain_priors:suppression_score(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.62).
domain_priors:theater_ratio(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__hybrid_legitimation_reading, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__hybrid_legitimation_reading, "Hybrid Legitimation Channel: Imperial Exemplar Plus Institutional Incentives (Constantinian-Theodosian Arc)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__hybrid_legitimation_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__hybrid_legitimation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__hybrid_legitimation_reading, '1848bf9e-f1f2-4aaf-beaa-22f1794078dd').
narrative_ontology:cs_kernel_codification('1848bf9e-f1f2-4aaf-beaa-22f1794078dd', distributed).
narrative_ontology:cs_authority_grounding('1848bf9e-f1f2-4aaf-beaa-22f1794078dd', expertise).
narrative_ontology:cs_interpretation_layer_present('1848bf9e-f1f2-4aaf-beaa-22f1794078dd').
narrative_ontology:cs_reading_relation('1848bf9e-f1f2-4aaf-beaa-22f1794078dd', imposition_mechanism_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('1848bf9e-f1f2-4aaf-beaa-22f1794078dd', imposition_mechanism_kernel__exogenous_override_reading, influences).
narrative_ontology:cs_axiom('1848bf9e-f1f2-4aaf-beaa-22f1794078dd', foundational, legitimacy_requires_imperial_exemplification).
narrative_ontology:cs_axiom_status(legitimacy_requires_imperial_exemplification, holdable).
narrative_ontology:cs_axiom_grounding('1848bf9e-f1f2-4aaf-beaa-22f1794078dd', legitimacy_requires_imperial_exemplification, empirically_contingent).
narrative_ontology:cs_axiom('1848bf9e-f1f2-4aaf-beaa-22f1794078dd', foundational, institutional_incentives_mediate_stratified_adoption).
narrative_ontology:cs_axiom_status(institutional_incentives_mediate_stratified_adoption, holdable).
narrative_ontology:cs_axiom_grounding('1848bf9e-f1f2-4aaf-beaa-22f1794078dd', institutional_incentives_mediate_stratified_adoption, empirically_contingent).
narrative_ontology:cs_axiom('1848bf9e-f1f2-4aaf-beaa-22f1794078dd', secondary, deferred_coercion_is_not_legitimacy_source).
narrative_ontology:cs_axiom_status(deferred_coercion_is_not_legitimacy_source, holdable).
narrative_ontology:cs_axiom_grounding('1848bf9e-f1f2-4aaf-beaa-22f1794078dd', deferred_coercion_is_not_legitimacy_source, empirically_contingent).
narrative_ontology:cs_reference_frame('1848bf9e-f1f2-4aaf-beaa-22f1794078dd', exemplar_incentive_equilibrium).
narrative_ontology:cs_drift_state('1848bf9e-f1f2-4aaf-beaa-22f1794078dd', post_coercion_turn_historiography, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('1848bf9e-f1f2-4aaf-beaa-22f1794078dd', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_court).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, scholar_official_elite).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, local_lineage_elders).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, commoner_households).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, household_women).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, rival_norm_entrepreneurs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, scholar_official_elite).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, local_lineage_elders).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_exemplar_doctrine).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__hybrid_legitimation_reading, incentive_gradient_integration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets which proposed norms enter the channel by adopting them visibly in the emperor's person and court ceremony, then funds and staffs the incentive ladder - offices, honors, exemptions, patronage - that rewards demonstrable alignment. Collects the channel's product: consolidated discretion over the empire's normative direction and a cheap substitute for pervasive enforcement. Can re-anchor any practice at will; nothing about the arrangement binds the throne.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_court, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_court, beneficiary).

% Competes for office, honor, and patronage by mastering and displaying the exemplar-aligned practice; converts conformity into career capital and distributes access downward as patrons. Bears the first-mover costs of changing ritual, dress, association, and doctrine, and the later costs of disciplining clients and kin who lag. Leaving the alignment economy would forfeit livelihood and standing together; careers, marriages, and self-conception are built inside it.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, scholar_official_elite, beneficiary,
    organized, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__hybrid_legitimation_reading, scholar_official_elite, payer).

% Mediate the norm's last mile: exemplify it locally, enroll households, adjudicate the disputes that changed practice generates, and answer upward for local conformity. Collect authority and mediation standing from the role; absorb the friction - resentment, evasion, occasional defiance - that enforcement at the village scale produces. Bound to place; cannot relocate the obligations that come with the role.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, local_lineage_elders, beneficiary,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__hybrid_legitimation_reading, local_lineage_elders, payer).

% Adopt last and on notice: proclamations, gentry instruction, and neighborly pressure reach them after terms are fixed elsewhere. Re-tool ritual, dress, diet, and cultivation at household expense, on schedules they did not set, under the eye of local notables who answer to the center. Migration is possible in principle but costly and does not outrun the arrangement, which follows the state's writ.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, commoner_households, payer,
    powerless, generational, trapped, regional).

% Absorb much of the domestic cost of changed practice - re-made garments, altered rites, revised marriage and inheritance expectations - while appearing in the decision record only as objects of regulation. No assembly, petition, or patronage route carries their objections to any decision layer; their stake is registered by others or not at all.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, household_women, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__hybrid_legitimation_reading, household_women, excluded).

% Lead practices and movements that did not originate at court - urban teaching circles, rural cults, philosophical schools outside the patronage web. The channel's gatekeeping strips unanchored proposals of standing regardless of merit: recruitment, venues, and legal toleration all price palace-alignment. Their options are seeking anchoring on the center's terms, retreating to tolerated margins, or absorbing the delegitimization.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, rival_norm_entrepreneurs, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__hybrid_legitimation_reading, rival_norm_entrepreneurs, excluded).

% Reconstruct the channel from the record - dated imperial acts, adoption chronologies, prosopography, legal sequences - and compare it across imperial formations. Hold no stake in the arrangement's survival or demise; their exit is the archive itself.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, comparative_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_court).
narrative_ontology:fixing_cost_class(imposition_mechanism_kernel__hybrid_legitimation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieves regime-wide normative convergence across a vast, administratively thin empire: one visible exemplar fixes the target practice, a graded ladder of offices, honors, and patronage recruits each stratum to imitate the stratum above it, and local notables mediate the last mile - so adoption propagates by status emulation instead of universal persuasion or pervasive policing.
% TRANSFER_FUNCTION: Moves normative authority downward from the imperial person through elite and municipal intermediaries to households, while moving conformity, loyalty signaling, and the material costs of changed practice upward: elites remit demonstrable alignment and distribute career access as patronage; commoner households remit obedience and re-tooled ritual, dress, and cultivation; the court receives consolidated discretion over which proposed norms may enter the channel at all.
% ABSENT_VOICES: Commoner households, and especially the women who absorbed the domestic costs of re-tooled practice, had no seat anywhere in the decision chain: terms were set by emperor and court, negotiated by elite councils and synods, implemented by municipal curiae and lineage heads, and announced by proclamation. Rival norm-entrepreneurs - leaders of unanchored urban movements, rural cultic networks, philosophical circles outside the patronage web - would object that the channel's gatekeeping stripped every non-palace route of legitimacy; they appear in the record chiefly as objects of legislation, not as participants.
% DISAPPEARANCE_RATIONALE: If the exemplar-plus-incentive channel vanished overnight, regime-wide normative change would stall or fragment: no shared exemplar to fix the target practice, no gradient to recruit elites, no intermediary layer to carry adoption to households. Regions would drift culturally apart, the court would lose its cheapest integration instrument and face a choice between expensive coercion and pluralism it could not steer, and the career economy built on demonstrable alignment would collapse with the alignments it priced.
% FOUNDING_PROBLEM: An empire assembled from conquered peoples, cults, and customary laws, governed by an administration too thin to police practice district-by-district, needed its dispersed populations to converge on common norms without garrisons in every village.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set by contemporaries who described the mechanism while opposing its products - Symmachus's relatio pleading for the altar of Victory against a patronage gradient that had already flipped elite allegiance, and Julian's polemics against a Christianity carried by court connection. Modern historiography corroborates the hybrid structure from the analytical seat (Drake on Constantine's consensus politics, Brown on the impoverished intermediary clergy who delivered the last mile, MacMullen on the coercion endgame). No extra-beneficiary source attests a purely voluntary or purely coercive account; both sibling readings fail the same outside-corroboration test.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__hybrid_legitimation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__hybrid_legitimation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__hybrid_legitimation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.72 at interval end) is substantial but bounded: the channel taxes normative autonomy (only palace-anchored proposals could enter), imposes asymmetric adaptation costs (early on elites, durably on households), and by the 380s attaches career and legal consequences to non-conformity; it stops short of snare-grade extraction because the coordination delivered is real - convergence across a continental empire with enforcement that stayed delegated and episodic for seven decades. Suppression (0.62) tracks the enforcement ratchet: privilege-granting in the 310s-350s, exclusionary legislation in the 360s-370s, penalty-backed prohibition from 380-392; even at peak, enforcement ran through bishops, curiae, and lineage heads rather than a standing coercive apparatus. Suppression is authored as a raw structural property; only extractiveness is scaled downstream by directionality and scope. Theater (0.45) rises with careerist conversion - ceremonial exemplification was load-bearing signaling early and increasingly performative as adoption became an entry ticket. Accessibility collapse (0.45): unanchored advocacy and naked coercion remain conceivable strategies but are demonstrably inferior inside the channel's logic, so alternatives degrade without vanishing - ascetic and rural circuits persisted for centuries. Resistance (0.42): aristocratic petition (Symmachus 384), schismatic refusal (Donatists), rural cultic persistence, and the Eugenius usurpation's pagan backing. The measurement series share one six-point grid (312/330/348/366/380/392) so no metric is sampled against another's end-state; trajectories are monotonic - a ratchet, not a cycle - and no intermittent-reinforcement dynamic is claimed. identity_coordination is declared because the channel's dominant function is membership signaling - adoption gates careers and marks loyal membership - and the coupling is genuine rather than cover: adoption demonstrably tracked patronage access, and the aligned-versus-suspect boundary was maintained against evolving criteria.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute differently. From the throne, the channel is the empire's cheapest steering instrument: one visible choice replaces a million enforcement actions, and discretion over which norms enter is consolidated sovereignty. From commoner households and household women, the same structure is a gated pipeline: practices are re-tooled at household expense on schedules set far away, with discipline delegated to local notables who answer upward, not downward. The elite seat experiences both faces at once - conformity purchases career capital while identity fuses with imperial alignment such that exit would cost the self, not just the post (professional-institutional fusion: the office-holder's worth becomes indistinguishable from demonstrated harmony with the exemplar; if that frame broke - a legitimate rival court, a discredited dynasty - the adoption calculus would collapse overnight). Same nominal stratum, different constraint-experience: senatorial magnates could slow-walk adoption behind wealth and tradition, while curial office-holders were trapped by liturgical obligations into early compliance - equal rank, unequal exit. Coalition potential among the powerless was thin by design: households were dispersed and unlettered, women's costs were privatized inside domestic space, and the gatekeeping fragmented rival entrepreneurs before they could federate - which is part of why suppression requirements stayed moderate for so long. Inter-institutionally, the emerging ecclesiastical hierarchy became the channel's enforcement intermediary, gaining jurisdiction as it delivered compliance: the court experienced the church as instrument; the church experienced the court as patron; both drew on the same conformity.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to the real structure. imperial_court is declared beneficiary and holds the agenda_setter role: it collects the channel's product - consolidated norm-setting discretion - so its derived d sits at the beneficiary extreme; no override is needed because the beneficiary declaration plus arbitrage-grade exit already places it there. scholar_official_elite and local_lineage_elders are declared beneficiaries with payer secondary roles: net winners whose conformity costs and enforcement friction temper but do not invert their position. commoner_households and household_women are declared victims with trapped exits: near-full targets whose effective extraction is amplified by the continental verification problem (a dispersed populace cannot audit the center, and the center audits them through notables). rival_norm_entrepreneurs are declared victims whose specific loss is channel access - their extraction is the delegitimization itself. comparative_historians sit at the analytical seat with no stake. Directionality overrides are unnecessary: every seat's d follows from its declaration plus exit atom, and the one tempting correction (raising the elders' d for enforcement friction) is already captured by the secondary payer role. On the receipt surface: receipt is distinguished from benefit - elites and elders hold beneficiary roles, but the channel's product, consolidated norm-setting discretion, demonstrably accrues to the throne alone, hence gain_flow names imperial_court; fixing_cost is prohibitive because abandoning the channel mid-consolidation would forfeit the court's cheapest integration instrument amid frontier war and succession crisis, and no alternative legitimacy infrastructure existed at comparable cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against two mislabels. Read as pure rope, the channel's gatekeeping disappears: the court's monopoly on norm-entry, the privatized costs borne by households and women, and the deliberate fragmentation of rival channels are extraction, not overhead. Read as pure snare, the coordination achievement disappears: seventy years of convergence with delegated, episodic enforcement is a real collective-action solution, and the reading's own structural delta (moderate enforcement costs, stratified adoption) is incompatible with coercion-as-cover. On mandatrophy proper: the founding problem (integration without pervasive coercion) was live throughout the interval and the mechanism was not maintained past its function - the late enforcement ratchet is rent-and-control accumulation on a working machine, not theatrical upkeep of a dead one; theater_ratio peaks at 0.45, below the atrophy band. The genealogy interview records the founding problem as contested rather than dead: historians dispute whether integration was ever the operative problem versus sincere transformation, and whether the mechanism solved integration or relocated conflict into schism and repression. The obsolescence risk runs forward, not backward: the mechanism's success created the conditions (saturated incentives, an empowered intermediary church) under which coercion became cheap enough to substitute for exemplification - which is precisely the residue the exogenous_override_reading mistakes for the whole story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the hybrid_legitimation_reading of imposition_mechanism_kernel; what would adopting a sibling reading change structurally?',
    'Adjudication by the historiographical community against the primary record: adoption chronologies against dated imperial acts, prosopography of converts against patronage flows, legal sequence against enforcement archaeology.',
    'Under endogenous_climb_reading, epsilon falls and the victim set contracts (voluntary adoption attributes less to the channel); under exogenous_override_reading, suppression dominates and the arrangement trends toward snare. This file''s epsilon, beneficiaries, and victims are valid only within this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: reading-of-kernel routing and sibling structural deltas.').

omega_variable(
    extraction_attribution_boundary,
    'How much of the measured extraction belongs to the exemplar-plus-incentive channel itself, versus to the particular norm being diffused and its own demands?',
    'Compare diffusion episodes within the same imperial repertoire that varied the norm while holding the mechanism constant (dress codes, ritual reforms, doctrinal settlements); isolate channel-intrinsic costs (gatekeeping delay, conformity signaling, delegated discipline) from norm-intrinsic costs.',
    'If channel-intrinsic extraction is low, epsilon drops toward rope territory and the tangled_rope claim weakens; if high, the channel is extractive across whatever norms pass through it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_attribution_boundary, empirical, 'Whether the channel or the diffused norm carries the measured extraction.').

omega_variable(
    counterfactual_enforcement_floor,
    'What enforcement intensity would the same normative change have required without imperial exemplification - is the mechanism''s moderate enforcement intrinsic efficiency or merely deferred coercion?',
    'Compare against genuinely coercive mandates of comparable scope and against mandate-less diffusion failures; model the enforcement curve pure decree would have needed to reach the same adoption depth by 392.',
    'Calibrates the coordination credit the channel deserves; if the counterfactual enforcement floor approaches the realized curve, the hybrid account collapses toward override with better manners.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_enforcement_floor, empirical, 'Whether moderate enforcement is intrinsic efficiency or deferred coercion.').

omega_variable(
    emulation_vs_conviction_mix,
    'What share of elite adoption was conviction rather than career-signaling, and does the mix shift across the interval?',
    'Prosopographic analysis of converts'' behavior under regime uncertainty (usurpations, the Julian interlude 361-363): reversions, hedged patronage, and post-392 piety depth separate signaling from commitment.',
    'A high signaling share raises theater_ratio and supports the extraction reading of elite seats; a conviction-heavy mix lowers theater and strengthens the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emulation_vs_conviction_mix, empirical, 'Conviction versus career-signaling composition of elite adoption.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the kernel best framed as a property of the imperial state (a governance instrument with an agenda-setter) or of the receiving communities (a reception process with no single setter)?',
    'Test whether the dispute''s own evidence - dated imperial acts preceding stratified adoption - survives a reception-centered reframing; if adoption timing decouples from imperial acts in enough cases, the state-side frame loses its anchor.',
    'Under a reception-centered framing the agenda_setter seat dissolves, the court''s beneficiary declaration loses its object, and the classification trends toward rope; the declared framing was chosen because the readings'' dispute over causal primacy presupposes an instrument whose primacy is in question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'CS-framing under-determination: state-instrument versus reception-process framing of the kernel.').

omega_variable(
    mechanism_generality,
    'Is the exemplar-plus-incentive signature a real recurring structure of imperial norm change, or a retrospective narrative fitted to one well-documented case?',
    'Cross-case comparison for the same signature - exemplar-first adoption, incentive-mediated stratification, deferred coercion - in Han China''s canon establishment, Meiji Japan''s Westernization, and Ottoman reform edicts.',
    'If the signature recurs, the constraint generalizes beyond the Constantinian arc and the corpus gains a reusable pattern; if not, this story dissolves into case-specific arrangements and the kernel fragments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_generality, empirical, 'Recurrence of the hybrid signature across imperial formations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__hybrid_legitimation_reading, 312, 392).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t312, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 312, 0.18).
narrative_ontology:measurement_basis(impo_tr_t312, observed).
narrative_ontology:measurement(impo_tr_t330, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 330, 0.22).
narrative_ontology:measurement_basis(impo_tr_t330, observed).
narrative_ontology:measurement(impo_tr_t348, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 348, 0.27).
narrative_ontology:measurement_basis(impo_tr_t348, observed).
narrative_ontology:measurement(impo_tr_t366, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 366, 0.33).
narrative_ontology:measurement_basis(impo_tr_t366, observed).
narrative_ontology:measurement(impo_tr_t380, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 380, 0.4).
narrative_ontology:measurement_basis(impo_tr_t380, observed).
narrative_ontology:measurement(impo_tr_t392, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 392, 0.45).
narrative_ontology:measurement_basis(impo_tr_t392, observed).

% Extraction over time
narrative_ontology:measurement(impo_be_t312, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 312, 0.48).
narrative_ontology:measurement_basis(impo_be_t312, observed).
narrative_ontology:measurement(impo_be_t330, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 330, 0.53).
narrative_ontology:measurement_basis(impo_be_t330, observed).
narrative_ontology:measurement(impo_be_t348, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 348, 0.58).
narrative_ontology:measurement_basis(impo_be_t348, observed).
narrative_ontology:measurement(impo_be_t366, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 366, 0.63).
narrative_ontology:measurement_basis(impo_be_t366, observed).
narrative_ontology:measurement(impo_be_t380, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 380, 0.68).
narrative_ontology:measurement_basis(impo_be_t380, observed).
narrative_ontology:measurement(impo_be_t392, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 392, 0.72).
narrative_ontology:measurement_basis(impo_be_t392, observed).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t312, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 312, 0.18).
narrative_ontology:measurement_basis(impo_su_t312, observed).
narrative_ontology:measurement(impo_su_t330, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 330, 0.22).
narrative_ontology:measurement_basis(impo_su_t330, observed).
narrative_ontology:measurement(impo_su_t348, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 348, 0.28).
narrative_ontology:measurement_basis(impo_su_t348, observed).
narrative_ontology:measurement(impo_su_t366, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 366, 0.35).
narrative_ontology:measurement_basis(impo_su_t366, observed).
narrative_ontology:measurement(impo_su_t380, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 380, 0.48).
narrative_ontology:measurement_basis(impo_su_t380, observed).
narrative_ontology:measurement(impo_su_t392, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 392, 0.62).
narrative_ontology:measurement_basis(impo_su_t392, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__hybrid_legitimation_reading, identity_coordination).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, exogenous_override_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'how did the new norm gain legitimacy?' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints: endogenous_climb_reading (acceptance precedes mandate; low epsilon, victims largely absent), this hybrid_legitimation_reading (exemplar plus incentives; moderate epsilon, stratified victim set, deferred coercion), and exogenous_override_reading (violence grounds legitimacy; high suppression, broad victim set). The hybrid reading sits mid-chain: it treats climb dynamics as the pre-anchoring substrate and override dynamics as the post-380 endgame, and each sibling's story should cite this file as the boundary condition it must respect. All three files link one another through affects_constraints; no member of the family is an orphan.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
