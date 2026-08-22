% ============================================================================
% CONSTRAINT STORY: market_naturalization__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__lapsed_alternative_reading, []).

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
 *   constraint_id: market_naturalization__lapsed_alternative_reading
 *   human_readable: Market Dominance as Lapsed Closure (Inertial Persistence Reading)
 *   domain: political economy / economic history / institutional analysis
 *
 * SUMMARY:
 *   A market arrangement in which dominance persists after the closure that
 *   produced it has lapsed. The consolidation-era apparatus that eliminated
 *   alternative channels — exclusionary contracts, gatekeeping, deliberate
 *   elimination of duplicative infrastructure — was dismantled or allowed to
 *   decay generations ago; the standing structure persists because everyone
 *   operates within it, and the alternative path's components (supplier
 *   networks, distribution channels, workforce know-how) dissolved through
 *   disuse. No seat maintains the arrangement; no seat captures its costs;
 *   what it charges participants is a thin coordination margin on top of
 *   infrastructure operating cost. Constraint-family note (epsilon
 *   decomposition): the colloquial claim 'market dominance is natural' splits
 *   into three linked stories with distinct epsilon over the same historical
 *   referent — beneficiary_maintained_reading authors high epsilon (defended
 *   rents, identifiable beneficiary class), hybrid_reading intermediate
 *   epsilon, and this lapsed_alternative_reading epsilon at coordination-cost
 *   levels (0.28). This file instantiates only the lapsed reading; its
 *   epsilon is indexed to that reading's lights over the fixed referent, the
 *   standing arrangement under contest. KEY AGENTS (by structural
 *   relationship): - dominant_incumbents: administrator-inertia seat
 *   (institutional/constrained) — occupies and operates the structure without
 *   defending its closure - would_be_entrants: blocked entrants
 *   (moderate/mobile) — bear the atrophy as foregone entry -
 *   incumbent_workforce: skill-fused payers (moderate/identity_locked) —
 *   careers presuppose the standing structure - diffuse_consumers: diffuse
 *   payers (powerless/constrained) — habit-bound to the dominant channels -
 *   legacy_trade_associations: ceremonial layer (organized/trapped) —
 *   collects dues for ratification rituals - competition_regulators: dormant
 *   authority (institutional/analytical) — no conduct to prosecute -
 *   atrophied_alternative_descendants: excluded heirs of the vanished path
 *   (powerless/trapped) - institutional_economic_historians: analytical
 *   observer (analytical/analytical) — holds the record of the
 *   construction-maintenance-lapse arc
 *
 * KEY AGENTS:
 *   - dominant_incumbents: administrator-inertia seat (institutional/constrained) — occupies and operates the dominant infrastructure without defending the closure that produced it
 *   - would_be_entrants: blocked entrants (moderate/mobile) — bear the atrophied alternatives as foregone entry; exit is redirection to other markets
 *   - incumbent_workforce: skill-fused payers (moderate/identity_locked) — skills, certifications, and professional networks presuppose the standing structure
 *   - diffuse_consumers: diffuse payers (powerless/constrained) — pay a thin coordination margin; can switch among dominant providers but not to a different structure
 *   - legacy_trade_associations: ceremonial layer (organized/trapped) — collect dues for standards rituals that ratify what the infrastructure already does
 *   - competition_regulators: dormant authority (institutional/analytical) — hold restructuring power but find no conduct to prosecute
 *   - atrophied_alternative_descendants: excluded heirs of the vanished path (powerless/trapped) — bear the loss of the alternative as a foregone road, with no seat in the conversation
 *   - institutional_economic_historians: analytical observer (analytical/analytical) — document the full construction-maintenance-lapse arc
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__lapsed_alternative_reading, 0.28).
domain_priors:suppression_score(market_naturalization__lapsed_alternative_reading, 0.25).
domain_priors:theater_ratio(market_naturalization__lapsed_alternative_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__lapsed_alternative_reading, piton).
narrative_ontology:human_readable(market_naturalization__lapsed_alternative_reading, "Market Dominance as Lapsed Closure (Inertial Persistence Reading)").
narrative_ontology:topic_domain(market_naturalization__lapsed_alternative_reading, "political economy / economic history / institutional analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__lapsed_alternative_reading, '8bed68c5-cfa0-4619-9ba7-64a4b38574e2').
narrative_ontology:cs_kernel_codification('8bed68c5-cfa0-4619-9ba7-64a4b38574e2', distributed).
narrative_ontology:cs_authority_grounding('8bed68c5-cfa0-4619-9ba7-64a4b38574e2', diffuse_epistemic).
narrative_ontology:cs_reading_relation('8bed68c5-cfa0-4619-9ba7-64a4b38574e2', market_naturalization__beneficiary_maintained_reading, forecloses).
narrative_ontology:cs_reading_relation('8bed68c5-cfa0-4619-9ba7-64a4b38574e2', market_naturalization__hybrid_reading, influences).
narrative_ontology:cs_axiom('8bed68c5-cfa0-4619-9ba7-64a4b38574e2', foundational, dominance_persists_through_inertia).
narrative_ontology:cs_axiom_status(dominance_persists_through_inertia, holdable).
narrative_ontology:cs_axiom_grounding('8bed68c5-cfa0-4619-9ba7-64a4b38574e2', dominance_persists_through_inertia, empirically_contingent).
narrative_ontology:cs_axiom('8bed68c5-cfa0-4619-9ba7-64a4b38574e2', foundational, alternatives_atrophied_through_non_use).
narrative_ontology:cs_axiom_status(alternatives_atrophied_through_non_use, holdable).
narrative_ontology:cs_axiom_grounding('8bed68c5-cfa0-4619-9ba7-64a4b38574e2', alternatives_atrophied_through_non_use, empirically_contingent).
narrative_ontology:cs_reference_frame('8bed68c5-cfa0-4619-9ba7-64a4b38574e2', lapsed_constructed_closure).
narrative_ontology:cs_drift_state('8bed68c5-cfa0-4619-9ba7-64a4b38574e2', contemporary_industrial_organization_evidence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8bed68c5-cfa0-4619-9ba7-64a4b38574e2', '2026-08-20T12:00:00Z').
narrative_ontology:cs_kernel_id(market_naturalization__lapsed_alternative_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__lapsed_alternative_reading, dominant_incumbents).
narrative_ontology:constraint_beneficiary(market_naturalization__lapsed_alternative_reading, legacy_trade_associations).
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, would_be_entrants).
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, incumbent_workforce).
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, diffuse_consumers).
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, atrophied_alternative_descendants).
narrative_ontology:constraint_vindicates(market_naturalization__lapsed_alternative_reading, path_dependence_doctrine).
narrative_ontology:constraint_vindicates(market_naturalization__lapsed_alternative_reading, institutional_inertia_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Occupy and operate the market's dominant infrastructure — exchanges, platforms, distribution channels — inherited from a consolidation era whose defensive apparatus has been dismantled. They collect operating margins on flow volume that track the cost of running the infrastructure rather than supra-competitive rents, and their conduct shows no systematic defensive response to marginal entry. They could fund the rebuilding of alternative channels but would bear the disruption of restructuring their own operations for gains they do not need. Exit would mean divesting sunk infrastructure at a loss.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, dominant_incumbents, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(market_naturalization__lapsed_alternative_reading, dominant_incumbents, beneficiary).

% Hold capital and business models suited to market paths that no longer exist — the supplier networks, distribution channels, and trained labor pools that once supported alternative structures have dissolved through disuse. Entry attempts stall not against opposition but against absence: there is nothing to plug into. They can redirect their capital to other markets and industries, and many do; the cost they bear is the foregone opportunity in this one.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, would_be_entrants, payer,
    moderate, biographical, mobile, global).

% Careers and skills are built inside the dominant firms' ecosystems — certifications, internal ladders, and professional networks all presuppose the standing structure. Wages flow from the incumbents; the alternative path an earlier cohort might have taken — cooperative plants, regional exchanges — no longer exists to be hired into. Leaving would mean retraining from scratch in a different trade, which few do.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, incumbent_workforce, payer,
    moderate, biographical, identity_locked, national).

% Buy through the dominant channels out of habit and infrastructure: payment rails, storefronts, and expectations all assume the standing arrangement. They pay prices that embed a thin coordination margin and bear whatever friction the dominant system produces. Switching among the dominant providers is possible; switching to a different market structure is not, because none is operating.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, diffuse_consumers, payer,
    powerless, biographical, constrained, national).

% Collect member dues to run standards processes, annual conventions, and interoperability committees that were decisive a century ago and now ratify what the infrastructure already does. Their publications describe completed mandates. Their existence is bound to the standing structure — if it dissolved, the dues and the convening role would dissolve with it.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, legacy_trade_associations, beneficiary,
    organized, generational, trapped, national).

% Hold statutory authority to investigate and restructure concentrated markets but find no conduct to prosecute: the closure is not being enforced by anyone, so case-making targets are absent. Their dockets fill with episodic complaints that resolve into settlements; the standing structure itself is outside their remedial reach without a new mandate and budget.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, competition_regulators, observer,
    institutional, generational, analytical, national).

% Inherit the memory of cooperative suppliers, regional exchanges, and municipal distribution networks that once operated alongside the dominant structure and dissolved through disuse — the charters, know-how, and supplier relationships are gone beyond living practice. They bear the loss as a foregone path and would argue the structure's persistence forecloses rebuilding, but no seat in the market's governance, standards processes, or regulatory dockets speaks for them.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, atrophied_alternative_descendants, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(market_naturalization__lapsed_alternative_reading, atrophied_alternative_descendants, excluded).

% Document the consolidation era in which the closure was constructed and enforced, and the later decades in which its defensive apparatus was dismantled or allowed to decay. They observe the full arc — construction, maintenance, lapse — from outside the market's operations, and their archives are the main record that the standing arrangement ever required maintenance at all.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, institutional_economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__lapsed_alternative_reading, diffuse).
narrative_ontology:fixing_cost_class(market_naturalization__lapsed_alternative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The standing structure coordinates trade at scale: common standards, deep liquid markets, and shared infrastructure let participants transact without renegotiating market structure. The coordination is real and continues, though the work that built it ended generations ago.
% TRANSFER_FUNCTION: Moves transaction flow through the dominant infrastructure, carrying thin operating margins from all market participants to the position-holding firms as the price of infrastructure use; moves dues from member firms to the trade associations for the ceremonial layer; moves nothing else systematically — the arrangement no longer transfers rents to anyone.
% ABSENT_VOICES: The descendants of the atrophied alternatives — former cooperative suppliers, regional exchanges, municipal networks — and would-be entrants whose models presuppose the dissolved infrastructure. They are outside the conversation entirely: no seat in the market's governance, standards processes, or regulatory dockets speaks for the vanished path or for the cost of its irrecoverability.
% DISAPPEARANCE_RATIONALE: If the standing structure dissolved overnight, every seat would scramble: the incumbents' infrastructure and margins, the workforce's skill ecosystem, consumers' habitual channels, and the associations' convening role all presuppose it. Rebuilding any market structure — dominant or plural — would take decades, because the alternative path's components no longer exist to be revived.
% FOUNDING_PROBLEM: Fragmented, illiquid, duplicative markets: regional price dispersion, incompatible standards, and high transaction costs from redundant infrastructure. The closure was built — through consolidation, standardization, and the deliberate elimination of alternative channels — to solve that.
% FOUNDING_PROBLEM_CORROBORATION: Institutional economic histories of the standardization and consolidation eras document the original fragmentation problem and its resolution as complete; the trade associations' own archived charters describe their standardization mandates as fulfilled — notably, even the beneficiary-side record attests the lapse; no incumbent firm's regulatory filings claim a live fragmentation problem. No corroborating source outside the historical record asserts the founding problem is still live.
narrative_ontology:disappearance_verdict(market_naturalization__lapsed_alternative_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__lapsed_alternative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__lapsed_alternative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_naturalization__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__lapsed_alternative_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__lapsed_alternative_reading_tests).
:- end_tests(market_naturalization__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.28 — this reading's coordination-costs-only claim: participants pay a thin margin over infrastructure operating cost, decayed from the maintained era's 0.55 as defensive rents eroded with the enforcement apparatus. All three series run on one shared grid (T0–T90) so every metric is authored at every examined time point. Suppression 0.25 decomposes as near-zero active coercion plus the structural absence of alternatives; the suppression_requirement series (0.62 → 0.25) is this story's central dynamic — enforcement decay, the lapse itself — and the end-state scalar matches it while carrying the atrophy-absence component, an ambiguity routed to suppression_mechanism_decomposition. Theater 0.20: the associations' standards rituals and conventions neither maintain anything nor charge for anything — they ratify what the infrastructure already does; the ratio rose as functional activity atrophied faster than ceremonial activity. Accessibility_collapse 0.75: alternatives are historically collapsed but contingently so — rebuildable at cost — hence below the 0.85+ characteristic of logical or natural collapse. Resistance 0.20: inertia presents no target; antitrust sentiment and entrant grievance find no conduct to resist. Claimed type piton is authored from the structure — persists by inertia, no seat captures the costs, the administrator could change the arrangement but fixing costs more than any seat's share of the friction — independently of these metric values. A payer coalition (entrants, descendants, consumers) is the theoretical exit, but the atrophy consumed the collective-action infrastructure itself: there is no vehicle through which the diffuse payers could finance rebuilding — authored descriptively, not as a suppression claim.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the incumbents' position the arrangement is the unremarkable background of operations — near-beneficiary directionality without any experienced regime to administer. Entrants and the descendants experience the same structure as a wall: their directionality sits near the full-target end, entrants mobile (they can leave for other markets), descendants trapped outside it entirely. The workforce is identity-fused — skills and professional networks presuppose the structure — so its exit is locked regardless of its moderate power. Regulators experience a non-case: no conduct, no target, no docket. Same structure, different constraint per seat; the engine computes this divergence from the structural data, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: dominant_incumbents (positional residue — they hold the structure's commanding positions and collect operating margins; this reading's claim is that those margins track infrastructure cost, not defended rents) and legacy_trade_associations (dues for the ceremonial layer). Victim declarations: would_be_entrants (foregone entry), incumbent_workforce (skill fusion), diffuse_consumers (habit-bound channels), atrophied_alternative_descendants (the vanished path). The derivation should place incumbents and associations near the beneficiary end, and entrants, workforce, consumers, and descendants toward the target end, with identity_locked (workforce) and trapped (descendants) exits amplifying toward full target. Gain_flow is authored 'diffuse' affirmatively, after checking every seat: the incumbents receive the coordination function's price (infrastructure compensation), not the closure's extraction; the extraction proper is dissipated friction with no recipient. Fixing_cost 'prohibitive': rebuilding the atrophied path costs more than any seat's share of the residual friction — incumbents would disrupt their own operations, regulators lack the mandate, and no payer could finance it. That cell (prohibitive + diffuse) is the inertial-persistence signature this reading asserts.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification work is double-edged. Read as rope, the arrangement's genuine coordination function (standardized, liquid markets) would mask the closure — but the function is vestigial: the work that built it ended generations ago, and the associations' ratification rituals add nothing. Read as snare, 'market dominance' would summon an extractor — but this reading authors no capturer: the enforcement requirement decayed (0.62 → 0.25), no seat receives the extraction, and the incumbents' margins track cost. The piton claim holds both mislabels apart: real coordination, atrophied; real costs, unowned. The R5 interview sharpens it: the founding problem (fragmentation, illiquidity) is dead — corroborated by economic history and by the beneficiaries' own completion records — while the world would rearrange if the arrangement vanished. Dead founding problem + load-bearing persistence + no capturer + prohibitive fixing is the signature of an arrangement sitting below maintenance entirely: not even theatrically defended, just persisting.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the market_naturalization kernel — the lapsed_alternative_reading. The sibling readings instantiate different constraints: beneficiary_maintained_reading holds market dominance is actively defended by incumbent capital holders (identifiable beneficiary class, high epsilon); hybrid_reading holds lapsed elements and active maintenance combine (intermediate epsilon). Where is the disagreement located, and what would adopting a sibling change?',
    'The disagreement is located on a single structural element: the maintenance status of the closure — whether its persistence is produced by ongoing incumbent defense or by inertia after the defense apparatus decayed. Reading assignment is resolved by the conduct evidence summarized in active_maintenance_detection; the committer framing (which readings are live, how they partition the phenomenon) is resolved by how the corpus classifies the three sibling files against the same historical record.',
    'If the maintained reading is right, this story''s epsilon is understated by a wide margin, a concentrated beneficiary class exists, and the classification moves from inertial persistence to extraction-with-enforcement; if the hybrid is right, epsilon sits between this story''s value and the maintained reading''s, and the beneficiary structure is partial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: this story is the lapsed reading of the market_naturalization kernel; sibling readings would change the beneficiary structure and epsilon.').

omega_variable(
    active_maintenance_detection,
    'Does incumbent conduct in fact contain systematic defensive maintenance of the closure — the maintained reading''s core claim — or only episodic, non-systematic responses consistent with the lapse account?',
    'Longitudinal conduct analysis of incumbent responses to entry episodes across the interval: systematic exclusionary patterns (exclusive dealing, predatory pricing, above-baseline acquisition of nascent rivals) would overturn the lapse account; their absence, against a matched-industry baseline, would confirm it.',
    'Detection of systematic defense reclassifies the arrangement''s persistence as actively produced: a concentrated beneficiary appears, epsilon rises toward defended-rent levels, and the type moves toward extraction-with-enforcement. Non-detection confirms the inertial-persistence account and this story''s metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_maintenance_detection, empirical, 'The pivotal factual question this reading hangs on: is the closure maintained or lapsed?').

omega_variable(
    suppression_mechanism_decomposition,
    'The suppression scalar blends near-zero active coercion with the structural absence of alternatives. Is the residual suppression a real independent component (the atrophied alternatives'' absence binds agents beyond what accessibility_collapse already registers), or a measurement artifact double-counted across the two metrics?',
    'Decomposition test: ask whether agents who fully understand the arrangement can identify any workable alternative path at non-prohibitive cost. If yes, the absence component is overcounted and suppression should sit near the coercion-only floor; if no, the atrophy itself is a binding wall.',
    'If artifact, suppression drops toward 0.1 and the arrangement reads as even more purely inertial; if real, the closure is more constraining than the coercion-free picture suggests and the wall-experience of entrants and descendants is structurally load-bearing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_decomposition, conceptual, 'Whether the residual suppression scalar measures structural absence or double-counts accessibility_collapse.').

omega_variable(
    alternative_rebuildability,
    'Could the atrophied alternatives actually be rebuilt, and at what cost relative to the residual friction they would relieve? The cost-asymmetry claim — fixing costs more than any seat''s share of the friction — hangs on this.',
    'Engineering-economic assessment of reconstructing alternative channels (supplier networks, distribution, workforce pipelines), priced against the measured coordination margin the dominant structure charges participants.',
    'If rebuilding is cheap, the closure is not binding and the arrangement is better read as transient neglect over a live coordination function; if prohibitive, the cost-asymmetry holds and the inertial-persistence account is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_rebuildability, empirical, 'Whether the atrophied alternative path is recoverable at a cost anyone would rationally bear.').

omega_variable(
    scale_economy_vs_sediment,
    'Is the market''s concentration a scale-economy equilibrium that would re-form even if dissolved (a structural feature of the industry''s cost curve), or historical sediment — a contingent arrangement persisting only because it exists? This is the kernel''s naturalization question in miniature.',
    'Natural experiments: entry waves after deregulation, technology shocks that lowered minimum efficient scale — did concentration re-form in the old shape, or did the market restructure?',
    'If concentration is a cost-curve equilibrium, the arrangement is closer to a structural feature no party could change and the inertial-persistence framing misleads (nothing is being maintained or neglected); if sediment, the arrangement is contingent history and this reading''s account holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scale_economy_vs_sediment, empirical, 'Whether the dominance structure is a natural equilibrium or contingent historical sediment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__lapsed_alternative_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__lapsed_alternative_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(mark_tr_t0, observed).
narrative_ontology:measurement(mark_tr_t15, market_naturalization__lapsed_alternative_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement_basis(mark_tr_t15, observed).
narrative_ontology:measurement(mark_tr_t30, market_naturalization__lapsed_alternative_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement_basis(mark_tr_t30, observed).
narrative_ontology:measurement(mark_tr_t45, market_naturalization__lapsed_alternative_reading, theater_ratio, 45, 0.14).
narrative_ontology:measurement_basis(mark_tr_t45, observed).
narrative_ontology:measurement(mark_tr_t60, market_naturalization__lapsed_alternative_reading, theater_ratio, 60, 0.16).
narrative_ontology:measurement_basis(mark_tr_t60, observed).
narrative_ontology:measurement(mark_tr_t75, market_naturalization__lapsed_alternative_reading, theater_ratio, 75, 0.18).
narrative_ontology:measurement_basis(mark_tr_t75, observed).
narrative_ontology:measurement(mark_tr_t90, market_naturalization__lapsed_alternative_reading, theater_ratio, 90, 0.2).
narrative_ontology:measurement_basis(mark_tr_t90, observed).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__lapsed_alternative_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(mark_be_t0, observed).
narrative_ontology:measurement(mark_be_t15, market_naturalization__lapsed_alternative_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement_basis(mark_be_t15, observed).
narrative_ontology:measurement(mark_be_t30, market_naturalization__lapsed_alternative_reading, base_extractiveness, 30, 0.44).
narrative_ontology:measurement_basis(mark_be_t30, observed).
narrative_ontology:measurement(mark_be_t45, market_naturalization__lapsed_alternative_reading, base_extractiveness, 45, 0.38).
narrative_ontology:measurement_basis(mark_be_t45, observed).
narrative_ontology:measurement(mark_be_t60, market_naturalization__lapsed_alternative_reading, base_extractiveness, 60, 0.33).
narrative_ontology:measurement_basis(mark_be_t60, observed).
narrative_ontology:measurement(mark_be_t75, market_naturalization__lapsed_alternative_reading, base_extractiveness, 75, 0.3).
narrative_ontology:measurement_basis(mark_be_t75, observed).
narrative_ontology:measurement(mark_be_t90, market_naturalization__lapsed_alternative_reading, base_extractiveness, 90, 0.28).
narrative_ontology:measurement_basis(mark_be_t90, observed).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_naturalization__lapsed_alternative_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(mark_su_t0, observed).
narrative_ontology:measurement(mark_su_t15, market_naturalization__lapsed_alternative_reading, suppression_requirement, 15, 0.56).
narrative_ontology:measurement_basis(mark_su_t15, observed).
narrative_ontology:measurement(mark_su_t30, market_naturalization__lapsed_alternative_reading, suppression_requirement, 30, 0.49).
narrative_ontology:measurement_basis(mark_su_t30, observed).
narrative_ontology:measurement(mark_su_t45, market_naturalization__lapsed_alternative_reading, suppression_requirement, 45, 0.42).
narrative_ontology:measurement_basis(mark_su_t45, observed).
narrative_ontology:measurement(mark_su_t60, market_naturalization__lapsed_alternative_reading, suppression_requirement, 60, 0.35).
narrative_ontology:measurement_basis(mark_su_t60, observed).
narrative_ontology:measurement(mark_su_t75, market_naturalization__lapsed_alternative_reading, suppression_requirement, 75, 0.29).
narrative_ontology:measurement_basis(mark_su_t75, observed).
narrative_ontology:measurement(mark_su_t90, market_naturalization__lapsed_alternative_reading, suppression_requirement, 90, 0.25).
narrative_ontology:measurement_basis(mark_su_t90, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__lapsed_alternative_reading, resource_allocation).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial naturalization claim — market dominance is natural or inevitable — decomposes into three readings of one kernel with distinct epsilon and beneficiary structure over the same historical referent: beneficiary_maintained_reading (high epsilon; identifiable beneficiary class — incumbent capital actively defending the closure), hybrid_reading (intermediate epsilon; lapse in some sectors, active maintenance in others), and this lapsed_alternative_reading (epsilon at coordination-cost levels, 0.28; no identifiable beneficiary class; alternatives atrophied through non-use). This file instantiates only the lapsed reading; the contest is carried in the omega variables and the sibling files. The three are linked as a constraint family because each cites the same historical record — the consolidation-era construction of the closure — as evidence, and each reading's plausibility conditions the others'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
