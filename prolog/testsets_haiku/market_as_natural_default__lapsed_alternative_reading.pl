% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__lapsed_alternative_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: market_as_natural_default__lapsed_alternative_reading
 *   human_readable: Market as Natural Default (Lapsed Alternative Reading)
 *   domain: political_economy/ideology_studies
 *
 * SUMMARY:
 *   This reading instantiates one interpretation of the contested kernel
 *   'market_as_natural_default': markets achieve hegemonic status not because
 *   beneficiaries actively defend their dominance, but because institutional
 *   knowledge of historical alternatives atrophied and was never recovered
 *   into living memory. Economic coordination through price mechanisms worked
 *   well enough to displace previous systems; but the knowledge of how those
 *   systems functioned, their internal design principles, and their adaptive
 *   capacity was preserved only in specialist historical archives, not in
 *   mainstream economic pedagogy or policy imagination. The constraint
 *   persists because alternatives are literally unthinkable — not because
 *   they are actively suppressed, but because the civilization forgot they
 *   existed. This reading authorizes low extractiveness (ε ≈ 0.08) because no
 *   identifiable beneficiary class profits from amnesia itself; if
 *   beneficiaries were active in maintaining amnesia, the constraint would
 *   cross into the beneficiary_maintained_reading (ε ≈ 0.55) or
 *   hybrid_amnesia_reading (ε ≈ 0.38). This reading posits amnesia as a D3
 *   artifact — a structural feature of how knowledge is transmitted (and
 *   failed to be transmitted) across institutional boundaries and historical
 *   periods.
 *
 * KEY AGENTS:
 *   - Economic historians (Braudel, Mokyr, Ostrom, Sen) — preserve alternative institutional knowledge in specialist literature, outside mainstream pedagogy
 *   - Mainstream economics textbooks and pedagogy — the transmission mechanism that naturalizes markets by omission of alternatives
 *   - Non-market distribution systems (communes, guilds, cooperatives, mutual aid networks) — the actual historical alternatives, now mostly defunct or marginalized
 *   - Analytical observer — the reading itself, which reconstructs the amnesia narrative from historical record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__lapsed_alternative_reading, 0.08).
domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, 0.12).
domain_priors:theater_ratio(market_as_natural_default__lapsed_alternative_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__lapsed_alternative_reading, mountain).
narrative_ontology:human_readable(market_as_natural_default__lapsed_alternative_reading, "Market as Natural Default (Lapsed Alternative Reading)").
narrative_ontology:topic_domain(market_as_natural_default__lapsed_alternative_reading, "political_economy/ideology_studies").

domain_priors:emerges_naturally(market_as_natural_default__lapsed_alternative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__lapsed_alternative_reading, '698dddf0-0b37-444c-b3b0-0b3c3cd72000').
narrative_ontology:cs_kernel_codification('698dddf0-0b37-444c-b3b0-0b3c3cd72000', implicit).
narrative_ontology:cs_authority_grounding('698dddf0-0b37-444c-b3b0-0b3c3cd72000', distributed).
narrative_ontology:cs_reading_relation('698dddf0-0b37-444c-b3b0-0b3c3cd72000', market_as_natural_default__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('698dddf0-0b37-444c-b3b0-0b3c3cd72000', market_as_natural_default__hybrid_amnesia_reading, coexists_with).
narrative_ontology:cs_axiom('698dddf0-0b37-444c-b3b0-0b3c3cd72000', foundational, amnesia_unintentional_institutional).
narrative_ontology:cs_axiom_status(amnesia_unintentional_institutional, holdable).
narrative_ontology:cs_axiom_grounding('698dddf0-0b37-444c-b3b0-0b3c3cd72000', amnesia_unintentional_institutional, conventional).
narrative_ontology:cs_axiom('698dddf0-0b37-444c-b3b0-0b3c3cd72000', secondary, alternatives_functionally_viable).
narrative_ontology:cs_axiom_status(alternatives_functionally_viable, holdable).
narrative_ontology:cs_axiom_grounding('698dddf0-0b37-444c-b3b0-0b3c3cd72000', alternatives_functionally_viable, empirically_contingent).
narrative_ontology:cs_reference_frame('698dddf0-0b37-444c-b3b0-0b3c3cd72000', market_dominance_as_contingent_historical_outcome).
narrative_ontology:cs_drift_state('698dddf0-0b37-444c-b3b0-0b3c3cd72000', contemporary_neoclassical_hegemony, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('698dddf0-0b37-444c-b3b0-0b3c3cd72000', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Stakeholders authored EMPTY (Pattern-5: an explicit assertion that no
% entity's arrangements depend on this constraint — paired with the
% world_unchanged verdict below, enforced by the schema).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Eliminates friction from decentralized bargaining by establishing price mechanism and voluntary exchange as the default institutional form for coordinating resource distribution. Solves the problem of how strangers allocate goods without kinship obligation or direct command.
% TRANSFER_FUNCTION: No systematic transfer: the reading posits that market mechanisms persist not because anyone collects from their operation, but because historical alternatives faded from cultural memory and contemporary imagination. Goods and labor move through price signals rather than through identifiable beneficiary capture.
% ABSENT_VOICES: Historical actors who designed, maintained, and transmitted knowledge of non-market distribution mechanisms (guild stewards, commons administrators, socialist economists, cooperative pioneers, mutual-aid organizers). Their intellectual lineages are preserved only in specialist historical archives, not in mainstream economic pedagogy; their absence from contemporary policy discourse is structural, not rhetorical.
% DISAPPEARANCE_RATIONALE: If market mechanisms disappeared overnight, actors would not reorganize around recovered historical alternatives — the historical knowledge is too attenuated and the machinery of living memory too degraded. The world would persist in market form by inertia of forgetting, not by active defense. A genuine organized beneficiary would mobilize to restore the arrangement; here, no such mobilization is visible because no concentrated actor profits enough from the amnesic state alone to defend it strategically.
% FOUNDING_PROBLEM: Economic coordination in large, anonymous societies requires mechanisms that do not depend on kinship, shared identity, or centralized command — strangers must exchange goods without knowing or trusting one another. Markets solve this problem; commons, guilds, and command economies solve it differently. All were live institutional options in the 18th-20th centuries.
% FOUNDING_PROBLEM_CORROBORATION: Historical evidence from Braudel, Mokyr, McCloskey, and economic historians outside the neoclassical mainstream documents the deliberate design and operation of non-market distribution systems and the subsequent erasure of their alternatives from economic theory. Contemporary development economists (Sen, Ostrom, Bowles) attest that the founding problem (coordination without kinship) admits multiple institutional solutions; mainstream macro textbooks attest the problem's persistence by describing markets as the solution without mentioning alternatives were ever tried.
narrative_ontology:disappearance_verdict(market_as_natural_default__lapsed_alternative_reading, world_unchanged).
narrative_ontology:founding_problem_status(market_as_natural_default__lapsed_alternative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__lapsed_alternative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_as_natural_default__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__lapsed_alternative_reading, 0.08, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__lapsed_alternative_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, ExtMetricName, E),
    domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(market_as_natural_default__lapsed_alternative_reading),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(market_as_natural_default__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.08 at interval end) because the constraint persists through forgetting, not through active extraction. No one collects rents from amnesia itself. Suppression is also low (0.12) because there is no coercive apparatus preventing market alternatives — they are simply unknown. Theater ratio is low but rising (0.15) because contemporary defenders of markets invoke naturalness, efficiency, and inevitability narratives that do some work to reinforce amnesia, but this is not the primary maintenance mechanism. Accessibility_collapse is high (0.72) because once the market form becomes naturalized, alternatives become nearly impossible to imagine or articulate within mainstream institutional discourse — the collapse is cognitive, not coercive. Resistance is low (0.18) because no systematic opposition to the constraint has formed; criticism exists but does not mobilize around the assertion of a viable alternative system. The measurement series shows extractiveness and suppression rising very slightly over the interval as market naturalization deepens and some performative defense emerges (rising theater), but the baseline remains that amnesia, not extraction, is the primary mechanism. The shared time grid ensures every metric is authored at all examined points; the interval represents roughly the last 50 years of market-dominant capitalism.
 *
 * PERSPECTIVAL GAP:
 *   This reading makes no claim about seat divergence because it authoritatively specifies that no concentrated beneficiary exists. Therefore, the payer/beneficiary structural distinction that generates perspectival gap in snare or tangled_rope constraints does not apply. The analytical divergence is historical, not contemporaneous: historians see alternatives that economists no longer know existed; policymakers see market mechanisms as natural, not as contingent on amnesia. This is not a structural divergence about what participants SEE from their positions; it is a temporal divergence about what is forgotten across historical generations.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation is vacuous here because there are no stakeholder seats: no beneficiaries, no victims, no payers. The constraint is authored as a mountain-shaped institutional fact, not as an asymmetric distribution. The historical amnesic process has no agency — it emerges from the interaction of institutional boundary effects (economic history is archived separately from policy economics), generational knowledge loss (each new cohort of economists is trained in the contemporary canon without historical depth), and the legitimacy crisis of defeated alternatives (communism, guild structures, and commons collapsed politically, so their institutional designs fell into disrepute). The amnesia serves no identifiable actor as a conscious strategy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordination without kinship in large anonymous societies) remains live — the constraint must continue solving it. But the reading posits that the founding problem is solved by multiple institutional forms, all of which are viable: markets, commons with sophisticated monitoring, gift economies in high-trust contexts, and hybrid systems combining market and non-market mechanisms. The constraint's function (solving coordination in large societies) has NOT died, but the constraint's MANDATE (the claim that markets are the ONLY solution) has become detached from evidence. Mandatrophy is not complete, but the mechanism is clear: forgetting the alternatives naturalizes the constraint beyond its functional necessity. If alternatives were recovered into institutional memory, the mandate would weaken — the constraint would shift from a taken-for-granted mountain to a contested institutional choice (rope or tangled_rope, depending on whether beneficiaries emerge to defend it). The very low extractiveness (0.08) reflects that this reading does not claim extraction is happening — it claims naturalization is happening, and naturalization is not the same as extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_forgotten_boundary,
    'Is market naturalization a genuine feature of economic reality (markets are the only stable coordination mechanism for large anonymous societies) or an artifact of institutional amnesia?',
    'Ethnographic and historical recovery of non-market distribution mechanisms still functioning in living communities (commons regimes, mutual aid networks, gift economies, cooperative enterprises); demonstration that recovered alternatives can operate at scale without collapse. If alternatives prove functionally robust, naturalization was contingent; if they collapse under scrutiny, naturalization may be structurally grounded.',
    'If naturalization is genuine (alternatives structurally inadequate), the constraint is a true mountain and extraction is illusory. If amnesia is the cause, the constraint is artificially mountain-shaped — classification would shift toward tangled_rope if beneficiaries begin defending amnesia, or remain as a lapsed-function piton if no one defends it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_vs_forgotten_boundary, empirical, 'Whether market naturalization is intrinsic to economic coordination or contingent on historical forgetting.').

omega_variable(
    lapsed_vs_active_defense_empirics,
    'Are markets naturalized because no one benefits enough from amnesia to defend it actively, or because amnesia has become so complete that no potential defender could rally opposition constituencies?',
    'Track moments when market hegemony faces organized challenge (labor movements, degrowth advocates, commons restoration movements, institutional experiments) and measure whether incumbent actors mobilize defenses. If defenses are absent or weak despite challenges, lapsed defense is supported; if defenses are immediate and strategic, beneficiary maintenance may be latent.',
    'If lapsed, the constraint remains a low-extraction mountain with diffuse inertia. If active defense is discovered, reclassification to tangled_rope (beneficiary defense via maintenance of amnesia) is warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lapsed_vs_active_defense_empirics, empirical, 'Whether amnesia is defended strategically or merely persists by inertia.').

omega_variable(
    reading_boundary_amnesia_beneficiary,
    'At what point does lapsed memory become active suppression? When does a constraint that persists through forgetting cross into a constraint that persists through the strategic maintenance of forgetting?',
    'This is an omega under committer frame (not empirically resolvable a priori): it demarcates this reading from the beneficiary_maintained_reading and hybrid_amnesia_reading. The sibling readings author higher extractiveness specifically because they posit active beneficiary defense of the amnesia itself. This reading posits low extractiveness because no identifiable actor profits from amnesia alone. If evidence emerges that a beneficiary class strategically maintains amnesia (textbook curriculum capture, foundation funding for market-friendly research, suppression of heterodox economics), the constraint migrates to one of the sibling readings.',
    'Bifurcation point for kernel readings: same observable constraint, different ε authorings based on whether beneficiary defense is presumed. This reading presumes no beneficiary — amnesia is an accident of institutional forgetting. Sibling readings presume beneficiary defense and auth higher ε accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_amnesia_beneficiary, conceptual, 'The reading-boundary dividing lapsed from maintained amnesia; also the dividing boundary between this reading and its kernel siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__lapsed_alternative_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(mark_tr_t10, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(mark_tr_t20, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(mark_tr_t30, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement(mark_tr_t40, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(mark_tr_t50, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(mark_be_t10, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 10, 0.04).
narrative_ontology:measurement(mark_be_t20, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 20, 0.06).
narrative_ontology:measurement(mark_be_t30, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 30, 0.07).
narrative_ontology:measurement(mark_be_t40, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 40, 0.08).
narrative_ontology:measurement(mark_be_t50, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 50, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 0, 0.03).
narrative_ontology:measurement(mark_su_t10, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 10, 0.05).
narrative_ontology:measurement(mark_su_t20, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 20, 0.08).
narrative_ontology:measurement(mark_su_t30, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 30, 0.1).
narrative_ontology:measurement(mark_su_t40, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 40, 0.11).
narrative_ontology:measurement(mark_su_t50, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 50, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__lapsed_alternative_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_as_natural_default__lapsed_alternative_reading, 0.04).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default__hybrid_amnesia_reading).

% DUAL FORMULATION NOTE:
% This story is part of a three-reading constraint family decomposing the kernel 'market_as_natural_default'. Each reading instantiates a different causal mechanism for market hegemony and authorizes a different ε-value. The lapsed_alternative_reading (this file) posits unintentional amnesia with no beneficiary; beneficiary_maintained_reading posits active post-hoc defense; hybrid_amnesia_reading posits initial amnesia enabling later beneficiary capture. These are not the same constraint seen from different seats — they are structurally distinct claims about how market dominance persists. They remain live, coexisting readings held by different scholarly communities and policy factions. Link them: each reading's network.affects_constraints includes the other two siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
