% ============================================================================
% CONSTRAINT STORY: market_naturalization__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__hybrid_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: market_naturalization__hybrid_reading
 *   human_readable: Market Dominance: Hybrid Lapsed-Maintained Closure
 *   domain: political_economy
 *
 * SUMMARY:
 *   Market dominance—a single firm controls a large share of a market or
 *   critical infrastructure—combines two distinct mechanisms: lapsed
 *   alternatives (switching costs and network effects set in decades ago when
 *   they were valuable; users are now locked in by momentum and psychology
 *   more than by objective barriers) and active maintenance (incumbent firms
 *   spend real resources on regulatory capture, technical lock-in deepening,
 *   pricing strategies that prevent margin-eroding entry, and ecosystem
 *   integration that raises switching costs for would-be defectors). The
 *   hybrid reading holds that BOTH mechanisms operate simultaneously. Some
 *   dominance persists because it would: switching costs are now endogenously
 *   low but users believe they are high. Some persists because incumbents
 *   defend it: they maintain technical barriers, lobby for regulatory
 *   favoritism, price to prevent entry. The question is how much of each.
 *   This reading instantiates one interpretation of the contested kernel
 *   'market_naturalization': neither pure lapsed closure (which would require
 *   no active defense) nor pure maintenance (which would attribute all
 *   dominance to incumbent action), but a structurally mixed arrangement. The
 *   claim/metric gap is intentional and substantive: the constraint is
 *   CLAIMED as tangled_rope (coordination with asymmetric extraction), while
 *   the metrics describe a situation where one actor (the incumbent)
 *   benefits, others (excluded entrants, consumers via supernormal pricing)
 *   bear costs, enforcement is moderately active (not yet intensive enough to
 *   be flagged as snare-level), and a growing share of enforcement effort is
 *   directed at maintaining lock-in rather than defending the original
 *   coordination function.
 *
 * KEY AGENTS:
 *   - incumbent_firms: institutional power, arbitrage-exit options, long time horizon — control the constraint structure and administer enforcement; primary beneficiaries
 *   - excluded_entrants: moderate power, constrained exit, biographical horizon — face mixed barriers (partly lapsed, partly maintained); suffer extraction through denied market entry
 *   - consumers: powerless, identity-locked exit, biographical horizon — benefit from coordination (ecosystem depth, reliability) but bear costs through supernormal pricing and reduced innovation; caught in a feedback loop where their own lock-in perception reinforces dominance
 *   - regulatory_authorities: institutional power, analytical exit, generational horizon — disputed seat whose interpretation determines whether dominance is natural or abusive; the framing choice hinges on their reading
 *   - potential_alternative_technologies: not agents, but structurally excluded by self-reinforcing economics (not authored as a payer because they are not real actors yet, only latent)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__hybrid_reading, 0.62).
domain_priors:suppression_score(market_naturalization__hybrid_reading, 0.58).
domain_priors:theater_ratio(market_naturalization__hybrid_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__hybrid_reading, "Market Dominance: Hybrid Lapsed-Maintained Closure").
narrative_ontology:topic_domain(market_naturalization__hybrid_reading, "political_economy").

domain_priors:requires_active_enforcement(market_naturalization__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__hybrid_reading, '7a903629-d62c-4338-8ff4-c472239c96a7').
narrative_ontology:cs_kernel_codification('7a903629-d62c-4338-8ff4-c472239c96a7', distributed).
narrative_ontology:cs_authority_grounding('7a903629-d62c-4338-8ff4-c472239c96a7', extraction).
narrative_ontology:cs_reading_relation('7a903629-d62c-4338-8ff4-c472239c96a7', market_naturalization__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('7a903629-d62c-4338-8ff4-c472239c96a7', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_axiom('7a903629-d62c-4338-8ff4-c472239c96a7', foundational, dominance_combines_lapsed_and_maintained).
narrative_ontology:cs_axiom_status(dominance_combines_lapsed_and_maintained, holdable).
narrative_ontology:cs_axiom_grounding('7a903629-d62c-4338-8ff4-c472239c96a7', dominance_combines_lapsed_and_maintained, empirically_contingent).
narrative_ontology:cs_axiom('7a903629-d62c-4338-8ff4-c472239c96a7', foundational, founding_coordination_problem_contested).
narrative_ontology:cs_axiom_status(founding_coordination_problem_contested, holdable).
narrative_ontology:cs_axiom_grounding('7a903629-d62c-4338-8ff4-c472239c96a7', founding_coordination_problem_contested, instrumental).
narrative_ontology:cs_reference_frame('7a903629-d62c-4338-8ff4-c472239c96a7', dual_mechanism_framework).
narrative_ontology:cs_drift_state('7a903629-d62c-4338-8ff4-c472239c96a7', contemporary_market_discipline_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7a903629-d62c-4338-8ff4-c472239c96a7', '').
narrative_ontology:cs_kernel_id(market_naturalization__hybrid_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, incumbent_firms).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, rent_extractors).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, path_dependent_lock_ins).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, excluded_entrants).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, consumer_surplus_claimants).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, suppressed_alternatives).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, consumers).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dominate markets through a mix of inherited structural advantages (network effects locked in decades ago, switching costs now internalized by users, technical standards set when alternatives had no voice) and active defense (lobbying to raise regulatory barriers, pricing strategies that preserve margin while appearing competitive, R&D directed at deepening lock-in rather than competitive innovation). They collect supernormal returns and administer the constraint by resisting entry and maintaining enforcement.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, incumbent_firms, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(market_naturalization__hybrid_reading, incumbent_firms, beneficiary).

% Face barriers to entry that partly dissolved naturally (original switching costs have eroded, user base is not locked in by physics) and partly persist by active enforcement (incumbents fund regulatory capture, set technical standards that favor them, use pricing power to deny margin to potential competitors). The barrier structure is ambiguous: unclear how much is inherited structural inertia versus active suppression.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, excluded_entrants, payer,
    moderate, biographical, constrained, national).

% Enjoy the coordination benefits of a settled dominant firm: reliable service, ecosystem depth, consumer experience calibrated to the installed base. They also carry supernormal-pricing costs where the firm exploits its position. Exit is technically possible but psychologically fused with the ecosystem (social network, sunk content, workflow integration, identity as adopters of the 'standard').
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, consumers, beneficiary,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(market_naturalization__hybrid_reading, consumers, payer).

% Would represent genuine functional improvements or cost-effective alternatives if developed. They are not excluded by explicit policy but by the self-reinforcing economics: network effects and switching costs mean even superior alternatives cannot reach critical mass because the installed base is locked in. Partly this is lapsed natural closure; partly it is actively maintained by incumbent pricing and integration strategy.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, potential_alternative_technologies, excluded,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(market_naturalization__hybrid_reading, potential_alternative_technologies).

% Monitor whether dominance is abusive or merely competitive. They dispute whether the barrier is natural (lapsed switching cost equilibrium) or maintained (abuse of dominance). Their enforcement power is high but contested—whether the constraint violates law depends on interpreting dominance persistence as natural entropy or as enforced exclusion.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% The structural feature that originally locked users in and gave incumbents competitive advantage. These costs partially lapsed as technology evolved and user sophistication grew, but the lapsing is asymmetric: users experience the costs as real even as objective barriers fell.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, inherited_switching_costs, beneficiary,
    powerless, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(market_naturalization__hybrid_reading, inherited_switching_costs).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__hybrid_reading, incumbent_firms).
narrative_ontology:fixing_cost_class(market_naturalization__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unified technical standard and ecosystem interoperability: single dominant platform solves network effects, eliminates interoperability friction, provides consumers with reliable ecosystem depth and developer certainty. The original founding problem (fragmented platforms, switching friction, ecosystem uncertainty) is substantially addressed by the unified structure.
% TRANSFER_FUNCTION: Supernormal rents flow from excluded entrants (who cannot enter to compete away margins) and consumers (who pay higher prices than competitive entry would support) to the dominant incumbent; also flows effort from consumers and entrants toward defending/maintaining lock-in barriers rather than toward productive innovation.
% ABSENT_VOICES: Excluded entrants and suppressed alternative technologies are structurally unable to testify; if they could, they would argue that inherited switching costs are lower than users believe and that active incumbent lock-in maintenance creates artificial barriers. Competing regulatory reading (held by competition authorities and consumer advocates) holds that dominance is abuse that harms consumer welfare and innovation.
% DISAPPEARANCE_RATIONALE: Lapsed-alternative reading: if dominance vanished (incumbent was broken up), the market would not significantly reorganize because the coordination benefits would persist and users would drift back to a standard even without lock-in enforcement. Beneficiary-maintained reading: if dominance vanished, the market would rapidly fragment because without active incumbent maintenance, switching costs would collapse and entry would accelerate. Hybrid reading (this one): the market would partially reorganize—some users would switch and some entrants would enter (proving some lock-in was active), but the dominant incumbent would retain a large share (proving some dominance is genuinely lapsed and users do value the coordination).
% FOUNDING_PROBLEM: In the constraint's founding period, switching costs were objectively high (incompatible platforms, data portability was technically difficult, ecosystems were fragmented and unreliable). Unified dominance around a single standard solved genuine coordination problems: users got reliability, developers got certainty about installed-base size, the ecosystem grew to serve both.
% FOUNDING_PROBLEM_CORROBORATION: The incumbent attests the founding problem remains live: ecosystem fragmentation would return if dominance dissolved, platforms still need unified standards, network effects still reward scale. Technology historians and economic analysts attest the founding problem is substantially solved: switching costs have fallen dramatically, portable data, cross-platform interoperability, and user sophistication make fragmentation survivable. Regulatory authorities, competition advocates, and entrant testimony support the contested status: the founding coordination function was real, but has become partially unnecessary while dominance has become partially about extraction rather than coordination. No single non-benefiting authority attests unambiguously, which is itself signal—the fact of contestation is corroboration of contest.
narrative_ontology:disappearance_verdict(market_naturalization__hybrid_reading, contested).
narrative_ontology:founding_problem_status(market_naturalization__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_naturalization__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__hybrid_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_naturalization__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_naturalization__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.62) because the incumbent collects supernormal rents and the arrangement persists despite excluded entrants who would compete it away if barriers dissolved. Suppression is moderate (0.58) because some barriers are inherited and self-perpetuating (identity lock, sunk switching costs, user behavior momentum) while others require active incumbents defense (regulatory capture, technical standards favoring the incumbent, pricing strategies that preserve margin). Theater is rising but moderate (0.48): the constraint carries genuine coordination function (single standard, ecosystem depth, network effects are real), but a growing share of enforcement activity performs maintenance rather than delivering those functions—new switching costs are invented, interoperability is restricted, lock-in narratives are marketed. The temporal trajectory shows extraction and suppression rising through the interval: as inherited advantages naturally erode and user sophistication grows, incumbents must work harder to maintain dominance, so active maintenance increases relative to passive lapsing. The plateau by the end of the interval suggests a stable mixed equilibrium: the incumbent cannot raise extraction or suppression much further without triggering legislative intervention (the regulatory observer's power threshold), so dominance stabilizes at the enforced level. The coercion_grid shows level-resolved dynamics: suppression is highest at the organizational level (excluded entrants face the most active exclusion—incumbent lobbying, technical barriers, predatory pricing targeting rivals) and lowest at the structural level (the overall market structure does not require massive societal-scale coercion, just localized incumbent action). Resistance is highest at the organizational level (excluded entrants are attempting entry and organizing political pressure for remedies) and lowest at the structural level (no wholesale challenge to the market mechanism itself). Accessibility_collapse (how closed alternatives have become) is highest at the organizational level (entry is effectively barred) and lowest at the individual level (consumers technically could switch, but psychology and network effects make it costly).
 *
 * PERSPECTIVAL GAP:
 *   The incumbent and the beneficiary-maintained reader see dominance as requiring active defense against eroding competitive pressure: switching costs have naturally fallen, but the incumbent must continuously rebuild lock-in via ecosystem integration, technical standards, pricing strategy, and regulatory favors to maintain share. Excluded entrants and the lapsed-alternative reader see dominance as mostly self-perpetuating inertia: the inherited switching costs are endogenously low but users do not realize it; a small regulatory change or coordinated entrant push would dissolve dominance without incumbent action. Consumers and pragmatist observers see the hybrid picture: dominance is real, some of it is lapsed (user psychology, network effects that are no longer objectively binding), some is actively maintained (incumbent strategy), and the exact mix determines whether market discipline or regulatory intervention is the appropriate remedy. The engine computes each seat's experience of the constraint's type from the structural data—incumbent and regulatory observer will compute it differently, which is the point of the per-seat classification system.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent firms sit at d near 1.0 (full target from the perspective of extraction, but full beneficiary from the perspective of the coordination function; the dual role creates ambiguity—the engine resolves this via power + exit + beneficiary/victim status, landing roughly at d ≈ 0.3 as beneficiary, meaning effective extraction χ is reduced by the beneficiary status). Excluded entrants sit at high d (near 0.8–0.9): they bear costs (denied entry), have constrained exit (cannot establish without breaking incumbent barriers), and are not benefiting from the coordination (they would disrupt incumbency if admitted). Consumers sit at d ≈ 0.5 (symmetric): they benefit from coordination (ecosystem, reliability, network effects), but they also pay via supernormal pricing and suppressed innovation; their identity lock means they cannot arbitrage, so exit is constrained, which increases d slightly, but the beneficiary role offsets it. The directionality override could be applied to incumbent_firms if the structural derivation wrongly classified them as targets rather than beneficiaries; in this story, the beneficiary/victim declarations are clear enough that no override is needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading avoids a mandatrophy trap that ensnares both sibling readings. Lapsed_alternative_reading would claim that dominance requires no enforcement (it is natural lapsing), which would imply the constraint is a mountain—but mountains do not carry enforcers, and we observe active incumbent maintenance, so this reading's classification would fail. Beneficiary_maintained_reading would claim that dominance is pure incumbent extraction (a snare), but we observe real coordination benefits (ecosystem, network effects, single standard solved a genuine collective action problem), so this reading's classification would fail snare gates (snares have no genuine coordination). The hybrid reading instantiates tangled_rope: genuine coordination (the original problem was real, unified standards are valuable), asymmetric extraction (incumbent collects supernormal rents), and active enforcement (maintaining the lock-in barriers that preserve incumbency against entrant competition and consumer switching). The mandatrophy is avoided because the hybrid reading does not claim the coordination has disappeared—it claims the founding problem is contested (partly solved, partly still valuable, partly used as cover for extraction), which is consistent with tangled_rope's gate: a live coordination function with extractive overhead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lapsed_vs_maintained_boundary,
    'What proportion of current market dominance is due to lapsed natural switching costs (sunk, self-perpetuating by user behavior alone) versus actively maintained by incumbent strategy (lobbying, technical lock-in deepening, predatory pricing)?',
    'Counterfactual analysis: regulatory removal of active-maintenance mechanisms (IP licensing restrictions, interoperability barriers, regulatory capture channels) while holding inherited switching costs in place. Measure entrant success and consumer welfare change.',
    'If maintenance is small (< 20% of dominance), the constraint is closer to lapsed_alternative_reading and requires no active enforcement to persist. If maintenance is large (> 60%), the constraint is closer to beneficiary_maintained_reading and depends entirely on incumbent action. The hybrid reading (this one) sits in the middle: meaningful persistence from both channels.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(lapsed_vs_maintained_boundary, empirical, 'The decomposition of dominance into lapsed and actively-maintained components.').

omega_variable(
    identity_lock_persistence,
    'Is the identity lock-in that keeps consumers from leaving (ecosystem identity, switching-cost psychology, social network effects) structurally genuine or a performance of dominance—would it dissolve if competitors offered equivalent switching costs?',
    'Natural experiment: new entrant offering subsidized switching (free data migration, app ecosystem parity, ecosystem-transfer guarantees). Measure defection and consumer switching-cost reassessment.',
    'If identity lock dissolves readily once barriers fall, consumer suppression is partly internalized (psychology of lapsed barriers) and partly structural (active incumbent action to reinforce the psychology). If identity lock persists even at parity, the lock is genuine and the constraint''s suppression is lower than the authored metrics. The hybrid reading assumes partial dissolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Whether consumer identity lock-in is structural or a performance of enforcement.').

omega_variable(
    sibling_reading_empirical_signature,
    'Which empirical facts would favor each sibling reading over this one? What would the temporal trajectory, level-resolved suppression, and theater ratio look like under each reading?',
    'Measurement protocol: lapsed_alternative_reading predicts theater_ratio → 0 (enforcement maintenance drops to background); suppression plateaus or falls as incumbents stop active defense; entry barriers erode as inherited advantages decay. Beneficiary_maintained_reading predicts theater_ratio rising (more orchestrated performance to maintain dominance); suppression stable or rising (active defense intensifies as inherited advantages weaken); entry barriers maintained or heightened.',
    'This hybrid reading predicts mixed signals: theater_ratio rising moderately to ~0.48 (active maintenance of some barriers, lapsed inattention to others); suppression rising moderately to ~0.58 (some barriers self-perpetuate, others need active defense); both trend lines plateauing by mid-interval as the mixed structure stabilizes. Which empirical signature materializes determines which reading is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_empirical_signature, empirical, 'Distinguishing the three kernel readings by their temporal and level-resolved signatures.').

omega_variable(
    kernel_framing_ambiguity,
    'Is the boundary between lapsed coordination and active extraction itself a commitment (a frame choice) or an empirical fact? Does labeling inherited switching costs as ''lapsed'' depend on a normative choice about what dominance ''ought'' to look like?',
    'Examine the discourse: beneficiary parties define lapsed barriers as ''natural market outcomes'' (deontological framing); entrant and consumer advocates define the same barriers as ''incumbent-engineered lock-in'' (instrumental framing). The disagreement is partly empirical (how much maintenance is needed) and partly framing (whether inherited advantage is legitimate). No pure empirical test dissolves the framing choice.',
    'If the reading choice is partly framing, then this hybrid reading''s ''mixed'' characterization is itself a frame (acknowledging both legitimacy claims) rather than a discovery. The constraint''s actual character (lapsed or maintained) cannot be read off the data alone—it depends on which tradition you inhabit. The hybrid reading is coherent only within a pluralist frame that honors both interpretations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the lapsed/maintained boundary is an empirical fact or a frame choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__hybrid_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(mark_tr_t0, observed).
narrative_ontology:measurement(mark_tr_t5, market_naturalization__hybrid_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(mark_tr_t5, observed).
narrative_ontology:measurement(mark_tr_t10, market_naturalization__hybrid_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(mark_tr_t10, observed).
narrative_ontology:measurement(mark_tr_t15, market_naturalization__hybrid_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement_basis(mark_tr_t15, observed).
narrative_ontology:measurement(mark_tr_t20, market_naturalization__hybrid_reading, theater_ratio, 20, 0.46).
narrative_ontology:measurement_basis(mark_tr_t20, observed).
narrative_ontology:measurement(mark_tr_t25, market_naturalization__hybrid_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(mark_tr_t25, observed).
narrative_ontology:measurement(mark_tr_t30, market_naturalization__hybrid_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement_basis(mark_tr_t30, observed).
narrative_ontology:measurement(mark_tr_t40, market_naturalization__hybrid_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(mark_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__hybrid_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(mark_be_t0, observed).
narrative_ontology:measurement(mark_be_t5, market_naturalization__hybrid_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(mark_be_t5, observed).
narrative_ontology:measurement(mark_be_t10, market_naturalization__hybrid_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(mark_be_t10, observed).
narrative_ontology:measurement(mark_be_t15, market_naturalization__hybrid_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(mark_be_t15, observed).
narrative_ontology:measurement(mark_be_t20, market_naturalization__hybrid_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(mark_be_t20, observed).
narrative_ontology:measurement(mark_be_t25, market_naturalization__hybrid_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(mark_be_t25, observed).
narrative_ontology:measurement(mark_be_t30, market_naturalization__hybrid_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(mark_be_t30, observed).
narrative_ontology:measurement(mark_be_t40, market_naturalization__hybrid_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(mark_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_naturalization__hybrid_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(mark_su_t0, observed).
narrative_ontology:measurement(mark_su_t5, market_naturalization__hybrid_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement_basis(mark_su_t5, observed).
narrative_ontology:measurement(mark_su_t10, market_naturalization__hybrid_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement_basis(mark_su_t10, observed).
narrative_ontology:measurement(mark_su_t15, market_naturalization__hybrid_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement_basis(mark_su_t15, observed).
narrative_ontology:measurement(mark_su_t20, market_naturalization__hybrid_reading, suppression_requirement, 20, 0.54).
narrative_ontology:measurement_basis(mark_su_t20, observed).
narrative_ontology:measurement(mark_su_t25, market_naturalization__hybrid_reading, suppression_requirement, 25, 0.57).
narrative_ontology:measurement_basis(mark_su_t25, observed).
narrative_ontology:measurement(mark_su_t30, market_naturalization__hybrid_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(mark_su_t30, observed).
narrative_ontology:measurement(mark_su_t40, market_naturalization__hybrid_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement_basis(mark_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(mark_grid_01, market_naturalization__hybrid_reading, accessibility_collapse(class), 0, 0.68).
narrative_ontology:measurement(mark_grid_02, market_naturalization__hybrid_reading, accessibility_collapse(class), 40, 0.74).
narrative_ontology:measurement(mark_grid_03, market_naturalization__hybrid_reading, accessibility_collapse(individual), 0, 0.62).
narrative_ontology:measurement(mark_grid_04, market_naturalization__hybrid_reading, accessibility_collapse(individual), 40, 0.68).
narrative_ontology:measurement(mark_grid_05, market_naturalization__hybrid_reading, accessibility_collapse(organizational), 0, 0.75).
narrative_ontology:measurement(mark_grid_06, market_naturalization__hybrid_reading, accessibility_collapse(organizational), 40, 0.82).
narrative_ontology:measurement(mark_grid_07, market_naturalization__hybrid_reading, accessibility_collapse(structural), 0, 0.78).
narrative_ontology:measurement(mark_grid_08, market_naturalization__hybrid_reading, accessibility_collapse(structural), 40, 0.8).
narrative_ontology:measurement(mark_grid_09, market_naturalization__hybrid_reading, resistance(class), 0, 0.54).
narrative_ontology:measurement(mark_grid_10, market_naturalization__hybrid_reading, resistance(class), 40, 0.58).
narrative_ontology:measurement(mark_grid_11, market_naturalization__hybrid_reading, resistance(individual), 0, 0.42).
narrative_ontology:measurement(mark_grid_12, market_naturalization__hybrid_reading, resistance(individual), 40, 0.39).
narrative_ontology:measurement(mark_grid_13, market_naturalization__hybrid_reading, resistance(organizational), 0, 0.68).
narrative_ontology:measurement(mark_grid_14, market_naturalization__hybrid_reading, resistance(organizational), 40, 0.72).
narrative_ontology:measurement(mark_grid_15, market_naturalization__hybrid_reading, resistance(structural), 0, 0.35).
narrative_ontology:measurement(mark_grid_16, market_naturalization__hybrid_reading, resistance(structural), 40, 0.38).
narrative_ontology:measurement(mark_grid_17, market_naturalization__hybrid_reading, stakes_inflation(class), 0, 0.52).
narrative_ontology:measurement(mark_grid_18, market_naturalization__hybrid_reading, stakes_inflation(class), 40, 0.58).
narrative_ontology:measurement(mark_grid_19, market_naturalization__hybrid_reading, stakes_inflation(individual), 0, 0.44).
narrative_ontology:measurement(mark_grid_20, market_naturalization__hybrid_reading, stakes_inflation(individual), 40, 0.48).
narrative_ontology:measurement(mark_grid_21, market_naturalization__hybrid_reading, stakes_inflation(organizational), 0, 0.62).
narrative_ontology:measurement(mark_grid_22, market_naturalization__hybrid_reading, stakes_inflation(organizational), 40, 0.71).
narrative_ontology:measurement(mark_grid_23, market_naturalization__hybrid_reading, stakes_inflation(structural), 0, 0.41).
narrative_ontology:measurement(mark_grid_24, market_naturalization__hybrid_reading, stakes_inflation(structural), 40, 0.42).
narrative_ontology:measurement(mark_grid_25, market_naturalization__hybrid_reading, suppression(class), 0, 0.38).
narrative_ontology:measurement(mark_grid_26, market_naturalization__hybrid_reading, suppression(class), 40, 0.44).
narrative_ontology:measurement(mark_grid_27, market_naturalization__hybrid_reading, suppression(individual), 0, 0.28).
narrative_ontology:measurement(mark_grid_28, market_naturalization__hybrid_reading, suppression(individual), 40, 0.32).
narrative_ontology:measurement(mark_grid_29, market_naturalization__hybrid_reading, suppression(organizational), 0, 0.48).
narrative_ontology:measurement(mark_grid_30, market_naturalization__hybrid_reading, suppression(organizational), 40, 0.64).
narrative_ontology:measurement(mark_grid_31, market_naturalization__hybrid_reading, suppression(structural), 0, 0.22).
narrative_ontology:measurement(mark_grid_32, market_naturalization__hybrid_reading, suppression(structural), 40, 0.24).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__hybrid_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_naturalization__hybrid_reading, 0.18).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, market_naturalization__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, market_naturalization__beneficiary_maintained_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the market_naturalization kernel family (three stories, one kernel, three readings). The hybrid_reading sits between the lapsed_alternative_reading (which attributes dominance to natural lapsing) and the beneficiary_maintained_reading (which attributes dominance to active incumbent extraction). The three stories share a referent (market dominance) but differ in how they frame the persistence mechanism. Decomposition is necessary because ε is different for each reading: the lapsed reading authorizes low ε (dominance requires no extraction effort, it is natural inertia), the maintained reading authorizes high ε (dominance is pure rent extraction with coordination cover story), and the hybrid reading (this one) authorizes moderate ε (mixed mechanisms, moderate extraction overlay on real coordination). All three readings affect each other: if the maintained reading is correct, the lapsed reading's claim of natural inertia is false cover story; if the lapsed reading is correct, the maintained reading over-attributes intentionality. The hybrid reading influences both by claiming their disagreement is partly empirical (how much maintenance is actually needed) and partly framing (what counts as legitimate coordination vs. extractive overhead).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
