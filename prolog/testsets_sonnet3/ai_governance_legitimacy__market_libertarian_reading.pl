% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__market_libertarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__market_libertarian_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: ai_governance_legitimacy__market_libertarian_reading
 *   human_readable: Market-Libertarian Reading of AI Governance Legitimacy (Property Rights and Voluntary Exchange)
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested
 *   ai_governance_legitimacy kernel: the market-libertarian reading, which
 *   grounds legitimacy in voluntary exchange and property rights and treats
 *   these as pre-political structural facts rather than political choices. It
 *   claims mountain status — property rights and contract as natural,
 *   discovered order — while declaring beneficiaries (entrepreneurs,
 *   investors, high-autonomy professionals), which is precisely the condition
 *   the false-summit-mountain signature is designed to catch: a claimed
 *   natural law with identifiable, concentrated beneficiaries. The ε referent
 *   is the standing market-contract arrangement for AI governance as this
 *   reading itself describes it, not the reading's endorsed alternative
 *   (there is no alternative arrangement to compare against within this
 *   reading's own frame, since the reading holds the current arrangement IS
 *   the natural order). Sibling readings — magisterial_subsidiarity,
 *   technocratic_optimization, democratic_pluralist — are NOT part of this
 *   constraint; they are separate constraints linked via
 *   network.affects_constraints, each with its own ε and beneficiary/victim
 *   structure per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - ai_entrepreneurs: Primary beneficiary (powerful/arbitrage) — captures gains from unencumbered deployment
 *   - venture_investors: Primary beneficiary (institutional/arbitrage) — capital flows to jurisdictions honoring this premise
 *   - gig_economy_ai_workers: Primary target (powerless/trapped) — bears costs the reading defines as voluntary
 *   - communities_facing_algorithmic_coordination_failures: Secondary target (powerless/trapped) — externalities treated as outside governance scope
 *   - vatican_magisterium: Excluded voice (institutional/analytical) — solidarity claim ruled illegitimate by this reading's own axioms
 *   - libertarian_legal_theorists: Agenda setter (institutional/analytical) — articulates and defends the doctrine as structural fact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__market_libertarian_reading, 0.26).
domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, 0.22).
domain_priors:theater_ratio(ai_governance_legitimacy__market_libertarian_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, extractiveness, 0.26).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__market_libertarian_reading, mountain).
narrative_ontology:human_readable(ai_governance_legitimacy__market_libertarian_reading, "Market-Libertarian Reading of AI Governance Legitimacy (Property Rights and Voluntary Exchange)").
narrative_ontology:topic_domain(ai_governance_legitimacy__market_libertarian_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__market_libertarian_reading, '49acb96c-1a95-410f-ad19-784dce136c4d').
narrative_ontology:cs_kernel_codification('49acb96c-1a95-410f-ad19-784dce136c4d', distributed).
narrative_ontology:cs_authority_grounding('49acb96c-1a95-410f-ad19-784dce136c4d', distributed).
narrative_ontology:cs_reading_relation('49acb96c-1a95-410f-ad19-784dce136c4d', ai_governance_legitimacy__magisterial_subsidiarity_reading, forecloses).
narrative_ontology:cs_reading_relation('49acb96c-1a95-410f-ad19-784dce136c4d', ai_governance_legitimacy__technocratic_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('49acb96c-1a95-410f-ad19-784dce136c4d', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_axiom('49acb96c-1a95-410f-ad19-784dce136c4d', foundational, property_rights_prepolitical).
narrative_ontology:cs_axiom_status(property_rights_prepolitical, holdable).
narrative_ontology:cs_axiom_grounding('49acb96c-1a95-410f-ad19-784dce136c4d', property_rights_prepolitical, deontological).
narrative_ontology:cs_axiom('49acb96c-1a95-410f-ad19-784dce136c4d', foundational, solidarity_demands_are_illegitimate_coercion).
narrative_ontology:cs_axiom_status(solidarity_demands_are_illegitimate_coercion, holdable).
narrative_ontology:cs_axiom_grounding('49acb96c-1a95-410f-ad19-784dce136c4d', solidarity_demands_are_illegitimate_coercion, conventional).
narrative_ontology:cs_reference_frame('49acb96c-1a95-410f-ad19-784dce136c4d', natural_rights_prepolitical_order).
narrative_ontology:cs_drift_state('49acb96c-1a95-410f-ad19-784dce136c4d', contemporary_ai_governance_debate, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('49acb96c-1a95-410f-ad19-784dce136c4d', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, ai_entrepreneurs).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, venture_investors).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_technical_professionals).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, gig_economy_ai_workers).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, communities_facing_algorithmic_coordination_failures).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, monopsony_labor_market_workers).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, property_rights_as_prepolitical).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, voluntary_exchange_as_legitimating_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and deploy AI systems under a governance frame that treats property rights and contract as the sole legitimate source of obligation. Free to relocate operations, restructure entities, and route around jurisdictions that impose collective mandates. The reading's rejection of solidarity-based obligation directly removes compliance costs they would otherwise bear.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, ai_entrepreneurs, beneficiary,
    powerful, biographical, arbitrage, global).

% Allocate capital to AI ventures on the expectation that governance will remain contract-based and exit-driven rather than subject to political redistribution or common-good mandates. Their returns depend on the reading's premise holding across the portfolio's jurisdictions; capital can flow to whichever regime honors this premise most completely.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, venture_investors, beneficiary,
    institutional, generational, arbitrage, global).

% Skilled AI researchers and engineers whose labor is scarce and portable. They benefit from a governance frame that treats their employment terms as purely voluntary and unencumbered by sectoral mandates, since their market power lets them negotiate favorable terms without needing collective protection.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_technical_professionals, beneficiary,
    powerful, biographical, mobile, national).

% Perform data-labeling, content moderation, and model-training support work under contracts framed as purely voluntary exchange. Lack the market power the reading assumes all parties possess; 'exit' from one platform typically means moving to an equally precarious one, not genuine market discipline on the employer.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, gig_economy_ai_workers, payer,
    powerless, immediate, trapped, national).

% Face externalities from AI deployment (algorithmic discrimination, infrastructure strain, labor displacement) that the reading treats as matters for private remedy through contract or tort, never collective mandate. Coordination failures that only political authority could resolve are, under this reading, simply not governance's business — leaving the costs to fall where they land.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, communities_facing_algorithmic_coordination_failures, payer,
    powerless, generational, trapped, regional).

% Work in regions or sectors where a small number of AI-adjacent employers dominate hiring. The reading's 'exit options and competitive markets protect dignity' premise assumes a competitive labor market that does not exist for them; their formally voluntary contracts are negotiated under conditions the reading does not recognize as coercive.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, monopsony_labor_market_workers, payer,
    powerless, biographical, constrained, regional).

% Articulate and defend the doctrine that property rights and voluntary contract precede and legitimate political authority, treating this as a discovered structural fact about legitimate order rather than a policy preference. They author the framework that arbitration bodies and contract law draw on to adjudicate AI governance disputes.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, libertarian_legal_theorists, agenda_setter,
    institutional, civilizational, analytical, global).

% Holds that solidarity is not optional charity but a demand of justice binding on economic actors, and that subsidiarity operates alongside solidarity rather than displacing it. This reading treats the Magisterium's solidarity claim as illegitimate coercion, excluding its account of AI governance's proper obligations from the legitimacy frame entirely.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, vatican_magisterium, excluded,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_governance_legitimacy__market_libertarian_reading, diffuse).
narrative_ontology:fixing_cost_class(ai_governance_legitimacy__market_libertarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates AI development and deployment through decentralized contract and property-rights enforcement rather than centralized mandate — parties negotiate terms directly, arbitration and reputational mechanisms substitute for regulatory oversight, and capital and talent flow to whichever jurisdictions honor these terms most reliably.
% TRANSFER_FUNCTION: Moves the cost of AI-driven externalities and market-power asymmetries away from firms and capital holders (who bear only what they voluntarily contract for) onto workers and communities without comparable market power, by defining anything beyond voluntary exchange as outside governance's legitimate scope.
% ABSENT_VOICES: The Vatican Magisterium and communities facing coordination failures would object that treating solidarity as coercion erases obligations of justice toward the vulnerable; they are structurally excluded because this reading defines their claims as categorically illegitimate rather than as competing considerations to weigh.
% DISAPPEARANCE_RATIONALE: If this reading's legitimacy claim were abandoned tomorrow, AI governance disputes currently routed through private contract and arbitration would become contestable through political and regulatory channels; monopsony workers and affected communities would gain standing to demand collective remedies currently foreclosed by the property-rights-as-prepolitical premise.
% FOUNDING_PROBLEM: The reading was built to answer a real problem: centralized, politically-directed technology mandates can be captured, slow, and can suppress beneficial innovation through blunt collective rules that ignore local knowledge and voluntary preference.
% FOUNDING_PROBLEM_CORROBORATION: Entrepreneurs and investors attest the problem remains live, citing regulatory capture and innovation-chilling mandates elsewhere. Labor economists studying monopsony AI labor markets and the Magisterium itself attest that the reading's remedy has outrun the founding problem — treating ALL collective obligation as illegitimate coercion addresses regulatory overreach by denying that coordination failures or power asymmetries exist at all, which is a stronger claim than the founding problem warrants.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__market_libertarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__market_libertarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__market_libertarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_governance_legitimacy__market_libertarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__market_libertarian_reading, 0.26, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__market_libertarian_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, ExtMetricName, E),
    domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ai_governance_legitimacy__market_libertarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   ε is authored low (0.20-0.30 band, landing at 0.26) per the expected structural delta: within this reading's own terms, voluntary exchange between formally equal parties is not extraction. But the metrics are authored honestly rather than tuned to the mountain claim — accessibility_collapse (0.6) and resistance (0.45) are both meaningfully below genuine-mountain territory (0.85+/near-zero), because real alternatives to property-rights-only governance exist and are actively contested (regulatory movements, labor organizing, the Magisterium's own competing doctrine). This divergence between the claimed mountain and the sub-mountain metric profile, combined with declared beneficiaries, is exactly the false-summit-mountain condition the schema requires an omega for.
 *
 * PERSPECTIVAL GAP:
 *   From the entrepreneur/investor seat, the arrangement reads as pure coordination — contracts are honored, capital flows freely, no one is coerced. From the monopsony-worker or coordination-failure-community seat, the same rules operate as extraction dressed in the language of voluntary choice: their formal freedom to exit does not correspond to any real alternative. The engine computes these divergent per-seat classifications from the declared power/exit data; the claimed_type does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Entrepreneurs, investors, and high-autonomy professionals sit near the full-beneficiary end: they hold market power, face no coordination failure, and the reading's rules were built around their actual bargaining position. Gig workers, monopsony-market workers, and affected communities sit near the full-target end: they are structurally powerless within the market the reading describes as neutral, and their exit options (trapped, constrained) contradict the reading's own premise that dignity is protected through exit — the premise assumes exit capacity these agents do not have.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (capture-prone, innovation-chilling centralized mandates) is real and partially live, which is why founding_problem_status is authored as contested rather than dead — this reading is not pure mandatrophy dressed as principle. But the remedy has drifted past the problem: denying that ANY coordination failure or power asymmetry can generate legitimate collective obligation is a much stronger claim than 'centralized mandates carry capture risk.' The classification as a claimed mountain with declared beneficiaries and moderate accessibility_collapse lets the engine flag this drift rather than accepting the reading's own self-description as settled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_rights_natural_or_constructed,
    'Are property rights and voluntary exchange a pre-political structural fact this reading discovers, or a historically contingent legal-institutional arrangement that this reading naturalizes to benefit those who currently hold capital and market power?',
    'Comparative institutional history: property-rights regimes vary substantially across jurisdictions and eras in ways that track political settlement rather than discovered natural order; a genuinely pre-political fact would not exhibit this variation. Track whether the specific AI-governance property claims being defended (e.g., training-data ownership, model IP) predate or postdate the commercial interests they now serve.',
    'If constructed rather than discovered, the mountain claim fails and the constraint reclassifies toward tangled_rope or snare — a coordination mechanism (contract enforcement) riding on asymmetric extraction (concentrated benefit to capital-holders, cost externalized to powerless workers and communities) rather than a natural law with zero degrees of freedom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_rights_natural_or_constructed, conceptual, 'False-summit ambiguity: is property-rights-as-legitimacy a natural law or a naturalized construction serving declared beneficiaries?').

omega_variable(
    exit_option_reality_gap,
    'Does the reading''s claim that ''dignity is protected through exit options and competitive markets'' hold for the powerless stakeholders as a factual matter, or only for the high-market-power stakeholders it was modeled on?',
    'Empirical labor-market concentration studies (HHI indices for AI-adjacent gig and data-labeling markets) and switching-cost analysis for affected communities facing algorithmic externalities.',
    'If exit options are illusory for the declared victim groups, the reading''s core legitimating mechanism (voluntary exchange under genuine alternatives) does not obtain for them, which would push effective extraction toward the target end far more sharply than the authored ε=0.26 suggests for those seats specifically.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_reality_gap, empirical, 'Whether the reading''s exit-option premise holds empirically for its declared victim groups.').

omega_variable(
    kernel_committer_structure,
    'This constraint is one of four readings of the ai_governance_legitimacy kernel; how would the classification differ under the magisterial_subsidiarity_reading, which treats solidarity as a binding demand of justice rather than illegitimate coercion?',
    'Compare this story''s ε (0.26, claimed mountain) against the sibling magisterial_subsidiarity_reading story''s ε and claimed_type once authored — the sibling would likely author substantially higher ε for the SAME standing market-contract arrangement, since it does not accept voluntary exchange alone as legitimating.',
    'Confirms the readings are structurally distinct constraints (per ε-invariance) rather than one constraint viewed from different angles — each reading''s ε is stable within its own framework but the frameworks disagree about what counts as extraction at all.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_committer_structure, conceptual, 'Committer-structure note: this reading''s relationship to the magisterial_subsidiarity sibling reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__market_libertarian_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ai_g_tr_t6, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 6, 0.11).
narrative_ontology:measurement(ai_g_tr_t12, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(ai_g_tr_t18, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 18, 0.14).
narrative_ontology:measurement(ai_g_tr_t24, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 24, 0.15).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(ai_g_be_t6, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 6, 0.21).
narrative_ontology:measurement(ai_g_be_t12, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 12, 0.23).
narrative_ontology:measurement(ai_g_be_t18, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 18, 0.25).
narrative_ontology:measurement(ai_g_be_t24, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 24, 0.26).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ai_governance_legitimacy__market_libertarian_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__democratic_pluralist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the ai_governance_legitimacy kernel, each authored as a separate constraint per the ε-invariance principle (a single natural-language label — 'AI governance legitimacy' — covers four structurally distinct legitimacy claims with different ε, different beneficiary/victim structures, and different claimed types). market_libertarian_reading authors the lowest ε (0.26) and claims mountain status; magisterial_subsidiarity_reading is expected to author substantially higher ε reflecting its rejection of the market-contract arrangement as sufficient; technocratic_optimization_reading and democratic_pluralist_reading occupy intermediate positions grounded in different legitimating mechanisms (expert-authority welfare maximization vs. democratic consent). All four share the same underlying contested arrangement (AI governance built on contract/property enforcement) but disagree fundamentally about whether and how much of that arrangement constitutes extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
