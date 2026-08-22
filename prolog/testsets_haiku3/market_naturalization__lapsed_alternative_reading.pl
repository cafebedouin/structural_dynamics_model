% ============================================================================
% CONSTRAINT STORY: market_naturalization__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Market Dominance as Lapsed Closure (No Active Maintenance Reading)
 *   domain: political_economy/economic_history
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested kernel
 *   'market_naturalization': the claim that observed market dominance by
 *   incumbent firms is a LAPSED CLOSURE — a once-functional coordination
 *   mechanism that solved real distribution, standardization, and quality
 *   problems during early industrial development, but which now persists
 *   through historical inertia and atrophied alternatives rather than through
 *   active maintenance or ongoing coordination benefit. Under this reading,
 *   market dominance requires NO active enforcement; alternatives have faded
 *   through non-use and normalization of incumbent structures; extractiveness
 *   is low (coordination costs only); suppression is minimal (there is
 *   nothing actively suppressed). The sibling readings dispute this: the
 *   beneficiary_maintained_reading claims active defense by incumbent capital
 *   holders (snare); the hybrid_reading claims mixed maintenance. This story
 *   authors only the lapsed_alternative_reading as a clean, ε-invariant
 *   constraint.
 *
 * KEY AGENTS:
 *   - incumbent_market_holders: institutional, arbitrage exit — benefit from dominance through historical positioning but do not actively maintain it
 *   - potential_entrants: moderate power, constrained exit — face high barrier costs but the barrier is structural inertia, not active exclusion
 *   - consumers: organized, constrained exit — benefit from stability and standardization; do not perceive active suppression
 *   - regulators: institutional observers — struggle to identify whether concentration is natural or maintained
 *   - would_be_alternatives: powerless, excluded — organizational forms that could have competed but atrophied
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__lapsed_alternative_reading, 0.28).
domain_priors:suppression_score(market_naturalization__lapsed_alternative_reading, 0.15).
domain_priors:theater_ratio(market_naturalization__lapsed_alternative_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__lapsed_alternative_reading, piton).
narrative_ontology:human_readable(market_naturalization__lapsed_alternative_reading, "Market Dominance as Lapsed Closure (No Active Maintenance Reading)").
narrative_ontology:topic_domain(market_naturalization__lapsed_alternative_reading, "political_economy/economic_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__lapsed_alternative_reading, '864d54f2-484d-4672-a9ab-4be068d0e853').
narrative_ontology:cs_kernel_codification('864d54f2-484d-4672-a9ab-4be068d0e853', distributed).
narrative_ontology:cs_authority_grounding('864d54f2-484d-4672-a9ab-4be068d0e853', extraction).
narrative_ontology:cs_reading_relation('864d54f2-484d-4672-a9ab-4be068d0e853', market_naturalization__beneficiary_maintained_reading, forecloses).
narrative_ontology:cs_reading_relation('864d54f2-484d-4672-a9ab-4be068d0e853', market_naturalization__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('864d54f2-484d-4672-a9ab-4be068d0e853', foundational, dominance_structural_inertia).
narrative_ontology:cs_axiom_status(dominance_structural_inertia, holdable).
narrative_ontology:cs_axiom_grounding('864d54f2-484d-4672-a9ab-4be068d0e853', dominance_structural_inertia, empirically_contingent).
narrative_ontology:cs_axiom('864d54f2-484d-4672-a9ab-4be068d0e853', foundational, alternatives_atrophied_through_non_use).
narrative_ontology:cs_axiom_status(alternatives_atrophied_through_non_use, holdable).
narrative_ontology:cs_axiom_grounding('864d54f2-484d-4672-a9ab-4be068d0e853', alternatives_atrophied_through_non_use, empirically_contingent).
narrative_ontology:cs_reference_frame('864d54f2-484d-4672-a9ab-4be068d0e853', market_coordination_through_incumbent_dominance).
narrative_ontology:cs_drift_state('864d54f2-484d-4672-a9ab-4be068d0e853', contemporary_regulatory_scrutiny_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('864d54f2-484d-4672-a9ab-4be068d0e853', '').
narrative_ontology:cs_kernel_id(market_naturalization__lapsed_alternative_reading, market_naturalization).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_naturalization__lapsed_alternative_reading, consumers).
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, potential_entrants).
narrative_ontology:constraint_vindicates(market_naturalization__lapsed_alternative_reading, market_equilibrium_naturalism).
narrative_ontology:constraint_vindicates(market_naturalization__lapsed_alternative_reading, invisible_hand_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold dominant positions in their markets. Under this reading, they do not actively defend dominance through coordinated exclusionary practice — the reading treats their market positions as self-maintaining through historical inertia and structural convenience. They benefit from the arrangement but do not organizationally maintain it.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, incumbent_market_holders, agenda_setter,
    institutional, generational, arbitrage, global).

% Face high costs to enter concentrated markets. Under this reading, these costs are not imposed through active exclusion but through atrophied alternatives and the historical accumulation of scale advantages. They bear the cost of non-competition but the constraint carries no identifiable maintainer enforcing it.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, potential_entrants, payer,
    moderate, biographical, constrained, global).

% Receive stable, standardized goods and services from dominant incumbent firms. They benefit from the absence of turbulent market reorganization. Under this reading, they experience the arrangement as natural — products exist as they have always existed — and do not perceive active suppression.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, consumers, beneficiary,
    organized, biographical, constrained, global).

% Monitor market structure and competitive conditions. They struggle to identify whether observed concentration is the result of active exclusionary conduct or natural attrition of alternatives. Under this reading, enforcement is directed at symptoms (prices, service quality) rather than at maintaining incumbency — no causal mechanism exists to enforce.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, regulators, observer,
    institutional, generational, analytical, national).

% Organizational forms, business models, and supplier networks that could have competed but atrophied through non-use and normalization of incumbent structures. Under this reading, they are excluded not through active suppression but through the structural convenience of the existing arrangement — they fade rather than being actively suppressed.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, would_be_alternatives, excluded,
    powerless, immediate, trapped, local).

% Examine the historical trajectory of market concentration. They ask whether dominance was ever actively defended or whether it crystallized through path dependence and then required no maintenance — the temporal question this reading stakes.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__lapsed_alternative_reading, diffuse).
narrative_ontology:fixing_cost_class(market_naturalization__lapsed_alternative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides stable, standardized products and services at scale, with predictable pricing and universal availability. Eliminates the transaction costs of discovering and evaluating multiple competing suppliers; consumers benefit from the absence of constant market churn. Coordination remains functional but is now routine, requiring minimal active problem-solving.
% TRANSFER_FUNCTION: Concentrates market surplus (rents) in the hands of incumbent firms, primarily through locked-in consumer positions and atrophied supplier alternatives. The transfer is not actively extracted through enforcement but flows from structural inertia — consumers pay more than they would under competition, but no one is actively collecting this premium; it accrues to incumbents by default.
% ABSENT_VOICES: Alternative economic arrangements — producer cooperatives, mutualist supply networks, open-source models in proprietary domains, regional supplier networks — are structurally excluded because they atrophied and normalization of incumbent structures made their reemergence unthinkable, not because they are actively suppressed. They would argue for economic pluralism if present, but their absence is enforced by the historical record, not by active mechanisms.
% DISAPPEARANCE_RATIONALE: The lapsed_alternative_reading claims that if incumbency disappeared, alternatives would not spontaneously regenerate — the organizational knowledge and supply networks required to compete have atrophied. Market reorganization would require deliberate reconstruction of alternative structures, not merely removal of exclusionary rules. The beneficiary_maintained_reading argues reorganization would be swift if active suppression ceased; this reading argues it would be slow and costly because nothing active is being suppressed — the constraint is structural inertia. The hybrid_reading argues both mechanisms operate.
% FOUNDING_PROBLEM: Early market formation required coordination on standards, distribution networks, and trust in supplier quality. Dominant incumbents solved this through scale and integration. Over time, these solutions became the only structures market participants could imagine.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians studying the emergence of modern mass production (Ford, Standard Oil, railroads) document that dominance often arose from genuine coordination solutions to distribution and standardization problems (Chandler, Scale and Scope; Zuboff, Age of Surveillance Capital; contemporary sources 1890s–1920s). Modern observers and alternative-economy advocates attest that the founding problem is no longer live: modern distribution networks, quality certification, and communication technologies make comparable coordination possible at smaller scales and through non-dominant structures (open-source communities, direct-to-consumer platforms, third-party certification, online review aggregation). The beneficiary_maintained_reading disputes this, arguing that incumbents actively suppress alternatives to prevent the reconstruction Zuboff and others claim is technically possible.
narrative_ontology:disappearance_verdict(market_naturalization__lapsed_alternative_reading, contested).
narrative_ontology:founding_problem_status(market_naturalization__lapsed_alternative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__lapsed_alternative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_naturalization__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__lapsed_alternative_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is authored as LOW (0.28 at interval end) because under this reading, the constraint operates primarily to coordinate on standards and distribution — functions that generated real value during their formation and continue to do so at low incremental cost. The measured extraction reflects the consumer surplus captured by incumbents through locked-in positions, but this surplus accrues by default (inertia) rather than through active collection. Suppression is MINIMAL (0.15) because no one is actively suppressing alternatives — alternatives atrophied through structural convenience and normalization of incumbent structures. Theater_ratio is HIGH (0.72) because the bulk of what incumbents do is maintain the appearance and narrative of market legitimacy (innovation narratives, quality reputation, technological leadership) rather than actively defending dominance through exclusion. Accessibility_collapse is high (0.81) because once alternatives atrophy, re-entering the market becomes structurally unthinkable — the collapse is not imposed, it is historical. Resistance is LOW (0.22) because few agents resist a constraint they experience as natural and beneficial (consumers) or inevitable (potential entrants). The measurement series shows extractiveness and suppression rising slowly over the interval as regulatory pressure increases and the constraint requires more theatrical legitimation — the underlying machine is stable, but the maintenance narrative intensifies. Theater_ratio plateaus around t=60 when regulatory attention peaks.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (incumbents) and the payer seat (potential entrants) compute fundamentally differently under the structural data here: incumbents experience the arrangement as coordinate-then-coast (they solved a problem once, now benefit passively), while entrants experience it as a structural barrier without an identifiable suppressor (the constraint persists but no one is enforcing it against them, so their exclusion feels like market reality rather than exclusion). Regulators occupy an analytical seat that should perceive the distinction — they should be able to determine empirically whether they face a lapsed constraint (requiring reconstruction of alternatives) or an active snare (requiring removal of exclusionary conduct). The gap is resoluble but empirically demanding.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbents derive d near 0.0 (beneficiaries without defensive cost); potential entrants derive d near 0.75 (bear costs through excluded positioning, but the cost has no active imposer); consumers derive d near 0.5 (benefit from coordination, pay diffuse cost through locked-in pricing, no active extraction against them); would-be_alternatives derive d near 1.0 (fully targeted but through historical fade rather than active suppression — directionality remains high because they are excluded, even if the exclusion mechanism is lapsed). The directionality grammar here is the puzzle: if suppression is low and no one is actively imposing the constraint, do excluded agents still compute as targets? The schema answer is yes — directionality describes structural relationship to the constraint, independent of the mechanism maintaining it. Potential entrants face costs through this constraint whether it is actively maintained or lapsed.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading embodies an argument about MANDATROPHY at the meta-level: the founding problem (coordination on standards, distribution, quality) was LIVE and genuinely solved by incumbents' dominance. But the founding problem is now DEAD — modern technology, communication, and regulation have made equivalent coordination possible at smaller scales and through non-dominant structures (open standards, certification bodies, direct-to-consumer logistics, online reviews). The constraint persists despite the death of its founding problem because alternatives atrophied and structures normalized. This is the piton signature: low function, high persistence, no identifiable maintainer, high theater. Mandatrophy is resolved by recognizing that the persistence mechanism is historical inertia, not value-delivery — if alternatives were deliberately reconstructed, dominance would erode not through suppression-removal but through competitive alternatives proving viability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_vs_lapsed_maintenance,
    'Is market dominance actively defended through coordinated exclusionary conduct by incumbent capital holders, or does dominance persist passively through atrophied alternatives and structural inertia?',
    'Documentary evidence of coordinated exclusionary practices (cartels, predatory pricing, tie-in agreements, exclusive dealing); testimony from would-be entrants about whether they face legal/organizational barriers or economic unviability; economic modeling of counterfactual market structure if exclusionary mechanisms were removed vs. if incumbents ceased active defense.',
    'If active defense is the primary mechanism, the constraint reclassifies from piton (lapsed) to snare (actively extractive); if lapsed maintenance dominates, the piton reading is confirmed. Different policy implications: active defense requires antitrust enforcement; lapsed closures require reconstruction of alternative institutions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(active_vs_lapsed_maintenance, empirical, 'Whether observed market concentration is maintained actively or by inertia.').

omega_variable(
    beneficiary_identification_ambiguity,
    'Are there identifiable beneficiary classes that actively maintain market dominance for their own extraction benefit, or is dominance a residual institutional fact that accrues to incumbents without organized defense?',
    'Organizational analysis: do incumbent firms coordinate to defend dominance, or do they each pursue individual advantage while dominance persists as a side effect? Trade association and lobbying records; patterns of joint litigation or shared exclusionary contracts.',
    'If a unified beneficiary class exists and defends dominance, the reading flips toward snare (coordinated extraction). If maintenance is diffuse or nonexistent, piton is correct. The identification determines whether the constraint is predatory or merely inertial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification_ambiguity, empirical, 'Whether dominance benefits a coordinated class that actively maintains it.').

omega_variable(
    alternative_atrophy_mechanism,
    'Did alternatives to incumbent market structures atrophy through structural convenience (this reading), or through active suppression and normalization imposed by incumbents (the beneficiary_maintained_reading)?',
    'Historical analysis of why specific alternative business forms (cooperatives, mutualist networks, regional suppliers, open-source production) disappeared: timing of disappearance relative to incumbent dominance; whether disappearance coincides with exclusionary conduct or with technological shifts favoring scale; testimony from participants in extinct alternatives about whether they faced barriers or simply could not compete.',
    'If atrophy is structural, the lapsed_alternative_reading holds and policy should focus on reconstructing alternatives. If atrophy is imposed, the reading flips toward beneficiary_maintained_reading and policy should focus on removing barriers. This distinction drives the therapeutic agenda.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_atrophy_mechanism, empirical, 'Whether alternative organizational forms atrophied through competitive inability or through suppression.').

omega_variable(
    kernel_reading_constitution,
    'Does the kernel ''market_naturalization'' instantiate one constraint with three readings, or three distinct structural constraints in a constraint family?',
    'Structural analysis: the three sibling readings produce materially different ε values (lapsed ≈ 0.28, beneficiary_maintained ≈ 0.72, hybrid ≈ 0.58), different suppression profiles, different identified beneficiaries (or lack thereof), and different policy implications. Per the ε-invariance principle, if the observable used to evaluate the constraint changes the ε value materially, the observer is looking at different constraints. The kernel is the shared question (''what sustains dominance?''); the readings are instantiations of three different answers that constitute three different structural mechanisms.',
    'If the three readings are truly constituent (one constraint, multiple readings), the committer frame applies and the kernel structure should be documented in cs_structure.reading_relations and axioms (as authored here). If the readings constitute three distinct constraints, each should be authored as a separate story with its own constraint_id, linked via network.affects_constraints, and the kernel documented in each story''s commentary.kernel_context (not in cs_structure). The author judges this a genuine kernel reading; the omega documents the alternative interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_constitution, conceptual, 'The boundary between kernel-reading and constraint-family decomposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__lapsed_alternative_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__lapsed_alternative_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement_basis(mark_tr_t0, observed).
narrative_ontology:measurement(mark_tr_t20, market_naturalization__lapsed_alternative_reading, theater_ratio, 20, 0.61).
narrative_ontology:measurement_basis(mark_tr_t20, observed).
narrative_ontology:measurement(mark_tr_t40, market_naturalization__lapsed_alternative_reading, theater_ratio, 40, 0.66).
narrative_ontology:measurement_basis(mark_tr_t40, observed).
narrative_ontology:measurement(mark_tr_t60, market_naturalization__lapsed_alternative_reading, theater_ratio, 60, 0.71).
narrative_ontology:measurement_basis(mark_tr_t60, observed).
narrative_ontology:measurement(mark_tr_t80, market_naturalization__lapsed_alternative_reading, theater_ratio, 80, 0.73).
narrative_ontology:measurement_basis(mark_tr_t80, observed).
narrative_ontology:measurement(mark_tr_t100, market_naturalization__lapsed_alternative_reading, theater_ratio, 100, 0.72).
narrative_ontology:measurement_basis(mark_tr_t100, observed).
narrative_ontology:measurement(mark_tr_t120, market_naturalization__lapsed_alternative_reading, theater_ratio, 120, 0.72).
narrative_ontology:measurement_basis(mark_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__lapsed_alternative_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(mark_be_t0, observed).
narrative_ontology:measurement(mark_be_t20, market_naturalization__lapsed_alternative_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement_basis(mark_be_t20, observed).
narrative_ontology:measurement(mark_be_t40, market_naturalization__lapsed_alternative_reading, base_extractiveness, 40, 0.25).
narrative_ontology:measurement_basis(mark_be_t40, observed).
narrative_ontology:measurement(mark_be_t60, market_naturalization__lapsed_alternative_reading, base_extractiveness, 60, 0.28).
narrative_ontology:measurement_basis(mark_be_t60, observed).
narrative_ontology:measurement(mark_be_t80, market_naturalization__lapsed_alternative_reading, base_extractiveness, 80, 0.27).
narrative_ontology:measurement_basis(mark_be_t80, observed).
narrative_ontology:measurement(mark_be_t100, market_naturalization__lapsed_alternative_reading, base_extractiveness, 100, 0.29).
narrative_ontology:measurement_basis(mark_be_t100, observed).
narrative_ontology:measurement(mark_be_t120, market_naturalization__lapsed_alternative_reading, base_extractiveness, 120, 0.28).
narrative_ontology:measurement_basis(mark_be_t120, observed).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_naturalization__lapsed_alternative_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(mark_su_t0, observed).
narrative_ontology:measurement(mark_su_t20, market_naturalization__lapsed_alternative_reading, suppression_requirement, 20, 0.1).
narrative_ontology:measurement_basis(mark_su_t20, observed).
narrative_ontology:measurement(mark_su_t40, market_naturalization__lapsed_alternative_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement_basis(mark_su_t40, observed).
narrative_ontology:measurement(mark_su_t60, market_naturalization__lapsed_alternative_reading, suppression_requirement, 60, 0.14).
narrative_ontology:measurement_basis(mark_su_t60, observed).
narrative_ontology:measurement(mark_su_t80, market_naturalization__lapsed_alternative_reading, suppression_requirement, 80, 0.16).
narrative_ontology:measurement_basis(mark_su_t80, observed).
narrative_ontology:measurement(mark_su_t100, market_naturalization__lapsed_alternative_reading, suppression_requirement, 100, 0.15).
narrative_ontology:measurement_basis(mark_su_t100, observed).
narrative_ontology:measurement(mark_su_t120, market_naturalization__lapsed_alternative_reading, suppression_requirement, 120, 0.15).
narrative_ontology:measurement_basis(mark_su_t120, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__lapsed_alternative_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_naturalization__lapsed_alternative_reading, 0.12).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'market_naturalization'. Sibling readings (beneficiary_maintained_reading, hybrid_reading) instantiate different structural constraints with different ε values, suppression profiles, and identified beneficiaries. The three readings form a constraint family linked by shared kernel. Each reading should be consulted for the full debate; no single reading adjudicates the contested kernel. See omegas for the conceptual boundary question: whether these are true readings or a misidentified constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
