% ============================================================================
% CONSTRAINT STORY: market_naturalization__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: market_naturalization__lapsed_alternative_reading
 *   human_readable: Market Dominance as Lapsed Closure (No Active Maintenance)
 *   domain: political_economy/economic_history/institutional_analysis
 *
 * SUMMARY:
 *   This story instantiates the 'lapsed_alternative_reading' of the
 *   market_naturalization kernel: it treats an incumbent firm's persistent
 *   market dominance as a closure that formed during a genuine period of
 *   coordination-building (standard-setting, supply-chain consolidation) but
 *   has since lapsed into pure inertia. No active gatekeeping, lobbying, or
 *   exclusionary conduct maintains the position today; the position persists
 *   because rival infrastructure atrophied through disuse and no one has
 *   found it worth the coordination cost to rebuild it. This is deliberately
 *   NOT the same constraint as the 'beneficiary_maintained_reading' (where
 *   incumbents actively defend the position through lobbying, litigation, or
 *   predatory pricing) or the 'hybrid_reading' (partial active defense
 *   layered on partial atrophy) — those are separate constraints with their
 *   own ε values, linked here via network.affects_constraints and
 *   cs_structure.reading_relations, not folded into this one story's
 *   classification.
 *
 * KEY AGENTS:
 *   - incumbent_firms: agenda_setter (institutional/mobile) — occupies the dominant position but does not actively defend it
 *   - would_be_market_entrants: payer (moderate/constrained) — bears the cost of atrophied alternative infrastructure
 *   - consumers: beneficiary/payer (organized/mobile) — near-neutral net position
 *   - historians_of_the_market: observer (analytical/analytical) — traces the lapse from active closure to inertial default
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__lapsed_alternative_reading, 0.18).
domain_priors:suppression_score(market_naturalization__lapsed_alternative_reading, 0.12).
domain_priors:theater_ratio(market_naturalization__lapsed_alternative_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__lapsed_alternative_reading, piton).
narrative_ontology:human_readable(market_naturalization__lapsed_alternative_reading, "Market Dominance as Lapsed Closure (No Active Maintenance)").
narrative_ontology:topic_domain(market_naturalization__lapsed_alternative_reading, "political_economy/economic_history/institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__lapsed_alternative_reading, '83f38a0e-99ec-44f1-b2b7-58726eb98832').
narrative_ontology:cs_kernel_codification('83f38a0e-99ec-44f1-b2b7-58726eb98832', distributed).
narrative_ontology:cs_authority_grounding('83f38a0e-99ec-44f1-b2b7-58726eb98832', practice).
narrative_ontology:cs_interpretation_layer_present('83f38a0e-99ec-44f1-b2b7-58726eb98832').
narrative_ontology:cs_reading_relation('83f38a0e-99ec-44f1-b2b7-58726eb98832', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('83f38a0e-99ec-44f1-b2b7-58726eb98832', market_naturalization__hybrid_reading, influences).
narrative_ontology:cs_axiom('83f38a0e-99ec-44f1-b2b7-58726eb98832', foundational, dominance_persists_without_agency).
narrative_ontology:cs_axiom_status(dominance_persists_without_agency, holdable).
narrative_ontology:cs_axiom_grounding('83f38a0e-99ec-44f1-b2b7-58726eb98832', dominance_persists_without_agency, empirically_contingent).
narrative_ontology:cs_axiom('83f38a0e-99ec-44f1-b2b7-58726eb98832', secondary, atrophy_is_not_suppression).
narrative_ontology:cs_axiom_status(atrophy_is_not_suppression, holdable).
narrative_ontology:cs_axiom_grounding('83f38a0e-99ec-44f1-b2b7-58726eb98832', atrophy_is_not_suppression, conventional).
narrative_ontology:cs_reference_frame('83f38a0e-99ec-44f1-b2b7-58726eb98832', post_consolidation_settled_standard).
narrative_ontology:cs_drift_state('83f38a0e-99ec-44f1-b2b7-58726eb98832', contemporary_market_observation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('83f38a0e-99ec-44f1-b2b7-58726eb98832', '').
narrative_ontology:cs_kernel_id(market_naturalization__lapsed_alternative_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, would_be_market_entrants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_naturalization__lapsed_alternative_reading, consumers).
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, consumers).
narrative_ontology:constraint_vindicates(market_naturalization__lapsed_alternative_reading, market_dominance_as_historical_accident).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Occupies the dominant market position inherited from an earlier period of active consolidation, but no longer invests meaningfully in defending it — no lobbying campaigns to block entrants, no coordinated pricing strategy, no legal apparatus maintained specifically to exclude rivals. The position persists because switching costs, network habits, and informational defaults never got disturbed, not because anyone is spending resources to keep them undisturbed. Could in principle lower prices to squeeze entrants but does not bother; the dominance is coasting on inertia rather than being piloted.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, incumbent_firms, agenda_setter,
    institutional, generational, mobile, national).

% Face a market where the incumbent's position looks formidable but is not actively policed. Entry is hard mainly because distribution channels, consumer habit, and standard-setting infrastructure atrophied around the incumbent decades ago and nobody rebuilt alternative rails since — not because the incumbent moves to block them. The cost of overcoming this is real (coordination and rebuilding cost) but does not stem from targeted suppression.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, would_be_market_entrants, payer,
    moderate, biographical, constrained, national).

% Benefit from the stability and predictability of a settled market structure (known products, known standards, known supply chains) while bearing modest cost from reduced competitive pressure on price and innovation. Free to switch if a credible alternative emerged, but none has, so the practical benefit-cost balance is close to neutral.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, consumers, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(market_naturalization__lapsed_alternative_reading, consumers, payer).

% Study why the dominant firm's position persisted long after the specific conditions (subsidies, wartime contracts, patent monopolies, or early-mover network effects) that created it lapsed. Their analysis distinguishes structures that are actively defended from those that simply were never dislodged because no one found it worth the coordination cost to try.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, historians_of_the_market, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__lapsed_alternative_reading, diffuse).
narrative_ontology:fixing_cost_class(market_naturalization__lapsed_alternative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The settled market structure lets buyers and downstream firms rely on a single dominant standard/supplier without re-negotiating supply relationships or re-learning product interfaces each cycle — a genuine, if modest, coordination saving.
% TRANSFER_FUNCTION: Very little is transferred on an ongoing basis; what exists is a residual toll on would-be entrants who must independently rebuild distribution and habit infrastructure that used to exist in multiple parallel forms before consolidation, without any active gatekeeper collecting rents from that toll.
% ABSENT_VOICES: Historical rivals whose alternative supply chains, standards, or distribution networks atrophied through disuse are not present to argue for their revival — they folded or diversified away decades ago and left no organized constituency to reassert the alternative.
% DISAPPEARANCE_RATIONALE: If the dominant firm vanished overnight, some argue the market would barely rearrange — the alternatives are gone, not suppressed, so nothing latent would spring back; others argue disappearance would immediately reveal how much of the current structure depended on the incumbent's mere continued existence as a coordination point, causing real short-term disruption even without any active defense mechanism to remove.
% FOUNDING_PROBLEM: The dominant position was originally built to solve a real coordination problem — establishing one interoperable standard/supply chain during a period when multiple incompatible options were fragmenting the market and raising costs for everyone.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians outside the incumbent firm attest that the original fragmentation problem was resolved decades ago and that no comparable coordination failure currently threatens the market; the incumbent itself makes no such claim because it does not campaign to justify its position at all — the absence of a defensive narrative is itself evidence for the lapsed reading.
narrative_ontology:disappearance_verdict(market_naturalization__lapsed_alternative_reading, contested).
narrative_ontology:founding_problem_status(market_naturalization__lapsed_alternative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__lapsed_alternative_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_naturalization__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__lapsed_alternative_reading, 0.18, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is low and falling over the interval (0.45 -> 0.18) reflecting the thesis: whatever extraction existed during the active-consolidation era has decayed as the mechanism shifted from deliberate exclusion to mere non-use of alternatives. Suppression follows the same declining trajectory (0.40 -> 0.12) because there is no ongoing enforcement apparatus to sustain it — what suppression exists is a residual echo of the original closure, not fresh coercive investment. Theater ratio is moderate-to-low and declining (0.35 -> 0.22): some early-period justificatory rhetoric about 'natural' market leadership persists but is not actively refreshed. Accessibility collapse remains fairly high (0.62) because alternatives really have atrophied structurally — that collapse is a fact about disuse, not about ongoing suppression, which is the structural signature this reading is built to capture.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent's seat, the position looks like nothing at all — it is simply where things settled, with no active choices being made to sustain it. From the entrant's seat, the same structure looks like a wall, even though no one built it recently. The engine should register this asymmetry not as evidence of hidden active maintenance but as the ordinary experience-gap between an inertial default and the cost of displacing one.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary group is declared because this reading's defining claim is that no one is currently capturing rents from the dominance — incumbent_firms hold agenda_setter power but are not coded as beneficiaries of an active extraction flow, only as the residual occupant of a position they no longer invest in maintaining. would_be_market_entrants are the nearest thing to victims: they pay a real cost (rebuilding atrophied infrastructure) but that cost is diffuse and structural, not the product of targeted suppression, which keeps their directionality closer to the coordination-cost end than a captured-target reading would.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordination-standard fragmentation) is genuinely dead, and the arrangement persists anyway — the classic mandatrophy signature. But because there is no identifiable beneficiary class currently profiting from that persistence, this reading resists reclassification to snare or tangled_rope; the correct diagnosis is piton (atrophied function, inertial persistence, theatrical residue only) rather than active capture. This is precisely the distinction the kernel's three readings exist to force apart: the same observable market structure supports a piton diagnosis under this reading and a tangled_rope or snare diagnosis under the sibling beneficiary_maintained_reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lapsed_vs_maintained_ambiguity,
    'Is the observed market dominance genuinely free of active maintenance, or does apparent passivity mask low-visibility defensive conduct (e.g. tacit coordination, informal signaling to potential entrants, or infrastructure lock-in effects the incumbent benefits from without needing to act)?',
    'Forensic examination of the incumbent''s conduct record: absence of exclusionary contracts, absence of predatory pricing episodes, absence of lobbying expenditure specifically targeting entry barriers, corroborated by independent antitrust or historical-economic review.',
    'If low-visibility maintenance is found, this story''s classification should shift toward the hybrid_reading or beneficiary_maintained_reading sibling constraints rather than remaining piton; the lapsed reading is only correct if the absence of maintenance is genuine, not merely undetected.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lapsed_vs_maintained_ambiguity, empirical, 'Whether the incumbent''s apparent non-maintenance is real or merely unobserved.').

omega_variable(
    committer_framing_choice,
    'Why was the lapsed_alternative_reading selected as the operative framing here rather than the hybrid_reading, given that most real-world dominant-firm histories show SOME residual defensive conduct alongside atrophy?',
    'This omega documents the committer-axis choice itself: the lapsed reading was selected because the narrative material specifies ''no active maintenance'' and ''low extractiveness (coordination costs only)'' as the expected structural delta; a hybrid framing would require evidence of at least partial active defense, which the source material explicitly excludes for this particular story instance.',
    'If future evidence surfaces of partial active defense (targeted litigation, coordinated pricing signals), this story should be retired in favor of the hybrid_reading sibling constraint rather than amended in place — per the ε-invariance principle, a change in the underlying facts produces a different constraint, not a revised value in this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_framing_choice, conceptual, 'Documents why this reading (not its siblings) was instantiated, and what would trigger migration to a sibling reading.').

omega_variable(
    no_beneficiary_stability,
    'Can a market structure with a clear agenda_setter (the incumbent) genuinely have no beneficiary class over a 40-year interval, or does the absence of active maintenance simply mean the incumbent captures a smaller, more diffuse benefit (avoided competition) that the beneficiaries array under-states?',
    'Comparative profitability analysis: does the incumbent''s margin structure show persistent above-competitive returns attributable to the dominance itself, even absent active defense? A persistent margin premium would suggest an unearned-but-real beneficiary relationship despite the lack of active maintenance.',
    'A demonstrated margin premium would require adding incumbent_firms to base_properties.beneficiaries, which would trigger re-evaluation as tangled_rope (coordination for consumers + asymmetric extraction for the margin premium) rather than piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(no_beneficiary_stability, empirical, 'Whether the incumbent quietly benefits from the dominance despite not actively maintaining it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__lapsed_alternative_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__lapsed_alternative_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(mark_tr_t8, market_naturalization__lapsed_alternative_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(mark_tr_t16, market_naturalization__lapsed_alternative_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(mark_tr_t24, market_naturalization__lapsed_alternative_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(mark_tr_t32, market_naturalization__lapsed_alternative_reading, theater_ratio, 32, 0.23).
narrative_ontology:measurement(mark_tr_t40, market_naturalization__lapsed_alternative_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__lapsed_alternative_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(mark_be_t8, market_naturalization__lapsed_alternative_reading, base_extractiveness, 8, 0.32).
narrative_ontology:measurement(mark_be_t16, market_naturalization__lapsed_alternative_reading, base_extractiveness, 16, 0.24).
narrative_ontology:measurement(mark_be_t24, market_naturalization__lapsed_alternative_reading, base_extractiveness, 24, 0.2).
narrative_ontology:measurement(mark_be_t32, market_naturalization__lapsed_alternative_reading, base_extractiveness, 32, 0.19).
narrative_ontology:measurement(mark_be_t40, market_naturalization__lapsed_alternative_reading, base_extractiveness, 40, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_naturalization__lapsed_alternative_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(mark_su_t8, market_naturalization__lapsed_alternative_reading, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(mark_su_t16, market_naturalization__lapsed_alternative_reading, suppression_requirement, 16, 0.2).
narrative_ontology:measurement(mark_su_t24, market_naturalization__lapsed_alternative_reading, suppression_requirement, 24, 0.15).
narrative_ontology:measurement(mark_su_t32, market_naturalization__lapsed_alternative_reading, suppression_requirement, 32, 0.13).
narrative_ontology:measurement(mark_su_t40, market_naturalization__lapsed_alternative_reading, suppression_requirement, 40, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__lapsed_alternative_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_naturalization__lapsed_alternative_reading, 0.1).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the market_naturalization kernel. beneficiary_maintained_reading claims active defense by incumbent capital holders (tangled_rope/snare shape, requires_active_enforcement: true, named beneficiary). hybrid_reading claims a mixture of lapsed and actively-maintained elements (likely tangled_rope with partial enforcement). This story (lapsed_alternative_reading) claims no active maintenance, no beneficiary class, and low extractiveness limited to residual coordination costs (piton shape). Each carries its own ε, stakeholder set, and classification; they are linked here for contamination-propagation analysis, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
