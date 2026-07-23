% ============================================================================
% CONSTRAINT STORY: exile_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exile_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: exile_reading
 *   human_readable: Discretionary Monetary Authority Held Apart From Constitutional Constraint
 *   domain: constitutional political economy / monetary governance
 *
 * SUMMARY:
 *   This story instantiates the 'exile' reading of the
 *   money-governance-coupling kernel: monetary authority is deliberately
 *   EXILED from constitutional constraint — held apart, delegated to a
 *   discretionary institution trusted to judge case-by-case rather than bound
 *   by rule or fused into ordinary constitutional accountability. The exile
 *   reading's distinctive failure mode is capture-by-discretion: because the
 *   authority's whole legitimacy rests on the claim that judgment cannot be
 *   reduced to rule, any capture of that judgment (by proximate financial
 *   intermediaries, by crisis-driven precedent-setting) does not show up as a
 *   concentrated stakeholder grab but as a diffuse burden distributed across
 *   current currency holders and, more severely, across future rule-takers
 *   bound by precedents set before they existed. This is structurally
 *   distinct from the fusion reading (where money and governance are
 *   integrated and capture is capture-by-capital of a fused mechanism) and
 *   the adjacency reading (where money and governance sit side by side with
 *   negotiated boundaries). Extraction here rides on a real coordination
 *   function — solving legislative time-inconsistency — but the boundary
 *   between 'necessary insulation' and 'unaccountable capture' has drifted as
 *   discretionary precedent has accumulated.
 *
 * KEY AGENTS:
 *   - central_bank_technocracy: agenda_setter/beneficiary (institutional/arbitrage) — administers discretion, insulated from override
 *   - incumbent_financial_intermediaries: beneficiary (organized/mobile) — privileged proximity to discretionary decisions
 *   - currency_holders: payer (powerless/trapped) — bear diffuse debasement and redistribution costs
 *   - future_rule_takers: payer (powerless/trapped) — inherit binding precedent set without their consent
 *   - legislature: excluded (powerful/constrained) — formally sovereign, practically deferential
 *   - constitutional_economists: observer (analytical/analytical) — assess long-run stability of the exile arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exile_reading, 0.62).
domain_priors:suppression_score(exile_reading, 0.58).
domain_priors:theater_ratio(exile_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exile_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(exile_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(exile_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exile_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(exile_reading, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exile_reading, tangled_rope).
narrative_ontology:human_readable(exile_reading, "Discretionary Monetary Authority Held Apart From Constitutional Constraint").
narrative_ontology:topic_domain(exile_reading, "constitutional political economy / monetary governance").

domain_priors:requires_active_enforcement(exile_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exile_reading, 'c94e23b6-9f4f-4daa-a195-784516d91278').
narrative_ontology:cs_kernel_codification('c94e23b6-9f4f-4daa-a195-784516d91278', distributed).
narrative_ontology:cs_authority_grounding('c94e23b6-9f4f-4daa-a195-784516d91278', extraction).
narrative_ontology:cs_interpretation_layer_present('c94e23b6-9f4f-4daa-a195-784516d91278').
narrative_ontology:cs_reading_relation('c94e23b6-9f4f-4daa-a195-784516d91278', money_governance_coupling__fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('c94e23b6-9f4f-4daa-a195-784516d91278', money_governance_coupling__adjacency_reading, influences).
narrative_ontology:cs_axiom('c94e23b6-9f4f-4daa-a195-784516d91278', foundational, discretion_irreducible_to_rule).
narrative_ontology:cs_axiom_status(discretion_irreducible_to_rule, holdable).
narrative_ontology:cs_axiom_grounding('c94e23b6-9f4f-4daa-a195-784516d91278', discretion_irreducible_to_rule, instrumental).
narrative_ontology:cs_axiom('c94e23b6-9f4f-4daa-a195-784516d91278', foundational, insulation_from_constitutional_override_is_legitimate_precommitment).
narrative_ontology:cs_axiom_status(insulation_from_constitutional_override_is_legitimate_precommitment, holdable).
narrative_ontology:cs_axiom_grounding('c94e23b6-9f4f-4daa-a195-784516d91278', insulation_from_constitutional_override_is_legitimate_precommitment, conventional).
narrative_ontology:cs_reference_frame('c94e23b6-9f4f-4daa-a195-784516d91278', legislative_supremacy_over_currency).
narrative_ontology:cs_drift_state('c94e23b6-9f4f-4daa-a195-784516d91278', post_independence_era_precedent_accumulation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c94e23b6-9f4f-4daa-a195-784516d91278', '').
narrative_ontology:cs_kernel_id(exile_reading, money_governance_coupling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exile_reading, central_bank_technocracy).
narrative_ontology:constraint_beneficiary(exile_reading, incumbent_financial_intermediaries).
narrative_ontology:constraint_victim(exile_reading, currency_holders).
narrative_ontology:constraint_victim(exile_reading, future_rule_takers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets policy rates, reserve requirements, and emergency liquidity terms case-by-case, insulated from ordinary constitutional amendment or legislative override. Justifies this insulation as necessary for credible, depoliticized judgment. Enjoys wide discretion to redefine its own mandate during crises, and its institutional prestige and staff careers are built on the presumption that its judgment cannot be reduced to rule.
narrative_ontology:constraint_stakeholder(exile_reading, central_bank_technocracy, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(exile_reading, central_bank_technocracy, beneficiary).

% Sit closest to the discretionary decision point — primary dealers, systemically important banks — and receive early information and privileged access to facilities the discretionary authority creates ad hoc. Can lobby the agenda setter directly and can exit into other jurisdictions or asset classes if domestic discretion turns against them, unlike ordinary currency users.
narrative_ontology:constraint_stakeholder(exile_reading, incumbent_financial_intermediaries, beneficiary,
    organized, biographical, mobile, national).

% Hold and transact in the currency whose value the discretionary authority manages by judgment rather than by rule. Bear the diffuse cost of unpredictable inflation, bailout-driven currency debasement, and redistribution toward asset holders whenever discretionary intervention favors financial stability over price stability. Have no meaningful exit short of full currency substitution, which is itself often restricted or costly.
narrative_ontology:constraint_stakeholder(exile_reading, currency_holders, payer,
    powerless, biographical, trapped, national).

% Inherit precedents set by today's discretionary judgment calls — bailout thresholds, inflation tolerances, crisis-response templates — without having participated in their formation. Are bound by accumulated discretionary precedent that functions as de facto constitutional material despite never passing through any constitutional process; they cannot renegotiate decisions made before they had standing.
narrative_ontology:constraint_stakeholder(exile_reading, future_rule_takers, payer,
    powerless, generational, trapped, national).

% Formally holds the power to constrain monetary authority but has ceded operational control to preserve credibility and avoid short-term political capture of interest rate policy. Can threaten mandate revision but rarely follows through, since doing so would itself look like the political interference the discretionary arrangement was built to prevent.
narrative_ontology:constraint_stakeholder(exile_reading, legislature, excluded,
    powerful, biographical, constrained, national).

% Study whether discretionary monetary authority held outside constitutional constraint is a stable equilibrium or a slow-motion capture mechanism. Compare rule-bound monetary regimes (currency boards, algorithmic issuance) against discretionary regimes to assess long-run outcomes for price stability and distributional effects.
narrative_ontology:constraint_stakeholder(exile_reading, constitutional_economists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Delegating monetary judgment to an institution insulated from day-to-day political pressure solves a genuine credibility problem: elected officials facing short election cycles have a structural incentive to inflate away debt or juice growth before votes, and a rule-bound or politically exposed monetary authority cannot credibly resist that pressure case-by-case during genuine crises that no ex ante rule anticipated.
% TRANSFER_FUNCTION: Moves the power to decide who bears the cost of monetary adjustment — inflation, bailout terms, liquidity access — from a constitutionally constrained, accountable process to a discretionary body whose case-by-case judgment calls accumulate into binding precedent for future currency holders and rule-takers who never consented to them.
% ABSENT_VOICES: Future rule-takers are definitionally absent — they do not yet exist as a political constituency and cannot object to precedents being set now. Ordinary currency holders are present but diffuse and rarely organized enough to contest specific discretionary calls (a bailout, a facility, a tolerance threshold) in real time; only the immediate winners of any given intervention are organized enough to be in the room.
% DISAPPEARANCE_RATIONALE: If discretionary insulation were removed and monetary authority folded back under ordinary constitutional constraint, the central bank technocracy and incumbent intermediaries would lose the ability to respond to novel crises without political veto, and they argue the world would rearrange badly (credibility collapse, run risk). Constitutional economists and currency holders dispute this, arguing that many discretionary interventions themselves created the instability they later claimed credit for managing — the verdict depends on which counterfactual crisis history you credit.
% FOUNDING_PROBLEM: Elected legislatures cannot credibly commit to sound money because short electoral horizons create a structural temptation to inflate; discretionary independence was built to solve this time-inconsistency problem by removing monetary judgment from direct political control.
% FOUNDING_PROBLEM_CORROBORATION: Central bank technocracy and incumbent intermediaries attest the time-inconsistency problem remains fully live and cite ongoing political pressure for rate cuts as evidence. Constitutional economists, writing from outside the beneficiary set, attest that the original problem has been substantially supplanted by a different one — discretionary authority now functions less as a constraint on legislative temptation and more as an unconstrained decision node capturable by whichever organized interest is best positioned to reach it in a crisis; several point to repeated ad hoc facility creation during liquidity crises as evidence the discretion has outgrown its founding justification.
narrative_ontology:disappearance_verdict(exile_reading, contested).
narrative_ontology:founding_problem_status(exile_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exile_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-23',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(exile_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exile_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exile_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exile_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exile_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored as substantially but not overwhelmingly high (0.62 at interval end) because a genuine coordination function — credible commitment against inflationary temptation — persists throughout; this is not authored as a pure snare. Suppression (0.58) reflects the structural insulation mechanism (legal independence, mandate walls) that prevents constitutional override, not brute coercion. Theater ratio is moderate-low (0.31): discretionary judgment does perform real crisis-response work, but a growing share of activity (justificatory communications, forward guidance theater, post-hoc rationalization of ad hoc facilities) is performative maintenance of the insulation's legitimacy rather than the crisis-response function itself. All three series rise together over the 40-unit interval, reflecting the diagnosed drift: discretionary precedent accumulates, each crisis response becomes a template for the next, and the gap between the founding time-inconsistency problem and the arrangement's current operation widens.
 *
 * PERSPECTIVAL GAP:
 *   From the central bank technocracy's seat, this is coordination solving a real time-inconsistency problem — a rope, or at worst a scaffold awaiting a rule-bound successor that never quite arrives. From the currency holder and future rule-taker seats, the same structure is enforced diffuse extraction: costs land on people with no seat at the table and no capacity to renegotiate precedent set before they had standing. The engine should compute divergent per-seat types from this same structural data; the claimed_type (tangled_rope) is the analytical middle reading — genuine coordination function AND asymmetric extraction, both present, requiring active enforcement (legal insulation) to hold.
 *
 * DIRECTIONALITY LOGIC:
 *   Central bank technocracy sits nearest full beneficiary: it administers the discretion, and its institutional survival depends on the insulation persisting. Incumbent financial intermediaries benefit through privileged proximity and can exit into other jurisdictions if the arrangement turns against them — this mobility is what keeps their derived d well below the trapped end despite being close to the discretionary mechanism. Currency holders and future rule-takers are the true targets: trapped, powerless, and bearing costs (debasement, precedent-lock) that are structurally diffuse rather than concentrated on any single identifiable class — this diffuseness is the exile reading's signature, distinguishing it from the fusion reading's concentrated stakeholder-class victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legislative time-inconsistency) has not fully disappeared — it remains partially live, which is why founding_problem_status is authored as contested rather than dead. But the accumulation of discretionary precedent has partially decoupled the arrangement's operation from that founding justification: what began as insulation against a specific temptation has become a general license for case-by-case judgment whose scope keeps expanding with each crisis. Classifying this as tangled_rope rather than snare prevents mislabeling a partially-still-functioning coordination mechanism as pure extraction; classifying it as tangled_rope rather than rope prevents ignoring the asymmetric, diffuse victim class that has emerged as discretion has drifted from its founding scope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    insulation_still_solving_founding_problem,
    'Does the discretionary insulation still solve the legislative time-inconsistency problem it was built for, or has the insulation itself become the primary source of the extraction it was meant to prevent?',
    'Compare monetary outcomes (inflation variance, crisis-response consistency, distributional effects) across jurisdictions with strict rule-bound regimes, discretionary-insulated regimes, and hybrid regimes over multiple crisis cycles; examine whether discretionary facilities created in one crisis systematically expand in subsequent crises.',
    'If the founding problem is substantially solved and residual discretion is mostly precedent-accumulation without a corresponding coordination benefit, the classification should drift toward snare over time; if the time-inconsistency problem remains acute and discretion is still doing real work, tangled_rope with a genuine coordination component remains the accurate reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insulation_still_solving_founding_problem, empirical, 'Whether discretionary insulation still performs its founding coordination function or has become self-perpetuating.').

omega_variable(
    kernel_reading_selection,
    'Is the money_governance_coupling kernel better modeled as exile (discretion held apart, diffuse victim set), fusion (money and governance integrated, concentrated capital-holding victim class), or adjacency (negotiated, revisable boundary)? Different jurisdictions and historical periods may instantiate different readings simultaneously.',
    'Comparative institutional analysis: does the jurisdiction''s monetary authority face binding constitutional override mechanisms (adjacency), operate as an arm of fiscal/political authority (fusion), or maintain formal-legal independence with only reputational constraint (exile)? Track whether victim harms concentrate on identifiable capital-holding classes or diffuse across the general currency-holding population.',
    'Selecting the wrong reading for a given real-world monetary authority would misattribute victim structure — treating a fusion-type capture as exile-type diffuse harm would obscure the concentrated beneficiary class that fusion readings are built to surface, and vice versa.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Documents that this story is one reading among three of a contested kernel; the reading choice is not empirically forced in every real jurisdiction.').

omega_variable(
    future_ratetaker_representability,
    'Can the interests of future rule-takers be represented in present discretionary decision-making at all, given they do not yet exist as a political constituency, or is their exclusion structurally irreducible regardless of institutional design?',
    'Examine mechanisms in other domains (long-term environmental governance, sovereign debt covenants) that attempt to bind or represent future-interest parties; assess whether analogous mechanisms (rule-bound monetary constitutions, algorithmic issuance caps) meaningfully reduce the future rule-taker burden versus merely relocating discretion to the rule-design stage.',
    'If future-interest representation is structurally impossible, the diffuse-victim signature of the exile reading is a permanent feature, not a fixable defect — which would argue for extreme caution in expanding discretionary scope rather than for representational reform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_ratetaker_representability, conceptual, 'Whether the exile reading''s diffuse future-victim structure is fixable or structurally irreducible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exile_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exil_tr_t0, exile_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(exil_tr_t8, exile_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(exil_tr_t16, exile_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(exil_tr_t24, exile_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement(exil_tr_t32, exile_reading, theater_ratio, 32, 0.29).
narrative_ontology:measurement(exil_tr_t40, exile_reading, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(exil_be_t0, exile_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(exil_be_t8, exile_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(exil_be_t16, exile_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(exil_be_t24, exile_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(exil_be_t32, exile_reading, base_extractiveness, 32, 0.59).
narrative_ontology:measurement(exil_be_t40, exile_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(exil_su_t0, exile_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(exil_su_t8, exile_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(exil_su_t16, exile_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(exil_su_t24, exile_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(exil_su_t32, exile_reading, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(exil_su_t40, exile_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exile_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(exile_reading, 0.12).
narrative_ontology:affects_constraint(exile_reading, fusion_reading).
narrative_ontology:affects_constraint(exile_reading, adjacency_reading).

% DUAL FORMULATION NOTE:
% exile_reading, fusion_reading, and adjacency_reading are three structurally distinct instantiations of the money_governance_coupling kernel, each with its own epsilon and victim structure. exile_reading models discretion held apart from constitutional constraint (diffuse victims: current currency holders, future rule-takers). fusion_reading models money and governance integrated into one mechanism (concentrated victims: an identifiable capital-holding stakeholder class). adjacency_reading models money and governance as side-by-side with negotiated, revisable boundaries. All three are linked here per the ε-invariance principle: they are not one constraint measured three ways but three constraints sharing a contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
