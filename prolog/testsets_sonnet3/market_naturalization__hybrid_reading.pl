% ============================================================================
% CONSTRAINT STORY: market_naturalization__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Market Dominance as Naturalized Arrangement (Hybrid Reading)
 *   domain: political economy / economic history / institutional analysis
 *
 * SUMMARY:
 *   This story reads market dominance not as a wholly lapsed closure (the
 *   'it's just settled, nobody defends it' story) nor as a wholly maintained
 *   snare (the 'active capture, full stop' story), but as a genuine hybrid:
 *   some of the advantage the dominant firm holds has settled into background
 *   market expectation that requires no active defense, while another
 *   substantial portion is a live product of contractual exclusivity, legal
 *   action, and lobbying that would erode within a few years if withdrawn.
 *   The rising theater_ratio and suppression_requirement series reflect a
 *   structure where the naturalized cover story (efficiency, scale economies)
 *   increasingly papers over a growing active-maintenance component — the mix
 *   shifts toward maintenance over time even as parts of the base remain
 *   genuinely settled.
 *
 * KEY AGENTS:
 *   - incumbent_platform_operators: agenda_setter/beneficiary — administers active maintenance mechanisms and collects returns
 *   - dominant_firm_shareholders: beneficiary — collects without directly administering
 *   - would_be_market_entrants: payer — bears foreclosed opportunity from both settled expectation and active barriers
 *   - downstream_suppliers: payer — dependent buyer relationship, mixed lapsed/maintained
 *   - regulatory_agencies: observer/excluded — structurally unable to cleanly separate lapsed from maintained components
 *   - consumers: beneficiary/payer — genuine coordination benefit plus rent premium, indistinguishable at point of purchase
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__hybrid_reading, 0.52).
domain_priors:suppression_score(market_naturalization__hybrid_reading, 0.48).
domain_priors:theater_ratio(market_naturalization__hybrid_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__hybrid_reading, "Market Dominance as Naturalized Arrangement (Hybrid Reading)").
narrative_ontology:topic_domain(market_naturalization__hybrid_reading, "political economy / economic history / institutional analysis").

domain_priors:requires_active_enforcement(market_naturalization__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__hybrid_reading, 'f6ceba76-8fb9-41a7-9994-0edbedb5d56f').
narrative_ontology:cs_kernel_codification('f6ceba76-8fb9-41a7-9994-0edbedb5d56f', distributed).
narrative_ontology:cs_authority_grounding('f6ceba76-8fb9-41a7-9994-0edbedb5d56f', distributed).
narrative_ontology:cs_reading_relation('f6ceba76-8fb9-41a7-9994-0edbedb5d56f', market_naturalization__lapsed_alternative_reading, influences).
narrative_ontology:cs_reading_relation('f6ceba76-8fb9-41a7-9994-0edbedb5d56f', market_naturalization__beneficiary_maintained_reading, influences).
narrative_ontology:cs_axiom('f6ceba76-8fb9-41a7-9994-0edbedb5d56f', foundational, dominance_is_structurally_composite).
narrative_ontology:cs_axiom_status(dominance_is_structurally_composite, holdable).
narrative_ontology:cs_axiom_grounding('f6ceba76-8fb9-41a7-9994-0edbedb5d56f', dominance_is_structurally_composite, empirically_contingent).
narrative_ontology:cs_axiom('f6ceba76-8fb9-41a7-9994-0edbedb5d56f', secondary, maintenance_component_is_independently_falsifiable).
narrative_ontology:cs_axiom_status(maintenance_component_is_independently_falsifiable, holdable).
narrative_ontology:cs_axiom_grounding('f6ceba76-8fb9-41a7-9994-0edbedb5d56f', maintenance_component_is_independently_falsifiable, empirically_contingent).
narrative_ontology:cs_reference_frame('f6ceba76-8fb9-41a7-9994-0edbedb5d56f', efficiency_scale_coordination_baseline).
narrative_ontology:cs_drift_state('f6ceba76-8fb9-41a7-9994-0edbedb5d56f', contemporary_platform_consolidation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f6ceba76-8fb9-41a7-9994-0edbedb5d56f', '').
narrative_ontology:cs_kernel_id(market_naturalization__hybrid_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, incumbent_platform_operators).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, dominant_firm_shareholders).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, would_be_market_entrants).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, downstream_suppliers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, consumers).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, consumers).
narrative_ontology:constraint_vindicates(market_naturalization__hybrid_reading, market_concentration_reflects_efficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Occupy the dominant position in the market and actively defend it through selective contracting, exclusivity terms, litigation against interoperability mandates, and lobbying against structural remedies. Some of their advantage rests on original network effects that have genuinely lapsed into settled fact (users no longer weigh alternatives because none are visible), but a substantial portion is actively re-manufactured each year through contract terms and legal action that would not survive if unenforced.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, incumbent_platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_naturalization__hybrid_reading, incumbent_platform_operators, beneficiary).

% Collect the returns generated by the dominant position without directly administering its maintenance. Have diversified enough that any single market's competitive dynamics matter less to them than the aggregate; can rotate capital toward whichever incumbent is winning.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, dominant_firm_shareholders, beneficiary,
    organized, biographical, arbitrage, global).

% Attempt to enter the market with a comparable or superior offering but find that switching costs, exclusivity agreements, and a public perception that the incumbent's position is simply how the market naturally is combine to keep customer acquisition costs prohibitive. Some of this barrier is genuinely settled expectation; some is a live legal and contractual wall built and rebuilt by the incumbent's counsel.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, would_be_market_entrants, payer,
    moderate, biographical, constrained, national).

% Depend on the dominant firm as the primary buyer or distribution channel for their output and must accept its terms because no comparably scaled alternative buyer exists. Whether this dependency is a lapsed structural fact (there simply is no other buyer of this scale) or an actively engineered chokepoint (the dominant firm suppresses would-be alternative buyers through exclusive deals) is genuinely mixed by product line.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, downstream_suppliers, payer,
    moderate, biographical, constrained, national).

% Investigate the market's structure periodically but face a genealogical puzzle: enforcement actions that would be justified against active maintenance look like industrial policy overreach against arrangements that have merely lapsed into settled equilibrium. Their evidentiary tools are not well suited to separating the two, so enforcement is intermittent and contested, and they are frequently kept out of the more technical contractual disputes that matter most.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, regulatory_agencies, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(market_naturalization__hybrid_reading, regulatory_agencies, excluded).

% Benefit from the standardization, reliability, and scale economies the dominant arrangement genuinely delivers, while also paying a premium relative to a more competitive counterfactual. Cannot easily tell how much of the price they pay reflects real coordination value versus rent, because the naturalized story and the maintained story are not visibly distinguishable from the point of purchase.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, consumers, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(market_naturalization__hybrid_reading, consumers, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__hybrid_reading, incumbent_platform_operators).
narrative_ontology:fixing_cost_class(market_naturalization__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The dominant arrangement genuinely solves real coordination problems in parts of the market — standardized interfaces, predictable logistics, aggregated demand that lets suppliers plan — that would be costly to reproduce from scratch if the incumbent vanished.
% TRANSFER_FUNCTION: Moves surplus from would-be entrants (foreclosed opportunity), downstream suppliers (below-competitive terms), and consumers (premium pricing) to incumbent operators and their shareholders, through a combination of settled market expectations that no longer need defending and contractual/legal mechanisms that are actively renewed.
% ABSENT_VOICES: Would-be entrants who exited the market or never entered are not visible in current market data and rarely testify in regulatory proceedings; downstream suppliers with the most to lose from confronting their primary buyer are structurally reluctant to be named complainants.
% DISAPPEARANCE_RATIONALE: Incumbents and their allies would argue the market structure reflects settled efficiency and would barely notice a change in doctrine; entrants, suppliers, and independent economists argue that removing active enforcement mechanisms (exclusivity terms, interoperability barriers) would measurably reopen competitive dynamics within a few years, even though some network-effect advantages would persist regardless. The parties do not agree on how much of the current structure is which kind.
% FOUNDING_PROBLEM: Early market consolidation solved genuine coordination problems: fragmented, incompatible offerings imposed real costs on users and suppliers, and a dominant standard-setter reduced those costs substantially.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent operators attest the founding coordination problem remains live and justifies continued consolidation. Antitrust economists and several regulatory agency reports (produced independently of incumbent funding) attest that the original coordination problem was substantially solved decades ago and that a meaningful share of current market structure is now actively maintained rent extraction rather than residual coordination benefit — though even these outside sources disagree on the precise ratio.
narrative_ontology:disappearance_verdict(market_naturalization__hybrid_reading, contested).
narrative_ontology:founding_problem_status(market_naturalization__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_naturalization__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__hybrid_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at a moderate 0.52 — lower than a pure-maintenance snare reading would claim, because a genuine coordination residue (network effects, standardization) is real and untaxed at the margin, but higher than a pure-lapsed reading would claim, because active enforcement mechanisms (exclusivity contracts, litigation, lobbying) demonstrably still do work. Theater ratio at 0.42 reflects that a meaningful share of the 'efficiency' narrative is performative cover for the actively maintained component, while suppression at 0.48 reflects real but not overwhelming active barrier-building — distinguishable from a mountain's near-zero suppression and from a full snare's high suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent operators and shareholders sit near the beneficiary end because the arrangement's asymmetric gains flow to them regardless of which component (lapsed or maintained) is doing the work in any given instance. Entrants and suppliers sit toward the target end because their constrained exit options mean they cannot arbitrage around either the settled or the actively defended barriers. Consumers are genuinely mixed — real beneficiaries of coordination value, real payers of the rent premium — which is why the hybrid reading assigns them dual roles rather than forcing a single directional bucket.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading exists precisely to prevent two symmetric mislabeling errors: treating the entire arrangement as natural (which would launder the actively maintained component as inevitable) and treating the entire arrangement as pure capture (which would ignore the genuine, no-longer-defended coordination residue and over-justify demolishing infrastructure that still functions). Mandatrophy is not resolved here — the founding coordination problem is genuinely contested as live/dead by domain, which is exactly the structural condition the hybrid reading is built to hold open rather than force-close.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lapsed_maintained_ratio_by_domain,
    'For any given product line or market segment, what fraction of the incumbent''s advantage is genuinely lapsed (would persist even if all active defense mechanisms were withdrawn) versus actively maintained (would erode within a few years without continued enforcement)?',
    'Natural experiments from antitrust remedies that force divestiture of specific contractual or legal mechanisms while leaving network-effect infrastructure intact — track which markets re-open competitively and which do not.',
    'A domain-by-domain mapping would let this hybrid story be decomposed into cleaner mountain-like and snare-like sub-constraints per segment, rather than carrying one blended ε across a genuinely heterogeneous arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lapsed_maintained_ratio_by_domain, empirical, 'How much of dominance is lapsed vs. actively maintained, and whether this varies by product line.').

omega_variable(
    kernel_reading_selection_evidence,
    'What evidence would distinguish this hybrid reading from the lapsed_alternative_reading and beneficiary_maintained_reading as the structurally correct account of the same kernel, rather than three equally defensible framings chosen by observer preference?',
    'Track whether withdrawal of specific active-maintenance mechanisms (litigation budgets, exclusivity contracts) produces measurable competitive re-entry (supports hybrid/maintained readings) or produces no change (supports lapsed reading) across multiple incumbents and time periods.',
    'If active-mechanism withdrawal consistently produces no competitive change, the hybrid reading should be abandoned in favor of the lapsed_alternative_reading; if it consistently produces strong re-entry, the beneficiary_maintained_reading is the better single account and the hybrid reading overstates the lapsed component''s independence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, conceptual, 'Whether the hybrid framing itself, versus its two sibling readings, is the correct account of the market_naturalization kernel.').

omega_variable(
    regulatory_evidentiary_capacity,
    'Do current antitrust evidentiary standards have the capacity to distinguish lapsed structural fact from actively maintained barrier within the timeframe and burden-of-proof structure regulators actually operate under?',
    'Comparative review of enforcement actions that attempted this distinction versus those that did not, and their outcomes on appeal.',
    'If regulatory tools cannot make this distinction reliably, the hybrid reading''s practical value is limited to academic/advocacy contexts rather than enforcement, and policy should default toward remedies that work under either sub-reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_evidentiary_capacity, empirical, 'Institutional capacity to operationalize the lapsed/maintained distinction this reading depends on.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mark_tr_t8, market_naturalization__hybrid_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(mark_tr_t16, market_naturalization__hybrid_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(mark_tr_t24, market_naturalization__hybrid_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(mark_tr_t32, market_naturalization__hybrid_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(mark_tr_t40, market_naturalization__hybrid_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__hybrid_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(mark_be_t8, market_naturalization__hybrid_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(mark_be_t16, market_naturalization__hybrid_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(mark_be_t24, market_naturalization__hybrid_reading, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(mark_be_t32, market_naturalization__hybrid_reading, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(mark_be_t40, market_naturalization__hybrid_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_naturalization__hybrid_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(mark_su_t8, market_naturalization__hybrid_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(mark_su_t16, market_naturalization__hybrid_reading, suppression_requirement, 16, 0.43).
narrative_ontology:measurement(mark_su_t24, market_naturalization__hybrid_reading, suppression_requirement, 24, 0.46).
narrative_ontology:measurement(mark_su_t32, market_naturalization__hybrid_reading, suppression_requirement, 32, 0.47).
narrative_ontology:measurement(mark_su_t40, market_naturalization__hybrid_reading, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__hybrid_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_naturalization__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, lapsed_alternative_reading).

% DUAL FORMULATION NOTE:
% This constraint is the hybrid member of a three-story family reading the market_naturalization kernel: lapsed_alternative_reading (near-mountain, ε low, no active maintenance claimed), hybrid_reading (this story, ε moderate ~0.52, mixed structure), and beneficiary_maintained_reading (tangled_rope/snare-adjacent, ε high, fully active capture claimed). Each story authors its own ε independently per the ε-invariance principle; they are linked as siblings rather than merged because the underlying empirical question (how much of dominance is lapsed vs. maintained) is itself unresolved and the three readings represent genuinely different structural claims about the same standing arrangement, not three measurements of one fixed fact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
