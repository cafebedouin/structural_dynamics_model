% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__regulatory_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__regulatory_takings_reading, []).

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
 *   constraint_id: takings_clause_boundary__regulatory_takings_reading
 *   human_readable: Regulatory Takings Boundary — Severe Value Diminution Reading ('Too Far' Standard)
 *   domain: constitutional law/property rights/regulatory theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel. The kernel is
 *   the Fifth Amendment Takings Clause ('nor shall private property be taken
 *   for public use, without just compensation'); this reading — the
 *   regulatory takings reading, announced in Pennsylvania Coal Co. v. Mahon
 *   (1922) and operationalized through Penn Central balancing — holds that
 *   regulation which goes 'too far' in diminishing economic value crosses the
 *   compensable line even though nothing physical is seized. Two sibling
 *   readings of the same text are separate constraints in separate files: the
 *   physical_appropriation_reading (only direct seizure or permanent physical
 *   occupation triggers compensation) and the categorical_takings_reading
 *   (per se rules for physical occupation and total economic wipeout,
 *   multifactor balancing for everything else). Per the epsilon-invariance
 *   principle, this file authors a single stable epsilon for a single
 *   referent: the standing arrangement under which governments must pay when
 *   regulation severely diminishes private economic value, assessed by this
 *   reading's own lights. The siblings have different victim sets, different
 *   enforcement surfaces, and different epsilons; they are linked through
 *   network.affects_constraints, and the decomposition is documented in the
 *   dual-formulation note. The claim/metric gap is deliberate: the
 *   arrangement is CLAIMED as tangled_rope from its structure (a genuine
 *   protective function bound to real asymmetric costs), while the metrics
 *   independently describe its actual operation — the engine measures the
 *   divergence.
 *
 * KEY AGENTS:
 *   - federal_judiciary: Agenda setter (institutional/analytical) — administers the 'too far' standard, owns the boundary, and binds every level of government through precedent
 *   - large_development_interests: Primary beneficiary (powerful/constrained) — captures the overwhelming share of successful compensation claims and deterrent concessions
 *   - small_parcel_owners: Formal beneficiary (moderate/trapped) — protected on paper, rarely able to fund the litigation the protection requires
 *   - municipal_governments: Primary payer (institutional/constrained) — bears liability, defensive regulation redesign, and litigation costs
 *   - general_taxpayers: Diffuse payer (powerless/trapped) — ultimately funds judgments and settlements through municipal budgets
 *   - conservation_and_preservation_publics: Excluded third party (organized/trapped) — lose regulatory protection when liability chills or kills regulations, with no standing in the proceedings
 *   - property_rights_legal_movement: Secondary beneficiary (organized/mobile) — builds dockets, funding, and professional standing around the claim stream
 *   - land_use_law_academy: Analytical observer (analytical/global) — maps coherence, traces genealogy, supplies the critiques and defenses that shape appointment politics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, 0.55).
domain_priors:suppression_score(takings_clause_boundary__regulatory_takings_reading, 0.58).
domain_priors:theater_ratio(takings_clause_boundary__regulatory_takings_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__regulatory_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__regulatory_takings_reading, "Regulatory Takings Boundary — Severe Value Diminution Reading ('Too Far' Standard)").
narrative_ontology:topic_domain(takings_clause_boundary__regulatory_takings_reading, "constitutional law/property rights/regulatory theory").

domain_priors:requires_active_enforcement(takings_clause_boundary__regulatory_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__regulatory_takings_reading, '3966fd60-055a-4c5a-a42c-65cedd9538c1').
narrative_ontology:cs_kernel_codification('3966fd60-055a-4c5a-a42c-65cedd9538c1', fixed_text).
narrative_ontology:cs_authority_grounding('3966fd60-055a-4c5a-a42c-65cedd9538c1', lineage).
narrative_ontology:cs_interpretation_layer_present('3966fd60-055a-4c5a-a42c-65cedd9538c1').
narrative_ontology:cs_reading_relation('3966fd60-055a-4c5a-a42c-65cedd9538c1', takings_clause_boundary__physical_appropriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('3966fd60-055a-4c5a-a42c-65cedd9538c1', takings_clause_boundary__categorical_takings_reading, influences).
narrative_ontology:cs_axiom('3966fd60-055a-4c5a-a42c-65cedd9538c1', foundational, economic_value_diminution_compensable).
narrative_ontology:cs_axiom_status(economic_value_diminution_compensable, holdable).
narrative_ontology:cs_axiom_grounding('3966fd60-055a-4c5a-a42c-65cedd9538c1', economic_value_diminution_compensable, deontological).
narrative_ontology:cs_axiom('3966fd60-055a-4c5a-a42c-65cedd9538c1', secondary, multifactor_balancing_locates_too_far_line).
narrative_ontology:cs_axiom_status(multifactor_balancing_locates_too_far_line, holdable).
narrative_ontology:cs_axiom_grounding('3966fd60-055a-4c5a-a42c-65cedd9538c1', multifactor_balancing_locates_too_far_line, conventional).
narrative_ontology:cs_reference_frame('3966fd60-055a-4c5a-a42c-65cedd9538c1', regulatory_value_destruction_in_scope).
narrative_ontology:cs_drift_state('3966fd60-055a-4c5a-a42c-65cedd9538c1', post_lucas_categorical_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3966fd60-055a-4c5a-a42c-65cedd9538c1', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, large_development_interests).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, small_parcel_owners).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, property_rights_legal_movement).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, municipal_governments).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, general_taxpayers).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, conservation_and_preservation_publics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the constitutional text through the 'goes too far' standard, deciding case-by-case whether a regulation crossed the compensable line. Sets precedents that bind every legislature, municipality, and agency in the country. Owns the boundary in the sense that the boundary exists nowhere except in its ongoing application; it cannot resign from the role without the arrangement ceasing to operate.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Land holders and developers whose projects collide with environmental, zoning, and preservation regulation. They can fund multi-year litigation, expert studies, and appellate campaigns, and they capture the large majority of successful compensation claims and settlements. Their holdings cannot be relocated out of regulatory jurisdiction, so their strategy is litigation and negotiated concession rather than exit; the prospect of a claim functions as leverage in permitting negotiations.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, large_development_interests, beneficiary,
    powerful, biographical, constrained, national).

% Owners of homes and small parcels subject to land-use restrictions. The compensation guarantee formally protects them, but the cost of proving a claim screens most of them out; they benefit mainly through the deterrent effect their better-funded counterparts generate, and occasionally through contingency representation. Their land cannot be moved, and their equity is often their principal asset, so they hold on under whatever restriction applies.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, small_parcel_owners, beneficiary,
    moderate, biographical, trapped, local).

% Cities, counties, and agencies that enact and administer zoning, environmental, and historic-preservation regulation. When a court finds a regulation went too far, they pay the judgment or settle, and they redesign regulations defensively — adding variance procedures, nexus studies, and hardship provisions — to stay clear of the line. They cannot opt out of constitutional review, they carry liability insurance and reserve funds against claims, and the redesign effort diverts staff capacity from the regulations' original purposes.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, municipal_governments, payer,
    institutional, generational, constrained, regional).

% Ultimately fund compensation judgments, settlements, and the municipal insurance premiums and legal reserves built against them. They have no direct participation in any takings proceeding, learn of the costs only through budget lines, and cannot decline to pay. The costs arrive diffuse and unbundled from any decision they were consulted on.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, general_taxpayers, payer,
    powerless, generational, trapped, national).

% Neighbors, conservation constituencies, historic-preservation communities, and future residents who depend on wetland, habitat, coastal, and landmark regulation for environmental and cultural goods. When liability exposure leads a government to weaken, delay, or abandon a regulation, these publics lose the protection — but they have no standing in a takings case, which runs between the owner and the government. They cannot exit their dependence on regulation that may be quietly thinned, and they enter the process only to the extent the government chooses to defend the regulation vigorously.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, conservation_and_preservation_publics, excluded,
    organized, generational, trapped, regional).

% Public-interest law firms, bar networks, and advocacy organizations that build dockets, donor bases, and professional reputations around property-rights claims. They identify and sponsor sympathetic plaintiffs, supply the litigation infrastructure that small owners could not fund alone, and convert favorable rulings into fundraising narratives. If the claim stream narrowed, they would redirect capacity to adjacent legal fields; their commitment to this particular mechanism is strategic rather than immovable.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, property_rights_legal_movement, beneficiary,
    organized, biographical, mobile, national).

% Scholars of property, constitutional law, and land-use regulation who map the doctrine's coherence, reconstruct its genealogy, and propose reforms ranging from abolition of the category to full codification. Their critiques and defenses feed judicial opinion writing and confirmation politics. They hold no material stake in any outcome and bear none of the arrangement's costs.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, land_use_law_academy, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__regulatory_takings_reading, large_development_interests).
narrative_ontology:fixing_cost_class(takings_clause_boundary__regulatory_takings_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents government from delivering public benefits by concentrating their costs on whichever private owners stand in the way, without payment: the compensation requirement forces the public fisc to internalize the price of its regulatory demands, disciplining confiscatory regulation and giving owners a protected floor against targeted value destruction.
% TRANSFER_FUNCTION: Moves money — judgments, settlements, and the litigation costs that precede them — from municipal treasuries and ultimately taxpayers to owners who suffer severe regulatory value loss; and moves regulatory discretion from governments to owners through the deterrent shadow the liability regime casts over every land-use decision.
% ABSENT_VOICES: Third-party beneficiaries of land-use regulation — neighbors, conservation publics, future residents — would object that compensation liability thins the protections they depend on, and that regulations killed or diluted by liability exposure never appear in any accounting of the arrangement's costs. They are absent because takings litigation is structured owner-versus-government and they lack standing; their interests surface only refracted through whatever vigor the government brings to defending its own regulation.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, land-use regulation would expand in the absence of any liability ceiling, development strategy would shift entirely from litigation to legislative lobbying, state courts would begin evolving their own compensation doctrines within a decade, and the practice of singling out obstructive owners to bear public burdens — the practice the founding cases confronted — would re-emerge piecemeal wherever it was locally cheap.
% FOUNDING_PROBLEM: Government regulation had begun destroying private economic value wholesale — the founding case involved orders that coal companies leave supporting coal unmined, annihilating its value — while the constitutional compensation guarantee covered only physical seizure. The arrangement was built to extend the compensation principle to regulatory destruction: when regulation goes too far, the owner should not silently bear the cost of a public benefit everyone else enjoys.
% FOUNDING_PROBLEM_CORROBORATION: The phenomenon is attested outside the beneficiary set: the courts' own factual records in the founding and canonical cases document regulations that destroyed substantial value (support-coal orders, terminal-construction plans, beachfront lot eliminations); multiple state legislatures enacted independent compensation statutes with formal findings citing the same problem; and the academic literature across ideological divisions accepts that regulatory value destruction occurs while disputing where the line sits. No perfectly neutral corroborator exists — the courts attesting the problem also administer the arrangement — but the adverse parties' own litigation concessions and the state statutory findings provide corroboration from seats that gain nothing from the doctrine's expansion.
narrative_ontology:disappearance_verdict(takings_clause_boundary__regulatory_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__regulatory_takings_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__regulatory_takings_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(takings_clause_boundary__regulatory_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__regulatory_takings_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__regulatory_takings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__regulatory_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-to-substantial (0.55 at interval end) because the arrangement's costs are systematically decoupled from regulatory merit: liability attaches to value diminution regardless of whether the regulation served a compelling public purpose, and the deterrent shadow extends far beyond the small fraction of claims that succeed. Suppression (0.58) is a raw structural property, unscaled by power or scope: the arrangement binds as supreme law, legislatures have no exit from it, and the removal of the state-litigation prerequisite in 2019 eliminated the last procedural buffer. Theater is moderate (0.42): the 'too far' formula performs a principled, determinate limit while the actual operation is famously unpredictable case-by-case adjudication — the phrase does rhetorical work the balancing test does not. Accessibility collapse is low-moderate (0.35): ordinary police-power regulation survives untouched (most regulations never approach the line), and governments retain workable design alternatives (variances, nexus findings, phased implementation). Resistance is substantial (0.6): the reading faces persistent scholarly attack from both textualist and progressive directions, recurring dissents calling for abolition of the category, and wide state-level divergence. The three measurement series run on one shared time grid (0/20/40/60/80/100) so every tracked metric is authored at every examined point; trajectories show extraction accumulating and enforcement hardening as the doctrine matured from a marginal caveat into a binding national constraint.
 *
 * PERSPECTIVAL GAP:
 *   Four seats should compute materially different types from identical structural data. From the judiciary's administrative seat the arrangement is a functioning governance instrument it built and maintains — coordination all the way down. From the municipal seat it is a liability regime that taxes regulatory ambition and forces defensive design. From the taxpayer seat it is a diffuse unfunded mandate. From the conservation-public seat it is a silent subtraction of protection they never agreed to trade away — they are not even parties to the transactions that cost them. And the two beneficiary seats diverge sharply from each other: well-resourced repeat players convert the arrangement into recoveries and negotiating leverage, while small owners hold a formal protection they mostly cannot access. The engine computes this divergence from power, exit, and role data; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Large development interests sit nearest the beneficiary pole: they collect the transfers and wield the deterrent as bargaining power, though their land holdings tie them to the jurisdiction (constrained, not arbitrage exit). Small parcel owners are formally beneficiaries but their realized benefit is thin — litigation costs screen them out — placing them somewhat off the pure-beneficiary pole. The property-rights legal movement collects docket, funding, and status, a genuine but indirect beneficiary with mobile exit. Municipal governments sit near the target pole: they pay judgments, absorb defensive-design costs, and cannot exit constitutional review, though retained discretion over regulation design keeps them short of the full-target extreme. General taxpayers sit near full-target diffusely — they fund everything and decide nothing. Conservation and preservation publics bear real costs (diluted or abandoned protections) despite formal absence from the arrangement; their directionality toward the constraint is high precisely because the constraint operates on regulations that exist for their benefit. The judiciary sits near the beneficiary pole as the administering institution whose authority the arrangement continuously exercises. Suppression is declared as a structural scalar and is not scaled by any context dimension; only extractiveness is scaled, by directionality and scope, in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — government destroying private economic value through regulation while shifting the cost of public benefits onto isolated owners — remains live: regulatory value destruction continues, and the case stream continues to process it. There is no mandatrophy to resolve and no sunset concept applicable to a constitutional doctrine. The classification discipline matters here in both directions: reading the arrangement as pure coordination (a simple property shield) erases the documented third-party costs — the chilled regulations, the defensive redesign, the unrepresented publics — which is exactly the cover story the beneficiary seats tell. Reading it as pure extraction erases the genuine function — without the compensation backstop, the cheapest way to deliver public benefits would be to single out owners whose land stands in the way, which is the practice the founding cases confronted. The tangled-rope structure holds both facts: coordination and asymmetric extraction through the same liability mechanism, held in place by active judicial enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of the takings_clause_boundary kernel; how would the classification and victim set change if the physical_appropriation_reading or categorical_takings_reading sibling were adopted as the operative boundary?',
    'Supreme Court consolidation around a sibling framework (e.g., a Cedar Point-style physical-focus majority, or full categorical codification) would be observable in doctrine; track the ratio of per se to balancing dispositions over successive Terms.',
    'Under the physical reading the victim set collapses to possession-deprived owners and the arrangement computes far less costly to regulators; under the categorical reading the boundary stabilizes and the uncertainty component of the cost falls; under this reading the expanded victim set and ad hoc boundary persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame delta: sibling readings instantiate different constraints with different victim sets and enforcement surfaces.').

omega_variable(
    too_far_threshold_coherence,
    'Does the ''goes too far'' standard track a coherent threshold of confiscatory effect, or does it operate as outcome-rationalizing rhetoric applied after the fact?',
    'Systematic coding of Penn Central-line outcomes against their stated factor profiles: if outcomes are predictable from the factors, a usable threshold exists; if not, the standard is post-hoc justification.',
    'If rhetorical, the authored theater_ratio understates the performative share and the arrangement''s persistence depends more heavily on judicial discretion than on any limiting principle; if coherent, the balancing is calibrated protection and the measured costs are largely the price of that calibration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(too_far_threshold_coherence, empirical, 'Whether the ad hoc boundary is a real threshold or rationalized discretion.').

omega_variable(
    deterrence_chill_net_effect,
    'Does the compensation requirement predominantly deter confiscatory regulation (a protective effect) or chill beneficial land-use, environmental, and preservation regulation (a cost borne by people who were never parties)?',
    'Natural experiments from state-level compensation mandates (Oregon Measures 37/49, Arizona Proposition 207): measure regulatory issuance, enforcement activity, and environmental outcomes before and after adoption.',
    'If chill dominates, the effective victim set is wider than the declared one and the arrangement sits deeper into extraction; if deterrence dominates, the arrangement sits nearer pure coordination and the declared victims overstate the harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_chill_net_effect, empirical, 'Net behavioral effect of liability exposure on regulators.').

omega_variable(
    cost_incidence_diffusion,
    'Who ultimately bears the arrangement''s costs — the visible budgetary defendants (municipal treasuries, taxpayers) or the invisible regulatory-loss publics whose protections are diluted or abandoned?',
    'Fiscal incidence studies tracing judgment and settlement funding, combined with counterfactual analysis of regulations withdrawn or redesigned defensively after liability exposure.',
    'If regulatory-loss publics dominate, the declared victim set understates the true one and the costs concentrate on seats with no voice in the process; if budgetary defendants dominate, the costs are at least visible and politically contestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_incidence_diffusion, empirical, 'Real incidence of the arrangement''s costs across represented and unrepresented seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__regulatory_takings_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t0, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(taki_tr_t20, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(taki_tr_t40, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(taki_tr_t60, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 60, 0.33).
narrative_ontology:measurement(taki_tr_t80, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement(taki_tr_t100, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(taki_be_t0, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(taki_be_t20, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(taki_be_t40, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 40, 0.36).
narrative_ontology:measurement(taki_be_t60, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 60, 0.46).
narrative_ontology:measurement(taki_be_t80, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 80, 0.51).
narrative_ontology:measurement(taki_be_t100, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 100, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t0, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(taki_su_t20, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 20, 0.33).
narrative_ontology:measurement(taki_su_t40, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 40, 0.39).
narrative_ontology:measurement(taki_su_t60, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 60, 0.47).
narrative_ontology:measurement(taki_su_t80, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 80, 0.53).
narrative_ontology:measurement(taki_su_t100, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__regulatory_takings_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary__physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary__categorical_takings_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the Takings Clause': the label conflates three structurally distinct boundary claims with different victim sets, enforcement surfaces, and epsilons. The physical_appropriation_reading is narrow and stable (possession-based, minimal contestation, negligible extraction). The categorical_takings_reading is a hybrid (per se rules plus balancing). This regulatory_takings_reading is the broadest and least determinate (value-diminution-based, ad hoc boundary, expanded victim set). Causal structure runs upstream-downstream: this reading's demonstrated indeterminacy created the legitimacy conditions under which the categorical refinement emerged (the per se categories were built to escape the balancing morass), so the influence edge runs from this reading to the categorical sibling; the physical reading coexists as the textualist alternative held by a different judicial coalition. Each member carries its own epsilon; none averages across the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
