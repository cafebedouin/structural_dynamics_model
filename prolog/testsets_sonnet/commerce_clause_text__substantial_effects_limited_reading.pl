% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__substantial_effects_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__substantial_effects_limited_reading, []).

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
 *   constraint_id: commerce_clause_text__substantial_effects_limited_reading
 *   human_readable: Commerce Clause — Substantial Effects Doctrine with Economic/Non-Economic Nexus Requirement
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   Since United States v. Lopez (1995) and United States v. Morrison (2000),
 *   the Supreme Court has maintained that Commerce Clause power under the
 *   substantial-effects doctrine extends to intrastate activity that
 *   substantially affects interstate commerce, but only where the activity is
 *   genuinely economic in nature and Congress has not simply relabeled a
 *   police-power subject (guns near schools, gender-motivated violence) as
 *   commercial regulation. Gonzales v. Raich (2005) then held that even
 *   non-commercial intrastate conduct (home-grown marijuana for personal use)
 *   could be reached via aggregation theory if it was part of a broader
 *   'economic class of activities' subject to a comprehensive regulatory
 *   scheme. The doctrine functions as a boundary-policing mechanism: it does
 *   real coordination work (giving Congress and federal agencies a stable,
 *   judicially cognizable category for national economic regulation) while
 *   also serving as the primary site where federal-state authority is
 *   contested and where the economic/non-economic line can be manipulated by
 *   careful statutory drafting (jurisdictional-nexus elements, legislative
 *   findings) to sweep marginal cases toward federal characterization.
 *
 * KEY AGENTS:
 *   - congress: agenda_setter (institutional/arbitrage) — drafts statutes to fit the doctrinal gate
 *   - federal_regulatory_agencies: beneficiary (institutional/arbitrage) — administer expanded jurisdiction
 *   - national_market_participants: beneficiary (powerful/mobile) — gain uniform regulatory floor
 *   - state_police_power_domains: payer (organized/constrained) — must litigate the boundary repeatedly
 *   - defendants_in_marginal_nexus_prosecutions: payer (powerless/trapped) — bear thin-nexus prosecutions
 *   - federal_courts: observer/agenda_setter (institutional/analytical) — administer the categorization test
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, 0.42).
domain_priors:suppression_score(commerce_clause_text__substantial_effects_limited_reading, 0.38).
domain_priors:theater_ratio(commerce_clause_text__substantial_effects_limited_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__substantial_effects_limited_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__substantial_effects_limited_reading, "Commerce Clause — Substantial Effects Doctrine with Economic/Non-Economic Nexus Requirement").
narrative_ontology:topic_domain(commerce_clause_text__substantial_effects_limited_reading, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__substantial_effects_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__substantial_effects_limited_reading, '5a4b9d60-4935-4542-b9eb-8714611efc52').
narrative_ontology:cs_kernel_codification('5a4b9d60-4935-4542-b9eb-8714611efc52', fixed_text).
narrative_ontology:cs_authority_grounding('5a4b9d60-4935-4542-b9eb-8714611efc52', lineage).
narrative_ontology:cs_interpretation_layer_present('5a4b9d60-4935-4542-b9eb-8714611efc52').
narrative_ontology:cs_reading_relation('5a4b9d60-4935-4542-b9eb-8714611efc52', commerce_clause_text__expansive_federal_reading, influences).
narrative_ontology:cs_reading_relation('5a4b9d60-4935-4542-b9eb-8714611efc52', commerce_clause_text__originalist_narrow_reading, coexists_with).
narrative_ontology:cs_axiom('5a4b9d60-4935-4542-b9eb-8714611efc52', foundational, economic_noneconomic_categorical_limit_required).
narrative_ontology:cs_axiom_status(economic_noneconomic_categorical_limit_required, holdable).
narrative_ontology:cs_axiom_grounding('5a4b9d60-4935-4542-b9eb-8714611efc52', economic_noneconomic_categorical_limit_required, conventional).
narrative_ontology:cs_axiom('5a4b9d60-4935-4542-b9eb-8714611efc52', secondary, aggregation_permissible_within_comprehensive_economic_scheme).
narrative_ontology:cs_axiom_status(aggregation_permissible_within_comprehensive_economic_scheme, holdable).
narrative_ontology:cs_axiom_grounding('5a4b9d60-4935-4542-b9eb-8714611efc52', aggregation_permissible_within_comprehensive_economic_scheme, instrumental).
narrative_ontology:cs_reference_frame('5a4b9d60-4935-4542-b9eb-8714611efc52', post_lopez_morrison_doctrinal_settlement).
narrative_ontology:cs_drift_state('5a4b9d60-4935-4542-b9eb-8714611efc52', post_raich_aggregation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5a4b9d60-4935-4542-b9eb-8714611efc52', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, federal_regulatory_agencies).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, national_market_participants).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, congress).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, state_police_power_domains).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, defendants_in_marginal_nexus_prosecutions).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, local_noneconomic_conduct_regulators).
narrative_ontology:constraint_vindicates(commerce_clause_text__substantial_effects_limited_reading, enumerated_powers_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_text__substantial_effects_limited_reading, dual_sovereignty_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts federal statutes reaching intrastate conduct by including jurisdictional-nexus elements (e.g., 'affecting commerce,' 'in or affecting commerce') and economic-activity findings, precisely to satisfy the doctrine's gate. Controls how far it can extend federal law by how carefully it drafts around the economic/non-economic line the courts have drawn.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, congress, agenda_setter,
    institutional, generational, arbitrage, national).

% Administer statutes (environmental, labor, antitrust, financial) that reach intrastate economic activity under the substantial-effects doctrine. Benefit from a stable doctrinal category that legitimates broad jurisdiction over economic conduct while requiring only that they characterize the regulated activity as 'economic' and show aggregation-based effects.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, federal_regulatory_agencies, beneficiary,
    institutional, generational, arbitrage, national).

% Interstate businesses benefit from a uniform federal regulatory floor that displaces a patchwork of inconsistent state rules for genuinely economic activity — reduced compliance friction across state lines, at the cost of federal oversight they would not face under a narrower doctrine.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, national_market_participants, beneficiary,
    powerful, biographical, mobile, national).

% States retain nominal authority over education, family law, general criminal law, and other traditionally local domains, but must litigate the economic/non-economic boundary every time Congress or an agency characterizes conduct as 'affecting commerce.' Their sovereignty is preserved only where they can win the categorization fight — a genuine but contestable protection.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, state_police_power_domains, payer,
    organized, generational, constrained, national).

% Individuals prosecuted or regulated under federal statutes whose jurisdictional hook is thin (e.g., a firearm that once crossed state lines years earlier, wholly local conduct swept in by an aggregation theory) bear the cost of doctrinal line-drawing they cannot control. Litigating the nexus question is expensive and outcome uncertain; most cannot afford to challenge it to the Supreme Court.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, defendants_in_marginal_nexus_prosecutions, payer,
    powerless, biographical, trapped, national).

% Local governments and traditionally state-regulated actors (school boards, local criminal justice systems) must anticipate federal preemption or overlapping federal jurisdiction whenever their subject matter can be recharacterized as touching an economic aggregate, ceding practical authority even where the doctrine formally protects them.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, local_noneconomic_conduct_regulators, payer,
    moderate, biographical, constrained, regional).

% Adjudicate whether a given statute's jurisdictional nexus is genuine and whether the regulated activity is 'economic' in nature or is police-power regulation wearing a commerce-clause label. They administer the boundary-policing function that is this reading's central constraint mechanism, and their own doctrine determines which stakeholders win the categorization fight.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, federal_courts, observer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__substantial_effects_limited_reading, federal_courts, agenda_setter).

% Analyze whether the economic/non-economic distinction is a principled limit or a manipulable proxy that tracks judicial policy preference more than any stable doctrinal content. Their scholarship shapes how litigants and courts frame future nexus disputes but does not itself resolve any case.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__substantial_effects_limited_reading, diffuse).
narrative_ontology:fixing_cost_class(commerce_clause_text__substantial_effects_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable, judicially administrable line between activity Congress can reach as 'commerce' and activity reserved to state police power, allowing national economic regulation to proceed with predictable jurisdictional limits rather than either unlimited federal reach or a border-crossing-only doctrine too narrow for an integrated national economy.
% TRANSFER_FUNCTION: Moves regulatory authority from the states to federal agencies and Congress wherever conduct can be characterized as economic and shown (often via aggregation) to substantially affect interstate commerce; correspondingly moves litigation costs and legal uncertainty onto defendants and local actors who must contest that characterization.
% ABSENT_VOICES: Individuals swept into federal jurisdiction through thin or aggregated nexus theories rarely have the resources to litigate the categorization question to a precedent-setting conclusion; their objections surface mainly through amicus briefs filed by ideologically motivated organizations rather than through the affected parties themselves.
% DISAPPEARANCE_RATIONALE: If this doctrinal limit vanished and were replaced by unconstrained federal commerce power, federal statutes would extend without meaningful judicial check into domains (family law, general crime, local land use) currently defended by the economic/non-economic distinction; if it were replaced by the narrower originalist reading instead, entire regulatory regimes (labor standards, environmental rules, financial regulation reaching intrastate conduct) would lose their constitutional footing and require re-enactment under different authority or invalidation.
% FOUNDING_PROBLEM: After Lopez and Morrison, the Court needed a doctrine that preserved decades of accumulated New Deal and civil-rights-era federal regulation resting on substantial-effects reasoning while reintroducing SOME outer limit on federal power, so that the Commerce Clause would not become a general federal police power.
% FOUNDING_PROBLEM_CORROBORATION: Federal agencies and national business groups attest the doctrine functions well, providing predictable jurisdiction for genuinely economic regulation. State attorneys general and federalism scholars outside the regulatory beneficiary class attest the economic/non-economic line is manipulable and has not meaningfully constrained federal reach in practice since Raich; independent doctrinal analysis (law review commentary uninvolved in enforcement) corroborates that the line's application has been inconsistent across subject areas.
narrative_ontology:disappearance_verdict(commerce_clause_text__substantial_effects_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__substantial_effects_limited_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__substantial_effects_limited_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_text__substantial_effects_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__substantial_effects_limited_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__substantial_effects_limited_reading_tests).
:- end_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because the doctrine genuinely constrains federal reach in some cases (Lopez, Morrison struck down statutes) even as it has expanded via aggregation theory (Raich) in others — it is not a rubber stamp, but the economic/non-economic line has proven manipulable enough that most contested applications ultimately favor federal characterization. Suppression is moderate and has DECLINED over the interval (0.50 to 0.38) as the doctrine stabilized post-Raich and fewer statutes required active judicial invalidation to enforce the line — the boundary became more self-enforcing through drafting practice (nexus elements, findings clauses) than through active litigation. Theater ratio has risen modestly (0.18 to 0.31) as legislative findings clauses and jurisdictional-nexus boilerplate have become a somewhat performative ritual — Congress routinely inserts economic-effects findings regardless of whether the underlying activity is genuinely economic, anticipating the doctrinal test rather than being constrained by it in good faith. Accessibility collapse is moderate (0.45): genuine alternative framings (narrower originalist reading, broader expansive reading) remain live in scholarship and in dissenting opinions, so the doctrine has not fully foreclosed contestation. Resistance is moderately high (0.55): states, defendants, and originalist scholars actively contest specific applications of the line in nearly every generation of cases.
 *
 * PERSPECTIVAL GAP:
 *   From Congress's and federal agencies' seats, this doctrine looks like a legitimate, judicially-validated coordination mechanism enabling national economic governance — a genuine Rope. From the seat of a defendant swept into federal jurisdiction via a thin nexus theory, or a state whose traditional police-power domain keeps eroding through aggregation reasoning, the same doctrine looks like a Tangled Rope at best: real coordination function for interstate commerce, but a boundary that is drawn and redrawn in ways that consistently favor federal characterization when contested. The engine's per-seat computation should reflect this asymmetry — the economic/non-economic gate provides genuine (if imperfect) protection to state seats while functioning as background law rather than active constraint from the federal seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Congress and federal agencies sit near the beneficiary end: they set the terms of engagement (how statutes are drafted) and collect the resulting jurisdictional reach. National market participants benefit from federal preemption/uniformity even though they are nominally 'regulated' — the coordination function serves them by displacing a patchwork of state rules. State police-power domains and local regulators are structural payers: they bear the recurring cost of defending the boundary, and their sovereignty is only as strong as their next litigation outcome. Defendants in marginal-nexus prosecutions are the sharpest victims — powerless, trapped, and unable to internalize the doctrinal contest cost the way an organized state government can.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing the Commerce Clause from becoming an unlimited federal police power while preserving the New Deal regulatory settlement — remains partially live: Lopez and Morrison show the doctrine still has teeth against clearly non-economic subject matter (gun possession, gender violence) relabeled as commerce regulation. But Raich's aggregation theory has proven capacious enough that the doctrine rarely blocks Congress when it drafts carefully. The classification as tangled_rope rather than snare captures this: there IS a genuine coordination function (a workable national economic law regime) and it is NOT purely extractive, but the asymmetric cost-bearing by states and marginal defendants, combined with active enforcement (the courts' ongoing boundary-policing function), means it cannot be waved through as a pure Rope either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_noneconomic_line_stability,
    'Is the economic/non-economic distinction a principled, judicially administrable limit on federal power, or is it a manipulable proxy that tracks the composition of the Court more than any stable doctrinal content?',
    'Longitudinal analysis of nexus-challenge outcomes across changing Court compositions, controlling for statutory drafting quality; convergence of outcomes across ideologically opposed Courts would support principled-limit reading, divergence would support manipulable-proxy reading.',
    'If the line is a stable principle, this reading functions closer to a genuine coordination mechanism with real limits (tangled_rope leaning rope). If the line tracks judicial composition, the doctrine functions closer to unconstrained federal power dressed in a limiting vocabulary (tangled_rope leaning snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_noneconomic_line_stability, conceptual, 'Whether the economic/non-economic boundary is principled or outcome-driven.').

omega_variable(
    kernel_reading_selection_pressure,
    'Why does this story adopt the substantial-effects-limited reading rather than the expansive_federal_reading or originalist_narrow_reading as the operative constraint, given that all three are simultaneously defensible readings of the same constitutional text held by different judicial and scholarly coalitions?',
    'This reading is selected because it reflects current controlling Supreme Court doctrine (Lopez/Morrison/Raich line) as of the interval''s endpoint; the sibling readings represent the losing originalist position (pre-1937 and post-1995 minority) and a hypothetical maximalist position not currently adopted by a majority of the Court. A future doctrinal shift (e.g., a Court majority adopting either sibling reading) would require re-authoring this constraint''s ε and beneficiary structure entirely rather than adjusting this file''s parameters.',
    'If the Court shifts toward the originalist_narrow_reading, federal regulatory reach contracts sharply and the beneficiary/victim structure in THIS story would need to be re-derived for the new reading, not patched onto this one. If the Court shifts toward the expansive_federal_reading, the economic/non-economic gate modeled here as a genuine (if imperfect) limit disappears and the constraint becomes closer to a pure tangled_rope/snare with minimal state-protective function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Why this reading, not its siblings, is the operative constraint modeled here.').

omega_variable(
    aggregation_theory_erosion_trajectory,
    'Does Raich''s aggregation theory represent a stable equilibrium within the substantial-effects-limited reading, or is it a doctrinal drift point that will eventually collapse the economic/non-economic distinction into the expansive_federal_reading in practice even without formal doctrinal change?',
    'Track whether post-Raich lower-court applications of aggregation theory increasingly accept thinner and thinner nexus showings over time; a monotonic loosening trend would support the erosion hypothesis.',
    'If aggregation theory is eroding the distinction in practice, the theater_ratio trajectory understates the drift and this reading is functionally converging toward its expansive_federal_reading sibling despite formally remaining distinct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(aggregation_theory_erosion_trajectory, empirical, 'Whether aggregation-theory practice is quietly converging this reading toward the expansive sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__substantial_effects_limited_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement_basis(comm_tr_t1995, observed).
narrative_ontology:measurement(comm_tr_t2000, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement_basis(comm_tr_t2000, observed).
narrative_ontology:measurement(comm_tr_t2005, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2005, 0.26).
narrative_ontology:measurement_basis(comm_tr_t2005, observed).
narrative_ontology:measurement(comm_tr_t2010, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2010, 0.27).
narrative_ontology:measurement_basis(comm_tr_t2010, observed).
narrative_ontology:measurement(comm_tr_t2015, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2015, 0.28).
narrative_ontology:measurement_basis(comm_tr_t2015, observed).
narrative_ontology:measurement(comm_tr_t2020, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2020, 0.3).
narrative_ontology:measurement_basis(comm_tr_t2020, observed).
narrative_ontology:measurement(comm_tr_t2025, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2025, 0.31).
narrative_ontology:measurement_basis(comm_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t1995, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1995, 0.28).
narrative_ontology:measurement_basis(comm_be_t1995, observed).
narrative_ontology:measurement(comm_be_t2000, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2000, 0.33).
narrative_ontology:measurement_basis(comm_be_t2000, observed).
narrative_ontology:measurement(comm_be_t2005, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2005, 0.4).
narrative_ontology:measurement_basis(comm_be_t2005, observed).
narrative_ontology:measurement(comm_be_t2010, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement_basis(comm_be_t2010, observed).
narrative_ontology:measurement(comm_be_t2015, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement_basis(comm_be_t2015, observed).
narrative_ontology:measurement(comm_be_t2020, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2020, 0.41).
narrative_ontology:measurement_basis(comm_be_t2020, observed).
narrative_ontology:measurement(comm_be_t2025, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2025, 0.42).
narrative_ontology:measurement_basis(comm_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1995, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement_basis(comm_su_t1995, observed).
narrative_ontology:measurement(comm_su_t2000, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2000, 0.44).
narrative_ontology:measurement_basis(comm_su_t2000, observed).
narrative_ontology:measurement(comm_su_t2005, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement_basis(comm_su_t2005, observed).
narrative_ontology:measurement(comm_su_t2010, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2010, 0.38).
narrative_ontology:measurement_basis(comm_su_t2010, observed).
narrative_ontology:measurement(comm_su_t2015, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2015, 0.37).
narrative_ontology:measurement_basis(comm_su_t2015, observed).
narrative_ontology:measurement(comm_su_t2020, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2020, 0.37).
narrative_ontology:measurement_basis(comm_su_t2020, observed).
narrative_ontology:measurement(comm_su_t2025, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2025, 0.38).
narrative_ontology:measurement_basis(comm_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__substantial_effects_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text__expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text__originalist_narrow_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the commerce_clause_text kernel. expansive_federal_reading removes the economic/non-economic gate entirely (aggregation alone suffices for any activity); originalist_narrow_reading removes the substantial-effects doctrine entirely (only literal border-crossing trade and instrumentalities qualify). This reading occupies the doctrinal middle currently controlling under Lopez/Morrison/Raich. Each reading carries its own epsilon, beneficiary/victim structure, and classification; they are linked here for contamination/coupling analysis, not averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
