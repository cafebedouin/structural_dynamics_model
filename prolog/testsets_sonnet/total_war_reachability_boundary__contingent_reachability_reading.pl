% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contingent_reachability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__contingent_reachability_reading, []).

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
 *   constraint_id: total_war_reachability_boundary__contingent_reachability_reading
 *   human_readable: Contingent Reachability of Total War (Technology-Dependent Boundary Reading)
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   This story instantiates the contingent-reachability reading of the
 *   total_war_reachability_boundary kernel: the post-1991 contraction of
 *   total war as a feasible strategic option is not a permanent closure
 *   (contraction_reading) nor merely a probability-reduced-but-open state
 *   maintained by an ongoing coordination equilibrium (dropping_reading), but
 *   a piton — an atrophied capability whose disuse reflects technological
 *   contingency rather than structural impossibility. The capability for a
 *   rationally pursued, winnable total war between major powers has degraded
 *   because second-strike assurance made first-strike calculations
 *   irrational, not because the underlying strategic logic changed. That
 *   degraded capability is being actively reconstituted through hypersonic
 *   weapons, precision counterforce, missile defense layering, and
 *   AI-assisted targeting — technologies that, if matured, could restore the
 *   pre-contraction feasible set. The measured extractiveness and
 *   theater_ratio rise across the interval to reflect this: the boundary is
 *   increasingly maintained through performative arms-control gestures
 *   (theater) even as the underlying technological equilibrium it depends on
 *   quietly shifts (extraction of strategic advantage by the states funding
 *   reconstitution).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, 0.42).
domain_priors:suppression_score(total_war_reachability_boundary__contingent_reachability_reading, 0.35).
domain_priors:theater_ratio(total_war_reachability_boundary__contingent_reachability_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contingent_reachability_reading, scaffold).
narrative_ontology:human_readable(total_war_reachability_boundary__contingent_reachability_reading, "Contingent Reachability of Total War (Technology-Dependent Boundary Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contingent_reachability_reading, "international_relations/strategic_studies/nuclear_deterrence").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__contingent_reachability_reading).
narrative_ontology:has_sunset_clause(total_war_reachability_boundary__contingent_reachability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contingent_reachability_reading, '3f516153-2b1f-4b61-8f5e-e38a7ec1111c').
narrative_ontology:cs_kernel_codification('3f516153-2b1f-4b61-8f5e-e38a7ec1111c', distributed).
narrative_ontology:cs_authority_grounding('3f516153-2b1f-4b61-8f5e-e38a7ec1111c', distributed).
narrative_ontology:cs_reading_relation('3f516153-2b1f-4b61-8f5e-e38a7ec1111c', total_war_reachability_boundary__contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('3f516153-2b1f-4b61-8f5e-e38a7ec1111c', total_war_reachability_boundary__dropping_reading, influences).
narrative_ontology:cs_axiom('3f516153-2b1f-4b61-8f5e-e38a7ec1111c', foundational, reachability_is_technologically_contingent_not_structurally_closed).
narrative_ontology:cs_axiom_status(reachability_is_technologically_contingent_not_structurally_closed, holdable).
narrative_ontology:cs_axiom_grounding('3f516153-2b1f-4b61-8f5e-e38a7ec1111c', reachability_is_technologically_contingent_not_structurally_closed, empirically_contingent).
narrative_ontology:cs_axiom('3f516153-2b1f-4b61-8f5e-e38a7ec1111c', foundational, atrophied_capability_persists_in_dormant_reconstitutable_form).
narrative_ontology:cs_axiom_status(atrophied_capability_persists_in_dormant_reconstitutable_form, holdable).
narrative_ontology:cs_axiom_grounding('3f516153-2b1f-4b61-8f5e-e38a7ec1111c', atrophied_capability_persists_in_dormant_reconstitutable_form, empirically_contingent).
narrative_ontology:cs_reference_frame('3f516153-2b1f-4b61-8f5e-e38a7ec1111c', cold_war_mad_technological_equilibrium).
narrative_ontology:cs_drift_state('3f516153-2b1f-4b61-8f5e-e38a7ec1111c', post_hypersonic_counterforce_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3f516153-2b1f-4b61-8f5e-e38a7ec1111c', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, states_investing_in_destabilizing_technology).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, arms_industrial_base).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, counterforce_strategists).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, civilian_populations_under_umbrella).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, non_nuclear_states).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, future_generations_if_deterrence_fails).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__contingent_reachability_reading, deterrence_stability_thesis).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__contingent_reachability_reading, mutually_assured_destruction_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fund hypersonic glide vehicles, counterforce-capable precision strike, AI-assisted targeting, and missile defense programs whose declared purpose is defensive but whose effect is to erode the second-strike assurance that currently holds total war outside the feasible set. Each program is individually justified as prudent modernization; collectively they are the mechanism by which the boundary could move. These states retain the option to slow or accelerate the arms race and thus set the pace of the boundary's actual position.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, states_investing_in_destabilizing_technology, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__contingent_reachability_reading, states_investing_in_destabilizing_technology, beneficiary).

% Contractors and research labs profit directly from continued investment in the technologies that erode the current strategic equilibrium. They have no stake in the boundary holding or moving in any particular direction, only in continued funding for capability development, which structurally favors treating the boundary as always-contestable rather than settled.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, arms_industrial_base, beneficiary,
    organized, biographical, mobile, global).

% Military planners and theorists whose institutional relevance depends on total war remaining a live strategic contingency requiring active management rather than a foreclosed impossibility. They benefit professionally from the reading that the boundary is contingent and requires their continued vigilance, expertise, and budget.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, counterforce_strategists, beneficiary,
    moderate, generational, constrained, national).

% Live under the strategic umbrella of nuclear states with no voice in the technological choices that determine whether the boundary holds or erodes. They bear catastrophic risk if the piton's atrophied capability is reconstituted through renewed technological capacity, but have no meaningful exit from the geography or alliance structure that puts them in the blast radius of any reconstitution.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, civilian_populations_under_umbrella, payer,
    powerless, biographical, trapped, national).

% Bear the systemic risk of a boundary shift they did not create and cannot meaningfully influence through the technology-investment decisions of major powers. Their exit options are limited to diplomatic protest, treaty advocacy, or alignment-switching, none of which touches the underlying technological drivers.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, non_nuclear_states, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__contingent_reachability_reading, non_nuclear_states, excluded).

% Bear the full cost of boundary reconstitution should it occur, with zero capacity to participate in present-day decisions about which technologies are funded. Their situation is the clearest expression of the piton's stakes: the atrophied capability is not gone, it is dormant, and its reconstitution would fall on people with no seat at any table.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, future_generations_if_deterrence_fails, payer,
    powerless, civilizational, trapped, universal).

% Treaty bodies, verification regimes, and multilateral forums that monitor and attempt to constrain the technological drift that would reconstitute total war reachability. They observe the erosion but possess only the enforcement authority that member states grant them, and are frequently excluded from decisions about emerging technology categories (cyber, AI, hypersonics) that fall outside existing treaty text.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, arms_control_institutions, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__contingent_reachability_reading, arms_control_institutions, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_reachability_boundary__contingent_reachability_reading, states_investing_in_destabilizing_technology).
narrative_ontology:fixing_cost_class(total_war_reachability_boundary__contingent_reachability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The current contraction of total war reachability functions as a coordination equilibrium among major powers: mutual restraint from developing or deploying technologies that would restore first-strike viability, sustained by a shared (if implicit) understanding that stability serves all nuclear powers' survival.
% TRANSFER_FUNCTION: The arrangement transfers systemic risk from the states actively shaping the technological equilibrium to civilian populations, non-nuclear states, and future generations who have no say in whether the boundary holds, narrows, or is quietly reconstituted through research investment.
% ABSENT_VOICES: Civilian populations, non-nuclear states, and future generations are structurally absent from the rooms where hypersonic, counterforce, and missile-defense investment decisions are made; arms control institutions are present but structurally lag the technologies being developed, since treaty text is negotiated after capability categories are already established.
% DISAPPEARANCE_RATIONALE: If the contingent-reachability framing vanished — if everyone agreed the boundary was permanently closed (contraction_reading) — investment in destabilizing technology would lose its strategic-hedge justification and could slow; if everyone agreed it was simply a live probability question (dropping_reading), the piton framing itself would not change behavior much. Whether the world rearranges depends on which sibling reading displaces this one, which is exactly the contest the kernel encodes.
% FOUNDING_PROBLEM: The mutual assured destruction equilibrium of the late Cold War produced a genuine, technologically grounded reduction in the feasibility of a rationally winnable total war between major nuclear powers — the founding problem was preventing existential conflict when both sides possessed assured second-strike capability.
% FOUNDING_PROBLEM_CORROBORATION: Arms control institutions and independent strategic studies scholars (outside the states funding destabilizing technology) attest that the technological basis for the contraction is actively eroding — hypersonic glide vehicles, AI-assisted targeting, and layered missile defense are cited in unclassified defense-analysis literature as reintroducing first-strike calculations that MAD-era doctrine assumed foreclosed. The investing states themselves characterize the same programs as defensive modernization rather than boundary erosion, which is precisely the self-interested framing this reading treats with suspicion.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contingent_reachability_reading, contested).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contingent_reachability_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contingent_reachability_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_reachability_boundary__contingent_reachability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contingent_reachability_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).
:- end_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate and rising (0.15 to 0.42) because the primary structural dynamic is not overt coercive extraction but a slow transfer of systemic risk from technology-investing states to populations and non-nuclear states who bear the tail risk without participating in the investment decisions. Suppression is moderate (0.35 at interval end) because no single actor coercively enforces the boundary — it holds through distributed strategic calculation, not centralized compulsion, but the arms-control apparatus that nominally governs it has hardening theatrical requirements even as its substantive grip loosens. Theater ratio is deliberately the fastest-rising metric (0.25 to 0.48): a rising share of the visible arms-control and deterrence-stability discourse increasingly performs boundary-maintenance (summits, review conferences, doctrine statements) while the underlying technological capability the boundary depends on is being actively eroded by the same states participating in that theater. Accessibility collapse is moderate (0.40) — the boundary is not fully understood as contingent by most publics, who treat post-Cold-War peace as closer to permanent than the technological record supports; resistance is moderately high (0.55) reflecting genuine arms-control advocacy and non-proliferation diplomacy pushing back against reconstitution.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of states investing in destabilizing technology, each program reads as prudent hedging against adversary capability gains — coordination-preserving, defensive modernization. From the seat of civilian populations and non-nuclear states, the identical technological trajectory reads as the erosion of a protective boundary they depend on but cannot influence. The engine should compute these seats as structurally divergent: the agenda-setter seat experiences low or moderate effective extraction (arbitrage exit, institutional power), while the payer seats experience high effective extraction (trapped exit, powerless-to-moderate power, civilizational time horizon for the future-generations seat).
 *
 * DIRECTIONALITY LOGIC:
 *   States investing in destabilizing technology sit at the beneficiary end: they gain strategic latitude, deterrent credibility, and industrial-base activity from treating the boundary as movable rather than fixed. Civilian populations, non-nuclear states, and future generations sit at the target end: they carry the catastrophic tail risk of reconstitution without controlling the investment decisions that produce it. The arms industrial base and counterforce strategists occupy an intermediate beneficiary position — their gains are more modest and professional/economic rather than geopolitical, but they share the interest in the boundary remaining a live, budget-justifying question rather than a settled fact in either direction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing a rationally winnable total war — is genuinely contested as live or dead: from the investing states' seat, the problem persists and justifies continued modernization (framed as maintaining deterrence); from the arms-control and independent-scholarship seat, the original MAD-era technological basis for the contraction has been substantially undermined by exactly the programs justified as sustaining it, making this a case where the founding coordination function is being hollowed out from within by parties who benefit from claiming continuity. The scaffold classification (with its required sunset clause) captures that this reading treats the current equilibrium as inherently temporary and technology-contingent, not as a mountain (permanent) or a stable rope (self-sustaining coordination) — the piton framing specifically flags that the underlying capability has atrophied but not vanished, and that its reconstitution is an active, funded project rather than a remote possibility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    piton_or_genuine_closure,
    'Is the post-1991 contraction of total war reachability a genuinely closed strategic possibility (as the contraction_reading holds) or an atrophied-but-reconstitutable capability contingent on the current technological equilibrium (as this reading holds)?',
    'Track whether emerging counterforce, hypersonic, and missile-defense technologies actually restore a rationally calculable first-strike advantage for any major power over the next decade; if such an advantage becomes strategically credible and is acted upon (or credibly threatened), the piton reading is vindicated over the permanent-closure reading.',
    'If genuine closure is correct, current technology investment is strategically inert noise and the classification should shift toward mountain; if the piton reading is correct, current investment is the active mechanism of boundary erosion and the classification remains scaffold/piton with rising extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(piton_or_genuine_closure, empirical, 'Whether the current strategic equilibrium reflects genuine structural closure or contingent, reversible technological suppression.').

omega_variable(
    sibling_reading_divergence,
    'Where exactly do the three kernel readings (contingent_reachability, contraction, dropping) diverge in their treatment of the same underlying technological trend data?',
    'Compare how each reading''s proponents interpret the same set of hypersonic and counterforce weapons programs: the contraction_reading treats them as marginal and non-boundary-relevant; the dropping_reading treats them as probability-modulating within a stable coordination regime; this reading treats them as the literal mechanism of boundary reconstitution.',
    'The disagreement is located specifically in whether current technology investment is boundary-irrelevant (contraction), probability-modulating (dropping), or boundary-reconstituting (this reading) — resolving this determines which of the three constraint stories best describes present strategic reality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_divergence, conceptual, 'Locating the structural point of disagreement among the three kernel readings regarding the same empirical technology trend.').

omega_variable(
    beneficiary_self_report_reliability,
    'Can the investing states'' own characterization of their technology programs as purely defensive modernization be trusted, or does it systematically understate boundary-erosion effects?',
    'Independent strategic-studies assessment (outside the investing states'' defense establishments) of whether hypersonic and counterforce capabilities produce first-strike-relevant effects regardless of stated intent, using declassified doctrine documents and war-gaming results where available.',
    'If self-reports are unreliable, the extraction and suppression metrics may be understated; if reliable, the piton framing may overstate the reconstitution risk relative to the dropping_reading''s more modest probability-shift framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_self_report_reliability, empirical, 'Reliability of investing-state self-characterization as evidence for or against the boundary-erosion thesis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contingent_reachability_reading, 1991, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1991, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 1991, 0.25).
narrative_ontology:measurement_basis(tota_tr_t1991, observed).
narrative_ontology:measurement(tota_tr_t2000, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement_basis(tota_tr_t2000, observed).
narrative_ontology:measurement(tota_tr_t2010, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement_basis(tota_tr_t2010, observed).
narrative_ontology:measurement(tota_tr_t2018, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2018, 0.4).
narrative_ontology:measurement_basis(tota_tr_t2018, observed).
narrative_ontology:measurement(tota_tr_t2024, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2024, 0.45).
narrative_ontology:measurement_basis(tota_tr_t2024, observed).
narrative_ontology:measurement(tota_tr_t2030, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2030, 0.48).
narrative_ontology:measurement_basis(tota_tr_t2030, projected).

% Extraction over time
narrative_ontology:measurement(tota_be_t1991, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 1991, 0.15).
narrative_ontology:measurement_basis(tota_be_t1991, observed).
narrative_ontology:measurement(tota_be_t2000, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2000, 0.2).
narrative_ontology:measurement_basis(tota_be_t2000, observed).
narrative_ontology:measurement(tota_be_t2010, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2010, 0.28).
narrative_ontology:measurement_basis(tota_be_t2010, observed).
narrative_ontology:measurement(tota_be_t2018, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2018, 0.35).
narrative_ontology:measurement_basis(tota_be_t2018, observed).
narrative_ontology:measurement(tota_be_t2024, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2024, 0.4).
narrative_ontology:measurement_basis(tota_be_t2024, observed).
narrative_ontology:measurement(tota_be_t2030, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2030, 0.42).
narrative_ontology:measurement_basis(tota_be_t2030, projected).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1991, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 1991, 0.2).
narrative_ontology:measurement_basis(tota_su_t1991, observed).
narrative_ontology:measurement(tota_su_t2000, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2000, 0.22).
narrative_ontology:measurement_basis(tota_su_t2000, observed).
narrative_ontology:measurement(tota_su_t2010, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2010, 0.26).
narrative_ontology:measurement_basis(tota_su_t2010, observed).
narrative_ontology:measurement(tota_su_t2018, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2018, 0.3).
narrative_ontology:measurement_basis(tota_su_t2018, observed).
narrative_ontology:measurement(tota_su_t2024, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2024, 0.33).
narrative_ontology:measurement_basis(tota_su_t2024, observed).
narrative_ontology:measurement(tota_su_t2030, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2030, 0.35).
narrative_ontology:measurement_basis(tota_su_t2030, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contingent_reachability_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_reachability_boundary__contingent_reachability_reading, 0.1).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__dropping_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, nuclear_nonproliferation_treaty_regime).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, missile_defense_technology_race).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language 'total war reachability boundary' kernel per the ε-invariance principle: contraction_reading (mountain-flavored permanent closure), dropping_reading (rope-flavored stable coordination at reduced probability), and this contingent_reachability_reading (scaffold/piton-flavored contingent, technology-dependent, reversible contraction). Each carries a distinct extractiveness profile because each makes a structurally different claim about whether the underlying capability is gone, managed, or dormant. Linked via affects_constraints to both siblings and to adjacent nonproliferation and missile-defense constraint stories that share stakeholders and technological drivers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
