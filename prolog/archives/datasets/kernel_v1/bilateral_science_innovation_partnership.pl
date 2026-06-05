% ============================================================================
% CONSTRAINT STORY: bilateral_science_innovation_partnership
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bilateral_science_innovation_partnership, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bilateral_science_innovation_partnership
 *   human_readable: Bilateral Science Innovation Partnership Between Research-Dominant and Research-Emerging Nations
 *   domain: international_science_policy/institutional_partnership
 *
 * SUMMARY:
 *   Bilateral science innovation partnerships between research-dominant and
 *   research-emerging nations present a structural ambiguity: they function
 *   simultaneously as coordination mechanisms enabling research capacity
 *   growth in emerging nations and as extraction mechanisms concentrating
 *   epistemic authority in dominant nations. The partnerships are formally
 *   structured as symmetric agreements with mutual benefit but operate
 *   through asymmetric institutional power, resource concentration, and
 *   research-agenda setting authority. The dominant nation gains access to
 *   novel research populations, geographic data collection sites,
 *   cost-reduced fieldwork, and talent from emerging regions; the emerging
 *   nation gains resource access, equipment, methodological training, and
 *   scientific legitimacy on the global stage. The constraint's evolution
 *   over the interval (extractiveness increasing from 0.38 to 0.52, theater
 *   ratio rising from 0.48 to 0.61) indicates that partnership maturation
 *   involves increasing performative governance and accumulating subtle
 *   extraction as initial coordination benefits reach diminishing returns and
 *   asymmetries become institutionalized.
 *
 * KEY AGENTS:
 *   - Research-Dominant Nation Institutions: Primary beneficiary (institutional/arbitrage) — captures unilateral access to research populations, data, cost reduction; can exit without research-capacity loss
 *   - Research-Dominant Nation Researchers: Primary beneficiary (institutional/arbitrage) — gain career advancement through novel collaborations, publication opportunities in emerging geographies; maintain epistemic authority in partnership governance
 *   - Research-Emerging Nation Researcher: Primary victim (powerless/trapped) — dependent on partnership for funding, equipment, publication venues, career visibility; constrained to align research agendas with dominant-nation priorities; no alternative pathways
 *   - Research-Emerging Nation Institution: Secondary victim and mixed participant (moderate/constrained) — gains infrastructure and legitimacy but loses research-agenda autonomy; dependence deepens over time as institutional capacity becomes partnership-tuned
 *   - Research-Emerging Nation State: Institutional participant (institutional/constrained) — benefits from research-capacity signals and talent retention but loses strategic research autonomy as funding follows dominant-nation priorities
 *   - Partnership Governance Framework: Institutional theater (institutional/arbitrage) — formal agreements perform legitimacy and symmetry; real decisions flow through informal power channels (dominant-nation researcher influence, funding-source direction)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bilateral_science_innovation_partnership, 0.52).
domain_priors:suppression_score(bilateral_science_innovation_partnership, 0.58).
domain_priors:theater_ratio(bilateral_science_innovation_partnership, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bilateral_science_innovation_partnership, extractiveness, 0.52).
narrative_ontology:constraint_metric(bilateral_science_innovation_partnership, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(bilateral_science_innovation_partnership, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bilateral_science_innovation_partnership, tangled_rope).
narrative_ontology:human_readable(bilateral_science_innovation_partnership, "Bilateral Science Innovation Partnership Between Research-Dominant and Research-Emerging Nations").
narrative_ontology:topic_domain(bilateral_science_innovation_partnership, "international_science_policy/institutional_partnership").

domain_priors:requires_active_enforcement(bilateral_science_innovation_partnership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bilateral_science_innovation_partnership, research_dominant_nation_institutions).
narrative_ontology:constraint_beneficiary(bilateral_science_innovation_partnership, research_dominant_nation_researchers).
narrative_ontology:constraint_victim(bilateral_science_innovation_partnership, research_emerging_nation_scientific_autonomy).
narrative_ontology:constraint_victim(bilateral_science_innovation_partnership, research_emerging_nation_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING NATION RESEARCHER (SNARE) — Structurally dependent on partnership for research funding, equipment access, publication venues, and international visibility. Career advancement requires alignment with dominant-nation research agendas and methodologies. No alternative pathways for independent research establishment. Maximum experienced extraction — caught between local resource scarcity and global epistemic hierarchy.
constraint_indexing:constraint_classification(bilateral_science_innovation_partnership, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EMERGING NATION INSTITUTION (TANGLED ROPE) — Genuine coordination function: partnership provides funding, infrastructure, and international legitimacy that enable local research capacity to grow. Simultaneously, partnership enforces research-agenda alignment with dominant nation priorities, constrains resource allocation autonomy, and creates institutional dependence on continued partnership access. Constrained exit: institution could theoretically exit but faces severe capacity reduction and loss of international standing.
constraint_indexing:constraint_classification(bilateral_science_innovation_partnership, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMINANT NATION INSTITUTION (ROPE) — Experiences partnership primarily as coordination mechanism: access to diverse research populations, geographic data collection sites, novel study populations, and cost reduction for fieldwork. Can easily exit partnership and establish alternative collaborations; faces minimal career or institutional cost from exit. Net beneficiary experiencing the constraint as pure coordination.
constraint_indexing:constraint_classification(bilateral_science_innovation_partnership, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EMERGING NATION STATE (TANGLED ROPE) — State-level actors see genuine coordination: partnership advances national research capacity, attracts talent retention, and signals scientific legitimacy on the global stage. Simultaneously, state loses autonomy over research priorities as funding flows toward domains prioritized by dominant partner, and national scientific infrastructure becomes dependent on foreign partnership continuity. Exit constrained by soft power implications and institutional momentum.
constraint_indexing:constraint_classification(bilateral_science_innovation_partnership, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PARTNERSHIP GOVERNANCE FRAMEWORK (PITON) — The formal structures (memoranda of understanding, joint research committees, resource-sharing agreements) are substantially performative. Real decisions flow through informal channels (dominant researcher influence, funding source direction, publication prestige hierarchies). The governance theater persists because formal agreements signal legitimacy and sustainable commitment, but actual research-agenda setting occurs through power dynamics that the governance framework does not capture.
constraint_indexing:constraint_classification(bilateral_science_innovation_partnership, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the partnership is structurally ambiguous: genuine coordination mechanism for building global research capacity (coordination function = real, extraction function = real). The constraint exhibits simultaneous coordination (both nations gain research capability) and extraction (dominant nation captures epistemic authority, emerging nation loses research autonomy). Tangled rope classification persists across all time horizons because the hybrid structure is irreducible — both functions are structural, neither can be eliminated without destroying the coordination benefit.
constraint_indexing:constraint_classification(bilateral_science_innovation_partnership, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bilateral_science_innovation_partnership_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bilateral_science_innovation_partnership, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bilateral_science_innovation_partnership, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bilateral_science_innovation_partnership, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bilateral_science_innovation_partnership, TR),
    TR >= 0.70.

:- end_tests(bilateral_science_innovation_partnership_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The partnership extracts emerging-nation research autonomy (research agenda becomes dependent on dominant-nation priority-setting), intellectual property (discoveries in partnership are typically attributed and controlled by dominant-nation institutions even when led by emerging-nation researchers), and talent (emerging-nation researchers who build capacity often emigrate to dominant nations, converting the capacity-building investment into brain drain). The extractiveness is not as severe as a pure snare (0.66+) because genuine research capacity does develop in emerging-nation institutions, and some fraction of talent remains. However, the extraction is substantial because the asymmetry in autonomy and epistemic authority is structural to the partnership design. Extractiveness increases over time (0.38→0.52) as initial enthusiasm for collaboration gives way to routine institutional dependence and subtle agenda-setting through funding channels. Suppression (0.58): Moderate-high. Barriers to emerging-nation researcher independence include: (1) equipment and funding concentration in dominant nation, (2) publication bias favoring dominant-nation research methodologies, (3) career incentives aligned with dominant-nation collaboration continuity, (4) lack of alternative sources for large-scale research infrastructure, (5) normalization of dominant-nation research standards as universal scientific standards. Suppression is not maximal (≥0.70) because emerging-nation researchers can and do conduct independent work; the suppression is structural constraint, not total barrier. Theater ratio (0.61): Moderate-high. Partnership governance is substantially performative. The formal structures (MOUs, joint committees, resource-sharing agreements) perform legitimacy and equal partnership; actual research-agenda setting flows through informal channels (dominant researcher influence in laboratory meetings, funding-source direction through grant cycles, publication prestige hierarchies that favor dominant-nation journals). The theater increases over time (0.48→0.61) as the governance framework becomes more elaborate and distant from actual decision-making. The governance theater is not as severe as piton levels (≥0.70) because the partnership does generate real research output; the theater is a layer over functional but asymmetric coordination, not a substitute for it.
 *
 * PERSPECTIVAL GAP:
 *   The partnership exhibits a stark perspectival gap between institutional beneficiaries and powerless victims. The dominant-nation institution perceives rope: coordination mechanism that solves the real problem of accessing diverse research populations and distributing research costs globally. The emerging-nation researcher perceives snare: structural dependence with no meaningful exit options. The emerging-nation state perceives tangled rope: genuine capacity building (coordination) but loss of strategic research autonomy (extraction). The partnership governance framework perceives itself as symmetric (piton theater) while real decision-making reproduces asymmetry (hidden through informal channels). The analytical observer perceives the full tangled rope structure: simultaneous coordination (research capacity genuinely grows in emerging nation) and extraction (emerging-nation research autonomy becomes dependent on dominant-nation priorities). This perspectival gap is diagnostic — the gap between 'rope' (dominant-nation beneficiary) and 'snare' (powerless victim) reveals that the partnership's coordination function is real but unequally distributed.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation reflects each agent's structural position relative to the extraction flow. Research-dominant-nation institutions are beneficiaries with arbitrage-level exit options: they derive d≈0.15 (low directionality toward extraction target), which produces f(d)≈-0.01 (negative chi, indicating net benefit). Research-emerging-nation researchers are victims with trapped exit options: they derive d≈0.95 (high directionality toward extraction target), which produces f(d)≈1.42 (high chi amplification, indicating severe experienced extraction). Research-emerging-nation institutions occupy intermediate position: they are partial beneficiaries (gain infrastructure) and partial victims (lose autonomy), with constrained exit options: d≈0.55, f(d)≈0.75, indicating moderate experienced extraction. The analytical observer derives d≈0.72 (victim-leaning from global scope where extraction concentrates on powerless emerging-nation researchers), f(d)≈1.15, confirming tangled rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The bilateral partnership resolves mandatrophy by demonstrating that tangled rope is the stable classification across perspectives when both coordination and extraction functions are genuinely present. The constraint does not collapse to rope (which would misclassify the extraction) or snare (which would misclassify the coordination). Instead, tangled rope accurately captures that the partnership creates real research capacity growth (coordination function, beneficiary perspective: rope) while concentrating epistemic authority and constraining research autonomy (extraction function, victim perspective: snare). The mandatrophy is resolved not by choosing between rope and snare but by accepting that the same institutional mechanism serves both functions simultaneously: it coordinates research activity (both nations gain research capacity) while extracting research autonomy (emerging nation loses independent research-agenda setting). The tangled rope classification permits this structural coexistence — the constraint exhibits both genuine coordination (χ ≤ 0.90 due to coordination function) and substantial extraction (χ ≥ 0.40 due to asymmetric benefit distribution). No single type can characterize the partnership; the full six-type variation across perspectives is the correct description.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    research_autonomy_definition,
    'What constitutes meaningful research autonomy in an asymmetric partnership? Is alignment with dominant-nation research agendas inherently extraction, or can it represent legitimate mutual problem-focusing?',
    'Longitudinal analysis of research direction emergence: tracking whether emerging-nation researchers independently generate research questions that happen to align with dominant-nation priorities (weak extraction signal) versus generating research questions that are constrained to align (strong extraction signal). Indicator: diversity of emerging-nation research questions outside partnership domains.',
    'If autonomy is preserved: extraction component is lower, classification shifts toward rope. If autonomy is constrained: extraction component is higher, classification confirmed as tangled rope or shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(research_autonomy_definition, empirical, 'Whether research autonomy is preserved or constrained by partnership').

omega_variable(
    knowledge_reverse_flow,
    'How much research capacity and methodological knowledge actually flows from emerging-nation institutions back to dominant-nation institutions? Is the partnership genuinely bidirectional or substantially unidirectional?',
    'Citation analysis: tracking citations of emerging-nation researchers in dominant-nation publications, and vice versa. Co-authorship lead positions: counting first-author publications where emerging-nation researcher leads and dominant-nation researcher follows. Methodology adoption: identifying dominant-nation adoption of emerging-nation research methods or theoretical frameworks.',
    'If reverse flow is substantial: partnership is more symmetric, classification confirms tangled rope with genuine coordination. If reverse flow is minimal: partnership is extraction mechanism, classification shifts toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(knowledge_reverse_flow, empirical, 'Whether knowledge flows bidirectionally or is concentrated in dominant nation').

omega_variable(
    partnership_exit_distribution,
    'Do emerging-nation partners achieve sufficient research capacity that they can exit the partnership without severe scientific degradation? Or does dependence increase over the partnership duration?',
    'Historical analysis: tracking research output metrics (publications, citations, funded grants) for emerging-nation institutions in years following partnership termination or reduction. Comparison with institutions that maintained similar partnerships continuously.',
    'If capacity persists after exit: partnership built genuine institutional capacity, extraction component is moderate, tangled rope classification confirmed. If capacity collapses after exit: partnership created artificial dependence, extraction component is severe, classification shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partnership_exit_distribution, empirical, 'Whether partnership builds exit-independent capacity or creates dependence').

omega_variable(
    epistemic_authority_concentration,
    'Over the partnership duration, does epistemic authority (standard-setting, methodology validation, theoretical framework legitimation) concentrate in dominant-nation hands or distribute toward emerging-nation institutions?',
    'Editorial board composition analysis: tracking representation of emerging-nation researchers on editorial boards of partnership-generated journals or flagship collaborative publications. Grant review committee composition: analyzing who sets standards for partnership-funded research allocation. Methodology canonization: identifying whether dominant-nation methodologies become standard for partnership research or whether hybrid approaches emerge.',
    'If authority distributes: partnership creates genuine scientific pluralism, suppression component is lower. If authority concentrates: partnership entrenches epistemic hierarchy, suppression component is higher.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epistemic_authority_concentration, empirical, 'Whether epistemic authority distributes or concentrates during partnership').

omega_variable(
    partnership_counterfactual_capacity,
    'What would have been the research trajectory of emerging-nation institutions absent the partnership? Is the partnership enabling research that would not otherwise occur, or accelerating research that was already underway?',
    'Comparative analysis: identifying matched pairs of emerging-nation institutions with and without dominant-nation partnerships from the same region and historical period. Pre-partnership research capacity assessment of partner institutions. Identification of alternative funding or collaboration sources available to emerging-nation institutions.',
    'If partnership enables novel research: coordination function is genuine and dominant, extraction is moderate, tangled rope confirmed. If partnership merely accelerates pre-existing trajectory: coordination function is weaker, extraction component becomes more salient, classification shifts toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(partnership_counterfactual_capacity, empirical, 'Whether partnership enables novel capacity or accelerates existing trajectory').

omega_variable(
    false_summit_partnership_naturalization,
    'Is the partnership''s asymmetry a natural consequence of research capability differences, or a constructed institutional arrangement that the capability difference is used to justify?',
    'Historical analysis: tracking how partnerships were framed at inception versus how framing evolved. Identification of moments where asymmetry was challenged and how those challenges were resolved. Comparison with partnerships that successfully shifted toward equality despite initial capability gaps.',
    'If asymmetry is naturalized: false summit risk present, engine FSM detector may reclassify. If asymmetry is acknowledged as constructed: tangled rope classification reflects institutional choice, not inevitable structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_partnership_naturalization, conceptual, 'Whether partnership asymmetry is naturalized or acknowledged as constructed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bilateral_science_innovation_partnership, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bsip_tr_t0, bilateral_science_innovation_partnership, theater_ratio, 0, 0.48).
narrative_ontology:measurement(bsip_tr_t5, bilateral_science_innovation_partnership, theater_ratio, 5, 0.56).
narrative_ontology:measurement(bsip_tr_t10, bilateral_science_innovation_partnership, theater_ratio, 10, 0.61).

% Extraction over time
narrative_ontology:measurement(bsip_be_t0, bilateral_science_innovation_partnership, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(bsip_be_t5, bilateral_science_innovation_partnership, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(bsip_be_t10, bilateral_science_innovation_partnership, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(bsip_su_t0, bilateral_science_innovation_partnership, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(bsip_su_t5, bilateral_science_innovation_partnership, suppression_requirement, 5, 0.56).
narrative_ontology:measurement(bsip_su_t10, bilateral_science_innovation_partnership, suppression_requirement, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bilateral_science_innovation_partnership, resource_allocation).
narrative_ontology:affects_constraint(bilateral_science_innovation_partnership, research_brain_drain_emerging_nations).
narrative_ontology:affects_constraint(bilateral_science_innovation_partnership, scientific_standard_harmonization_asymmetry).

% DUAL FORMULATION NOTE:
% The bilateral partnership encompasses multiple structurally distinct constraints. The knowledge-flow asymmetry (epistemic authority concentration) is a separate constraint from the resource-allocation asymmetry (funding concentration). The partnership story describes the composite institutional arrangement; sibling constraints track specific asymmetries that decompose the broader phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bilateral_science_innovation_partnership, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
