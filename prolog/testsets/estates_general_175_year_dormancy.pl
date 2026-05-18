% ============================================================================
% CONSTRAINT STORY: estates_general_175_year_dormancy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_estates_general_175_year_dormancy, []).

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
 *   constraint_id: estates_general_175_year_dormancy
 *   human_readable: Estates-General 175-Year Dormancy: Bandwidth Atrophy in Representation
 *   domain: french_history/institutional_bandwidth
 *
 * SUMMARY:
 *   The suspension of the Estates-General from 1614 to 1789 represents a
 *   175-year institutional dormancy that exemplifies how bandwidth—the
 *   operational capacity of a coordination mechanism—can atrophy through
 *   disuse while the formal constitutional structure remains intact. The
 *   crown benefited from the removal of an oversight body that could deny or
 *   condition taxation; parlements and intendants filled the operational void
 *   with administrative authority; the Third Estate lost its only formal
 *   channel for expressing corporate grievances. What began as a deliberate
 *   policy choice (Louis XIV's turn toward absolutism) calcified into
 *   institutional inertia: by the time Louis XVI attempted reconvocation in
 *   1789, the procedural knowledge, electoral mechanisms, debate structures,
 *   and decision rules had degraded so severely that the Estates-General had
 *   to be reinvented from scratch. This reinvention failed catastrophically,
 *   producing coordination deadlock that contributed directly to the
 *   revolution. The constraint demonstrates that the Deferential Realism
 *   framework's treatment of bandwidth as a structural parameter must account
 *   for drift: bandwidth can degrade below functional thresholds when the
 *   mechanism is not continuously exercised, transforming a coordination
 *   problem (how to include the Estates in governance) into an extraction
 *   mechanism (suppressing grievance channels) and eventually into a system
 *   collapse. The measurement data shows base extractiveness rising from 0.28
 *   to 0.64 over the dormancy period, with a sharp drop at reconvocation
 *   (0.58) reflecting the immediate coordination failure and political
 *   rupture. Theater ratio rises from 0.32 to 0.68, indicating that
 *   constitutional claims about Estates authority become increasingly
 *   performative as the operational reality of dormancy continues.
 *
 * KEY AGENTS:
 *   - Royal Absolutism (Crown): Institutional beneficiary (institutional/arbitrage) — gains unilateral fiscal and legislative authority; requires active enforcement to maintain dormancy
 *   - Parlements and Intendants: Secondary beneficiary (institutional/arbitrage) — gain operational authority as functional substitutes for Estates; experience constraint as coordination mechanism
 *   - Third Estate: Primary victim (powerless/trapped) — loses only formal channel for registering grievances; experiences suppression without alternative; bears full extractive burden
 *   - Constitutional Theory: Institutional artifact (institutional/arbitrage) — maintains the fiction of Estates authority in legal doctrine while operational capacity atrophies; increasingly performative
 *   - Reform Movement: Organized agents (organized/constrained) — by late 18th century advocate for Estates reconvocation as solution to fiscal and legitimacy crisis; see constraint as temporary and solvable
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the dormancy as a law of institutional physics rather than a deliberate policy choice maintained through suppression
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(estates_general_175_year_dormancy, 0.58).
domain_priors:suppression_score(estates_general_175_year_dormancy, 0.68).
domain_priors:theater_ratio(estates_general_175_year_dormancy, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(estates_general_175_year_dormancy, extractiveness, 0.58).
narrative_ontology:constraint_metric(estates_general_175_year_dormancy, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(estates_general_175_year_dormancy, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(estates_general_175_year_dormancy, tangled_rope).
narrative_ontology:human_readable(estates_general_175_year_dormancy, "Estates-General 175-Year Dormancy: Bandwidth Atrophy in Representation").
narrative_ontology:topic_domain(estates_general_175_year_dormancy, "french_history/institutional_bandwidth").

domain_priors:requires_active_enforcement(estates_general_175_year_dormancy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(estates_general_175_year_dormancy, royal_absolutism).
narrative_ontology:constraint_beneficiary(estates_general_175_year_dormancy, parlements_and_intendants).
narrative_ontology:constraint_victim(estates_general_175_year_dormancy, third_estate_grievance_channel).
narrative_ontology:constraint_victim(estates_general_175_year_dormancy, coordination_mechanism_continuity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THIRD ESTATE (SNARE) — No legitimate institutional channel to register accumulating economic grievances, fiscal burdens, or procedural subordination to nobility and clergy. Trapped within a subordinate constitutional position with no operational exit mechanism. Bears full extraction (taxation without representation) while suppression mechanism prevents alternative channels from forming. Maximum experienced constraint severity.
constraint_indexing:constraint_classification(estates_general_175_year_dormancy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PARLEMENTS AND INTENDANTS (ROPE) — Gain operational authority and prestige during the 175-year dormancy. Parlements exercise judicial review of royal edicts; intendants administer fiscal and administrative policy without Estates oversight. Experience the constraint as coordination: the suspension of the Estates frees these institutions to act as functional alternatives, solving the governing problem of how to run the state without constant resort to full-scale estate representation. Net beneficiaries with exit optionality (can exercise administrative authority under current arrangement). Low effective extraction from their perspective because the coordination function (bypassing cumbersome Estates procedures) genuinely serves their operational needs.
constraint_indexing:constraint_classification(estates_general_175_year_dormancy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ROYAL ABSOLUTISM (TANGLED ROPE) — The suspension coordinates absolutist governance (removes the constraint of Estates oversight on taxation and legislation) while simultaneously extracting acquiescence from the Third Estate through suppression of alternative grievance channels. The Estates suspension solves a real coordination problem for the crown: how to maintain fiscal authority and legislative supremacy without granting veto power to estates with conflicting interests. But it does so asymmetrically — the Third Estate's grievances accumulate without outlet, generating the long-term extractive drift. Requires active enforcement of the dormancy (preventing informal Estates meetings, suppressing alternative representative claims, controlling parlementary resistance). The beneficiary (crown) experiences this as legitimate coordination; the victim (Third Estate) experiences it as pure extraction. Perspectival gap is maximal.
constraint_indexing:constraint_classification(estates_general_175_year_dormancy, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL CONTINUITY (PITON) — The Estates-General remains formally written into constitutional theory throughout the dormancy. Jurisconsults and legal theorists maintain the fiction that the Estates are always available, the procedure remains valid, the mechanism persists. But the operational knowledge of how to run elections, structure cahiers, conduct debates, resolve disputes has atrophied. By 1789, the 175-year dormancy means that the actual mechanics must be reinvented from scratch. Theater ratio is high (0.65) because much of the constitutional discourse about Estates authority is performative—it exists in legal theory but has no living practice. The constraint is maintained through institutional inertia, not because the Estates mechanism still solves any governing problem. The theater has risen as the gap between the written constitution (Estates formally exist) and operational reality (they cannot function without reinvention) has widened.
constraint_indexing:constraint_classification(estates_general_175_year_dormancy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, coordinate institutional mechanisms degrade when not exercised, and this degradation appears as a law of social coordination: unused bandwidth contracts, procedures fossilize, institutional memory erodes, technical knowledge of operation becomes tacit and dispersed. The Estates suspension appears as an inevitable consequence of institutional physics—mechanisms that are not continuously practiced lose functional capacity. This perspective risks naturalizing what is actually a deliberate institutional choice (the crown's decision to suspend and keep suspended). The natural law reading misses the extraction dynamic that sustains the dormancy.
constraint_indexing:constraint_classification(estates_general_175_year_dormancy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: REFORM MOVEMENT (SCAFFOLD) — By the late 18th century, reformers and philosophes begin arguing that Estates convocation could solve accumulated governance problems: fiscal crisis, legitimacy deficit, provincial unrest. They see the dormancy as a temporary institutional failure with a sunset: reconvoke the Estates, hear grievances, restore legitimacy, establish clearer representation rules. Louis XVI's decision to convene the Estates in 1789 appears to vindicate this perspective—the constraint is temporary, solvable by reinstitutionalizing the mechanism. However, the scaffolding fails catastrophically because the atrophy is deeper than anticipated. The bandwidth has not just contracted—it has fragmented into incompatible procedural expectations across the three estates, and the reinvention process breaks coordination entirely, triggering revolution. The sunset is real as a theoretical hope but fails in practice.
constraint_indexing:constraint_classification(estates_general_175_year_dormancy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(estates_general_175_year_dormancy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(estates_general_175_year_dormancy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(estates_general_175_year_dormancy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(estates_general_175_year_dormancy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(estates_general_175_year_dormancy, TR),
    TR >= 0.70.

:- end_tests(estates_general_175_year_dormancy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising over time. The initial suspension (1614) was a deliberate policy choice with moderate extractive load—absolutism gained authority without massive Third Estate backlash because the grievance accumulation was still in early stages. Over 175 years, extractiveness increased as the Third Estate's fiscal burden rose (wars, debt, court spending) without any legitimate channel to contest it or negotiate relief. The slight drop at reconvocation (0.58 from 0.64 at T=160) reflects the violent transition—the constraint's extraction mechanism breaks under its own weight as the political system shatters. Suppression (0.68): High and sustained. The dormancy required active suppression (preventing informal Estates meetings, controlling parlementary resistance, denying petitioning rights) combined with the structural suppression of having no alternative channel. By the later period, suppression became self-reinforcing: the longer the Estates were dormant, the more threatening their reconvocation became, requiring even stronger enforcement. Theater ratio (0.65): High and rising. Constitutional discourse about Estates authority persists throughout the dormancy in legal writing and political theory, but this discourse is increasingly detached from operational reality. Jurisconsults maintained that the Estates could be convoked at will, yet the procedural knowledge to actually do so had largely disappeared. By 1789, the theater is high because the crown and legal establishment are claiming Estates legitimacy precisely when the institution is most degraded. The measurement data shows theater rising from 0.32 to 0.68, documenting this divergence between constitutional fiction and operational reality. The slight decline at T=175 reflects the immediate crisis period where the pretense of smooth reconvocation collapses into procedural chaos.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a maximal perspectival gap between the beneficiary and victim. The crown and parlements experience the dormancy as coordination (Rope from parlements; Tangled Rope from crown because the crown must enforce the arrangement). The Third Estate experiences it as pure extraction (Snare)—no benefit from the coordination of absolutist authority, only the cost of suppressed grievance channels. The analytical observer at civilizational time horizon risks misclassifying this as a mountain (natural law of institutional physics) when the structural data reveals it as a false summit: the dormancy is a deliberate, maintained policy choice, not an inherent limit. The piton perspective shows the constraint's degradation signature: constitutional language persists while operational capacity atrophies, producing theater that rises as the gap between theory and practice widens. The scaffold perspective (reform movement) sees the dormancy as temporary and solvable through reconvocation, but this reading underestimates the depth of bandwidth atrophy—the attempt at reinvention in 1789 fails catastrophically, proving that 175 years of disuse cannot be easily reversed.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from the agent's structural relationship to the extraction flow. Royal absolutism is a beneficiary with arbitrage-level exit options (can continue operating without Estates indefinitely, as it did for 175 years)—derives low d, negative effective extraction from the crown's perspective. Parlements and intendants are secondary beneficiaries with operational authority during the dormancy—also low d. The Third Estate are trapped victims with no formal exit mechanism—high d approaching 1.0, maximum experienced extractiveness. The coordination type (resource_allocation, for the fiscal and legislative authority distributed among crown, parlements, and Estates) has a complexity offset of 0.05, which reflects that managing this multi-institutional arrangement is genuinely complex. However, the offset does not eliminate the asymmetry: the crown extracts without reciprocal constraint, the Third Estate has no leverage. The tangled rope classification holds because the constraint simultaneously solves a real coordination problem (how to run the state without Estates consensus) and creates an asymmetric extraction mechanism (the Third Estate bears the cost of that solution). The piton perspective emerges because the constitutional claims about Estates authority persist (as facts in legal doctrine) even though the operational mechanism is degraded—theater is maintained by the legal profession and political theorists, not by functional use.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by decomposing the dormancy into its constituent mechanisms: (1) the coordination problem the crown solves by avoiding Estates oversight (real, solves a genuine governing problem), (2) the asymmetric extraction it enables (suppression of grievance channels, taxation without representation), (3) the bandwidth atrophy it creates (procedural knowledge lost, institutional capacity degraded), and (4) the eventual system collapse it produces (reconvocation attempt leads to revolution). The constraint is tangled rope because it successfully coordinates absolutist governance (beneficiary perspective is real: the crown does solve a coordination problem) while simultaneously extracting from the Third Estate through suppression. The mandatrophy is resolved by recognizing that the coordination function (removing Estates veto power) and the extraction mechanism (suppressing alternative grievance channels) are two aspects of the same institutional arrangement, not competing classifications. The tangled rope classification holds throughout the dormancy because both aspects are always present: the crown is always solving a coordination problem (how to govern without Estates consensus) and always extracting (by suppressing alternative channels for Third Estate voice). The piton perspective documents the degradation of the constraint's own mechanism: by 1789, the constitutional fiction persists but the operational capacity has atrophied so completely that the constraint becomes unstable and breaks. The mountain classification from the analytical observer is a false summit: the constraint naturalizes what is actually a deliberate policy choice, hiding the extraction mechanism beneath the framing of institutional physics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deliberate_vs_emergent_dormancy,
    'Is the 175-year dormancy a deliberate policy of crown suppression, or an emergent consequence of institutional adaptation?',
    'Historical analysis of crown actions: explicit prohibitions vs passive non-convocation; examination of royal correspondence regarding Estates authority; timeline of parlementary and intendant power consolidation',
    'If deliberate: extraction mechanism is active suppression (snare classification from all non-crown perspectives holds). If emergent: constraint drifts from rope to snare as administrative alternatives became entrenched—the extraction is a side effect of functional substitution, not intentional policy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deliberate_vs_emergent_dormancy, empirical, 'Whether dormancy is deliberate policy or emergent institutional adaptation').

omega_variable(
    bandwidth_atrophy_irreversibility,
    'Once institutional bandwidth (procedures, memory, participant skills, decision rules) atrophies, is it recoverable through convocation, or does the atrophy create structural damage to coordination?',
    'Empirical: analyze the 1789 Estates-General convocation; track procedural failures, conflict over election rules, inability to decide on debate structures, cascading coordination breakdown',
    'If recoverable: dormancy is a temporary coordination deficit (scaffold classification holds). If irreversible: dormancy creates structural damage that triggers revolution—the constraint''s extraction mechanism is not suppression but bandwidth collapse itself, which unleashes political instability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bandwidth_atrophy_irreversibility, empirical, 'Whether institutional bandwidth atrophy is recoverable through reinstitutionalization').

omega_variable(
    suppression_mechanism_allocation,
    'What portion of suppression (0.68) reflects active royal policy preventing Estates convocation, vs. passive institutional drift making convocation seem impossible?',
    'Counterfactual: if the crown had actively encouraged periodic consultations with estate representatives (formal or informal), would the bandwidth have been preserved? Comparison with other dormant institutions and their suppression profiles.',
    'If mostly active: suppression is deliberate policy (snare mechanism). If mostly passive: suppression is emergent coordination failure (tangled rope mechanism holds, but with different causal attribution).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_allocation, empirical, 'Active vs. passive mechanisms in bandwidth atrophy suppression').

omega_variable(
    third_estate_grievance_accumulation,
    'Without the Estates-General as a formal channel, what mechanisms allowed Third Estate grievances to accumulate into revolutionary pressure rather than dissipating?',
    'Historical record: cahiers de doléances (1789), pamphlet literature, cahier analysis, regional unrest documentation; tracking how grievances that could not be formally registered nonetheless propagated',
    'If informal channels were sufficient: the snare classification is too severe—third estate had some voice, just not institutionalized. If informal channels failed to provide outlet: snare classification confirmed, and the constraint''s extraction mechanism (suppression without alternative) directly triggered 1789.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_estate_grievance_accumulation, empirical, 'Mechanism of Third Estate grievance accumulation without formal representation').

omega_variable(
    false_summit_natural_law,
    'Is bandwidth atrophy a law of social coordination (mountain), or a contingent feature of how the French crown chose to manage institutional relationships (tangled rope)?',
    'Comparative institutional analysis: other early modern states with similar representative bodies (Spanish Cortes, English Parliament, German Imperial Diet). Did dormancy produce similar bandwidth collapse? Did active practice preserve function?',
    'If universal: bandwidth atrophy is a natural law applying to all large-scale representative mechanisms—the mountain classification is correct. If contingent: the dormancy resulted from specific crown policy choices and institutional alternatives (parlements, intendants) that crowded out Estates function—false summit detected, tangled_rope is the correct classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law, empirical, 'Whether bandwidth atrophy is universal law or contingent institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(estates_general_175_year_dormancy, 0, 175).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(estgen_tr_t0, estates_general_175_year_dormancy, theater_ratio, 0, 0.32).
narrative_ontology:measurement(estgen_tr_t40, estates_general_175_year_dormancy, theater_ratio, 40, 0.42).
narrative_ontology:measurement(estgen_tr_t80, estates_general_175_year_dormancy, theater_ratio, 80, 0.54).
narrative_ontology:measurement(estgen_tr_t120, estates_general_175_year_dormancy, theater_ratio, 120, 0.62).
narrative_ontology:measurement(estgen_tr_t160, estates_general_175_year_dormancy, theater_ratio, 160, 0.68).
narrative_ontology:measurement(estgen_tr_t175, estates_general_175_year_dormancy, theater_ratio, 175, 0.65).

% Extraction over time
narrative_ontology:measurement(estgen_be_t0, estates_general_175_year_dormancy, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(estgen_be_t40, estates_general_175_year_dormancy, base_extractiveness, 40, 0.42).
narrative_ontology:measurement(estgen_be_t80, estates_general_175_year_dormancy, base_extractiveness, 80, 0.52).
narrative_ontology:measurement(estgen_be_t120, estates_general_175_year_dormancy, base_extractiveness, 120, 0.58).
narrative_ontology:measurement(estgen_be_t160, estates_general_175_year_dormancy, base_extractiveness, 160, 0.64).
narrative_ontology:measurement(estgen_be_t175, estates_general_175_year_dormancy, base_extractiveness, 175, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(estates_general_175_year_dormancy, resource_allocation).
narrative_ontology:affects_constraint(estates_general_175_year_dormancy, third_estate_fiscal_burden_accumulation).
narrative_ontology:affects_constraint(estates_general_175_year_dormancy, parlementary_legislative_authority_usurpation).
narrative_ontology:affects_constraint(estates_general_175_year_dormancy, revolutionary_grievance_cascade).

% DUAL FORMULATION NOTE:
% The 175-year dormancy is upstream of the fiscal crisis and revolutionary grievance dynamics that emerge in the 1780s. Each downstream constraint has its own extractiveness value reflecting domain-specific extraction mechanisms (fiscal extraction, legislative encroachment, grievance suppression). The dormancy constraint models the bandwidth atrophy that makes those downstream mechanisms possible: without the Estates as a formal channel, alternative mechanisms (fiscal absolutism, parlementary authority, revolutionary organizing) operate unopposed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(estates_general_175_year_dormancy, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
