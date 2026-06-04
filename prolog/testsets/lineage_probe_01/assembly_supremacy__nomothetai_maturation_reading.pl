% ============================================================================
% CONSTRAINT STORY: assembly_supremacy__nomothetai_maturation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_assembly_supremacy__nomothetai_maturation_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: assembly_supremacy__nomothetai_maturation_reading
 *   human_readable: Nomothetai Maturation Reading: Assembly Supremacy Through Procedural Constraint
 *   domain: legal/institutional_history
 *
 * SUMMARY:
 *   This constraint instantiates the nomothetai maturation reading of the
 *   assembly_supremacy kernel. In the fourth century BCE, Athens retrofitted
 *   its radical assembly with a new procedural filter: the distinction
 *   between laws (nomoi) — standing, prospective rules requiring deliberative
 *   panel approval — and decrees (psephismata) — immediate assembly decisions
 *   of limited duration. The nomothetai panels, composed of selected jurors,
 *   reviewed laws before enactment. This reading emphasizes institutional
 *   maturation: the assembly evolved from pure decree rule (fifth-century
 *   vulnerability exemplified by Mytilene) toward a system where prospective
 *   law gained protection through procedure. The constraint is structural:
 *   the nomothetai filter suppresses decree-rule omnipotence and slows
 *   lawmaking, but in doing so it creates legal stability that enables
 *   institutional planning across generations. This is not a constraint
 *   imposed externally by an oligarchic faction — it is a self-imposed
 *   procedural structure chosen by the assembly itself. The constraint
 *   exhibits genuine coordination (legal stability benefits all agents)
 *   alongside real extraction (decree-speed omnipotence is constrained). This
 *   reading competes with two siblings: the mytilene_volatility_reading
 *   emphasizes the cost of unrestricted assembly passion (massacre voted and
 *   reversed), and the radical_self_rule_reading asserts that the ekklesia
 *   without remainder (including decree-speed authority) is the essential
 *   form of self-rule, making any filter a compromise of democratic
 *   principle.
 *
 * KEY AGENTS:
 *   - Legal Stability Regime: Primary beneficiary (institutional/arbitrage) — gains prospective certainty, cross-generational institutional continuity, and predictable governance through standing law
 *   - Decree Sovereignty: Primary victim (powerless/trapped) — loses direct assembly authority over immediate crises, constrained by nomothetai panel review, cannot exit the procedural bottleneck
 *   - Moderate Assembly Members: Secondary actor (moderate/constrained) — experience both coordination benefit (protection from decree reversals like Mytilene) and extraction cost (reduced assembly omnipotence in crisis response)
 *   - Wealthy Oligarchic Factions: Organized actor (organized/constrained) — gain institutional stability for property rights but lose decree-speed ability to legislate faction advantage
 *   - Philosophical Rationalist Tradition: Observer (institutional/arbitrage) — interprets nomothetai maturation as the gradual institutionalization of reason over assembly passion
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the nomothetai filter as an inevitable feature of law itself rather than a contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(assembly_supremacy__nomothetai_maturation_reading, 0.38).
domain_priors:suppression_score(assembly_supremacy__nomothetai_maturation_reading, 0.52).
domain_priors:theater_ratio(assembly_supremacy__nomothetai_maturation_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(assembly_supremacy__nomothetai_maturation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(assembly_supremacy__nomothetai_maturation_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(assembly_supremacy__nomothetai_maturation_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(assembly_supremacy__nomothetai_maturation_reading, tangled_rope).
narrative_ontology:human_readable(assembly_supremacy__nomothetai_maturation_reading, "Nomothetai Maturation Reading: Assembly Supremacy Through Procedural Constraint").
narrative_ontology:topic_domain(assembly_supremacy__nomothetai_maturation_reading, "legal/institutional_history").

domain_priors:requires_active_enforcement(assembly_supremacy__nomothetai_maturation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(assembly_supremacy__nomothetai_maturation_reading, 'cdf06444-f2c5-4564-a4dc-92a052bf8238').
narrative_ontology:cs_kernel_codification('cdf06444-f2c5-4564-a4dc-92a052bf8238', fixed_text).
narrative_ontology:cs_authority_grounding('cdf06444-f2c5-4564-a4dc-92a052bf8238', extraction).
narrative_ontology:cs_interpretation_layer_present('cdf06444-f2c5-4564-a4dc-92a052bf8238').
narrative_ontology:cs_reading_relation('cdf06444-f2c5-4564-a4dc-92a052bf8238', assembly_supremacy__mytilene_volatility_reading, coexists_with).
narrative_ontology:cs_reading_relation('cdf06444-f2c5-4564-a4dc-92a052bf8238', assembly_supremacy__radical_self_rule_reading, influences).
narrative_ontology:cs_axiom('cdf06444-f2c5-4564-a4dc-92a052bf8238', foundational, procedural_maturation_enables_stability).
narrative_ontology:cs_axiom_status(procedural_maturation_enables_stability, holdable).
narrative_ontology:cs_axiom_grounding('cdf06444-f2c5-4564-a4dc-92a052bf8238', procedural_maturation_enables_stability, instrumental).
narrative_ontology:cs_axiom('cdf06444-f2c5-4564-a4dc-92a052bf8238', foundational, decree_suppression_necessary_for_law_protection).
narrative_ontology:cs_axiom_status(decree_suppression_necessary_for_law_protection, holdable).
narrative_ontology:cs_axiom_grounding('cdf06444-f2c5-4564-a4dc-92a052bf8238', decree_suppression_necessary_for_law_protection, empirically_contingent).
narrative_ontology:cs_reference_frame('cdf06444-f2c5-4564-a4dc-92a052bf8238', assembly_with_nomothetai_procedure).
narrative_ontology:cs_drift_state('cdf06444-f2c5-4564-a4dc-92a052bf8238', late_fourth_century_stabilization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cdf06444-f2c5-4564-a4dc-92a052bf8238', '').
narrative_ontology:cs_kernel_id(assembly_supremacy__nomothetai_maturation_reading, assembly_supremacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(assembly_supremacy__nomothetai_maturation_reading, legal_stability).
narrative_ontology:constraint_beneficiary(assembly_supremacy__nomothetai_maturation_reading, prospective_certainty).
narrative_ontology:constraint_victim(assembly_supremacy__nomothetai_maturation_reading, decree_sovereignty).
narrative_ontology:constraint_victim(assembly_supremacy__nomothetai_maturation_reading, assembly_omnipotence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DECREE CONSTITUENCY (SNARE) — Citizens who benefited from the speed and directness of fifth-century decree rule (immediate response to crises, no deliberative delay) face maximum extraction under nomothetai maturation. Trapped by the procedural requirement; cannot exit the legislative bottleneck without leaving the polis. The nomothetai panel suppresses the old mode of rapid assembly decision-making.
constraint_indexing:constraint_classification(assembly_supremacy__nomothetai_maturation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MODERATE ASSEMBLY MEMBER (TANGLED ROPE) — Ordinary citizens experience both coordination benefit (standing laws enable predictable governance) and extraction cost (procedural delays weaken assembly omnipotence, reduce their immediate power). Constrained by panel review but also protected by prospective law from decree reversals. Mixed experience: genuine coordination plus moderate extraction.
constraint_indexing:constraint_classification(assembly_supremacy__nomothetai_maturation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LEGAL STABILITY REGIME (ROPE) — The nomothetai panels and law/decree distinction establish prospective certainty and institutional continuity. This is a net beneficiary position: the constraint coordinates lawmaking through a stable procedure. Institutions can plan across generations when laws persist and decrees remain distinct. Arbitrage available through institutional adaptation to the new legal framework.
constraint_indexing:constraint_classification(assembly_supremacy__nomothetai_maturation_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: WEALTHY OLIGARCHIC FACTIONS (TANGLED ROPE) — Organized agents (families with substantial property and influence) experience the constraint as both coordination and extraction. The nomothetai filter protects their property rights through stable law but constrains their ability to deploy decree-speed legislation to seize political advantage. They have agency (organized power) but face real constraints (procedural review of their preferred decrees).
constraint_indexing:constraint_classification(assembly_supremacy__nomothetai_maturation_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: PHILOSOPHICAL RATIONALIST VIEW (ROPE) — From a civilizational perspective, the nomothetai maturation represents the institutionalization of law-making as a rational, deliberative process distinct from the affective speed of assembly passion. This is a coordination achievement: reason gradually gains ground over emotion. The constraint is experienced as enabling rational governance.
constraint_indexing:constraint_classification(assembly_supremacy__nomothetai_maturation_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal analytical perspective, the distinction between law and decree, and the requirement for deliberative procedure, appear as immutable features of any stable governance system. The nomothetai maturation is read as the inevitable crystallization of how law operates naturally: stability requires standing rules, and standing rules require deliberative oversight. However, this naturalization masks the contingent institutional choice to retrofit the assembly with nomothetai panels — a deliberate structural intervention, not a law of political nature.
constraint_indexing:constraint_classification(assembly_supremacy__nomothetai_maturation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(assembly_supremacy__nomothetai_maturation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(assembly_supremacy__nomothetai_maturation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(assembly_supremacy__nomothetai_maturation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(assembly_supremacy__nomothetai_maturation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The nomothetai filter genuinely suppresses decree-rule omnipotence and slows lawmaking, reducing the assembly's immediate power over crises. But the constraint is not maximally extractive because it is self-imposed by the assembly (not externally coercive) and because it enables coordination benefits (legal stability) that all agents can access. The constraint extracts primarily from those who prefer decree-speed authority (oligarchs wanting rapid advantage, citizens wanting immediate crisis response), but the extraction is calibrated to the gain in institutional stability. Over the interval (measured in decades), extractiveness rises slightly (0.28 → 0.38) as the nomothetai procedure becomes institutionalized and the assembly develops path dependence on panel review. Theater ratio (0.48): Moderate. The nomothetai panels have some performative content — the elaborate procedure of law review adds ritual and deliberative theater. But the theater is not dominant (as it would be in a piton constraint). The panels genuinely slow legislation and enable substantive review. Theater rises over the interval as the procedure becomes routinized and the ritual gains weight relative to the practical effect. Suppression (0.52): Moderate-high. The nomothetai filter suppresses decree-rule authority and requires panel approval before law enactment. This is real suppression of an alternative (direct assembly legislation), not merely coordination cost. Suppression rises over the interval as the assembly develops stronger norms around the law/decree distinction and becomes more reluctant to bypass panels. By the end of the interval, suppression has stabilized at 0.52 — the assembly has internalized the nomothetai procedure as the proper mode of lawmaking.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence. Decree constituencies experience it as a snare (trapped by procedural bottleneck, losing crisis-response speed). Moderate assembly members experience tangled rope (coordination benefit balanced against reduced omnipotence). Legal stability regimes experience rope (pure coordination and benefit). Oligarchic factions experience tangled rope (their property rights are protected, but their decree-speed legislative advantage is constrained). The rationalist philosophical tradition experiences rope (reason gradually institutionalized). The analytical observer at civilizational scope risks seeing mountain (the law/decree distinction and nomothetai procedure as immutable features of any legal system). This last perspective instantiates the false summit: the mountain classification naturalizes what is actually a contingent choice. The constraint is NOT a law of nature — it is a deliberate institutional retrofit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position. Legal stability regimes are beneficiaries with arbitrage options (can adapt institutional behavior to exploit the new legal framework): d ≈ 0.15, f(d) ≈ -0.01, low effective extraction chi. Decree sovereigns are victims with trapped exit (cannot avoid the procedural bottleneck): d ≈ 0.95, f(d) ≈ 1.42, high effective extraction chi. Moderate assembly members are mixed (some benefit from stability, some pay cost of slowed decision-making): d ≈ 0.52, f(d) ≈ 0.65, moderate effective extraction chi. Oligarchic factions are mixed organized agents: d ≈ 0.48, f(d) ≈ 0.60, moderate effective extraction chi. The canonical fallback for institutional beneficiaries (legal stability) is d ≈ 0.00, f(d) ≈ -0.12 (institutional power atom, arbitrage exit). The analytical observer at universal scope has canonical d ≈ 0.73, f(d) ≈ 1.15 (analytical power atom). These directionalities are stable across the 30-year interval; the increase in extractiveness over time reflects the assembly's strengthening commitment to the nomothetai procedure (rising suppression_requirement), not changes in agent directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by clearly distinguishing coordination (legal stability, cross-generational planning) from extraction (suppression of decree-rule omnipotence, procedural slowdown in crisis response). The constraint is genuinely tangled rope: both functions are present, and both are structural. The nomothetai maturation is not merely disguised extraction (snare) because legal stability is a real coordination good. But it is not pure coordination (rope) because decree-rule suppression is real extraction. The intermediate extractiveness (0.38) and the presence of both beneficiaries and victims in the base properties correctly reflect the hybrid nature. The theater ratio (0.48) indicates that the procedure has meaningful deliberative content, not just performative ritual. The mandatrophy is resolved by accepting that institutional maturation often involves trading one form of authority (decree-speed omnipotence) for another (legal stability). This is not a flaw in the constraint classification — it is the normal operation of tangled rope constraints in institutional governance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decree_vs_law_boundary_stability,
    'How stable is the fourth-century distinction between decrees (psephismata) and laws (nomoi) in practice? Can the assembly consistently maintain the boundary or does boundary slippage occur?',
    'Textual analysis of fourth-century inscriptions and Aristotelian Constitution of the Athenians; tracking of cases where the assembly attempted to legislate directly vs. nomothetai review. Identification of boundary violations and reclassifications.',
    'If boundary is stable: nomothetai constraint is genuine coordination + extraction hybrid (tangled rope confirmed). If boundary slips frequently: the constraint is largely performative (theater_ratio increases, classification shifts toward piton).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decree_vs_law_boundary_stability, empirical, 'Stability of the law/decree distinction in fourth-century practice').

omega_variable(
    nomothetai_panel_capture_vulnerability,
    'To what extent can wealthy oligarchic factions (or democratic factions) capture or influence the nomothetai panels to block or enable legislation aligned with their interests?',
    'Analysis of nomothetai composition (selection process, socioeconomic background of jurors), voting patterns in documented cases, and correlation between faction preference and panel decision. Investigation of whether panel membership itself becomes a site of political struggle.',
    'If capture is easy: the constraint is disguised oligarchic extraction (snare, victims shift to exclude oligarchs). If panels are genuinely difficult to capture: constraint is a genuine institutional check on oligarchic power (tangled rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nomothetai_panel_capture_vulnerability, empirical, 'Vulnerability of nomothetai panels to factional capture').

omega_variable(
    crisis_response_suppression_cost,
    'How many crises (military threats, plague, starvation, external war) require rapid decree-speed decision-making that the nomothetai procedure cannot accommodate? What is the death toll or strategic cost of procedural delay?',
    'Historical reconstruction of crises during the fourth century; comparison of response times under nomothetai procedure vs. fifth-century decree-rule models. Evaluation of whether emergencies override the procedure or force workarounds.',
    'If crisis suppression cost is high: extractiveness increases (the constraint becomes costlier to the polity overall). If emergencies can override or the cost is manageable: extractiveness is correctly calibrated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crisis_response_suppression_cost, empirical, 'Suppression cost in crisis response due to procedural delay').

omega_variable(
    reading_ambiguity_naturalization_vs_choice,
    'Is this reading describing an inevitable institutional maturation (the natural law of governance evolution), or a contingent political choice to trade assembly speed for legal stability?',
    'Comparative historical analysis: other Greek city-states that did NOT implement nomothetai procedures; examination of whether the choice was debated or presented as natural inevitability; analysis of who advocated for the change and why.',
    'If described as natural: reading instantiates false summit (naturalizes contingent choice). If described as contested choice: reading is correctly positioned as ONE interpretation among siblings (sibling readings emphasize assembly omnipotence or volatility instead). Classification consequence: mountain perspective becomes false summit candidate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_ambiguity_naturalization_vs_choice, conceptual, 'Whether nomothetai maturation is inevitable institutional evolution or contingent political choice — kernel-level ambiguity that distinguishes this reading from radical_self_rule_reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(assembly_supremacy__nomothetai_maturation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nom_tr_t0, assembly_supremacy__nomothetai_maturation_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(nom_tr_t15, assembly_supremacy__nomothetai_maturation_reading, theater_ratio, 15, 0.44).
narrative_ontology:measurement(nom_tr_t30, assembly_supremacy__nomothetai_maturation_reading, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(nom_be_t0, assembly_supremacy__nomothetai_maturation_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(nom_be_t15, assembly_supremacy__nomothetai_maturation_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(nom_be_t30, assembly_supremacy__nomothetai_maturation_reading, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(nom_su_t0, assembly_supremacy__nomothetai_maturation_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(nom_su_t15, assembly_supremacy__nomothetai_maturation_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(nom_su_t30, assembly_supremacy__nomothetai_maturation_reading, suppression_requirement, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(assembly_supremacy__nomothetai_maturation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(assembly_supremacy__nomothetai_maturation_reading, assembly_supremacy__mytilene_volatility_reading).
narrative_ontology:affects_constraint(assembly_supremacy__nomothetai_maturation_reading, assembly_supremacy__radical_self_rule_reading).

% DUAL FORMULATION NOTE:
% The assembly_supremacy kernel is instantiated by three constraint stories representing three live interpretations of what assembly supremacy means after fourth-century reforms. The nomothetai_maturation_reading emphasizes institutional evolution toward legal stability through procedural filter. Mytilene_volatility_reading emphasizes trauma-driven awareness of decree-rule cost. Radical_self_rule_reading rejects any filter as compromise of democratic essence. All three share the same empirical domain (fourth-century Athenian assembly) but have different epsilon values reflecting different interpretations of whether the nomothetai procedure is primarily coordination, extraction, or degraded function. Link via network.affects_constraints to enable sibling constraint analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
