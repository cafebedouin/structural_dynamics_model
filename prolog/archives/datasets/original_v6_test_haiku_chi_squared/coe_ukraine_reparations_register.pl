% ============================================================================
% CONSTRAINT STORY: coe_ukraine_reparations_register
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coe_ukraine_reparations_register, []).

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
 *   constraint_id: coe_ukraine_reparations_register
 *   human_readable: Council of Europe's Register of Damage for Ukraine
 *   domain: geopolitical/legal/reparations
 *
 * SUMMARY:
 *   The Council of Europe's Register of Damage for Ukraine creates a
 *   structural tension between its genuine coordination function
 *   (establishing a shared, internationally-recognized legal documentation of
 *   war crimes and damages) and its extractive dimension (Western
 *   institutional control over evidentiary standards, credibility gates, and
 *   enforcement mechanisms). The register solves a real problem: without
 *   coordinated documentation across multiple jurisdictions and legal
 *   systems, future reparations claims will fragment, Ukraine's losses will
 *   be underestimated, and accountability will remain aspirational. Yet the
 *   solution embeds Western institutional power into the heart of Ukraine's
 *   reparations claim-making process. Ukrainian individuals must participate
 *   in a Western-controlled register to document their losses; Ukrainian
 *   state must accept Western-determined credibility standards; Ukrainian
 *   narratives of damage are filtered through international law frameworks
 *   that privilege certain claim types over others. The register is
 *   simultaneously a coordination breakthrough and a mechanism of Western
 *   institutional extraction.
 *
 * KEY AGENTS:
 *   - Ukrainian Individual Claimants: Primary victims (powerless/trapped) — must use register as only viable pathway to document reparations claims, but trapped by dependence on Western institutional gatekeeping
 *   - Ukrainian State and Civil Society: Primary secondary actor (organized/constrained) — benefits from coordination mechanism and Western security support conditioned on accountability frameworks; constrained by inability to operate independent verification parallel to register
 *   - Western Coalition (CoE, US, Canada, Japan): Primary beneficiary (institutional/arbitrage) — captures moral authority, legal precedent-setting, deterrence signaling, alliance cohesion; low institutional cost; can exit if calculations shift
 *   - International Criminal Justice System: Institutional (institutional/arbitrage) — maintains prestige and legitimacy claims despite low enforcement capacity; register documents claims but enforcement remains contingent on geopolitical shifts
 *   - Russian State: Secondary victim (powerful/mobile) — experiences register as extraction mechanism (legal record-building against it); can exit geopolitically but at extreme cost due to sanctions regime constraints
 *   - Alternative Reparations Pathway Coalition: Emerging challenger (moderate/mobile) — NGOs and civil society building parallel documentation systems with potential to reduce Western institutional dependency over time
 *   - Analytical Observer: Structural analyst — sees fusion of genuine coordination function with institutional extraction; identifies mandatrophy in the inseparability of the two
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coe_ukraine_reparations_register, 0.52).
domain_priors:suppression_score(coe_ukraine_reparations_register, 0.68).
domain_priors:theater_ratio(coe_ukraine_reparations_register, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coe_ukraine_reparations_register, extractiveness, 0.52).
narrative_ontology:constraint_metric(coe_ukraine_reparations_register, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(coe_ukraine_reparations_register, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coe_ukraine_reparations_register, tangled_rope).
narrative_ontology:human_readable(coe_ukraine_reparations_register, "Council of Europe's Register of Damage for Ukraine").
narrative_ontology:topic_domain(coe_ukraine_reparations_register, "geopolitical/legal/reparations").

domain_priors:requires_active_enforcement(coe_ukraine_reparations_register).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coe_ukraine_reparations_register, ukrainian_claimants).
narrative_ontology:constraint_beneficiary(coe_ukraine_reparations_register, western_legal_accountability_agenda).
narrative_ontology:constraint_beneficiary(coe_ukraine_reparations_register, international_law_precedent_builders).
narrative_ontology:constraint_victim(coe_ukraine_reparations_register, russian_state_defendants).
narrative_ontology:constraint_victim(coe_ukraine_reparations_register, dispute_resolution_legitimacy).
narrative_ontology:constraint_victim(coe_ukraine_reparations_register, alternative_reparations_pathways).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UKRAINIAN INDIVIDUAL CLAIMANT (SNARE) — Trapped by lack of alternative mechanisms for reparations documentation. Must participate in register to create legal record, but participation entrenches dependency on Western-controlled process. Cannot exit without abandoning reparations claim. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(coe_ukraine_reparations_register, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UKRAINIAN STATE AND CIVIL SOCIETY (TANGLED ROPE) — Benefits from coordination mechanism (international legal documentation, precedent-building for future claims, access to Western security/financial support tied to accountability frameworks). Constrained by dependency on Western institutional support and inability to operate independent verification parallel to register. Coordination function is genuine (creating shared legal record); extraction is asymmetric (register controlled by CoE, not Ukraine). Requires active enforcement: Western states determine which claims are 'credible,' have priority, get prosecuted. d≈0.58, f(d)≈0.72, σ=1.0 → χ≈0.38.
constraint_indexing:constraint_classification(coe_ukraine_reparations_register, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WESTERN COALITION (ROPE) — Experiences register primarily as coordination mechanism: documenting atrocities, establishing legal precedent, deterring future violations, signaling alliance cohesion on international law. Captures benefit of moral authority and legal precedent-setting without bearing extraction cost. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary via arbitrage exit option (can withdraw institutional support if calculations shift).
constraint_indexing:constraint_classification(coe_ukraine_reparations_register, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL CRIMINAL JUSTICE SYSTEM (PITON) — Register is partly performative: creates impression of accountability pathway while actual prosecutions remain contingent on geopolitical shifts, enforcement capacity, and ICC jurisdiction limitations. ICC has convicted very few sitting state leaders; CoE register documents claims but cannot compel enforcement against Russian state. theater_ratio=0.58 (moderate-high). The institutional apparatus persists through prestige and legitimacy claims despite low enforcement capacity. d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(coe_ukraine_reparations_register, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: RUSSIAN STATE (SNARE) — Experiences register as pure extraction mechanism: legal record-building used against it, international cooperation nodes strengthened against it, no reciprocal verification mechanism or appeal process. Can exit geopolitically (withdraw from international law frameworks) but this carries massive cost. Effective extraction χ≈0.71 despite powerful agent status, because mobility is constrained by sanctions regime. d≈0.88, f(d)≈1.30, σ=1.2 → χ≈0.68.
constraint_indexing:constraint_classification(coe_ukraine_reparations_register, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ALTERNATIVE REPARATIONS PATHWAY COALITION (SCAFFOLD) — NGOs, human rights organizations, and neutral mediators building parallel documentation systems (Ukrainian documentation, Belarusian civil society archives, independent investigative journalism) with sunset logic: as these mature, dependence on Western-controlled register decreases. Theater is lower in these parallel systems (more transparent methodologies, community-controlled narratives). d≈0.45, f(d)≈0.50, σ=0.9 → χ≈0.21. Scaffold gate satisfied: coordination function (documentation) + sunset clause (alternatives can replace register).
constraint_indexing:constraint_classification(coe_ukraine_reparations_register, scaffold,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Register simultaneously solves a genuine coordination problem (establishing shared legal documentation across jurisdictions for eventual reparations) AND extracts through Western institutional control (determines evidentiary standards, credibility gates, priority sequencing, enforcement mechanisms). The coordination and extraction are structurally inseparable. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.72. Mandatrophy: the register cannot be decomposed into 'pure coordination' and 'pure extraction' — they are institutionally fused.
constraint_indexing:constraint_classification(coe_ukraine_reparations_register, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coe_ukraine_reparations_register_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(coe_ukraine_reparations_register, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coe_ukraine_reparations_register, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(coe_ukraine_reparations_register, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(coe_ukraine_reparations_register, TR),
    TR >= 0.70.

:- end_tests(coe_ukraine_reparations_register_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The register extracts Western institutional control over Ukraine's reparations narrative and evidentiary standards. However, this is not maximum extraction (ε > 0.70) because the coordination benefit is genuine — without the register, Ukraine's claims would be far more fragmented and less credible. The extraction cost includes: (1) Western gatekeeping of which claims are 'credible'; (2) evidentiary standards that privilege Western legal frameworks over Ukrainian documentation practices; (3) conditionality linking security support to accountability participation; (4) long-term dependence on Western-controlled institution. Suppression (0.68): High. Ukrainian claimants have few alternatives — this is the primary international mechanism for reparations documentation. Alternative pathways (Ukrainian state archive, civil society documentation, investigative journalism) exist but have lower international legal standing. Ukrainian state cannot easily exit without appearing to abandon accountability. Theater ratio (0.58): Moderate. The register involves substantial performative elements: international coordination ceremonies, credibility assessments, legitimacy claims about 'rule of law.' However, the underlying documentation work is functional — claims are actually recorded, evidence is preserved, legal precedent is being established. The theater increases over time as the gap between accountability promises and enforcement capacity becomes visible.
 *
 * PERSPECTIVAL GAP:
 *   The gap between Ukrainian claimant experience and Western coalition experience is maximal. A Ukrainian individual sees a snare (trapped in Western-controlled process, high extraction cost to participate). The Western coalition sees a rope (coordination mechanism solving accountability problem with minimal cost to them). The Ukrainian state sees tangled rope (genuine coordination benefit from international legal recognition, but asymmetric extraction through Western institutional control). The Russian state sees a snare (pure extraction mechanism with constrained exit options). The alternative pathway coalition sees a scaffold (temporary institution that will be supplemented/replaced as Ukrainian-led documentation matures). The international criminal justice system sees a piton (performative apparatus — the register documents claims but enforcement capacity remains minimal). The analytical observer sees the mandate ambiguity: the register is genuinely needed, but its Western institutional embedding prevents it from being pure coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Ukrainian claimants: Victim + trapped → d≈0.92, f(d)≈1.38. Extreme directionality — no exit option, full cost-bearing. Ukrainian state: Victim + constrained (organized power but constrained exit) → d≈0.58, f(d)≈0.72. Significant but not maximal extraction due to organizational capacity and alternative pathways emerging. Western coalition: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary position; arbitrage exit means they can withdraw support if calculations shift. Russian state: Victim + mobile (theoretically mobile but practically constrained by sanctions) → d≈0.88, f(d)≈1.30. High directionality despite powerful agent status because exit costs (further sanctions escalation) are extreme. Alternative pathway coalition: Moderate + mobile → d≈0.45, f(d)≈0.50. Low-moderate extraction because they have agency to develop alternatives. International criminal justice system: Institutional + arbitrage → d≈0.10, f(d)≈-0.08. Net beneficiary from prestige gains; arbitrage exit via redefining enforcement metrics.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY PARTIALLY RESOLVED: The register cannot be classified as pure coordination because the Western institutional embedding (credibility gates, evidentiary standards, enforcement mechanisms) constitutes genuine extraction. However, it also cannot be classified as pure extraction because the coordination benefit to Ukraine (international legal recognition, precedent-setting for future reparations, deterrence signaling) is substantial and real. The constraint is genuinely tangled: the coordination function and the extraction mechanism are institutionally fused. Decomposition into two separate constraints (one for coordination, one for extraction) fails because they are operationally inseparable — you cannot have the coordination benefit without accepting the Western institutional control, and you cannot extract the control without undermining the coordination function. The mandatrophy resolution lies in accepting the tangled rope classification as the only structurally honest description. The analytical observer might be tempted to see a mountain ('reparations coordination is inherently Western-controlled due to power asymmetries and expertise requirements') — this is a false summit. The Western embedding is contingent and institutional, not natural or inevitable. Alternative pathways (Ukrainian-led documentation, non-aligned mediators) could provide coordination functions with different control structures, though currently with lower international legal standing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reparations_enforcement_capacity,
    'Will Ukraine/Western coalition actually enforce asset seizures and reparations against Russia once conflict ends, or does the register become a symbolic documentation of claims without material consequence?',
    'Post-conflict settlement negotiations; asset seizure mechanisms; treaty enforcement mechanisms; comparison with historical reparations (Germany post-WWI, Japan post-WWII, Iraq post-invasion)',
    'If enforcement is real: register is genuine tangled rope with extraction mechanism backed by power. If symbolic only: register becomes piton (performative apparatus maintained by legitimacy claims).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reparations_enforcement_capacity, empirical, 'Whether documented claims translate to actual reparations enforcement').

omega_variable(
    western_control_legitimacy,
    'Do Ukrainian claimants experience Western-controlled evidentiary and credibility gates as legitimate international standards or as Western institutional power extracting control over reparations narratives?',
    'Ukrainian civil society surveys; comparison of Ukrainian narratives in register vs Ukrainian documentation systems; analysis of rejected vs accepted claims by origin',
    'If legitimate: tangled rope with genuine coordination function, moderate extraction due to procedural fairness. If extractive: snare (Western institutional capture of reparations narratives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(western_control_legitimacy, empirical, 'Ukrainian perception of Western credibility gates as legitimate vs extractive').

omega_variable(
    alternative_pathway_viability,
    'Can Ukrainian-led and neutral documentation systems (Ukrainian state archive, Belarusian civil society, investigative journalism) actually replace or substantially supplement the CoE register as credible legal evidence?',
    'International legal analysis of evidentiary admissibility; adoption rates in parallel documentation systems; comparative prosecutorial reliance on register vs alternatives in future proceedings',
    'If alternatives are viable: register becomes scaffold with genuine sunset (Western dependency declines over time). If register remains dominant: register is structural snare (no real exit option despite appearances).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_pathway_viability, empirical, 'Whether alternative documentation systems can provide competitive legal standing').

omega_variable(
    russian_exit_option_cost,
    'Are the costs of Russia exiting the international legal framework (further sanctions, permanent ICC indictments, sanctions escalation) actually constraining or is ''mobile'' exit option more realistic than the powerful agent classification assumes?',
    'Sanctions regime modeling; ICC enforcement capacity analysis; Russian strategic alternatives in post-conflict settlements',
    'If exit is truly constrained: Russian state experiences maximum extraction (snare classification valid). If exit is more mobile: Russian classification shifts toward tangled rope or institutional perspective (extraction is negotiable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(russian_exit_option_cost, empirical, 'Whether Russia''s exit from international legal regime is actually constrained or strategically mobile').

omega_variable(
    accountability_precedent_generalization,
    'Does the CoE register establish a precedent that will be applied symmetrically to future conflicts (Western-led interventions, allied state violations) or asymmetrically only to adversaries?',
    'Future application analysis; comparative treatment of accountability mechanisms in conflicts involving Western states vs non-aligned states; legal scholarship on precedent binding',
    'If symmetrical: register is pure coordination (Rope). If asymmetrical: register is extraction mechanism (Snare) used to consolidate Western legal hegemony.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(accountability_precedent_generalization, conceptual, 'Whether accountability register precedent will apply symmetrically across geopolitical contexts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coe_ukraine_reparations_register, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coe_reg_tr_t0, coe_ukraine_reparations_register, theater_ratio, 0, 0.42).
narrative_ontology:measurement(coe_reg_tr_t2, coe_ukraine_reparations_register, theater_ratio, 2, 0.52).
narrative_ontology:measurement(coe_reg_tr_t5, coe_ukraine_reparations_register, theater_ratio, 5, 0.58).

% Extraction over time
narrative_ontology:measurement(coe_reg_be_t0, coe_ukraine_reparations_register, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(coe_reg_be_t2, coe_ukraine_reparations_register, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(coe_reg_be_t5, coe_ukraine_reparations_register, base_extractiveness, 5, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coe_ukraine_reparations_register, enforcement_mechanism).
narrative_ontology:affects_constraint(coe_ukraine_reparations_register, international_criminal_court_enforcement).
narrative_ontology:affects_constraint(coe_ukraine_reparations_register, post_conflict_reparations_mechanisms).
narrative_ontology:affects_constraint(coe_ukraine_reparations_register, western_institutional_hegemony_in_law).

% DUAL FORMULATION NOTE:
% The CoE register decomposes into two perspectives: one emphasizing coordination (genuine international legal mechanism solving documentation problem), one emphasizing extraction (Western institutional control over reparations narratives). These cannot be separated into two constraint stories because they are operationally fused. The tangled rope classification is the only structurally honest model. The constraint affects downstream reparations enforcement (which mechanisms will be used to collect/distribute awards) and upstream institutional hegemony claims (whether international law is symmetrically applied or selectively enforced).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coe_ukraine_reparations_register, powerful, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
