% ============================================================================
% CONSTRAINT STORY: tribunician_sacrosanctity__imperial_absorption_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tribunician_sacrosanctity__imperial_absorption_reading, []).

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
 *   constraint_id: tribunician_sacrosanctity__imperial_absorption_reading
 *   human_readable: Tribunician Sacrosanctity as Imperial Absorption: The Plebs' Shield Refitted as the Throne's Foundation
 *   domain: legal/doctrinal/constitutional
 *
 * SUMMARY:
 *   Augustus's absorption of perpetual tribunician power represents the
 *   refashioning of a constraint designed to protect the powerless into an
 *   instrument of monarchic inviolability. The tribunate, established in 494
 *   BCE as the plebeian assembly's shield against magistrial tyranny,
 *   possessed sacrosanctity — the office itself was inviolable, and its veto
 *   could constrain any magistrate. By taking tribunician power perpetually
 *   and entirely, Augustus converted the plebs' counter-power into the
 *   throne's foundation. The aura of untouchability, once belonging to an
 *   office that stood between the citizen and the magistrate's rods, now
 *   belonged to the ruler alone. This reading instantiates the
 *   imperial_absorption_reading of the tribunician_sacrosanctity kernel — one
 *   of three competing readings that exist simultaneously in the contested
 *   Roman constitutional tradition. This constraint is a snare from the
 *   powerless perspective and a tangled rope from the Senate's perspective;
 *   it appears as rope to the princeps (who experiences it as legitimate
 *   coordination through inherited legitimacy); and it risks appearing as a
 *   natural law (mountain) to observers who naturalize the eventual
 *   concentration of power as inevitable. The key structural delta from the
 *   other readings: this reading assumes sacrosanctity was successfully
 *   absorbed (the aura transferred to the emperor), benefiting the princeps
 *   inviolable forever while victimizing the tribunate as an independent
 *   counter-power and the plebeian assembly's collective shield. The
 *   theater_ratio rises over the interval (0.25 → 0.55) as the tribunate
 *   persists in increasingly ceremonial form; suppression rises (0.48 → 0.72)
 *   as the real mechanisms of veto and constraint are concentrated in the
 *   imperial person.
 *
 * KEY AGENTS:
 *   - Augustus (The Princeps): Primary beneficiary (institutional/arbitrage) — absorbs the aura of sacrosanctity, becoming inviolable without explicit monarchy; experiences the constraint as coordination of legitimacy
 *   - The Plebeian Assembly: Primary victim (powerless/trapped) — loses the tribune's veto entirely; the shield becomes the instrument of suppression
 *   - The Tribunate (as Institution): Secondary victim (institutional/constrained) — survives formally but loses functional independence; becomes tributary to the emperor rather than advocate for the plebs
 *   - The Senate: Secondary beneficiary-victim (organized/constrained) — gains legitimacy-sharing but loses sovereignty; bound by imperial veto dressed in tribunician language
 *   - Republican Institutional Forms: Tertiary actor (institutional/arbitrage) — persist as theater; maintained by institutional inertia
 *   - The Analytical Observer: Vantage point at risk of naturalizing contingency as necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tribunician_sacrosanctity__imperial_absorption_reading, 0.68).
domain_priors:suppression_score(tribunician_sacrosanctity__imperial_absorption_reading, 0.72).
domain_priors:theater_ratio(tribunician_sacrosanctity__imperial_absorption_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tribunician_sacrosanctity__imperial_absorption_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(tribunician_sacrosanctity__imperial_absorption_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(tribunician_sacrosanctity__imperial_absorption_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tribunician_sacrosanctity__imperial_absorption_reading, snare).
narrative_ontology:human_readable(tribunician_sacrosanctity__imperial_absorption_reading, "Tribunician Sacrosanctity as Imperial Absorption: The Plebs' Shield Refitted as the Throne's Foundation").
narrative_ontology:topic_domain(tribunician_sacrosanctity__imperial_absorption_reading, "legal/doctrinal/constitutional").

domain_priors:requires_active_enforcement(tribunician_sacrosanctity__imperial_absorption_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tribunician_sacrosanctity__imperial_absorption_reading, 'cfc2067f-4af8-4463-b030-a48440332ce2').
narrative_ontology:cs_kernel_codification('cfc2067f-4af8-4463-b030-a48440332ce2', formalized).
narrative_ontology:cs_authority_grounding('cfc2067f-4af8-4463-b030-a48440332ce2', lineage).
narrative_ontology:cs_interpretation_layer_present('cfc2067f-4af8-4463-b030-a48440332ce2').
narrative_ontology:cs_reading_relation('cfc2067f-4af8-4463-b030-a48440332ce2', tribunician_sacrosanctity__demagogic_lever_reading, influences).
narrative_ontology:cs_reading_relation('cfc2067f-4af8-4463-b030-a48440332ce2', tribunician_sacrosanctity__popular_shield_reading, forecloses).
narrative_ontology:cs_axiom('cfc2067f-4af8-4463-b030-a48440332ce2', foundational, sacrosanctity_transferred_to_imperial_person).
narrative_ontology:cs_axiom_status(sacrosanctity_transferred_to_imperial_person, holdable).
narrative_ontology:cs_axiom_grounding('cfc2067f-4af8-4463-b030-a48440332ce2', sacrosanctity_transferred_to_imperial_person, empirically_contingent).
narrative_ontology:cs_axiom('cfc2067f-4af8-4463-b030-a48440332ce2', foundational, tribunate_veto_absorbed_into_princeps_authority).
narrative_ontology:cs_axiom_status(tribunate_veto_absorbed_into_princeps_authority, holdable).
narrative_ontology:cs_axiom_grounding('cfc2067f-4af8-4463-b030-a48440332ce2', tribunate_veto_absorbed_into_princeps_authority, empirically_contingent).
narrative_ontology:cs_axiom('cfc2067f-4af8-4463-b030-a48440332ce2', secondary, plebeian_counter_power_structurally_foreclosed).
narrative_ontology:cs_axiom_status(plebeian_counter_power_structurally_foreclosed, holdable).
narrative_ontology:cs_axiom_grounding('cfc2067f-4af8-4463-b030-a48440332ce2', plebeian_counter_power_structurally_foreclosed, deontological).
narrative_ontology:cs_reference_frame('cfc2067f-4af8-4463-b030-a48440332ce2', republican_tribunician_protection).
narrative_ontology:cs_drift_state('cfc2067f-4af8-4463-b030-a48440332ce2', post_augustan_imperial_authority, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('cfc2067f-4af8-4463-b030-a48440332ce2', '').
narrative_ontology:cs_kernel_id(tribunician_sacrosanctity__imperial_absorption_reading, tribunician_sacrosanctity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tribunician_sacrosanctity__imperial_absorption_reading, princeps_inviolable).
narrative_ontology:constraint_victim(tribunician_sacrosanctity__imperial_absorption_reading, tribunate_as_counter_power).
narrative_ontology:constraint_victim(tribunician_sacrosanctity__imperial_absorption_reading, plebeian_assembly_collective).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PLEBEIAN ASSEMBLY (SNARE) — The assembly loses the tribune's veto entirely. Sacrosanctity, once their shield against magistrial rods, is now the emperor's inviolability. No exit from monarchic authority; the very mechanism designed to protect the powerless has been absorbed into the instrument of their suppression. Maximum experienced extraction with zero agency.
constraint_indexing:constraint_classification(tribunician_sacrosanctity__imperial_absorption_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TRIBUNATE AS COUNTER-POWER (SNARE, GENERATIONAL) — The office survives formally but loses functional independence. Tribunes under Augustus are tribute-collectors for the emperor, not advocates for the plebs. The institution persists as costume — performative rituals without veto capacity. Trapped at the generational horizon: a successor generation of tribunes cannot restore the office's original function without overthrowing the princeps.
constraint_indexing:constraint_classification(tribunician_sacrosanctity__imperial_absorption_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: THE PRINCEPS (ROPE) — Augustus experiences the constraint as pure coordination: he absorbs the sacrosanctity aura to become inviolable without appearing to overthrow Republican institutions. The tribune's untouchability is refitted seamlessly into the emperor's person. Net beneficiary with full arbitrage — he can exit this arrangement by returning sacrosanctity to the tribunal, but has no incentive to do so. He experiences legitimate coordination (stabilizing the regime through inherited legitimacy language) alongside maximal extraction (monopolizing the mechanism that once constrained him).
constraint_indexing:constraint_classification(tribunician_sacrosanctity__imperial_absorption_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE SENATE (TANGLED ROPE) — The Senate experiences this constraint as mixed coordination and extraction. Augustus preserves Senate debate and procedural forms, creating genuine coordination on routine matters. But the Senate's freedom is constrained by the tribune's (now imperial) power of veto — they cannot overturn the emperor's will. The Senate benefits from legitimacy-sharing (appearing consultative to provincial elites) but bears the cost of actual sovereignty loss. Organized but not mobile.
constraint_indexing:constraint_classification(tribunician_sacrosanctity__imperial_absorption_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REPUBLICAN INSTITUTIONAL FORM (PITON) — The forms of Republican governance (Senate, magistrates, assemblies) persist as theater. The tribunes still exist, elections still occur, veto power is still formally declared — but the real coordination and extraction happen through the imperial person, not the Republican structures. The Republican machinery is maintained by institutional inertia; Augustus preserves it because overthrowing it explicitly would delegitimize the regime. Theater ratio is moderate (0.55) because some genuine coordination still happens through Senate procedure, but the core power mechanics are theatrical. The constraint maintains itself through the performance of Republican continuity.
constraint_indexing:constraint_classification(tribunician_sacrosanctity__imperial_absorption_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, this reading naturalizes the absorption as inevitable: any regime that centralized must eventually consolidate veto power into a single hand; sacrosanctity must eventually attach to the ruler rather than the ruled. This perspective risks treating the contingent institutional choice (Augustus's decision to absorb rather than abolish) as a structural necessity. However, the sibling readings (popular_shield_reading, demagogic_lever_reading) prove the tribunitiate's fate was not predetermined — other regimes maintained or weaponized the office differently. This is a false summit: the naturalizing reading obscures the contingency of Augustus's choice to refashion legitimacy rather than eliminate it.
constraint_indexing:constraint_classification(tribunician_sacrosanctity__imperial_absorption_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: THE STRUCTURAL INNOVATOR (ROPE) — From the vantage of political engineering, Augustus's move solves a genuine coordination problem: how to stabilize autocratic rule without explicitly declaring it (which would trigger Republican resistance). By absorbing the tribunate's sacrosanctity, he reuses existing legitimacy language, reducing coordination cost. This perspective sees the constraint as elegant rather than extractive — a solution to the problem of imperial legitimacy. But this view ignores the cost borne by those (the plebeian assembly, the tribunate as institution) who lose their counter-power.
constraint_indexing:constraint_classification(tribunician_sacrosanctity__imperial_absorption_reading, rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tribunician_sacrosanctity__imperial_absorption_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tribunician_sacrosanctity__imperial_absorption_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tribunician_sacrosanctity__imperial_absorption_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tribunician_sacrosanctity__imperial_absorption_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(tribunician_sacrosanctity__imperial_absorption_reading, TR),
    TR >= 0.70.

:- end_tests(tribunician_sacrosanctity__imperial_absorption_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Augustus extracts monopoly over the untouchability mechanism; the princeps becomes inviolable where the tribune was once untouchable. The beneficiary gains permanent extraction capacity (perpetual tribunician power) while the victim set loses counter-power entirely. The extraction accumulates over the interval as successors cement the absorbed authority — by year 50, the tribunal is clearly subordinate. Suppression (0.72): High. The plebeian assembly faces complete institutional foreclosure: they cannot veto imperial decisions through the tribunate (the mechanism is now the emperor's). Tribunes cannot resist the emperor using tribunician power (it belongs to him). The suppression is structural and comprehensive — no alternative veto mechanism exists. Theater ratio (0.55, rising from 0.25): The Republican forms persist as ceremony. Elections still occur, tribunes still exist, procedural debate continues — but the real power mechanics (veto, inviolability, constraint on magistrates) are now imperial. The rise reflects increasing ceremonial maintenance as the regime solidifies; later emperors need more theatrical Republican performance to maintain legitimacy as the actual concentration becomes obvious.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is the distance between the princeps's rope classification and the plebeian assembly's snare classification. Augustus experiences the absorption as solving a coordination problem: how to stabilize monarchy without explicit tyranny? The answer is elegant — reuse the language and aura of an existing Republican institution, making the transition appear organic rather than revolutionary. This is genuine coordination from the regime's stability perspective. The plebeian assembly experiences the same event as the destruction of their only counter-power: the mechanism designed to protect them has been weaponized against them. From their perspective, the constraint is pure extraction with maximum suppression. The sibling readings (popular_shield_reading, demagogic_lever_reading) occupy different structural positions in this same gap: the popular shield reading would emphasize the tribunate's residual function (that it retained some protective capacity); the demagogic reading would emphasize that sacrosanctity, once absorbed, could be wielded by any ambitious emperor as a tool of unauthorized power-seizing.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality d value (structural relationship to the constraint) differs sharply across agents. Augustus, as the beneficiary with full arbitrage mobility, derives d ≈ 0.15 → f(d) ≈ -0.01 (he experiences near-zero or negative effective extraction — the constraint subsidizes him). The plebeian assembly, as the victim trapped without exit, derives d ≈ 0.95 → f(d) ≈ 1.42 (maximum experienced extraction). The Senate, as an organized but constrained secondary actor, derives d ≈ 0.60 → f(d) ≈ 0.80 (moderate-high extraction despite some coordination benefits). The tribunate as an institution derives d ≈ 0.85 (high-target status, but with some residual formal authority) → f(d) ≈ 1.20. The perspectival gap between beneficiary and victim is maximal: the same institutional mechanism (sacrosanctity absorbed into the imperial person) is experienced as pure coordination by one agent and pure extraction by another.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits genuine mandatrophy at the intersection of the imperial_absorption reading and the alternative readings. If the tribunate retained protective function (popular_shield reading), the classification would be tangled_rope or rope rather than snare. If sacrosanctity became a demagogue's lever (demagogic_lever reading), the classification shifts to snare but with a different power structure (the demagogue is a rogue agent, not the regime itself). This reading resolves mandatrophy by declaring Augustus as the beneficiary and the tribunate-as-counter-power as the victim: the constraint is a snare because the absorbed authority is now the emperor's tool, not the plebs' shield. The beneficiary/victim structure is explicit and structural, not interpretive. The sibling readings would have different beneficiary/victim pairs, producing different classification outcomes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    absorption_vs_elimination_intent,
    'Was Augustus''s absorption of tribunician sacrosanctity a calculated political strategy to preserve Republican legitimacy while consolidating power, or an organic institutional drift where sacrosanctity naturally migrated to the ruler?',
    'Textual analysis of Augustus''s own declarations (Res Gestae); comparison with regimes that explicitly abolished the tribunate vs those that preserved forms; modeling of plausible alternatives (what would explicit monarchy have cost him)',
    'If calculated strategy: the constraint is engineered extraction (snare) designed to suppress counter-power. If organic drift: the constraint might be legitimate institutional evolution (rope or scaffold). Classification hinges on intentionality and whether alternatives were available to Augustus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absorption_vs_elimination_intent, conceptual, 'Whether absorption was strategic consolidation or organic institutional drift').

omega_variable(
    sacrosanctity_transfer_mechanism,
    'Did sacrosanctity as a legal/theological property actually transfer from the tribunate to the emperor, or did Augustus merely appropriate the language while the institution''s real power structure remained subordinate?',
    'Jurisprudential analysis of post-Augustan legal sources; cases where imperial inviolability was tested vs cases where tribunician veto was invoked; reconstruction of what each institution could actually enforce',
    'If true transfer: the tribunate genuinely lost its protective function (snare confirmed). If merely linguistic: the tribunate might retain structural independence despite appropriated titles (tangled_rope or rope from tribunician perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sacrosanctity_transfer_mechanism, empirical, 'Whether sacrosanctity was actually transferred to the emperor or merely appropriated linguistically').

omega_variable(
    plebeian_awareness_of_loss,
    'Did the plebeian assembly perceive the absorption of tribunician sacrosanctity as a loss of counter-power, or was the transition masked by continued ceremonial functions and nominal veto authority?',
    'Analysis of popular unrest, inscription evidence, and literary sources documenting plebeian perception of the tribunate post-Augustus; correlation between nominal veto authority and actual power to constrain the emperor',
    'If consciously perceived as loss: suppression is structural (snare). If masked by ceremony: suppression is internalized (identity_locked might apply to some tribune actors who see themselves as inheritors of Republican office despite functional subordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plebeian_awareness_of_loss, empirical, 'Whether the plebeian assembly perceived the tribunate''s loss of counter-power').

omega_variable(
    competing_reading_empirical_basis,
    'Which sibling reading — demagogic_lever, popular_shield, imperial_absorption — best fits the historical evidence of how sacrosanctity actually functioned in practice across the full imperial period?',
    'Longitudinal jurisprudential and institutional analysis across multiple reigns; identification of moments where each reading''s core claim was empirically vindicated or refuted; analysis of what sacrosanctity actually protected or enabled in each era',
    'If demagogic_lever reading is empirically superior: sacrosanctity was indeed a tool of ambition (Caligula, Commodus). If popular_shield reading is superior: tribunician power persisted as protection mechanism despite absorption (suggests rope or tangled_rope classification). If imperial_absorption reading is superior: sacrosanctity ended as monarchic title, confirming snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_reading_empirical_basis, empirical, 'Which sibling reading best fits the historical evidence of sacrosanctity''s function').

omega_variable(
    reading_kernel_relationship,
    'This constraint is one reading of the kernel ''tribunician sacrosanctity.'' Are the sibling readings (demagogic_lever, popular_shield) genuinely incommensurable, or can they be integrated into a single evolving constraint with different phases?',
    'Meta-theoretical analysis: if the kernel is a single persisting commitment that different parties read differently at the same time, the readings are sibling constraints (different files, separate stories). If the kernel evolved through historical phases where sacrosanctity functioned differently in different eras, decompose into a constraint family with temporal links instead.',
    'If incommensurable readings: this is the right authoring choice (three separate constraints, one kernel, sibling relations). If phases: should restructure as temporal family (constraint_tribunician_sacrosanctity__phase_1_republican, etc.).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_relationship, conceptual, 'Whether sibling readings are simultaneous incommensurable readings or historical phases').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tribunician_sacrosanctity__imperial_absorption_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trib_sac_imp_tr_t0, tribunician_sacrosanctity__imperial_absorption_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(trib_sac_imp_tr_t25, tribunician_sacrosanctity__imperial_absorption_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(trib_sac_imp_tr_t50, tribunician_sacrosanctity__imperial_absorption_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(trib_sac_imp_be_t0, tribunician_sacrosanctity__imperial_absorption_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(trib_sac_imp_be_t25, tribunician_sacrosanctity__imperial_absorption_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement(trib_sac_imp_be_t50, tribunician_sacrosanctity__imperial_absorption_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(trib_sac_imp_su_t0, tribunician_sacrosanctity__imperial_absorption_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(trib_sac_imp_su_t25, tribunician_sacrosanctity__imperial_absorption_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(trib_sac_imp_su_t50, tribunician_sacrosanctity__imperial_absorption_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tribunician_sacrosanctity__imperial_absorption_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(tribunician_sacrosanctity__imperial_absorption_reading, tribunician_sacrosanctity__demagogic_lever_reading).
narrative_ontology:affects_constraint(tribunician_sacrosanctity__imperial_absorption_reading, tribunician_sacrosanctity__popular_shield_reading).
narrative_ontology:affects_constraint(tribunician_sacrosanctity__imperial_absorption_reading, imperial_inviolability_mechanism).
narrative_ontology:affects_constraint(tribunician_sacrosanctity__imperial_absorption_reading, republican_institutional_erosion).

% DUAL FORMULATION NOTE:
% The tribunician_sacrosanctity kernel has three structurally distinct readings (imperial_absorption, demagogic_lever, popular_shield) produced by different parties interpreting the same persisting commitment. Each reading constitutes a separate constraint with different ε values, beneficiary/victim pairs, and classification outcomes. This file (imperial_absorption_reading) models the reading where sacrosanctity was successfully absorbed into the imperial person. The network links show how this reading's classification outcome (snare from powerless perspective) depends on the empirical resolution of what sacrosanctity actually enabled or protected in practice — a resolution that the sibling readings dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
