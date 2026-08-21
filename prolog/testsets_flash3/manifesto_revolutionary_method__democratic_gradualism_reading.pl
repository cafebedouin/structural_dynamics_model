% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__democratic_gradualism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manifesto_revolutionary_method__democratic_gradualism_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: manifesto_revolutionary_method__democratic_gradualism_reading
 *   human_readable: Democratic Gradualism for Socialist Transformation
 *   domain: political_philosophy/revolutionary_theory/historical_materialism
 *
 * SUMMARY:
 *   This constraint represents the 'democratic gradualism' reading of the
 *   manifesto_revolutionary_method kernel, asserting that socialism is
 *   achievable through democratic electoral majorities and gradual
 *   institutional reform within existing liberal democratic structures.
 *   Working-class power is exercised primarily through these established
 *   channels. This reading emphasizes institutional continuity and
 *   incremental change, contrasting with more radical revolutionary
 *   approaches. The metrics reflect a moderate level of extraction (0.40) due
 *   to the inherent limitations and co-optation risks of working within a
 *   capitalist-democratic framework, and relatively low suppression (0.30)
 *   compared to revolutionary states, as it relies on persuasion and
 *   electoral success rather than overt coercion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4).
domain_priors:suppression_score(manifesto_revolutionary_method__democratic_gradualism_reading, 0.3).
domain_priors:theater_ratio(manifesto_revolutionary_method__democratic_gradualism_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__democratic_gradualism_reading, rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__democratic_gradualism_reading, "Democratic Gradualism for Socialist Transformation").
narrative_ontology:topic_domain(manifesto_revolutionary_method__democratic_gradualism_reading, "political_philosophy/revolutionary_theory/historical_materialism").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__democratic_gradualism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__democratic_gradualism_reading, 'e5dc3267-e5e9-4b99-9f1d-834014e72698').
narrative_ontology:cs_kernel_codification('e5dc3267-e5e9-4b99-9f1d-834014e72698', formalized).
narrative_ontology:cs_authority_grounding('e5dc3267-e5e9-4b99-9f1d-834014e72698', lineage).
narrative_ontology:cs_interpretation_layer_present('e5dc3267-e5e9-4b99-9f1d-834014e72698').
narrative_ontology:cs_reading_relation('e5dc3267-e5e9-4b99-9f1d-834014e72698', manifesto_revolutionary_method__vanguard_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('e5dc3267-e5e9-4b99-9f1d-834014e72698', manifesto_revolutionary_method__council_communist_reading, coexists_with).
narrative_ontology:cs_axiom('e5dc3267-e5e9-4b99-9f1d-834014e72698', foundational, parliamentary_path_to_socialism_is_primary).
narrative_ontology:cs_axiom_status(parliamentary_path_to_socialism_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('e5dc3267-e5e9-4b99-9f1d-834014e72698', parliamentary_path_to_socialism_is_primary, conventional).
narrative_ontology:cs_axiom('e5dc3267-e5e9-4b99-9f1d-834014e72698', foundational, liberal_democratic_institutions_are_reformable).
narrative_ontology:cs_axiom_status(liberal_democratic_institutions_are_reformable, holdable).
narrative_ontology:cs_axiom_grounding('e5dc3267-e5e9-4b99-9f1d-834014e72698', liberal_democratic_institutions_are_reformable, empirically_contingent).
narrative_ontology:cs_reference_frame('e5dc3267-e5e9-4b99-9f1d-834014e72698', kautskyist_parliamentary_socialism).
narrative_ontology:cs_drift_state('e5dc3267-e5e9-4b99-9f1d-834014e72698', post_cold_war_neoliberal_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e5dc3267-e5e9-4b99-9f1d-834014e72698', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, trade_unions).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, electoral_working_class).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, radical_left_factions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, liberal_democratic_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for and implements socialist policies through electoral victories and parliamentary processes. Benefits from the legitimacy of existing democratic institutions and the gradualist approach, which allows for sustained political careers and party building within the established system.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties, agenda_setter,
    institutional, generational, constrained, national).

% Works within existing labor laws and collective bargaining frameworks to improve working conditions and wages. Benefits from the stability of democratic institutions and the ability to influence policy through lobbying and electoral support for social democratic parties. Their power is tied to the existing legal and political structures.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, trade_unions, beneficiary,
    organized, biographical, constrained, national).

% Participates in democratic elections to vote for parties promising socialist reforms. Benefits from incremental improvements in social welfare and labor rights, but remains subject to the limitations of capitalist economic structures and the slow pace of reform. Their power is aggregated through the ballot box.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, electoral_working_class, beneficiary,
    moderate, biographical, constrained, national).

% Rejects the democratic gradualist path as insufficient or co-opted by capital. Often marginalized, suppressed, or dismissed as 'adventurist' by both the state and mainstream left parties. Bears the cost of political isolation and lack of institutional support for their more radical approaches.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants, payer,
    powerless, immediate, trapped, local).

% Operates outside or on the fringes of mainstream social democratic parties, advocating for more rapid or fundamental change. While not always directly suppressed, they face an uphill battle for legitimacy and resources within a political system that prioritizes gradualism and electoralism.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, radical_left_factions, payer,
    moderate, biographical, constrained, national).

% The existing state apparatus, which is preserved and incrementally reformed rather than overthrown. Benefits from the stability and legitimacy conferred by the democratic gradualist approach, avoiding revolutionary rupture and maintaining institutional continuity.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, liberal_democratic_state, beneficiary,
    institutional, generational, arbitrage, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the efforts of social democratic parties, trade unions, and the working class to achieve socialist goals through established democratic and parliamentary channels, avoiding violent revolution or extra-parliamentary seizure of power.
% TRANSFER_FUNCTION: Transfers political legitimacy and resources from revolutionary movements to established democratic institutions and parties, in exchange for the promise of gradual social and economic reforms.
% ABSENT_VOICES: Anarchist and council communist factions, who would argue that the existing democratic structures are inherently capitalist and cannot be reformed to achieve genuine socialism, are excluded from the mainstream discourse and institutional power structures.
% DISAPPEARANCE_RATIONALE: If the belief in democratic gradualism vanished, social democratic parties would lose their core mandate, trade unions would question their strategy, and the working class would seek alternative, potentially revolutionary, paths to power. The entire political landscape of the left would be fundamentally reshaped.
% FOUNDING_PROBLEM: The historical problem of achieving social justice and economic equality without resorting to violent revolution or authoritarian state control, seeking a path compatible with individual liberties and democratic governance.
% FOUNDING_PROBLEM_CORROBORATION: Social democratic parties and many trade unions attest that the problem is still live, citing ongoing economic inequality and the need for continuous reform. Critics from revolutionary factions argue that the problem has not been adequately addressed by gradualism, but the mainstream view holds it as a persistent challenge requiring ongoing democratic engagement.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__democratic_gradualism_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__democratic_gradualism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(manifesto_revolutionary_method__democratic_gradualism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manifesto_revolutionary_method__democratic_gradualism_reading_tests).
:- end_tests(manifesto_revolutionary_method__democratic_gradualism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate because while the democratic path offers real gains, it also entails compromises and a slower pace of change, effectively extracting the potential for more rapid or fundamental transformation. Suppression is low as this reading largely operates within existing legal frameworks, but it does suppress more radical alternatives through political marginalization and delegitimization. Theater ratio is low, as the commitment to democratic processes is generally genuine, though some performativity exists in electoral promises that are difficult to deliver. The temporal measurements show a slight increase in extractiveness and theater over the 20th century, reflecting the challenges of achieving deep structural change through gradual means, followed by a slight decrease in suppression as radical alternatives became less prominent post-Cold War.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of social democratic parties, this is a genuine 'rope' that coordinates broad social forces for progressive change. From the perspective of revolutionary militants, it functions more like a 'snare' or 'tangled rope,' co-opting working-class energy into a system that ultimately preserves capitalist power, extracting revolutionary potential. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Social democratic parties, trade unions, and the electoral working class are beneficiaries, as this path offers them a legitimate and stable means to pursue their goals, albeit with inherent limitations. The liberal democratic state itself also benefits from the stability and legitimacy this approach provides. Revolutionary militants and radical left factions are victims, as their preferred methods are delegitimized and suppressed within this framework, forcing them into marginal positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (achieving socialism democratically) is still live, but its effectiveness is contested. The classification as 'rope' (claimed) with moderate extractiveness and suppression acknowledges the genuine coordination function while also capturing the costs borne by those who seek more radical change. This prevents mislabeling it as pure extraction (snare) by recognizing its real, albeit limited, coordination, or as a pure coordination (rope) by acknowledging its extractive aspects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pace_of_change_efficacy,
    'Is the pace of change achievable through democratic gradualism sufficient to address the systemic crises of capitalism (e.g., climate change, extreme inequality) before catastrophic breakdown?',
    'Empirical analysis of historical and contemporary social democratic reforms against the accelerating timelines of global crises, comparing outcomes to counterfactuals of more rapid transformation.',
    'If insufficient, the ''rope'' classification might shift towards ''tangled_rope'' or ''snare'' for those bearing the costs of delayed action, as the coordination function fails to deliver on its implicit promise of timely systemic change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pace_of_change_efficacy, empirical, 'Efficacy of gradualism in addressing systemic crises.').

omega_variable(
    co_optation_risk,
    'To what extent does participation in existing democratic structures co-opt socialist movements, leading to a dilution of revolutionary goals and integration into the capitalist system?',
    'Historical analysis of social democratic parties'' trajectories, examining instances where radical platforms were abandoned or moderated after gaining power, and comparing outcomes to non-electoral movements.',
    'If co-optation is high, the ''extractiveness'' metric would increase, and the constraint might reclassify towards ''tangled_rope'' or ''snare'' for the working class, as their revolutionary potential is extracted by the very system meant to empower them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(co_optation_risk, conceptual, 'Risk of co-optation for socialist movements within democratic structures.').

omega_variable(
    legitimacy_of_extra_parliamentary_action,
    'Is extra-parliamentary action (e.g., general strikes, direct action) a legitimate and effective means of exercising working-class power within a democratic gradualist framework, or is it inherently ''adventurist'' and counterproductive?',
    'Analysis of historical instances where extra-parliamentary action either complemented or undermined democratic socialist movements, assessing its impact on policy outcomes and public support.',
    'If deemed legitimate and effective, the ''suppression'' metric for revolutionary militants and radical left factions would decrease, and their ''exit_options'' might shift from ''trapped'' to ''constrained'' or ''mobile'', altering their directionality and reducing the constraint''s overall extractiveness from these groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_extra_parliamentary_action, preference, 'Role and legitimacy of extra-parliamentary action in democratic socialism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__democratic_gradualism_reading, 1900, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t1900, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(mani_tr_t1930, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1930, 0.08).
narrative_ontology:measurement(mani_tr_t1960, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(mani_tr_t1990, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(mani_tr_t2020, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(mani_be_t1900, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement(mani_be_t1930, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1930, 0.35).
narrative_ontology:measurement(mani_be_t1960, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1960, 0.4).
narrative_ontology:measurement(mani_be_t1990, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(mani_be_t2020, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 2020, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t1900, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1900, 0.4).
narrative_ontology:measurement(mani_su_t1930, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1930, 0.35).
narrative_ontology:measurement(mani_su_t1960, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1960, 0.3).
narrative_ontology:measurement(mani_su_t1990, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1990, 0.25).
narrative_ontology:measurement(mani_su_t2020, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 2020, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__democratic_gradualism_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the 'democratic_gradualism_reading' of the 'manifesto_revolutionary_method' kernel. It coexists with 'vanguard_rupture_reading' and 'council_communist_reading', which offer alternative methods for socialist transformation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
