% ============================================================================
% CONSTRAINT STORY: irreversible_policy_commitment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irreversible_policy_commitment, []).

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
 *   constraint_id: irreversible_policy_commitment
 *   human_readable: The Burned Bridge Protocol: Irreversible Policy Commitment
 *   domain: political/economic
 *
 * SUMMARY:
 *   The Burned Bridge Protocol describes a class of policy commitments where
 *   reversal costs are so high that the decision becomes functionally
 *   irreversible, regardless of whether subsequent generations or affected
 *   populations would prefer different terms. The paradigm cases include:
 *   nuclear energy lock-in (decommissioning costs exceed budgets; technical
 *   expertise atrophies), infrastructure corridor commitments (highway
 *   networks, railroad routes create 50-year path dependencies), land use
 *   zoning (converting developed land back to natural state is astronomically
 *   expensive), and regulatory tier-locking (once a regime has operated under
 *   a rule for 10+ years, reversing it destabilizes settled expectations
 *   worth billions). The constraint exhibits tangled_rope structure at the
 *   analytical level: it solves a genuine coordination problem (commitment
 *   credibility: if you can always reverse, actors won't adjust behavior as
 *   desired) while extracting from those who must live with the consequences.
 *   The theater_ratio (0.62) reflects that a significant portion of the
 *   constraint's force is rhetorical — institutional narratives that 'this is
 *   settled' and 'we cannot undo it' — rather than purely structural. The
 *   extractiveness (0.58) captures the real asymmetry: the incumbent
 *   coalition captures optionality (they can reinterpret or slowly dismantle
 *   policy), while displaced populations and future generations inherit
 *   irreversibility without consent.
 *
 * KEY AGENTS:
 *   - Incumbent Policy Coalition: Primary beneficiary (institutional/arbitrage) — locks in preferred policy as institutional default; can reinterpret or exit if conditions change
 *   - Displaced Population: Primary victim (powerless/trapped) — bears full cost of irreversible commitment; cannot exit territory or policy framework; generational damage compounds
 *   - Opposition Party: Secondary actor (institutional/constrained) — constrained by political toxicity of reversal; also benefits from stable policy framework for alternative initiatives
 *   - Affected Regional Economy: Secondary victim (moderate/constrained) — experiences mixed coordination (predictability for long-term investment) and extraction (regional assets locked into path-dependent trajectory)
 *   - Institutional Memory: Performative actor (organized/constrained) — maintains narrative that commitment is irreversible; actual reversibility is weaker than rhetoric suggests
 *   - Future Generations: Powerless actors (powerless/trapped in time) — inherit landscape designed without their input; cannot retroactively consent to or exit commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irreversible_policy_commitment, 0.58).
domain_priors:suppression_score(irreversible_policy_commitment, 0.68).
domain_priors:theater_ratio(irreversible_policy_commitment, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irreversible_policy_commitment, extractiveness, 0.58).
narrative_ontology:constraint_metric(irreversible_policy_commitment, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(irreversible_policy_commitment, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irreversible_policy_commitment, tangled_rope).
narrative_ontology:human_readable(irreversible_policy_commitment, "The Burned Bridge Protocol: Irreversible Policy Commitment").
narrative_ontology:topic_domain(irreversible_policy_commitment, "political/economic").

domain_priors:requires_active_enforcement(irreversible_policy_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irreversible_policy_commitment, incumbent_policy_coalition).
narrative_ontology:constraint_beneficiary(irreversible_policy_commitment, path_dependent_interests).
narrative_ontology:constraint_victim(irreversible_policy_commitment, displaced_populations).
narrative_ontology:constraint_victim(irreversible_policy_commitment, future_policy_optionality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED POPULATION (SNARE) — Bears full cost of irreversible commitment. Cannot exit the territory or reverse policy course. Generational horizon compounds the trap: children inherit a landscape designed against their interests with no recourse. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(irreversible_policy_commitment, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: AFFECTED REGIONAL ECONOMY (TANGLED ROPE) — Experiences both coordination function (infrastructure lock-in creates predictability for long-term investment) and extraction (regional assets become collateral for path-dependent commitments). Constrained exit through migration costs and industrial restructuring. d≈0.68, f(d)≈1.02, σ=0.9 → χ≈0.57.
constraint_indexing:constraint_classification(irreversible_policy_commitment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT POLICY COALITION (ROPE) — Primary beneficiary. The irreversible commitment locks in their policy preferences as the institutional default. Experiences the constraint as pure coordination: 'We have burned the bridge, so now we must all march forward together.' Arbitrage exit means they can abandon the coalition without cost — they authored the commitment and can reinterpret it. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.05.
constraint_indexing:constraint_classification(irreversible_policy_commitment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPPOSITION PARTY (TANGLED ROPE) — Constrained by institutional lock-in: reversing the policy is politically toxic ('undoing settled law') even if policy merits favor reversal. However, also benefits from coordination: the irreversible commitment is now a stable policy framework they can build alternative initiatives within, rather than fighting ground-level implementation battles. requires_active_enforcement=true reflects that maintaining the irreversibility narrative requires continuous institutional effort. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.44.
constraint_indexing:constraint_classification(irreversible_policy_commitment, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL MEMORY (PITON) — The 'irreversibility' is partly theatrical: every policy can technically be reversed, but the institutional rhetoric that surrounds burned-bridge commitments makes reversal appear impossible. theater_ratio=0.62 reflects that ~62% of the constraint's force comes from narrative maintenance ('this is settled,' 'we cannot undo it') rather than structural irreversibility. The actual lock-in is weaker than the performative commitment suggests, but the performance has become institutionalized. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.40.
constraint_indexing:constraint_classification(irreversible_policy_commitment, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the burned bridge protocol is a hybrid coordination-extraction mechanism. It solves the coordination problem of commitment credibility (actors know the decision is irreversible, so they adjust expectations accordingly), but it extracts from future generations who inherit the landscape without consent and cannot escape path dependence. The constraint combines genuine coordination benefit (reduces future uncertainty) with genuine extraction (eliminates future optionality). d≈0.52, f(d)≈0.70, σ=1.0 → χ≈0.41.
constraint_indexing:constraint_classification(irreversible_policy_commitment, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irreversible_policy_commitment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(irreversible_policy_commitment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(irreversible_policy_commitment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(irreversible_policy_commitment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(irreversible_policy_commitment, TR),
    TR >= 0.70.

:- end_tests(irreversible_policy_commitment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The burned bridge protocol exhibits real extraction because it eliminates optionality from those without power to author the commitment. However, it is not maximal extraction (0.66+) because some coordination value is genuine — the commitment does solve credibility problems. The trajectory (0.42→0.58 over 20 years) reflects institutional hardening: early in the policy's life, reversal is theoretically possible; over time, sunk costs and institutional inertia make reversal prohibitively expensive, increasing extractiveness. Suppression (0.68): High. The constraint operates through multiple suppression mechanisms: (1) exit suppression (populations cannot leave affected regions without massive relocation costs), (2) information suppression (damage from irreversible choice is not transparent until years later), (3) alternative suppression (no parallel policy pathways exist once the bridge is burned), (4) rhetorical suppression ('irreversible' narrative blocks discussion of reversal). Theater ratio (0.62): Moderate-high. The institutional narrative that a policy is 'irreversible' and 'settled' constitutes significant theatrical component. However, the constraint is not pure theater (Piton would require ≥0.70): there are genuine structural costs to reversal. The 0.62 value reflects that institutional rhetoric amplifies the actual structural lock-in, but does not create it from whole cloth. The theater trajectory (0.35→0.62) shows that over time, the rhetorical maintenance of irreversibility increases as the actual structural reversibility declines.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates dramatic perspectival divergence. The incumbent coalition sees Rope: a coordination mechanism that locks in their policy and creates predictability. The displaced population sees Snare: extraction without exit or recourse. The opposition party sees Tangled Rope: constrained by the irreversibility narrative but also benefiting from stable policy ground. The regional economy sees Tangled Rope: lock-in provides investment predictability but eliminates optionality. The institutional memory sees Piton: the 'irreversibility' is substantially performative, maintained through institutional inertia. The analytical observer sees Tangled Rope: real coordination benefit (commitment credibility) mixed with real extraction (future optionality eliminated). The perspectival gap reflects that 'irreversibility' is not a structural property but a distributed perception across stakeholders with different exit options. Those with arbitrage exit (the coalition) experience Rope; those with no exit experience Snare; those with constrained exit experience Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent policy coalition: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Strong net beneficiary position. They authored the commitment and retain interpretive authority; they experience the constraint as enabling (coordination solved). Displaced population: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction position. No exit options; generational damage; cannot retroactively consent. Affected regional economy: Victim + constrained → d≈0.68, f(d)≈1.02. Moderate-high extraction. Some exit via migration or industrial transition, but costs are severe and take decades. Opposition party: Victim (to some extent) + constrained → d≈0.55, f(d)≈0.75. Mixed position: constrained by political toxicity of reversal (victim status) but also constrained by actual structural lock-in they might want to work within (quasi-beneficiary status). The net d reflects that they are neither pure beneficiary nor pure victim. Institutional memory: Neither pure beneficiary nor victim; maintains theater. d derives from organized/constrained hybrid: d≈0.50, f(d)≈0.65. Analytical observer: Observer position. d≈0.52, f(d)≈0.70, reflecting civilizational scope where the constraint affects all future generations symmetrically.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The burned bridge protocol is a legitimate Tangled Rope, not a false Snare. Mandatrophy resolution requires showing that the constraint has genuine coordination function (not pure extraction) AND genuine extraction (not pure coordination). The coordination function: commitment credibility. By burning the bridge, the policy coalition makes a binding signal about future policy. This reduces uncertainty for actors trying to make long-term plans (businesses investing in infrastructure, populations adjusting expectations). The extraction function: elimination of future optionality. Generations born under the burned bridge cannot undo the commitment without catastrophic cost. Their preferred policy landscape is constrained by decisions they did not author. The coordination-extraction balance is captured in the tangled_rope structure: χ = 0.58 × f(d) × σ(S) shows that effective extractiveness varies by observer position (f(d) ranges from -0.08 for beneficiaries to 1.38 for trapped populations), while base extractiveness (0.58) is invariant. The theater_ratio (0.62) prevents false Rope classification (which would require χ ≤ 0.35): the institutional narrative amplifies the commitment's perceived irreversibility beyond structural reality, enabling the coalition to extract ongoing narrative legitimacy. The mandatrophy is resolved by the following logic: If this were pure Snare (extraction disguised as necessity), we would expect theater_ratio to be very high (≥0.85) and beneficiaries to be absent. But beneficiaries are explicitly present (incumbent coalition benefits from commitment credibility), and theater is moderate (0.62), not overwhelming. If this were pure Rope (coordination with benign distributional effects), we would expect suppression to be low (≤0.30) and victims to be absent. But victims are explicitly present (displaced populations, future optionality), and suppression is high (0.68). The tangled_rope classification correctly captures the hybrid: real coordination value + real extraction cost, with perspectival variance reflecting different exit options.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technical_reversibility_threshold,
    'At what cost does a policy cease to be ''irreversible'' and become merely ''expensive to reverse''?',
    'Historical case studies: infrastructure removal costs, decommissioning timelines, asset stranding valuations. Empirical threshold analysis across policy domains (energy, transportation, land use).',
    'If threshold is low (cost < 5% GDP): constraint is Rope or Scaffold (reversible with political will). If threshold is high (cost > 25% GDP): constraint is Snare (structurally trapped). Classification pivots on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_reversibility_threshold, empirical, 'Technical cost threshold that defines irreversibility boundary').

omega_variable(
    generational_consent_legitimacy,
    'Does a policy commitment binding future generations without their consent constitute a structural extraction mechanism or merely intergenerational governance?',
    'Philosophical/normative: compare legitimacy frameworks (contractarian, Rawlsian veil, historical consent models). Empirical: measure intergenerational conflict rates and policy reversal attempts as proxy for consent withdrawal.',
    'If extraction framing legitimate: constraint is coordination-heavy Tangled Rope. If extraction framing illegitimate: constraint is high-suppression Snare. Affects mandatrophy resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generational_consent_legitimacy, conceptual, 'Whether binding future generations constitutes extraction').

omega_variable(
    institutional_narrative_sufficiency,
    'Is the constraint''s force primarily structural (sunk costs, infrastructure lock-in) or rhetorical (institutional narrative that ''this is irreversible'')?',
    'Empirical decomposition: measure the cost of actual policy reversal vs. the cost of rhetorical reversal (dissolving the narrative that change is impossible). Compare actual vs. attempted reversals; measure opposition intensity.',
    'If primarily structural: constraint is true Snare/Tangled Rope. If primarily rhetorical: constraint is Piton (theatrical). Directs intervention strategies: structural constraints require institutional redesign; rhetorical constraints require narrative disruption.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_narrative_sufficiency, empirical, 'Proportion of constraint force from rhetoric vs. structural lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irreversible_policy_commitment, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irrev_tr_t0, irreversible_policy_commitment, theater_ratio, 0, 0.35).
narrative_ontology:measurement(irrev_tr_t10, irreversible_policy_commitment, theater_ratio, 10, 0.52).
narrative_ontology:measurement(irrev_tr_t20, irreversible_policy_commitment, theater_ratio, 20, 0.62).

% Extraction over time
narrative_ontology:measurement(irrev_be_t0, irreversible_policy_commitment, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(irrev_be_t10, irreversible_policy_commitment, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(irrev_be_t20, irreversible_policy_commitment, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irreversible_policy_commitment, enforcement_mechanism).
narrative_ontology:affects_constraint(irreversible_policy_commitment, path_dependency_lock_in).
narrative_ontology:affects_constraint(irreversible_policy_commitment, sunk_cost_fallacy_institutional).
narrative_ontology:affects_constraint(irreversible_policy_commitment, intergenerational_social_contract).

% DUAL FORMULATION NOTE:
% The burned bridge protocol decomposes into three related constraints: (1) path_dependency_lock_in (structural irreversibility via sunk costs, ε≈0.35, Mountain-Rope boundary), (2) intergenerational_social_contract (ethical irreversibility via consent, ε≈0.42, Snare-Tangled Rope boundary), (3) irreversible_policy_commitment (institutional irreversibility via rhetorical lock-in, ε≈0.58, Tangled Rope). This story models the institutional-rhetorical constraint at ε=0.58. The upstream structural and ethical constraints have lower extractiveness but provide causal foundation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(irreversible_policy_commitment, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
