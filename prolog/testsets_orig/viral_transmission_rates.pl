% ============================================================================
% CONSTRAINT STORY: viral_transmission_rates
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_viral_transmission_rates, []).

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
 *   constraint_id: viral_transmission_rates
 *   human_readable: Socio-Political Response to Viral Transmission
 *   domain: political/technological/biological
 *
 * SUMMARY:
 *   The socio-political response to viral transmission creates a structural
 *   tension between biological necessity (containing pathogen spread requires
 *   behavioral modification) and institutional power consolidation (emergency
 *   authority expands institutional capacity and control mechanisms). This
 *   constraint exhibits the classic tangled rope signature: a genuine
 *   coordination function (disease suppression through collective action)
 *   coupled with asymmetric extraction (concentrated power gains, dispersed
 *   costs). The theater_ratio (0.72) reflects the phenomenon of performative
 *   public health measures: daily case counts, mask mandates, quarantine
 *   protocols that may exceed epidemiological necessity, maintained partly
 *   through institutional momentum rather than active problem-solving.
 *   Suppression (0.72) is high because alternatives to mandated behavior are
 *   systematically eliminated: movement restrictions, employment mandates,
 *   institutional access gating. Yet the constraint is not a pure snare
 *   because the primary beneficiary (public health authority) genuinely
 *   solves a collective coordination problem — individuals do benefit from
 *   disease suppression, but the distribution of benefit is asymmetric. Over
 *   the 12-month interval, extractiveness increased from 0.32 to 0.58 as
 *   emergency measures persisted beyond their initial justification and
 *   theater mechanisms (increasingly perfunctory restrictions) replaced
 *   active epidemic control.
 *
 * KEY AGENTS:
 *   - Public Health Authority: Primary institutional beneficiary (institutional/arbitrage) — consolidates disease surveillance authority, expands institutional capacity, captures policy control
 *   - National Government: Secondary beneficiary (organized/mobile) — captures emergency powers but faces exit cost (political legitimacy erosion if policy reversal precedes threat decline)
 *   - Locked-Down Worker: Primary victim (powerless/trapped) — loses income and autonomy simultaneously; no exit option; bears maximum experienced extraction
 *   - Small Business Operator: Primary victim (powerless/trapped) — forced closure; no compensation mechanisms matching losses; capital locked in territorial jurisdiction
 *   - Healthcare Worker: Secondary actor (moderate/constrained) — benefits from coordinated disease suppression but bears extraction through mandate compliance and occupational hazard exposure
 *   - International Health Bureaucracy: Tertiary actor (institutional/arbitrage) — maintains performative global coordination; benefits from emergency status through funding and authority
 *   - Civil Liberties Coalition: Organized opposition (organized/mobile) — sees constraint as temporary with sunset potential; has mobilization capacity but constrained by immediate institutional power imbalance
 *   - Analytical Observer: Detached perspective (analytical/analytical) — risks naturalizing contingent policy design as biological necessity; reveals false summit when comparing extractiveness to mountain thresholds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(viral_transmission_rates, 0.58).
domain_priors:suppression_score(viral_transmission_rates, 0.72).
domain_priors:theater_ratio(viral_transmission_rates, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(viral_transmission_rates, extractiveness, 0.58).
narrative_ontology:constraint_metric(viral_transmission_rates, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(viral_transmission_rates, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(viral_transmission_rates, tangled_rope).
narrative_ontology:human_readable(viral_transmission_rates, "Socio-Political Response to Viral Transmission").
narrative_ontology:topic_domain(viral_transmission_rates, "political/technological/biological").

domain_priors:requires_active_enforcement(viral_transmission_rates).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(viral_transmission_rates, public_health_authorities).
narrative_ontology:constraint_beneficiary(viral_transmission_rates, institutional_power_consolidators).
narrative_ontology:constraint_victim(viral_transmission_rates, economically_dependent_workers).
narrative_ontology:constraint_victim(viral_transmission_rates, small_business_operators).
narrative_ontology:constraint_victim(viral_transmission_rates, civil_liberties).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCKED-DOWN WORKER (SNARE) — Faces binary choice: comply with mobility restrictions and employment mandates, or lose income and social participation. No genuine exit option; bears full cost of transmission controls. Maximum experienced extraction through loss of autonomy and economic coercion.
constraint_indexing:constraint_classification(viral_transmission_rates, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL BUSINESS OPERATOR (SNARE) — Forced closure orders eliminate revenue streams without compensation mechanisms matching losses. Capital locked into physical premises; no arbitrage option. Trapped within territorial jurisdiction; suppression is absolute (regulatory closure, not market competition).
constraint_indexing:constraint_classification(viral_transmission_rates, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: HEALTHCARE WORKER (TANGLED ROPE) — Benefits from coordinated disease suppression (reduced patient volume crises, public health infrastructure investment). Also bears extraction through mandate compliance, occupational hazard exposure, and suppressed voice in policy design. Constrained exit: occupational commitment prevents departure, but some negotiating power through essential worker status.
constraint_indexing:constraint_classification(viral_transmission_rates, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PUBLIC HEALTH AUTHORITY (ROPE) — Primary beneficiary of coordination function: centralizes disease surveillance, concentrates response authority, and expands institutional capacity. Experiences the constraint as legitimate coordination tool. Can exit if political will shifts; arbitrage position allows negotiation of enforcement scope. Net beneficiary.
constraint_indexing:constraint_classification(viral_transmission_rates, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: NATIONAL GOVERNMENT (TANGLED ROPE) — Captures emergency powers and consolidated control authority (extraction beneficiary). Simultaneously faces organizational costs of enforcement infrastructure, political resistance, and legitimacy erosion over duration. Has exit options (policy reversal) but faces coordination trap: early exit risks reputational cost if resurgence occurs. Mobile but path-dependent.
constraint_indexing:constraint_classification(viral_transmission_rates, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL HEALTH BUREAUCRACY (PITON) — Maintains performative coordination (WHO guidance, emergency declarations) that often lags epidemiological reality. Theater ratio (0.68) reflects that much declared 'public health emergency' structure persists through institutional inertia rather than active problem-solving. Organization expands during crisis; contracts slowly afterward due to bureaucratic path-dependency.
constraint_indexing:constraint_classification(viral_transmission_rates, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: CIVIL LIBERTIES COALITION (SCAFFOLD) — Sees temporary emergency with sunset potential. High suppression (0.72) during active enforcement phase, but coalition has agency and mobilization capacity. Classification as scaffold reflects belief that emergency measures decay as threat perception declines and alternative coordination mechanisms mature. Constrained by immediate institutional power; mobile long-term.
constraint_indexing:constraint_classification(viral_transmission_rates, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (BIOLOGICAL NECESSITY VIEW) — From civilizational scope, viral transmission is an irreducible feature of pathogen biology and human social organization. This perspective risks naturalizing as immutable law what is actually contingent policy design. The constraint exhibits false summit signature: extractiveness (0.58) and suppression (0.72) are incompatible with mountain classification (ε ≤ 0.25, suppression ≤ 0.05). The mountain classification reveals naturalization fallacy.
constraint_indexing:constraint_classification(viral_transmission_rates, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(viral_transmission_rates_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(viral_transmission_rates, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(viral_transmission_rates, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(viral_transmission_rates, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(viral_transmission_rates, TR),
    TR >= 0.70.

:- end_tests(viral_transmission_rates_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The primary extraction flow runs from locked-down workers and small business operators toward institutional authorities (public health, national government). Workers lose income and autonomy; businesses lose revenue and asset value. These are direct, measurable transfers. However, extractiveness is not maximal (would be >0.70) because some portion of suppression genuinely produces collective benefit — disease rates do decline under strict transmission control, and healthcare workers and vulnerable populations do benefit from reduced infection risk. The mid-range value reflects genuine mixed coordination and extraction. Suppression (0.72): High. Alternatives to mandated behavior are systematically blocked: you cannot work if your job is deemed non-essential; you cannot move if lockdown is in effect; you cannot access public institutions without compliance certification. These are coercive mechanisms, not market alternatives. Theater ratio (0.68): High and rising. At t=0 (early emergency), theater is lower (0.35) because measures respond to actual transmission spikes. By t=12, theater rises (0.72) because measures persist despite declining transmission risk — they are maintained through institutional inertia, regulatory caution, and political coordination failure. Daily case counts, quarantine protocols, and mask mandates become performative: they continue not because epidemiological evidence supports them, but because institutional consensus treats them as default response.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates dramatic perspectival disagreement. The public health authority sees rope (pure coordination tool for disease control). The locked-down worker sees snare (binary coercion with no exit). The civil liberties coalition sees scaffold (temporary emergency with sunset). The national government sees tangled_rope (genuine coordination function plus genuine power consolidation). The healthcare worker sees tangled_rope (benefits from disease suppression, bears extraction from mandate compliance). The international bureaucracy sees piton (performative global coordination maintained through inertia). The analytical observer risks seeing mountain (transmission is biological law, so suppression is natural necessity) — but the structural data contradicts this: extractiveness (0.58) exceeds mountain threshold (≤0.25), and suppression (0.72) exceeds mountain threshold (≤0.05). The perspectival gap reveals that the disagreement is not about facts (everyone agrees transmission occurs) but about whether the institutional response is justified coordination or extractive power-grab.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies dramatically across perspectives because the same constraint produces opposite extraction flows depending on institutional position. Public health authorities experience d ≈ 0.10 (institutional/arbitrage) — they are net beneficiaries; the constraint subsidizes their institutional power. Locked-down workers experience d ≈ 0.95 (powerless/trapped) — they are maximum targets; the constraint extracts from them with no exit option. National government experiences d ≈ 0.55 (organized/mobile) — they benefit from emergency authority but face exit cost; the directionality reflects this path-dependency. Healthcare workers experience d ≈ 0.65 (moderate/constrained) — they benefit from disease suppression but bear extraction through mandate compliance. The sigmoid f(d) translates these structural positions into experienced extractiveness: beneficiaries with arbitrage get f(d) ≈ -0.12 (their effective extraction is subsidized/negative); targets with trapped exit get f(d) ≈ 1.42 (maximum experienced extraction). The scope modifier σ(S) = 1.0 (national) does not amplify extraction in this case because the enforcement mechanisms are territorially bounded — they rely on state apparatus, not on global complexity.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: This constraint has extractiveness (0.58) exceeding the 0.46 threshold, which requires mandatrophy resolution (mandatrophy_resolved: false indicates analysis is incomplete). The core mandatrophy is whether the constraint should be classified as rope (legitimate coordination) or snare (pure extraction) — and the answer varies by perspective in a way that cannot be collapsed into a single type. Public health authority perspective legitimately sees rope: they are coordinating a genuine collective action problem (disease suppression). Locked-down worker perspective legitimately sees snare: they face coercive extraction with no exit. Both perspectives are structurally valid descriptions of the same constraint. The mandatrophy does not resolve because the perspectives are not epistemically equivalent — they describe different causal mechanisms operating simultaneously. The constraint is genuinely a tangled rope: it contains both coordination function (disease suppression that produces collective benefit) AND asymmetric extraction (power consolidation and economic losses that flow in one direction). Resolving the mandatrophy would require either (a) decomposing the constraint into separate stories (coordination component vs extraction component) or (b) accepting tangled_rope as the valid classification that captures the simultaneous operation of both mechanisms. The v1.0 status reflects that mandatrophy is NOT resolved — the analysis documents the ambiguity rather than resolving it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transmission_risk_quantification,
    'What objective transmission risk threshold justifies suppression cost? Is it 10% infection rate, 1%, or lower?',
    'Epidemiological data on actual transmission patterns; comparative analysis of policy thresholds across jurisdictions; post-hoc correlation between suppression intensity and transmission outcomes',
    'If threshold is high (>5%): many jurisdictions over-suppressed relative to epidemiological justification (higher extraction perception). If threshold is low (<1%): suppression is justified as coordination, reducing snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_risk_quantification, empirical, 'Objective threshold for transmission risk justifying suppression cost').

omega_variable(
    economic_substitution_possibility,
    'Could rapid wage replacement and business compensation mechanisms have converted snare into rope, reducing extraction perception?',
    'Historical comparison: jurisdictions with robust compensation vs those without; economic impact analysis showing whether compensation closed income gap',
    'If robust substitution was possible: snare classification becomes policy choice (could have been rope); reveals extractive intent. If substitution was impossible: snare reflects biological constraint plus economic structure, not intentional extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(economic_substitution_possibility, empirical, 'Whether economic compensation could have mitigated snare structure').

omega_variable(
    authority_power_expansion_intent,
    'Did institutional actors deliberately use health emergency to consolidate durable power gains, or was expanded authority incidental to epidemic response?',
    'Post-emergency policy retention analysis: which emergency powers were repealed vs which became permanent; timeline of repeal attempts; institutional resistance to reversion',
    'If permanent retention: extraction motive is evident (snare + tangled_rope becomes clearer). If rapid repeal: constraint was primarily coordination (rope). If mixed: confirms tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_power_expansion_intent, conceptual, 'Whether authority power expansion was intentional or incidental to response').

omega_variable(
    epistemic_authority_competition,
    'Was suppression of alternative epidemiological models (Great Barrington Declaration, alternative risk-benefit analyses) driven by scientific consensus or authority gatekeeping?',
    'Analysis of publication bias in peer review; citation patterns for contrarian models; institutional response timing to emerging alternative data',
    'If scientific consensus: suppression is coordination (rope/scaffold). If authority gatekeeping: suppression is extraction mechanism (snare). If mixed: tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_authority_competition, conceptual, 'Whether epistemic authority suppression reflects consensus or gatekeeping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(viral_transmission_rates, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(viral_tr_t0, viral_transmission_rates, theater_ratio, 0, 0.35).
narrative_ontology:measurement(viral_tr_t6, viral_transmission_rates, theater_ratio, 6, 0.68).
narrative_ontology:measurement(viral_tr_t12, viral_transmission_rates, theater_ratio, 12, 0.72).

% Extraction over time
narrative_ontology:measurement(viral_be_t0, viral_transmission_rates, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(viral_be_t6, viral_transmission_rates, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(viral_be_t12, viral_transmission_rates, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(viral_transmission_rates, enforcement_mechanism).
narrative_ontology:affects_constraint(viral_transmission_rates, vaccine_distribution_equity).
narrative_ontology:affects_constraint(viral_transmission_rates, economic_inequality_amplification).
narrative_ontology:affects_constraint(viral_transmission_rates, epistemic_authority_consolidation).

% DUAL FORMULATION NOTE:
% This constraint operates at the intersection of biological necessity (viral transmission is real) and institutional design (policy response choices). The biological component (transmission rates follow epidemiological laws) is mountain-like (ε ≈ 0.05). The institutional policy component (enforcement mechanisms, power consolidation) is tangled_rope (ε ≈ 0.58). These decompose into separate constraint stories: one for 'biological transmission constraints' (mountain), one for 'socio-political response design' (tangled_rope). The current story focuses on the socio-political response layer and is downstream of the biological transmission layer.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(viral_transmission_rates, powerless, 0.95).
constraint_indexing:directionality_override(viral_transmission_rates, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
