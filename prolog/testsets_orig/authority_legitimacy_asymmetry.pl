% ============================================================================
% CONSTRAINT STORY: authority_legitimacy_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_authority_legitimacy_asymmetry, []).

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
 *   constraint_id: authority_legitimacy_asymmetry
 *   human_readable: Authority Legitimacy Asymmetry
 *   domain: political/social/institutional
 *
 * SUMMARY:
 *   Authority legitimacy asymmetry is the structural constraint that
 *   determines whose knowledge, expertise, and decision-making power are
 *   accepted as valid within a social system. The constraint operates through
 *   institutional gatekeeping mechanisms (credentialism, peer review,
 *   professional licensing, formal hierarchy) that concentrate legitimacy in
 *   established authorities while systematically delegitimizing emerging
 *   claimants and alternative knowledge systems. This creates extraction:
 *   established authorities benefit from monopoly on legitimacy while
 *   emerging authorities and excluded populations bear the cost of being
 *   labeled illegitimate. Simultaneously, the constraint provides genuine
 *   coordination function — some mechanism is necessary to distinguish
 *   reliable knowledge from unreliable — making this a Tangled Rope from the
 *   analytical perspective. The theater ratio (0.68) reflects that legitimacy
 *   assessment is substantially performative: credentials signal competence
 *   but actual verification often happens through social ritual rather than
 *   direct evidence. Alternative legitimation pathways (citizen science,
 *   digital reputation, participatory research) are gradually building
 *   parallel systems that bypass traditional gatekeeping, creating a sunset
 *   mechanism for the constraint's extractive component.
 *
 * KEY AGENTS:
 *   - Established Authority Holders: Primary beneficiary (institutional/arbitrage) — monopoly on legitimacy; benefits from gatekeeping that prevents competitive authority claims
 *   - Institutional Gatekeepers: Primary beneficiary (institutional/arbitrage) — control legitimacy standards; extract rents through credentialing monopolies and professional licensing
 *   - Emerging Authority Claimants: Primary victim (powerless/trapped) — systematically delegitimized regardless of substantive merit; face insurmountable barriers to establishing authority
 *   - Excluded Populations: Primary victim (powerless/trapped) — knowledge systems and expertise systematically devalued; no pathway to institutional legitimacy
 *   - Alternative Knowledge Systems: Secondary victim (moderate/constrained) — knowledge is appropriated without credit while authority to speak is denied
 *   - Democratization Movements: Organized agents (organized/constrained) — building alternative legitimation pathways (citizen science, digital reputation) as scaffold replacing traditional gatekeeping
 *   - Credentialing Systems: Institutional actor (institutional/arbitrage) — maintains performative gatekeeping through degree/license requirements; theater increases as credential inflation proceeds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(authority_legitimacy_asymmetry, 0.58).
domain_priors:suppression_score(authority_legitimacy_asymmetry, 0.65).
domain_priors:theater_ratio(authority_legitimacy_asymmetry, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(authority_legitimacy_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(authority_legitimacy_asymmetry, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(authority_legitimacy_asymmetry, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(authority_legitimacy_asymmetry, tangled_rope).
narrative_ontology:human_readable(authority_legitimacy_asymmetry, "Authority Legitimacy Asymmetry").
narrative_ontology:topic_domain(authority_legitimacy_asymmetry, "political/social/institutional").

domain_priors:requires_active_enforcement(authority_legitimacy_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(authority_legitimacy_asymmetry, established_authority_holders).
narrative_ontology:constraint_beneficiary(authority_legitimacy_asymmetry, institutional_gatekeepers).
narrative_ontology:constraint_victim(authority_legitimacy_asymmetry, emerging_authority_claimants).
narrative_ontology:constraint_victim(authority_legitimacy_asymmetry, excluded_populations).
narrative_ontology:constraint_victim(authority_legitimacy_asymmetry, alternative_knowledge_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED AUTHORITY CLAIMANT (SNARE) — Individuals or communities whose authority claims are systematically delegitimized regardless of substantive merit. Trapped by institutional gatekeeping and resource asymmetry. Extraction is maximal: legitimacy is extracted and concentrated in established authorities while claimants bear full cost of being labeled illegitimate.
constraint_indexing:constraint_classification(authority_legitimacy_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALTERNATIVE KNOWLEDGE HOLDER (TANGLED ROPE) — Holders of alternative epistemic systems (indigenous knowledge, craft traditions, community expertise) experience both coordination function (their knowledge does contribute to collective problem-solving) and asymmetric extraction (their knowledge is appropriated without credit while their authority to speak is denied). Constrained by resource barriers and credentialism, but some pathways exist (academic partnerships, policy integration).
constraint_indexing:constraint_classification(authority_legitimacy_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED AUTHORITY (ROPE) — Existing institutional authorities experience the constraint as pure coordination: establishing and maintaining standards for legitimacy enables reliable knowledge production and social order. Possesses arbitrage options (can switch between legitimation frameworks) and benefits from coordination benefits. Low experienced extraction — the system works in their favor.
constraint_indexing:constraint_classification(authority_legitimacy_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DEMOCRATIZATION MOVEMENT (SCAFFOLD) — Organized collective action (citizen science, participatory research, open expertise platforms) sees the legitimacy asymmetry as a temporary institutional problem with a sunset clause. Building alternative legitimation pathways (blockchain verification, distributed expertise networks, reputation systems) that bypass traditional gatekeepers. High suppression but declining over generational timescale as digital infrastructure matures.
constraint_indexing:constraint_classification(authority_legitimacy_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CREDENTIALISM RITUAL (PITON) — Formal credentialing systems (degrees, licenses, professional memberships) maintain authority gatekeeping through performative institutional mechanisms. Theater ratio high: the credential signals competence, but its actual verification power has declined as credential inflation proceeds. Persists through institutional inertia despite reduced functional value — maintained because alternatives haven't fully replaced it.
constraint_indexing:constraint_classification(authority_legitimacy_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal perspective, authority asymmetry appears as an immutable feature of human social coordination: any social system requires some mechanism to distinguish reliable knowledge/authority from unreliable; this distinction inherently creates asymmetry between those deemed legitimate and those deemed illegitimate. However, the structural data contradicts the mountain classification — the engine will identify this as a false summit, revealing that variable historical legitimacy frameworks contradict naturalization as law.
constraint_indexing:constraint_classification(authority_legitimacy_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(authority_legitimacy_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(authority_legitimacy_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(authority_legitimacy_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(authority_legitimacy_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(authority_legitimacy_asymmetry, TR),
    TR >= 0.70.

:- end_tests(authority_legitimacy_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Established authorities capture legitimacy benefits during the interval, and the asymmetry has been increasing as credential inflation accelerates (rising from 0.42 to 0.58). This reflects that gatekeeping has become more valuable as knowledge systems have become more complex and differentiated. However, extractiveness is not at snare level (0.66+) because some alternative legitimation pathways do exist and have achieved partial traction. Suppression (0.65): High. Significant barriers to alternative authority establishment include: resource asymmetry (established institutions have funding, platforms, institutional support), credentialism (alternative authorities must meet formal credential requirements to be heard), publication bias (peer review systems systematically defer to established authorities), and social stigma (alternative knowledge systems are framed as illegitimate by default). These barriers operate simultaneously as external structures and internalized beliefs. Theater ratio (0.68): High and rising. Legitimacy assessment is substantially performative: holding credentials signals competence but does not guarantee it; passing peer review signals rigor but does not eliminate bias; institutional affiliation signals reliability but does not eliminate error. The theater has increased over the interval as credential inflation has accelerated (degrees proliferate, licenses expand) without proportional increases in actual verification capacity. Theater ratio trajectory (0.55 → 0.71) suggests degradation of the ritual's functional value relative to its performative intensity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates strong perspectival divergence. Established authorities see Rope: legitimate institutions maintaining standards necessary for reliable knowledge. Gatekeepers see their own gatekeeping as coordination — necessary filtering of illegitimate claims. Excluded populations see Snare: systematic delegitimization with no exit pathway. Alternative knowledge holders see Tangled Rope: genuine contribution to knowledge (coordination function) alongside appropriation and credit denial (extraction). Democratization movements see Scaffold: temporary institutional problem being solved by digital platforms and distributed legitimation. The credentialing system itself sees Piton: performative ritual maintaining gatekeeping through inertia despite declining verification power. The civilizational analytical observer risks seeing Mountain: legitimacy asymmetry as inherent to any social system — some must be legitimate, some illegitimate. This naturalizes a contingent institutional arrangement as law, revealed as false summit when historical variation in legitimacy frameworks is examined.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to the legitimacy extraction flow. Established authorities with arbitrage options experience low effective extraction (they set the rules); trapped excluded populations experience maximum extraction (they are systematically delegitimized); organized democratization movements experience moderate extraction (they can build alternative pathways but face persistent suppression from traditional gatekeepers). The pipeline computes d from power atom, exit options, and beneficiary/victim status, then applies sigmoid f(d) to produce chi. Beneficiaries (institutional/arbitrage) derive low d ≈ 0.15, producing negative or near-zero chi. Victims (powerless/trapped) derive high d ≈ 0.95, producing maximum chi ≈ 1.42. Organized challengers (organized/constrained) derive medium d ≈ 0.55, producing moderate chi ≈ 0.75. Inter-institutional dynamics: gatekeepers perceive legitimacy standards as necessary coordination mechanism (Rope perspective with low d); excluded populations perceive the same standards as extraction mechanism (Snare perspective with high d). The gap reveals that legitimacy is contested — different agents genuinely disagree about whether the asymmetry provides coordination or extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that the six types represent genuine structural differences in how different agents experience the same legitimacy mechanism. The beneficiary experiences Rope because gatekeeping genuinely provides coordination benefits (distinguishing reliable from unreliable authority). The victim experiences Snare because they are trapped in a system that denies them legitimacy regardless of merit. Both are correct descriptions of their structural situation — the mandatrophy is resolved by recognizing that the constraint has asymmetric effects. The Tangled Rope classification at the analytical level captures both functions: coordination (legitimate knowledge distinction) exists alongside extraction (monopoly on what counts as legitimate). The Scaffold perspective is particularly diagnostic: the sunset mechanism is real and empirically observable (alternative legitimation pathways are maturing) but the timeline remains contested (10-30 year horizon depending on domain). The Piton perspective identifies theater degradation: credentialing expands while verification capacity stagnates, producing expanding gap between ritual performance and functional gatekeeping.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_criteria_stability,
    'Are the criteria for institutional legitimacy stable and transparent, or do they shift to exclude emerging challengers?',
    'Historical analysis of legitimacy standards: track how criteria change when previously illegitimate actors begin to gain influence. Compare stated criteria with actual application patterns.',
    'If criteria are stable and transparently applied: legitimacy asymmetry is coordination mechanism (Rope from more perspectives). If criteria shift to exclude: asymmetry is extraction mechanism (Snare from more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_criteria_stability, empirical, 'Whether legitimacy standards are stable or adaptively exclusive').

omega_variable(
    alternative_legitimation_sufficiency,
    'Can alternative legitimation pathways (digital reputation, decentralized verification, community endorsement) achieve functional equivalence to traditional institutional authority?',
    'Comparative analysis of trust patterns, knowledge adoption rates, and institutional integration. Do systems that bypass traditional gatekeeping produce equivalent reliability and social coordination?',
    'If functionally equivalent: scaffold perspective is real and sunset is achievable (10-30 year horizon). If not equivalent: alternative pathways create fragmentation rather than replacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_legitimation_sufficiency, empirical, 'Whether alternative legitimation can replace institutional gatekeeping').

omega_variable(
    extraction_motivation_mechanism,
    'Is the legitimacy asymmetry primarily driven by extractive intent (gatekeepers actively benefiting from exclusion) or by genuine uncertainty about how to integrate heterogeneous knowledge systems?',
    'Institutional behavior analysis: examine whether gatekeepers make active efforts to delegitimize emerging authorities or whether delegitimation is passive byproduct of maintaining existing standards. Track resource flows and adaptation behavior.',
    'If extractive intent dominant: snare classification appropriate. If genuine uncertainty dominant: tangled_rope with different mitigation pathway (integration support rather than breaking gates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_motivation_mechanism, conceptual, 'Whether legitimacy gatekeeping is actively extractive or passively inertial').

omega_variable(
    suppression_mechanism_internalization,
    'What portion of the suppression (0.65) is externally structural (resource barriers, institutional gatekeeping) versus internalized (excluded populations accepting illegitimacy framing)?',
    'Post-gatekeeping removal analysis: when external barriers are removed (e.g., online platform emergence, regulatory change enabling alternative practitioners), do suppression levels decline proportionally or does internalized suppression persist?',
    'If primarily structural: removing gatekeeping barriers will significantly reduce suppression and enable rapid ascent of alternative authorities. If primarily internalized: post-removal suppression trajectory will show slower decline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural versus internalized components of suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(authority_legitimacy_asymmetry, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(auth_tr_t0, authority_legitimacy_asymmetry, theater_ratio, 0, 0.55).
narrative_ontology:measurement(auth_tr_t15, authority_legitimacy_asymmetry, theater_ratio, 15, 0.65).
narrative_ontology:measurement(auth_tr_t30, authority_legitimacy_asymmetry, theater_ratio, 30, 0.68).
narrative_ontology:measurement(auth_tr_t45, authority_legitimacy_asymmetry, theater_ratio, 45, 0.71).

% Extraction over time
narrative_ontology:measurement(auth_be_t0, authority_legitimacy_asymmetry, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(auth_be_t15, authority_legitimacy_asymmetry, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(auth_be_t30, authority_legitimacy_asymmetry, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(auth_be_t45, authority_legitimacy_asymmetry, base_extractiveness, 45, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(authority_legitimacy_asymmetry, identity_coordination).
narrative_ontology:affects_constraint(authority_legitimacy_asymmetry, credentialism_certification_inflation).
narrative_ontology:affects_constraint(authority_legitimacy_asymmetry, gatekeeping_monopoly_power).
narrative_ontology:affects_constraint(authority_legitimacy_asymmetry, epistemic_justice_deficit).

% DUAL FORMULATION NOTE:
% Authority legitimacy asymmetry decomposes into multiple structural constraints: credentialing systems (with their own theater ratio dynamics), gatekeeping power asymmetries (with their own beneficiary/victim structure), and epistemic justice deficits (with their own suppression mechanisms). This constraint represents the family of institutional mechanisms that concentrate and asymmetrically distribute legitimacy. The downstream constraints are domain-specific instantiations of the general asymmetry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(authority_legitimacy_asymmetry, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
