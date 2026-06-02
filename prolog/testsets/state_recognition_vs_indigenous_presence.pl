% ============================================================================
% CONSTRAINT STORY: state_recognition_vs_indigenous_presence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_recognition_vs_indigenous_presence, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: state_recognition_vs_indigenous_presence
 *   human_readable: State Recognition vs Indigenous Presence: Territorial Legitimacy Conflict
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   The territorial conflict between Israeli state and Palestinian political
 *   entities presents a constraint story where competing legitimacy claims
 *   operate within a single bounded territory. The constraint is framed here
 *   as a **kernel-coherence boundary test**: whether 'territorial legitimacy'
 *   is one kernel (shared foundational commitment to legitimate authority
 *   over land) read two incompatible ways, or two incoherent kernels with no
 *   shared substrate. The analytical question is not which claimant is
 *   'correct' but what structural conditions would resolve or perpetuate the
 *   constraint. The extractiveness measurement (0.68) reflects that the
 *   recognized state apparatus benefits from international legal frameworks
 *   that treat state sovereignty as the mechanism for territorial legitimacy,
 *   while the unrecognized indigenous population bears the cost of this
 *   framework — they are trapped within territory they claim but cannot
 *   govern, with their presence unrecognized despite continuous occupation.
 *   Suppression (0.72) reflects multi-component enforcement: military control
 *   (explicit), legal exclusion (non-recognition), administrative authority
 *   (state monopoly on governance), and epistemic control (historical
 *   narrative dominance). Theater ratio (0.65) reflects that much of the
 *   legitimacy discourse is performative: bilateral recognition ceremonies,
 *   UN seat ritual, sovereignty declarations that do not correspond to actual
 *   governing capacity or population consent.
 *
 * KEY AGENTS:
 *   - Indigenous Population Without State Recognition: Primary victim (powerless/trapped/national scope) — bears full extraction cost; cannot exit territorial claim without abandoning ancestral identity; faces military enforcement, legal exclusion, administrative dependence
 *   - Recognized State Apparatus: Primary beneficiary (institutional/arbitrage/global scope) — benefits from international legal recognition framework; controls territorial governance, resource extraction, law enforcement; has exit optionality (can negotiate, change borders, conduct diplomacy)
 *   - Organized Indigenous Resistance Movement: Secondary actor (organized/constrained/regional scope) — has partial negotiation capacity (autonomy proposals, land claims processes) but faces high structural costs; benefits from coordination mechanisms (NGO support, humanitarian access) while bearing extraction restrictions
 *   - International Legal Recognition System: Institutional maintainer (institutional/arbitrage/global scope) — Westphalian framework treats state recognition as mechanism for territorial legitimacy; maintains recognition monopoly through UN system, treaty ratification, bilateral diplomacy; benefits from state-centric order
 *   - Third-Party States: Secondary institutional actors (institutional/arbitrage/variable scope) — may enforce or mediate suppression; benefit from territorial status quo stability or instability depending on geopolitical interest; can shift recognition patterns for diplomatic leverage
 *   - Analytical Observer: Civilizational perspective (analytical/analytical/universal scope) — risks naturalizing contingent institutional arrangements as immutable features of human territoriality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_recognition_vs_indigenous_presence, 0.68).
domain_priors:suppression_score(state_recognition_vs_indigenous_presence, 0.72).
domain_priors:theater_ratio(state_recognition_vs_indigenous_presence, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_recognition_vs_indigenous_presence, extractiveness, 0.68).
narrative_ontology:constraint_metric(state_recognition_vs_indigenous_presence, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_recognition_vs_indigenous_presence, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_recognition_vs_indigenous_presence, snare).
narrative_ontology:human_readable(state_recognition_vs_indigenous_presence, "State Recognition vs Indigenous Presence: Territorial Legitimacy Conflict").
narrative_ontology:topic_domain(state_recognition_vs_indigenous_presence, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(state_recognition_vs_indigenous_presence).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_recognition_vs_indigenous_presence, '638a92ef-4e94-47af-8b8a-cc7a5c9d9316').
narrative_ontology:cs_kernel_codification('638a92ef-4e94-47af-8b8a-cc7a5c9d9316', distributed).
narrative_ontology:cs_authority_grounding('638a92ef-4e94-47af-8b8a-cc7a5c9d9316', extraction).
narrative_ontology:cs_reading_relation('638a92ef-4e94-47af-8b8a-cc7a5c9d9316', indigenous_presence_reading, forecloses).
narrative_ontology:cs_reading_relation('638a92ef-4e94-47af-8b8a-cc7a5c9d9316', third_party_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('638a92ef-4e94-47af-8b8a-cc7a5c9d9316', foundational, state_recognition_constitutes_legitimacy).
narrative_ontology:cs_axiom_status(state_recognition_constitutes_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('638a92ef-4e94-47af-8b8a-cc7a5c9d9316', state_recognition_constitutes_legitimacy, conventional).
narrative_ontology:cs_axiom('638a92ef-4e94-47af-8b8a-cc7a5c9d9316', foundational, westphalian_sovereignty_framework).
narrative_ontology:cs_axiom_status(westphalian_sovereignty_framework, holdable).
narrative_ontology:cs_axiom_grounding('638a92ef-4e94-47af-8b8a-cc7a5c9d9316', westphalian_sovereignty_framework, conventional).
narrative_ontology:cs_axiom('638a92ef-4e94-47af-8b8a-cc7a5c9d9316', secondary, recognition_monopoly_by_international_institutions).
narrative_ontology:cs_axiom_status(recognition_monopoly_by_international_institutions, holdable).
narrative_ontology:cs_axiom_grounding('638a92ef-4e94-47af-8b8a-cc7a5c9d9316', recognition_monopoly_by_international_institutions, conventional).
narrative_ontology:cs_reference_frame('638a92ef-4e94-47af-8b8a-cc7a5c9d9316', westphalian_recognition_supremacy).
narrative_ontology:cs_drift_state('638a92ef-4e94-47af-8b8a-cc7a5c9d9316', contemporary_self_determination_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('638a92ef-4e94-47af-8b8a-cc7a5c9d9316', '2026-02-26T00:00:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_recognition_vs_indigenous_presence, recognized_state_apparatus).
narrative_ontology:constraint_victim(state_recognition_vs_indigenous_presence, indigenous_population_without_state_recognition).
narrative_ontology:constraint_victim(state_recognition_vs_indigenous_presence, territorial_displacement_cohort).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS POPULATION (SNARE) — Trapped within territory claimed by recognized state; cannot exit without abandoning ancestral presence claim or physical displacement. Faces structural extraction: land access controlled by state apparatus, governance authority denied, legal recognition withheld. Suppression is enforced through state monopoly on territorial administration and international law's recognition of state sovereignty over inhabited territory.
constraint_indexing:constraint_classification(state_recognition_vs_indigenous_presence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RECOGNIZED STATE APPARATUS (ROPE) — Benefits from international legal recognition framework that treats state sovereignty as paramount. Experiences territorial control as legitimate coordination of governance and population management. State actors see the constraint as solving the collective action problem of territorial authority — who governs, who enforces law, who collects revenue. Net beneficiary with exit optionality (arbitrage: can negotiate borders, conduct diplomacy, change governance models).
constraint_indexing:constraint_classification(state_recognition_vs_indigenous_presence, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ORGANIZED INDIGENOUS RESISTANCE (TANGLED ROPE) — Has partial capacity to negotiate (statehood proposals, autonomy arrangements, land claims processes) but faces high structural costs (military asymmetry, diplomatic isolation, economic dependence). Benefits from coordination mechanisms (humanitarian access, NGO support, international advocacy networks) while bearing extraction costs (resource restriction, administrative exclusion). Constrained exit — can organize and press claims but cannot unilaterally change recognition status.
constraint_indexing:constraint_classification(state_recognition_vs_indigenous_presence, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: INTERNATIONAL RECOGNITION SYSTEM (PITON) — Westphalian sovereignty framework treats state recognition as the mechanism for territorial legitimacy. Theater ratio (0.65) reflects that the system's actual function (allocating governance authority) is substantially obscured by performative rituals: bilateral recognition ceremonies, UN seat allocation, treaty ratification theater. The core function (determining who exercises territorial control) is separate from and often contradicts the performative legitimacy claims. System persists through institutional inertia — alternative frameworks (indigenous consent, territorial pluralism, shared governance) are conceptually available but normatively rejected by state apparatus.
constraint_indexing:constraint_classification(state_recognition_vs_indigenous_presence, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational scope, territorial conflict appears as an immutable feature of political organization: finite land, competing claims, irreducible scarcity. This reading naturalizes the constraint as inherent to human territoriality. However, the structural data contradicts the mountain classification — clear beneficiaries (recognized states), clear victims (unrecognized populations), and enforceable suppression mechanisms reveal this as a false summit. The 'inevitability' framing naturalizes what is a contingent institutional arrangement (state monopoly on recognition authority).
constraint_indexing:constraint_classification(state_recognition_vs_indigenous_presence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: TWO-STATE SOLUTION SCAFFOLD (SCAFFOLD) — International consensus frames statehood creation as a sunset mechanism: if new state is recognized, the constraint dissolves because both parties achieve recognition parity. This perspective requires `has_sunset_clause: true` — the constraint is explicitly temporary pending state establishment. However, the scaffold is contested: one party views statehood as the solution; the other may view it as partial recognition inadequate to ancestral territorial claims. The sunset clause's operationality is the empirical omega variable.
constraint_indexing:constraint_classification(state_recognition_vs_indigenous_presence, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_recognition_vs_indigenous_presence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(state_recognition_vs_indigenous_presence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(state_recognition_vs_indigenous_presence, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_recognition_vs_indigenous_presence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(state_recognition_vs_indigenous_presence, TR),
    TR >= 0.70.

:- end_tests(state_recognition_vs_indigenous_presence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High-moderate. The recognized state apparatus extracts substantial benefit from the territorial control framework — governance authority, resource access, population administration, diplomatic standing. However, extractiveness is not maximal (0.72 or higher) because the extraction is constrained by: (1) international humanitarian norms that impose costs on suppression, (2) organized resistance that prevents complete administrative control, (3) third-party mediation that introduces negotiation costs. The measurement trajectory (0.55→0.68, plateau) reflects that initial recognition asymmetry allowed lower suppression; as indigenous resistance organized, suppression requirement increased to maintain control. Suppression (0.72): High. Multi-component enforcement: military (occupation forces, checkpoints, security operations), legal (non-recognition, administrative exclusion), economic (resource control, trade restrictions), epistemic (historical narrative dominance). The measurement trajectory (0.58→0.72, plateau) reflects that suppression machinery was built up over time; after reaching effective threshold, it stabilizes because further intensification faces international resistance. Theater ratio (0.65): Moderate-high. Legitimacy discourse involves substantial performative content: UN recognition ceremonies, sovereignty declarations, diplomatic protocols that obscure the underlying reality (one group controls territory; the other does not). Functional recognition is separate from performative recognition — the system allocates governing authority through military-administrative facts on the ground, not through UN votes or treaty signatures. Theater rose over time (0.42→0.65) as the initial de facto control became institutionalized and required performative legitimation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The recognized state sees coordination (Rope perspective: solving governance authority problem). The indigenous population sees pure extraction (Snare perspective: trapped, non-negotiable, suppressed). The organized movement sees mixed coordination-extraction (Tangled Rope: some negotiation capability, some benefit from international structures, but asymmetric extraction). The international legal system sees its own procedure as legitimate (Piton perspective: the recognition ritual is substantially performative, theater ratio 0.65, but the system persists through inertia). The two-state solution sees a temporary problem (Scaffold perspective: sunset clause operative if statehood is established). The analytical observer at civilizational scope risks seeing an immutable natural law (Mountain perspective: territorial conflict is inherent to human organization, but this is a false summit revealing naturalization of contingent institutional arrangements). The perspectival gap is the full 6-type range because the underlying structural data supports all readings from their respective observation points.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position relative to extraction flow. Recognized state apparatus: beneficiary with arbitrage options → low d (0.15) → negative f(d) → negative χ (benefits exceed costs). Indigenous population without recognition: victim with trapped exit → high d (0.95) → high f(d) (1.42) → high χ (maximum extraction). Organized resistance: victim with constrained exit → moderate-high d (0.70) → moderate f(d) (1.02) → moderate χ. International legal system: beneficiary with arbitrage → low d → institutional support for recognition framework. Third-party states: variable d depending on whether they benefit from status quo (higher d if mediation role) or seek change (lower d if pushing for new state recognition). The scope modifier σ(S) amplifies extraction at national/regional scope (σ=1.0-1.0) because the constraint's enforcement is territorial and locally visible. At global scope (σ=1.2), diplomatic abstraction somewhat amplifies the extractiveness metric, but the core local suppression mechanisms remain primary.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by exposing the kernel-coherence boundary. If territorial legitimacy is ONE contested kernel, the constraint is a reading problem: two parties accept 'legitimate authority requires X' but disagree on whether X is satisfied. The engine's cs_structure fields (reading_relations: coexists_with, axioms: grounding_type) would apply, and the constraint is a Tangled Rope (mixed coordination and extraction reflecting the contested reading). If territorial legitimacy is TWO INCOHERENT KERNELS, the constraint is genuinely a Snare: no shared substrate exists, no internal resolution mechanism, and extraction is terminal. The snare classification is correct only if the kernels are incoherent. The measurements show that suppression increased over time (0.58→0.72) and theater ratio plateaued (0.65), suggesting that the state-recognition reading stabilized as the institutional dominant (recognition authority is now institutionalized) while the indigenous-presence reading was forced into non-recognition status (trapped population, snare perspective). This institutional stabilization itself is the answer to mandatrophy: the constraint persists not because it is unresolvable in principle, but because the state apparatus has the power to enforce one reading over the other. Recognition of this structural asymmetry clarifies that the constraint is a snare FROM THE PERSPECTIVE OF THE UNRECOGNIZED POPULATION, even if it appears as legitimate coordination (rope) or temporary solution (scaffold) from other perspectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_coherence_boundary,
    'Is territorial legitimacy ONE kernel read two incompatible ways, or TWO incoherent kernels with no shared substrate?',
    'Examine whether both readings accept a common foundational claim about what ''territorial legitimacy'' means. If one reading accepts ''legitimacy derives from state recognition'' and the other accepts ''legitimacy derives from continuous indigenous presence,'' and these cannot coexist in a single framework, then two kernels, not one. If both accept ''legitimate authority requires X'' and disagree only on whether X is satisfied, then one kernel.',
    'ONE KERNEL: The constraint is a tangled reading problem solvable through framework dialogue. The engine''s cs_structure fields (reading_relations: coexists_with/influences) apply. TWO KERNELS: The constraint is a real collision with no internal resolution — classify as snare with network decomposition into separate constraint stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_coherence_boundary, conceptual, 'Whether territorial legitimacy is one contested kernel or two incoherent kernels').

omega_variable(
    recognition_ontology_ambiguity,
    'Does international legal recognition CREATE legitimacy (constitutive view) or RECOGNIZE pre-existing legitimacy (declaratory view)?',
    'Legal-historical analysis: examine whether recognition frameworks have treated legitimacy as ontologically prior to or dependent on recognition acts. Declaratory view predicts that unrecognized polities retain legitimacy; constitutive view predicts they do not. Empirical test: track third-party attitude changes before vs after recognition events.',
    'CONSTITUTIVE: State recognition is a structural gate that creates legitimacy — the snare classification is correct and terminal. DECLARATORY: Recognition is secondary — legitimacy exists independently, and non-recognition is extraction overlaid on pre-existing claims. Changes interpretation but not χ (extractiveness remains 0.68).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recognition_ontology_ambiguity, conceptual, 'Ontology of recognition: constitutive vs declaratory').

omega_variable(
    continuous_presence_proof_burden,
    'What standard of evidence establishes ''continuous indigenous presence'' sufficient to ground territorial legitimacy? Who bears the burden of proof?',
    'Comparative legal analysis: examine how different sovereignty frameworks treat presence claims (demography, linguistic continuity, archaeological evidence, genealogical records, administrative history, cultural institutions). Map burden allocation: must indigenous groups prove presence, or must states prove displacement? International courts'' precedent analysis.',
    'If burden on indigenous groups and standard is high (archaeological + documentary + demographic): extraction is higher — suppression becomes epistemic (control over evidence and historical narrative). If burden on states: legitimacy distribution shifts, potentially upgrading indigenous perspective from snare to tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuous_presence_proof_burden, empirical, 'Evidence standard and burden for continuous presence claims').

omega_variable(
    two_state_solution_viability,
    'Does the two-state solution model actually function as a sunset mechanism, or is it a perpetual scaffold that cannot trigger its termination condition?',
    'Historical-empirical: track statehood establishment timelines, negotiation progress, international recognition readiness. If statehood is established and recognition parity achieved, scaffold sunset fires. If negotiations remain indefinitely deadlocked, the scaffold is aspirational theater (piton reversion). If new state is recognized but territorial disputes persist, sunset partially misfires.',
    'VIABLE SUNSET: Scaffold classification is correct — generational time horizon makes sense, and the constraint is genuinely temporary. PERPETUAL DEADLOCK: Scaffold reverts to piton (theater_ratio rises as statehood becomes aspirational ritual rather than functional pathway). PARTIAL RESOLUTION: Constraint fragments into two separate stories (state recognition achieved, but territorial disputes persist as separate snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(two_state_solution_viability, empirical, 'Viability of two-state solution as sunset mechanism').

omega_variable(
    suppression_mechanism_composition,
    'Does the measured suppression (0.72) derive from military enforcement, legal exclusion, economic dependence, epistemic control, or some combination? What is the stability of each component?',
    'Structural decomposition: analyze suppression components separately. Military suppression can be reduced through ceasefires. Legal suppression (non-recognition) is institutional inertia. Economic suppression (resource control, trade restrictions) is policy-dependent. Epistemic suppression (historical narrative control) is cultural-institutional. Measure each independently.',
    'If military component dominates: suppression is unstable (ceasefires reduce it rapidly). If institutional/epistemic components dominate: suppression is stable (legal inertia and narrative control persist across military cycles). Affects generational time horizon credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_composition, empirical, 'Composition and stability of suppression mechanisms').

omega_variable(
    third_party_state_role_ambiguity,
    'Do third-party states (neighbors, powers, international community) function as neutral arbiters of legitimacy, as beneficiaries of the status quo, or as extractors themselves?',
    'Network analysis: map recognition patterns. Do third parties recognize one claimant exclusively, both, or neither? Do they benefit from territorial instability or stability? Do they enforce suppression or mediate it? Track how third-party interests shift with geopolitical context.',
    'NEUTRAL ARBITERS: legitimacy framework is external and stable. BENEFICIARIES OF STATUS QUO: third parties actively maintain suppression for their own extraction (adds institutional depth to snare). EXTRACTORS: the constraint becomes multi-level — third parties extract from both claimants (changes directionality and chi calculations).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_state_role_ambiguity, empirical, 'Role of third-party states in territorial legitimacy conflict').

omega_variable(
    identity_lock_mechanism_indigenous_population,
    'To what extent is the indigenous population''s inability to exit the territorial claim rooted in identity fusion (ancestral territory as constitutive of collective identity) vs structural barriers (military enforcement, legal prohibition, economic dependence)?',
    'Comparative analysis: examine exit scenarios. If a population could safely emigrate with full legal and economic rights elsewhere, would they? If the answer is ''identity would dissolve,'' it is identity-locked. If the answer is ''they cannot afford emigration'' or ''state prevents it,'' it is trapped/constrained. Survey and testimony analysis.',
    'PRIMARILY IDENTITY-LOCKED: The snare derives its hold from cognitive/identity binding (the constraint is psychologically irreversible). Exit would require abandoning identity, not just paying material costs. PRIMARILY TRAPPED: The snare derives its hold from external barriers. Exit is materially impossible but identity-wise thinkable. Different measurement approaches for suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_indigenous_population, conceptual, 'Identity-lock vs structural barriers in indigenous territorial attachment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_recognition_vs_indigenous_presence, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(staterec_theater_t0, state_recognition_vs_indigenous_presence, theater_ratio, 0, 0.42).
narrative_ontology:measurement(staterec_theater_t25, state_recognition_vs_indigenous_presence, theater_ratio, 25, 0.65).
narrative_ontology:measurement(staterec_theater_t50, state_recognition_vs_indigenous_presence, theater_ratio, 50, 0.65).

% Extraction over time
narrative_ontology:measurement(staterec_extractiveness_t0, state_recognition_vs_indigenous_presence, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(staterec_extractiveness_t25, state_recognition_vs_indigenous_presence, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(staterec_extractiveness_t50, state_recognition_vs_indigenous_presence, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(staterec_suppression_t0, state_recognition_vs_indigenous_presence, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(staterec_suppression_t25, state_recognition_vs_indigenous_presence, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(staterec_suppression_t50, state_recognition_vs_indigenous_presence, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_recognition_vs_indigenous_presence, enforcement_mechanism).
narrative_ontology:affects_constraint(state_recognition_vs_indigenous_presence, diaspora_return_entitlement).
narrative_ontology:affects_constraint(state_recognition_vs_indigenous_presence, settlement_expansion_logic).
narrative_ontology:affects_constraint(state_recognition_vs_indigenous_presence, refugee_compensation_asymmetry).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel about territorial legitimacy. Sibling constraints (diaspora_return_entitlement, settlement_expansion_logic) represent downstream claims that depend on which reading of the legitimacy kernel is operative. Each sibling story instantiates a different reading of the same kernel, with different epsilon values and beneficiary/victim structures. All three are linked via network.affects_constraints to show the kernel family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_recognition_vs_indigenous_presence, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
