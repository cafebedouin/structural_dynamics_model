% ============================================================================
% CONSTRAINT STORY: territorial_occupation_1967
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_occupation_1967, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: territorial_occupation_1967
 *   human_readable: Territorial Occupation and Legitimacy Claims (1967 Forward)
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   The territorial occupation that began in 1967 presents one of the
 *   clearest exemplars of constraint escalation: the accumulation of
 *   extraction mechanisms layered onto an initially security-oriented
 *   occupation structure. The constraint exhibits snare characteristics at
 *   the powerless/trapped level but reveals snare-to-piton degradation at the
 *   institutional/international legal level. The theater ratio's rise from
 *   0.48 to 0.64 reflects Goodhart drift — the international legal regime's
 *   original function (preventing territorial acquisition by force) has been
 *   progressively hollowed while performative maintenance (Security Council
 *   resolutions, ICJ advisories) continues. The extractiveness rise from 0.45
 *   to 0.68 reflects accumulating institutional mechanisms for resource and
 *   movement control. The constraint manifests as two separable legitimacy
 *   claims operating on different temporal registers: (1) existential
 *   legitimacy of the state (foundational, 1948) and (2) territorial
 *   occupation enforcement (contingent, post-1967). The source material's
 *   observation about structural incoherence is diagnostically significant:
 *   the legitimacy claims do not resolve into a single kernel that could be
 *   read two ways; they represent distinct structural problems requiring
 *   decomposition into separate constraint stories. The present story models
 *   the occupation as an enforcement structure targeting trapped populations;
 *   a companion story should model the foundational legitimacy claim and its
 *   relationship to the occupation claim.
 *
 * KEY AGENTS:
 *   - Occupied Population: Primary victim (powerless/trapped) — subject to military administration, permit systems, resource restrictions, movement constraints
 *   - Occupying State Security Apparatus: Primary beneficiary (institutional/arbitrage) — operates within occupation framework; has capacity to reframe security doctrine
 *   - Occupying State Political Leadership: Secondary beneficiary/constrained actor (institutional/constrained) — maintains domestic coalition around occupation; faces international legal pressure but experiences generational institutional lock-in
 *   - Palestinian Authority & Diaspora: Secondary victim (moderate/constrained) — institutional capacity but operates under occupation constraints; faces resource denial and administrative subordination
 *   - International Legal Regime: Tertiary victim (institutional/constrained) — formally charged with preventing territorial acquisition by force; functionally degraded through selective non-enforcement and exception accumulation
 *   - Settlement Expansion Actors: Secondary beneficiary (powerful/mobile) — benefit from occupation framework through land access and security guarantees; have exit options (relocation, institutional reorientation)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing occupation as inevitable consequence of military asymmetry rather than contingent institutional structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_occupation_1967, 0.68).
domain_priors:suppression_score(territorial_occupation_1967, 0.75).
domain_priors:theater_ratio(territorial_occupation_1967, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_occupation_1967, extractiveness, 0.68).
narrative_ontology:constraint_metric(territorial_occupation_1967, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(territorial_occupation_1967, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_occupation_1967, snare).
narrative_ontology:human_readable(territorial_occupation_1967, "Territorial Occupation and Legitimacy Claims (1967 Forward)").
narrative_ontology:topic_domain(territorial_occupation_1967, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_occupation_1967).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_occupation_1967, occupying_state_security_apparatus).
narrative_ontology:constraint_beneficiary(territorial_occupation_1967, settlement_expansion_actors).
narrative_ontology:constraint_victim(territorial_occupation_1967, occupied_population).
narrative_ontology:constraint_victim(territorial_occupation_1967, international_legal_regime).
narrative_ontology:constraint_victim(territorial_occupation_1967, two_state_solution_viability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OCCUPIED POPULATION (SNARE) — Structurally trapped. No exit from occupying governance regime without displacement or institutional surrender. High suppression through permit systems, movement restrictions, economic dependency, and asymmetric legal authority. Extraction operates through resource appropriation (water, land, airspace) and coercive administrative control. Powerless position, trapped exit options, biographical horizon = maximum experienced extractiveness.
constraint_indexing:constraint_classification(territorial_occupation_1967, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: DIASPORA & PALESTINIAN AUTHORITY (TANGLED ROPE) — Structurally constrained rather than trapped. Possess institutional identity and diplomatic capacity but operate under asymmetric constraints: limited territorial control, economic dependency, and security constraints. Experience both extraction (resource denial, administrative subordination) and coordination function (social service provision, governance of internal affairs). Moderate power with constrained exit (exit would require state dissolution). Generational horizon captures institutional persistence.
constraint_indexing:constraint_classification(territorial_occupation_1967, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: OCCUPYING STATE SECURITY APPARATUS (ROPE) — Primary beneficiary with arbitrage options (can reposition security doctrine, can negotiate borders, can exit occupation). Experiences constraint as coordination mechanism: maintaining security perimeter, managing population, controlling flows. Net benefits flow to this actor. Immediate time horizon reflects tactical security operations. Arbitrage exit option reflects institutional capacity to reframe security strategy.
constraint_indexing:constraint_classification(territorial_occupation_1967, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OCCUPYING STATE POLITICAL LEADERSHIP (TANGLED ROPE) — Distinct from security apparatus. Faces generational institutional constraints: domestic coalition maintenance, legitimacy narratives, international legal constraints. Experiences extraction (consolidating territorial gains, settlement expansion benefits) alongside coordination (security, resource management). Constrained exit: ending occupation would require domestic political realignment. Suppression directed at domestic dissent and international pressure.
constraint_indexing:constraint_classification(territorial_occupation_1967, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: CIVILIZATIONAL/ANALYTICAL OBSERVER (MOUNTAIN CANDIDATE) — Risk of naturalizing the occupation as an immutable consequence of military superior power and unresolvable historical claims. Treats power asymmetry as a law of international relations rather than a contingent institutional arrangement. Theater ratio and structural data suggest this is a false summit: the occupation is sustained through specific institutional practices (permit systems, settlement policy, legal administrative structures) that are contingent and contestable, not immutable.
constraint_indexing:constraint_classification(territorial_occupation_1967, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: INTERNATIONAL LEGAL REGIME (PITON) — Originally designed to prevent territorial acquisition by force (UN Charter, Geneva Conventions). Functionally degraded: regime maintains formal condemnation while enforcement is absent and exceptions accumulate. Theater ratio high (0.64): legal pronouncements, Security Council resolutions, ICJ advisories persist despite non-compliance. Constraint persists through institutional inertia (resolutions continue being issued, legal structures remain formally intact) while actual deterrence capacity has eroded. The regime cannot exit — it persists through repeated formal utterance despite functional failure.
constraint_indexing:constraint_classification(territorial_occupation_1967, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_occupation_1967_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(territorial_occupation_1967, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(territorial_occupation_1967, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_occupation_1967, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(territorial_occupation_1967, TR),
    TR >= 0.70.

:- end_tests(territorial_occupation_1967_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The constraint operates through multiple extraction mechanisms: (1) land appropriation for settlement and military installations (~0.18 value extracted), (2) resource control (water, airspace, electromagnetic spectrum allocation) (~0.15), (3) permit/administrative systems that extract economic value through delay and denial (~0.20), (4) coercive taxation and fee structures (~0.10), (5) labor and movement constraints that extract value through asymmetric opportunity costs (~0.05). The trajectory from 0.45 to 0.68 reflects institutional layering: early occupation (1967-1975) was primarily security-focused with moderate extraction. Progressive addition of settlement policy, permit system formalization, and administrative subordination mechanisms increased extractiveness. The 0.68 value reflects 57 years of institutional consolidation. Suppression (0.75): Very high. Multiple non-fungible barriers to exit: (a) geographical isolation (no internationally-recognized exit pathway), (b) legal subordination (occupied population has minimal property rights under military administration), (c) resource dependency (water allocation, economic corridor access controlled by occupying state), (d) physical barriers (walls, checkpoints), (e) military force capacity (asymmetric power to enforce compliance), (f) political fragmentation (victims divided between occupied territory, diaspora, and dispersed communities). Suppression is not declining — if anything, physical barriers and administrative mechanisms have increased since 1990s. Theater ratio (0.64): Moderately high and rising. International legal regime produces Security Council resolutions, ICJ advisories, UN General Assembly condemnations with high regularity. However, enforcement is absent, exceptions accumulate (various legal doctrines, military necessity claims, security exceptions), and actual deterrent effect is minimal. The regime maintains form while function has degraded. Theater increases from 0.48 to 0.64 as the gap between formal legal positions and actual enforcement widens. The original 1967 occupation was presented as temporary security measure; 57 years of persistence while the regime continues formal opposition suggests high theater — the legal structure performs an opposition function while the occupation persists unchanged.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a striking perspectival gap between trapped victims (snare), constrained institutional actors (tangled rope), beneficiary institutions (rope), and the international legal regime (piton). The occupied population perceives immutable extraction with no exit — snare classification with d approaching 1.0. The occupying state security apparatus perceives manageable security coordination — rope classification with d approaching 0.0 (net beneficiary). The international legal regime perceives its own degradation — piton classification reflecting that formal opposition persists while function has eroded. The civilizational analytical observer risks snapping to a mountain classification (power imbalances are immutable) but this is a false summit: the extraction mechanisms are institutionally contingent (permit systems, settlement policy, administrative structures) that could be otherwise. The perspectival gap is not primarily observational (same facts, different frames) but structural (victims and beneficiaries experience the constraint through genuinely different mechanisms).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values flow from structural position: (1) Trapped victims with no exit experience maximum d (approaching 1.0, f(d) approaching 1.42), yielding high experienced extractiveness. (2) Institutional beneficiaries with arbitrage options experience low d (approaching 0.05, f(d) approaching -0.12), experiencing the constraint as beneficial. (3) Moderate actors with constrained exit experience intermediate d (0.55-0.75, f(d) 0.75-1.15), experiencing mixed extraction and coordination. The directionality calculation χ = ε × f(d) × σ(S) shows scope amplification: regional scope (σ=0.9) means that base extractiveness of 0.68 yields regional-scale χ reaching 0.55-0.65 for trapped agents. Global scope (σ=1.2) for the international legal regime yields χ reaching 0.82 (maximum tension between formal position and actual powerlessness). No overrides are necessary — the structural derivation accurately captures the asymmetric experience of the constraint across agent positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy — the risk of mislabeling extractive constraints as coordination — is resolved through the snare classification at the powerless/trapped level and the tangled rope at the institutional/moderate level. This constraint does NOT exhibit the mandatrophy problem: there is no temptation to call it a coordination mechanism (rope) because the beneficiary structure is asymmetric and the victims are trapped without exit options. The constraint is unambiguously extractive at the point of enforcement. However, a related mandatrophy risk exists at the institutional level: occupying state political leadership might claim the occupation is a coordination mechanism for security (collective action problem: how do states maintain security in conflict zones?). The tangled rope classification at this level correctly disambiguates: the constraint does exhibit a coordination function (managing security, providing order), but it is coupled with extraction (consolidating territorial gains, enabling settlement expansion). The mandatrophy is resolved by maintaining both classification components rather than resolving to either pure coordination or pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_or_single_constraint,
    'Is this one constraint (territorial occupation enforced through law and military) or two kernels (1948 existential legitimacy vs 1967 territorial occupation) requiring separate stories?',
    'Structural analysis of legitimacy claims: (a) Do the claims operate on the same temporal register (founding vs ongoing)? (b) Do they share the same victims and beneficiaries? (c) Would resolving one constraint resolve the other? Current evidence: claims operate on different registers (existential legitimacy is foundational; occupation is contingent on specific institutional choices). Victims differ partly (occupation creates victims of occupation; legitimacy dispute creates victims of exclusion). Resolution of occupation would not resolve existential legitimacy dispute.',
    'If two kernels: decompose into (a) foundational_legitimacy_claim_1948 and (b) territorial_occupation_structure_1967. If one constraint: current story is correct. Recommend decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_or_single_constraint, conceptual, 'Whether occupation is one constraint or manifests two distinct kernels').

omega_variable(
    extraction_vs_security_mechanism,
    'Does suppression operate primarily as resource extraction (land appropriation, permit systems controlling labor/resources) or as security mechanism (restricting movement to prevent violence)?',
    'Empirical analysis of permit system allocations: correlate permit densities with military security requirements vs economic/settlement expansion benefits. Historical analysis: were permit restrictions tightened in response to specific security events or in response to settlement expansion? Economic data: do permit systems disproportionately benefit settlement enterprises vs security operations?',
    'If primarily extraction: snare classification is robust. If primarily security: reclassify to rope or tangled_rope (coordination mechanism with defensive justification). If mixed: suppress is accurately measured but beneficiary analysis must clarify which actor benefits from the security mechanism vs which benefits from extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_security_mechanism, empirical, 'Whether suppression mechanisms serve extraction or security').

omega_variable(
    two_state_solution_contingency,
    'Would establishment of a Palestinian state resolve the territorial occupation constraint or would it persist as a state-to-state boundary control mechanism with similar extraction properties?',
    'Comparative analysis: review historical precedents where occupation ended (e.g., France/Algeria, India/Pakistan). Analyze whether post-occupation state-to-state control mechanisms exhibit similar suppression signatures (permits, resource allocation asymmetry, movement restrictions). If similar, the constraint persists in modified form; the two-state solution does not resolve the underlying extraction mechanism.',
    'If constraint persists post-statehood: strategic focus on constraint architecture (permit systems, resource allocation) not on political status. If constraint resolves: political settlement is both necessary and sufficient. Current evidence suggests partial persistence: even post-independence/sovereignty scenarios show similar control mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(two_state_solution_contingency, empirical, 'Whether two-state solution resolves or transforms the occupation constraint').

omega_variable(
    naturalizing_power_asymmetry,
    'Is the occupation presented as inevitable consequence of military power imbalance (naturalizing mountain view) or as a contingent institutional choice (snare/tangled_rope views)?',
    'Examine counterfactuals: (a) Military-superior occupying powers that did NOT maintain occupation (rare but present in historical record). (b) Specific institutional choices (permit systems, settlement expansion policy) that could be reformed without surrendering security apparatus. (c) Alternative governance models for occupied territory that maintain security benefits without extraction architecture. Evidence of contingency: institutional choices that could be otherwise.',
    'If naturalizing view is dominant: false summit detection should trigger; mountain classification is misleading. If institutional contingency is evident: snare/tangled_rope are appropriate; reform pathways exist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naturalizing_power_asymmetry, conceptual, 'Whether occupation is immutable power law or contingent institutional arrangement').

omega_variable(
    permit_system_sufficiency,
    'Do permit and mobility restriction systems constitute the primary extraction mechanism or are they secondary to land appropriation and resource control?',
    'Accounting analysis: (a) Economic value extracted through permit denial/delay vs value extracted through land seizure and resource allocation asymmetry. (b) Historical sequence: did permit systems emerge before or after land appropriation became the primary extraction mechanism? (c) Structural interdependence: do permit systems protect land seizure or operate independently as extraction mechanisms?',
    'If permits are primary: focus reform on administrative systems (legal/procedural). If land seizure is primary: focus reform on foundational property rights framework. If tightly coupled: both must be addressed simultaneously.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permit_system_sufficiency, empirical, 'Whether permit systems or land appropriation is primary extraction mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_occupation_1967, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_occupation_1967, theater_ratio, 0, 0.48).
narrative_ontology:measurement(terr_tr_t15, territorial_occupation_1967, theater_ratio, 15, 0.58).
narrative_ontology:measurement(terr_tr_t30, territorial_occupation_1967, theater_ratio, 30, 0.64).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_occupation_1967, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(terr_be_t15, territorial_occupation_1967, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(terr_be_t30, territorial_occupation_1967, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_occupation_1967, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_occupation_1967, 0.25).
narrative_ontology:affects_constraint(territorial_occupation_1967, foundational_legitimacy_claim_1948).
narrative_ontology:affects_constraint(territorial_occupation_1967, settlement_expansion_mechanism).
narrative_ontology:affects_constraint(territorial_occupation_1967, palestinian_state_viability).

% DUAL FORMULATION NOTE:
% Territorial occupation (this story, ε=0.68, snare) is downstream of foundational legitimacy claims (sibling story, ε varies by reading) but operates as independent extraction mechanism. Settlement expansion operates as secondary mechanism amplifying occupation extractiveness. Palestinian state viability is structurally dependent on occupation resolution but represents distinct constraint family. The occupation story should decompose into: (1) foundational_legitimacy_claim_1948 (kernel reading story, addresses existential legitimacy), (2) territorial_occupation_structure_1967 (this story, addresses enforcement and extraction), (3) settlement_expansion_mechanism (sibling story, addresses beneficiary coalitions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_occupation_1967, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
