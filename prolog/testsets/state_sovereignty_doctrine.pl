% ============================================================================
% CONSTRAINT STORY: state_sovereignty_doctrine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_sovereignty_doctrine, []).

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
 *   constraint_id: state_sovereignty_doctrine
 *   human_readable: State Sovereignty Doctrine
 *   domain: political/international_relations
 *
 * SUMMARY:
 *   The doctrine of state sovereignty asserts that territorial governments
 *   possess absolute authority within their borders and are not subject to
 *   external intervention or higher law. Formally established in the Treaty
 *   of Westphalia (1648) and institutionalized in the UN Charter (1945), the
 *   doctrine has become the foundational principle of international
 *   relations. However, the constraint exhibits structurally distinct
 *   functions from different positions: it serves as a genuine coordination
 *   mechanism for collective security and public goods provision (genuine
 *   rope function), while simultaneously enabling extraction through
 *   taxation, conscription, and suppression of exit (snare function). The
 *   doctrine naturalizes what is a contingent institutional arrangement as
 *   universal principle (false mountain), maintains ritualistic theater
 *   around sovereignty even as actual power flows through hegemonic and
 *   alliance structures (piton), and is gradually being transcended by
 *   supranational coordination mechanisms that preserve state capacity while
 *   reducing absolute sovereignty (scaffold). The doctrine is particularly
 *   extractive for stateless populations, indigenous nations, and weak
 *   states; it is substantially beneficial for territorial governments and
 *   hegemonic powers. The theater ratio has increased over 300 years as
 *   sovereignty discourse has proliferated in international forums (treaties,
 *   diplomatic protocols, legal opinions) while the actual functional basis
 *   of the doctrine has eroded — real authority increasingly flows through
 *   military alliances, economic coercion, and technical standards rather
 *   than through the formal principle of non-interference.
 *
 * KEY AGENTS:
 *   - Stateless/Trapped Populations: Primary victim (powerless/trapped) — cannot exit state territory; bear maximum suppression through border control, taxation, conscription, surveillance
 *   - Population with Exit Options: Secondary victim (moderate/constrained) — can emigrate at high cost; benefits from state coordination while bearing extraction
 *   - Territorial Government: Primary beneficiary (institutional/arbitrage) — doctrine is operational framework enabling collective action and monopoly on coercive force
 *   - Regional Integration Movement: Organized agents (organized/constrained) — supranational frameworks gradually replacing absolute sovereignty with distributed authority; explicit sunset logic
 *   - International Law System: Institutional actor (institutional/arbitrage) — maintains theatrical enforcement of sovereignty while actual coordination flows through power asymmetry
 *   - Hegemonic State: Powerful beneficiary (powerful/arbitrage) — extracts asymmetric benefits from sovereignty doctrine (permanent UN veto, NATO authority) while maintaining non-interference rules for military supremacy
 *   - Indigenous Nations: Primary victim with identity lock (powerless/identity_locked) — territory is constitutive of identity; cannot exit without abandoning cultural identity despite structural mobility
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangement as universal principle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_sovereignty_doctrine, 0.58).
domain_priors:suppression_score(state_sovereignty_doctrine, 0.65).
domain_priors:theater_ratio(state_sovereignty_doctrine, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_sovereignty_doctrine, extractiveness, 0.58).
narrative_ontology:constraint_metric(state_sovereignty_doctrine, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(state_sovereignty_doctrine, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_sovereignty_doctrine, tangled_rope).
narrative_ontology:human_readable(state_sovereignty_doctrine, "State Sovereignty Doctrine").
narrative_ontology:topic_domain(state_sovereignty_doctrine, "political/international_relations").

domain_priors:requires_active_enforcement(state_sovereignty_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_sovereignty_doctrine, territorial_governments).
narrative_ontology:constraint_beneficiary(state_sovereignty_doctrine, established_state_apparatus).
narrative_ontology:constraint_victim(state_sovereignty_doctrine, sub_state_populations).
narrative_ontology:constraint_victim(state_sovereignty_doctrine, non_state_actors).
narrative_ontology:constraint_victim(state_sovereignty_doctrine, international_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STATELESS/MINORITY POPULATIONS (SNARE) — Cannot exit the jurisdiction without state permission; doctrine of sovereignty creates absolute territorial authority with no escape mechanism. Suppression is maximal: physical borders, passport control, asset seizure. No coordination benefit — the doctrine exists to enable extraction of tax, conscription, and compliance from those who cannot leave. Maximum experienced extraction.
constraint_indexing:constraint_classification(state_sovereignty_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: POPULATION WITH CONSTRAINED EXIT (TANGLED ROPE) — Can emigrate at significant cost (relocation, credential transfer, family separation, financial loss). Benefits from state-provided infrastructure, legal protections, and collective security coordination. Also bears extraction: taxation, conscription, surveillance, restrictions on movement and association. Both genuine coordination (public goods) and asymmetric extraction coexist.
constraint_indexing:constraint_classification(state_sovereignty_doctrine, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TERRITORIAL GOVERNMENT (ROPE) — Experiences sovereignty doctrine as pure coordination: the doctrine is the legal mechanism that enables the state to organize collective action (national defense, infrastructure, law enforcement). Exit is costless for the state apparatus itself — it can revoke sovereignty claims and re-delegate authority. The doctrine creates no extraction from the state's perspective; it is the state's operational framework.
constraint_indexing:constraint_classification(state_sovereignty_doctrine, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGIONAL INTEGRATION MOVEMENT (SCAFFOLD) — Supranational frameworks (EU, ASEAN, African Union) and free trade agreements gradually reduce the functional meaning of absolute sovereignty through coordination mechanisms that preserve state capacity while distributing authority. Low theater (explicit institutional design) and explicit sunset logic: as regional integration deepens, absolute state sovereignty becomes obsolete and is replaced by distributed authority. Constrained exit because states can withdraw (Brexit precedent) but at significant political/economic cost.
constraint_indexing:constraint_classification(state_sovereignty_doctrine, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: INTERNATIONAL LAW SYSTEM (PITON) — The doctrine persists through institutional inertia and theater despite degraded functional purpose. International law nominally enforces sovereignty (UN Charter Article 2.1: 'respect for the principle of sovereign equality') but is actually powerless to prevent major violations by powerful states. The theatrical maintenance of the doctrine (formal treaties, diplomatic protocols, legal opinions affirming sovereignty) masks that the real coordination mechanism is power asymmetry and NATO alliance structure, not the doctrine itself. Theater ratio is high: sovereignty discourse fills international forums while actual authority flows through military and economic power.
constraint_indexing:constraint_classification(state_sovereignty_doctrine, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: HEGEMONIC STATE (TANGLED ROPE) — Powerful states benefit enormously from sovereignty doctrine as a coordination mechanism: it creates non-interference rules that protect military supremacy and allow unilateral action under the doctrine. Simultaneously, hegemonic states extract from weaker states by using sovereignty doctrine to prevent international intervention while maintaining the right to intervene (through the 'permanent five' veto in UN Security Council, through NATO authority, through economic sovereignty conditioning). Both genuine coordination (states coordinate on non-interference rules) and asymmetric extraction (powerful states can override the rules; weak states cannot) are present. The hegemonic state's effective extraction is mediated by power asymmetry.
constraint_indexing:constraint_classification(state_sovereignty_doctrine, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: INDIGENOUS NATION (SNARE WITH IDENTITY_LOCKED EXIT) — Structurally mobile (could relocate, could declare secession) but identity-locked: territorial sovereignty is constitutive of indigenous identity. The doctrine denies indigenous nations recognition as sovereign actors while simultaneously trapping them within state territories (via the sovereignty doctrine that prevents secession). Exit is unthinkable because it would require abandoning territorial claim and therefore cultural identity itself. High suppression (military enforcement, legal prohibition on secession, border control) combined with identity lock creates maximum experienced extraction. The binding mechanism is cognitive (identity fusion with territory) rather than purely material.
constraint_indexing:constraint_classification(state_sovereignty_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some form of bounded territorial authority is inherent to large-scale collective action: groups must have some mechanism to coordinate internally and differentiate insiders from outsiders. The specific doctrine of absolute state sovereignty naturalizes what might be inherent to collective organization itself. However, the structural data (suppression 0.65, active enforcement required, asymmetric benefits) contradicts the mountain classification. The engine will flag this as a false summit: the doctrine naturalizes what is contingent institutional arrangement (Westphalian order, nation-state system) rather than universal principle of social organization.
constraint_indexing:constraint_classification(state_sovereignty_doctrine, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_sovereignty_doctrine_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(state_sovereignty_doctrine, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(state_sovereignty_doctrine, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_sovereignty_doctrine, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(state_sovereignty_doctrine, TR),
    TR >= 0.70.

:- end_tests(state_sovereignty_doctrine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The doctrine creates genuine coordination for collective security and public goods provision (reducing extractiveness from ~0.75) but the asymmetric benefits to governments and hegemonic powers, combined with suppression of exit, justify the moderate-high score. The trajectory shows increasing extractiveness over 300 years as (a) state capacity has grown (conscription, taxation, surveillance), and (b) the coordination benefits have become increasingly offset by their distribution — public goods provision does not increase proportionally while extraction mechanisms expand. Suppression (0.65): High. Multiple enforcement mechanisms: military enforcement of borders, legal prohibition on secession, passports and border control, taxation enforcement, conscription, restrictions on political organization. However, suppression is not total — states provide legitimate public goods and substantial populations voluntarily participate in state structures. Theater ratio (0.68): Moderately high. The doctrine maintains substantial theatrical element: sovereignty is invoked rituistically in international forums (UN General Assembly, ICJ opinions, diplomatic protocols) while actual authority increasingly flows through military/economic power asymmetry rather than through the formal principle of non-interference. Regional integration frameworks (EU, free trade agreements) undermine the doctrine's functional meaning while maintaining its rhetorical force.
 *
 * PERSPECTIVAL GAP:
 *   The gap between stateless/trapped (Snare) and territorial government (Rope) perspectives is maximal. Both are describing the same constraint, but from opposite structural positions. The difference is not in how the constraint functions but in who bears the cost and who receives the benefit. The doctrine is genuinely a coordination mechanism for collective action (legitimate rope function) — but the coordination benefits are radically asymmetrically distributed, and the mechanism for distributing them is suppression of exit for the least-powerful agents. This is the signature of Tangled Rope viewed from different power levels: rope for the powerful, snare for the powerless.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows the pipeline: beneficiary status + exit options + power level → d value → f(d) → χ multiplier. Beneficiaries with arbitrage exit (territorial governments) derive d ≈ 0.05, producing f(d) ≈ -0.12 (negative χ: they experience the constraint as a subsidy, not a cost). Victims with trapped exit (stateless populations) derive d ≈ 0.95, producing f(d) ≈ 1.42 (maximum χ: they experience maximum extraction). The identity-locked indigenous nation derives d from victim status + structurally mobile exit but identity-locked constraint, producing d ≈ 0.88 (nearly maximal, because the identity lock prevents exercising structural mobility). Hegemonic states with arbitrage exit but victim-status nominal constraints derive d ≈ 0.48, producing f(d) ≈ 0.60, but their actual experience is substantially reduced by power asymmetry that allows selective rule violation. No directionality overrides needed — the structural data maps cleanly to the derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The state sovereignty doctrine resolves mandatrophy through perspectival plurality: it is simultaneously Rope (genuine coordination function for collective security), Snare (pure extraction for trapped populations), Tangled Rope (mixed coordination and extraction for moderate agents), Scaffold (being replaced by regional integration with sunset logic), and Piton (maintained through institutional inertia and theater). The analytical false summit (Mountain classification) is revealing: the doctrine naturalizes what is a contingent Westphalian institutional arrangement as an inherent principle of human organization. The question 'is state sovereignty an inherent requirement for large-scale coordination?' is exactly the mandatrophy tension. If it is inherent, the Mountain classification is correct and other types are perspectival projections of the same underlying natural law. If it is contingent, the Mountain is a false summit and the Tangled Rope / Snare / Scaffold classifications are the true structure. The omega variables establish empirical tests: comparative analysis of non-state authority viability, longitudinal measurement of coordination quality vs extraction asymmetry, and historical case studies of alternative authority structures. Until these are resolved, all eight perspectives are provisionally valid — the mandatrophy stands as an unresolved tension between the naturalizing logic and the structural evidence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_domination_boundary,
    'Is the state sovereignty doctrine primarily a coordination mechanism for collective action or a domination mechanism that naturalizes hierarchy?',
    'Comparative analysis of state capacity in low-extraction contexts (Nordic social democracies) vs high-extraction contexts (authoritarian regimes, colonial systems). Measurement of public goods provision relative to extraction levels. Longitudinal tracking of taxation, conscription, and surveillance burden against measurable coordination benefits (GDP growth, life expectancy, education access).',
    'If coordination-dominant: reclassify as Rope from most perspectives, reduce suppression score to ~0.35. If domination-dominant: reclassify as Snare from most perspectives, increase suppression score to ~0.80.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_domination_boundary, empirical, 'Whether sovereignty doctrine primarily coordinates or dominates').

omega_variable(
    non_state_authority_viability,
    'Can large-scale human coordination occur without state sovereignty doctrine, through networked non-state authority (local governance, corporate law, technical standards, reputation systems)?',
    'Historical case studies: medieval city-states, maritime merchant networks, current autonomous zones and network governance experiments (cypherpunk communities, decentralized autonomous organizations, Free Software projects at scale). Measurement of coordination quality and stability without central enforcement.',
    'If viable: sovereignty doctrine is contingent institutional choice (Tangled Rope or Scaffold from most perspectives); not inherent to large-scale coordination. If not viable: doctrine may be closer to Mountain (inherent structural requirement). This is the core mandatrophy tension.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(non_state_authority_viability, conceptual, 'Whether non-state authority can achieve comparable coordination').

omega_variable(
    identity_lock_vs_structural_mobility,
    'For indigenous nations claiming sovereignty: is the barrier to exit structural (military enforcement, legal prohibition) or cognitive (identity fusion with territory)? Can decoupling be achieved?',
    'Ethnographic analysis of indigenous sovereignty movements; documented cases of indigenous communities relocating while maintaining cultural identity; measurement of actual exit costs vs cognitive barriers; analysis of repatriation frameworks that separate identity from current territory.',
    'If primarily structural: reclassify indigenous perspective as trapped (not identity_locked); suppression remains high but binding mechanism is external. If primarily cognitive: identity_lock classification confirmed; oracle gap applies (analytical position cannot see what identity frame prevents). Either way, the constraint is extractive from indigenous perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_mobility, empirical, 'Structural vs cognitive barriers to indigenous exit from sovereignty doctrine').

omega_variable(
    hegemonic_power_sustainability,
    'Does the sovereignty doctrine actually stabilize hegemonic state power, or does it constrain hegemonic states and eventually enable coalition against them?',
    'Long-term analysis of hegemonic state stability under sovereignty doctrine vs under explicit imperial rule; measurement of coalition formation against hegemons; historical comparison (Pax Britannica, Pax Americana, potential post-American order) for regime stability and extraction sustainability.',
    'If stabilizing: sovereignty doctrine is core hegemonic tool (Tangled Rope from hegemonic perspective confirmed). If constraining: hegemonic states have incentive to violate doctrine and reassert imperial authority (Piton or degraded constraint classification). This determines whether hegemonic states are genuine beneficiaries or nominal beneficiaries concealing regime instability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hegemonic_power_sustainability, empirical, 'Whether sovereignty doctrine stabilizes or constrains hegemonic power').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_sovereignty_doctrine, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_sovereignty_doctrine, theater_ratio, 0, 0.55).
narrative_ontology:measurement(stat_tr_t150, state_sovereignty_doctrine, theater_ratio, 150, 0.62).
narrative_ontology:measurement(stat_tr_t300, state_sovereignty_doctrine, theater_ratio, 300, 0.68).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_sovereignty_doctrine, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(stat_be_t150, state_sovereignty_doctrine, base_extractiveness, 150, 0.52).
narrative_ontology:measurement(stat_be_t300, state_sovereignty_doctrine, base_extractiveness, 300, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_sovereignty_doctrine, enforcement_mechanism).
narrative_ontology:affects_constraint(state_sovereignty_doctrine, international_law_enforcement).
narrative_ontology:affects_constraint(state_sovereignty_doctrine, national_security_state).
narrative_ontology:affects_constraint(state_sovereignty_doctrine, territorial_dispute_escalation).
narrative_ontology:affects_constraint(state_sovereignty_doctrine, refugee_asylum_system).

% DUAL FORMULATION NOTE:
% State sovereignty doctrine is a constraint family with multiple decomposable structures. The coordination function (collective security, public goods provision) is distinct from the extraction function (monopoly on coercive force, suppression of exit). Constraint family members: sovereignty_as_coordination (ε ≈ 0.15, Rope), sovereignty_as_domination (ε ≈ 0.75, Snare), sovereignty_as_theater (ε ≈ 0.42, Piton). This story aggregates all three with ε ≈ 0.58 (Tangled Rope) to capture the hybrid structure from the primary beneficiary perspective. Downstream constraints inherit the sovereignty doctrine's structure and extractiveness profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_sovereignty_doctrine, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
