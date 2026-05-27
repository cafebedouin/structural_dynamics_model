% ============================================================================
% CONSTRAINT STORY: sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereignty_primary, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sovereignty_primary
 *   human_readable: Territorial Sovereignty and Border Exclusion as Collective Self-Determination
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   This constraint models the 'sovereignty_primary' reading of the
 *   border_normative_status kernel: territorial boundaries are treated as
 *   legitimate instruments of collective self-determination, and states have
 *   foundational authority to exclude non-members from their territory and
 *   political community. Under this reading, the boundary is not a negotiable
 *   feature but a constitutive property of statehood. Excluded migrants,
 *   displaced persons, and non-citizen residents bear systematic extraction
 *   through legal prohibition and enforcement machinery. The citizen body and
 *   state apparatus experience the boundary as a coordination mechanism — a
 *   legitimate framework for collective provision and democratic voice. From
 *   the analytical perspective, the naturalization of territorial sovereignty
 *   risks obscuring the contingent institutional arrangements (Westphalian
 *   system, nationalist ideology, historical empire dissolution) that
 *   constructed this norm. The constraint exhibits increasing extractiveness
 *   over the 378-year interval (0.35 → 0.58) as migration pressure has
 *   intensified and border enforcement has become more sophisticated. Theater
 *   ratio has also risen (0.30 → 0.55), indicating that an increasing share
 *   of sovereignty rhetoric is performative — the legitimacy appeal to
 *   'self-determination' must do more work as actual enforcement becomes more
 *   coercive and the extraction becomes more visible. This is diagnostic of a
 *   piton-trajectory: the underlying coordination function (protecting
 *   citizen welfare, enabling collective provision) has been partially
 *   replaced by enforcement performance and legitimacy maintenance.
 *
 * KEY AGENTS:
 *   - Excluded Migrant: Primary victim (powerless/trapped) — faces absolute legal prohibition; bears maximum extraction with zero negotiating capacity
 *   - Displaced Person / Refugee: Secondary victim (moderate/constrained) — subject to exclusion but partially protected by humanitarian protocols; organized through international law
 *   - Mobile Capital / Skilled Migrant: Secondary beneficiary (powerful/mobile) — experiences border as coordination filter rather than extraction; arbitrage among jurisdictions
 *   - Citizen Body: Primary beneficiary (institutional/arbitrage) — benefits from coordinated exclusion; maintains labor market and welfare system bounds; can migrate if dissatisfied
 *   - State Institutional Apparatus: Mixed actor (institutional/constrained) — both coordinates collective self-determination and extracts through enforcement; constrained by international law and migration pressure
 *   - International Sovereignty Regime: Institutional actor (institutional/arbitrage) — maintains universal norm through treaty law and diplomatic recognition; sees own functional justification atrophying (piton perspective)
 *   - Analytical Observer: Universalizing perspective (analytical/analytical) — risks naturalizing contingent Westphalian arrangements as immutable features of human social organization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereignty_primary, 0.58).
domain_priors:suppression_score(sovereignty_primary, 0.72).
domain_priors:theater_ratio(sovereignty_primary, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereignty_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(sovereignty_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sovereignty_primary, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(sovereignty_primary, "Territorial Sovereignty and Border Exclusion as Collective Self-Determination").
narrative_ontology:topic_domain(sovereignty_primary, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereignty_primary, '97cfa93a-3de6-43f4-8e1b-4cf9d3c0eff3').
narrative_ontology:cs_created_at('97cfa93a-3de6-43f4-8e1b-4cf9d3c0eff3', '').
narrative_ontology:cs_kernel_codification('97cfa93a-3de6-43f4-8e1b-4cf9d3c0eff3', formalized).
narrative_ontology:cs_authority_grounding('97cfa93a-3de6-43f4-8e1b-4cf9d3c0eff3', lineage).
narrative_ontology:cs_interpretation_layer_present('97cfa93a-3de6-43f4-8e1b-4cf9d3c0eff3').
narrative_ontology:cs_kernel_id(sovereignty_primary, border_normative_status).
narrative_ontology:cs_reading_relation('97cfa93a-3de6-43f4-8e1b-4cf9d3c0eff3', freedom_primary, coexists_with).
narrative_ontology:cs_reading_relation('97cfa93a-3de6-43f4-8e1b-4cf9d3c0eff3', qualified_sovereignty, coexists_with).
narrative_ontology:cs_axiom('97cfa93a-3de6-43f4-8e1b-4cf9d3c0eff3', foundational, territorial_closure_prerequisite_for_self_determination).
narrative_ontology:cs_axiom_status(territorial_closure_prerequisite_for_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('97cfa93a-3de6-43f4-8e1b-4cf9d3c0eff3', territorial_closure_prerequisite_for_self_determination, deontological).
narrative_ontology:cs_axiom('97cfa93a-3de6-43f4-8e1b-4cf9d3c0eff3', foundational, state_authority_to_exclude_non_members).
narrative_ontology:cs_axiom_status(state_authority_to_exclude_non_members, holdable).
narrative_ontology:cs_axiom_grounding('97cfa93a-3de6-43f4-8e1b-4cf9d3c0eff3', state_authority_to_exclude_non_members, deontological).
narrative_ontology:cs_reference_frame('97cfa93a-3de6-43f4-8e1b-4cf9d3c0eff3', territorial_sovereignty_westphalian).
narrative_ontology:cs_drift_state('97cfa93a-3de6-43f4-8e1b-4cf9d3c0eff3', contemporary_migration_crisis, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereignty_primary, citizen_body).
narrative_ontology:constraint_beneficiary(sovereignty_primary, state_institutional_apparatus).
narrative_ontology:constraint_victim(sovereignty_primary, excluded_migrants).
narrative_ontology:constraint_victim(sovereignty_primary, displaced_persons).
narrative_ontology:constraint_victim(sovereignty_primary, non_citizen_residents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED MIGRANT (SNARE) — Faces absolute legal prohibition on entry and residence; bears the full extraction of territorial exclusion with zero negotiating capacity. No alternative pathways at this time horizon. Maximum experienced extraction — structurally immobile, legally powerless, no organizational exit mechanism.
constraint_indexing:constraint_classification(sovereignty_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DISPLACED PERSON (TANGLED ROPE) — Bears extraction through forced exclusion but also benefits from humanitarian protocols and refugee coordination that provide some access to neighboring territory and international support. Constrained by asylum caps and documentation barriers; organized through international norms (Geneva Convention). Mixed coordination (humanitarian protection apparatus) and asymmetric extraction (border enforcement).
constraint_indexing:constraint_classification(sovereignty_primary, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MOBILE CAPITAL / SKILLED MIGRANT (ROPE) — Experiences border exclusion as coordination mechanism rather than extraction: visa regimes, labor permits, and professional reciprocity create managed pathways for high-value migration. Extraction is minimal because exit options are broad and the migrant can arbitrage between jurisdictions. The constraint functions as a coordination filter identifying qualified participants.
constraint_indexing:constraint_classification(sovereignty_primary, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: CITIZEN BODY (ROPE) — Benefits from coordinated exclusion: maintains labor market control, welfare system bounds, and political community stability. Experiences the border as a coordination mechanism protecting collective goods (job availability, public services, political voice). Low experienced extraction because this agent is the beneficiary of the constraint. Arbitrage exit: citizens can migrate elsewhere if dissatisfied.
constraint_indexing:constraint_classification(sovereignty_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE INSTITUTIONAL APPARATUS (TANGLED ROPE) — Both coordinates legitimate collective self-determination (citizens have voice in membership) and extracts through border enforcement infrastructure. The state apparatus experiences the boundary as a genuine coordination function (enabling collective provision) but also as a mechanism of control and extraction (excluding competing claims, managing labor supply, protecting jurisdictional rent). Constrained by international law norms and migration pressure.
constraint_indexing:constraint_classification(sovereignty_primary, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL SOVEREIGNTY REGIME (PITON) — The universal norm of territorial sovereignty and border control persists through institutional inertia and legitimacy inheritance from the Westphalian system. The functional justification (protecting citizen welfare and enabling self-determination) has atrophied as global interdependence and migration pressure have intensified. The regime maintains itself through treaty law, diplomatic protocol, and mutual recognition agreements rather than through effective coordination. Theater_ratio is elevated because much of the regime's operation is performative legitimation rather than functional exclusion enforcement.
constraint_indexing:constraint_classification(sovereignty_primary, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some form of bounded community and membership distinction is inherent to human social organization: collective goods require membership boundaries, political voice requires defined constituencies, and resource allocation requires closure. This perspective sees territorial sovereignty as a natural consequence of collective life. However, the structural data contradicts the mountain classification — identifiable beneficiaries (citizens, state apparatus), active enforcement machinery, and contingent historical institutions indicate a constructed constraint. The engine will compute this as a false summit.
constraint_indexing:constraint_classification(sovereignty_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereignty_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sovereignty_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sovereignty_primary, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sovereignty_primary, TR),
    TR >= 0.70.

:- end_tests(sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint produces measurable asymmetric extraction: excluded migrants and displaced persons bear costs (prohibition on residence, exclusion from labor markets, denial of public services) while citizens and the state apparatus receive benefits (labor market control, welfare system bounds, jurisdictional rent). The value reflects that this is not maximal extraction (enforcement is incomplete, alternative pathways exist for some migrants, international humanitarian law provides partial protection) but that extraction is substantial and systematic. The trajectory from 0.35 (Westphalian emergence) to 0.58 (contemporary migration pressure) indicates accumulating extraction as enforcement capacity and incentive have both increased. Suppression (0.72): High. Multiple mechanisms suppress alternatives to sovereign exclusion: legal prohibition (passports, visas, asylum bars), enforcement machinery (border patrol, deportation), economic barriers (work authorization requirements), and ideological naturalization (treating sovereignty as inevitable). The suppression is not total — humanitarian corridors, temporary visas, and international protection mechanisms create small escape valves — but significant alternatives to exclusion are structurally unavailable to most potential migrants. Theater ratio (0.55): Moderate-high and rising. Contemporary sovereignty rhetoric (appeals to 'self-determination,' 'democratic community,' 'protecting citizens') performs legitimacy work that enforcement machinery alone cannot accomplish. Much border policy discourse is performative: campaigns emphasizing enforcement severity despite limited resources, citizenship ceremonies and national identity rhetoric maintaining community cohesion, multilateral treaties affirming sovereignty while undermining reciprocal protection. Theater is lower than in truly degraded institutions (piton-stage) because enforcement still has real coercive capacity, but theater has risen significantly from the early Westphalian period (when legitimacy was presumed) to contemporary period (when legitimacy must be actively maintained).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits dramatic perspectival variation because the sovereignty principle produces opposite structural positions for different agents. Excluded migrants experience absolute snare — legal bar and enforcement machinery with no exit. The citizen body experiences pure rope — the boundary coordinates legitimate collective provision and democracy. The state apparatus experiences tangled rope — genuine coordination function mixed with extraction leverage. The international regime experiences degraded piton — legitimacy maintained through inertia and treaty law as functional justification atrophies. The analytical observer risks mountain classification — naturalizing a contingent institutional arrangement as inevitable human organization. The perspectival gap is not due to disagreement about facts but to genuine structural differentiation: agents occupy different positions relative to the extraction flow. The beneficiary (citizen) and victim (excluded migrant) have incommensurable experiences of the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (citizen_body, state_institutional_apparatus) derive d values toward 0.0 (beneficiary, arbitrage exit) → f(d) negative → low or negative χ. They experience coordination, not extraction. Victims (excluded_migrants, displaced_persons, non_citizen_residents) derive d values toward 1.0 (trapped or constrained exit) → f(d) positive → high χ. They experience extraction. Mobile migrants (powerful/mobile) derive d values toward 0.5 (moderate relationship) → f(d) moderate → they experience negotiated access, not pure extraction. The directionality values are empirically grounded in the structural relationships: citizens can exit (emigrate); excluded migrants cannot. State apparatus is constrained by international law but has enforcement capacity; displaced persons have no enforcement capacity. The chi formula translates these structural differences into experienced extractiveness multipliers.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING CASE: The mandatrophy for this reading is resolved by recognizing that 'legitimate territorial boundary' is not a univocal claim but a contested reading of a shared kernel. The false-summit risk is acute: the analytical observer perspective naturally tends toward mountain classification (territorial organization is inherent to human social life), which naturalizes the sovereignty_primary reading as inevitable. The constraint story shows this as a false summit through two routes: (1) structural: identifiable beneficiaries (citizen body, state apparatus) and victims (excluded migrants) indicate a constructed constraint, not a natural law; (2) temporal: the rising extractiveness (0.35 → 0.58) and theater ratio (0.30 → 0.55) track institutional development and legitimacy maintenance rather than discovering an eternal property. The mandatrophy is resolved by showing that the same kernel (territorial boundaries) admits multiple readings with different extraction profiles. Sibling readings would produce different ε values and beneficiary/victim sets, confirming that the classification depends on the reading adopted, not on invariant structural properties. The perspectival gap across the seven perspectives is diagnostically appropriate: it reflects that sovereignty_primary produces genuine structural differentiation, not mere disagreement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimate_self_determination_scope,
    'How is legitimate self-determination bounded? Can a majority of current citizens legitimately exclude all non-members indefinitely, or does self-determination include some obligation to consider non-citizen claims?',
    'Comparative analysis of justifications used by exclusionary and inclusionary states; assessment of whether self-determination is purely internal (citizens deciding among themselves) or includes duties to outsiders (dignity-based or capability-based frameworks)',
    'If self-determination is purely internal: snare classification is maximized for excluded migrants; state has absolute authority. If self-determination includes external duties: extraction is reduced; constraint becomes more clearly tangled rope across all perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_self_determination_scope, conceptual, 'Scope of legitimate self-determination: internal-only vs. including duties to non-members').

omega_variable(
    extraction_vs_coordination_boundary,
    'At what point does border enforcement transition from a legitimate coordination function (managing membership) to extraction (using exclusion as a rent-seeking mechanism)?',
    'Empirical: comparison of state welfare outcomes under closed vs. open border regimes; assessment of whether borders primarily protect public goods or transfer rents to favored groups. Conceptual: whether extraction intent (state exploiting non-citizens) vs. extraction effect (border produces systematic asymmetry) determines the classification.',
    'If coordination dominates: constraint should be primarily rope across more perspectives. If extraction dominates: snare classification expands; suppression values should increase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, empirical, 'Whether borders function primarily as coordination or extraction mechanism').

omega_variable(
    natural_vs_constructed_boundary,
    'Is territorial sovereignty with exclusion an inherent feature of human social organization (mountain), or a contingent institutional arrangement that has become naturalized (false summit)?',
    'Historical and anthropological: existence and duration of non-territorial social organization; comparison of past vs. present boundary rigidity. Logical: whether excluding non-members is logically required for any form of collective goods provision, or whether alternative mechanisms (fee-for-service, temporary residency, graduated membership) could provide coordination without absolute exclusion.',
    'If natural law: mountain classification is correct; exclusion is immutable. If constructed: false summit detection fires; constraint reclassifies based on chi formula; beneficiary presence + institutional enforcement indicate tangled rope / snare depending on perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_boundary, conceptual, 'Whether territorial sovereignty is natural law or constructed institutional arrangement').

omega_variable(
    sovereign_equality_vs_asymmetric_power,
    'Does the sovereignty primary reading presume equal bargaining power among states (symmetrical constraint), or does it accommodate vast asymmetries in state capacity and geopolitical power (asymmetrical extraction)?',
    'Empirical: analysis of border outcomes for weak vs. strong states; assessment of whether powerful states impose exclusion unilaterally (extraction) or whether weak states freely choose boundaries (coordination). Logical: whether the ''sovereignty'' principle requires mutual recognition (presuming symmetry) or can justify dominance hierarchies.',
    'If symmetrical: rope or balanced tangled rope across state-level perspectives. If asymmetrical: snare classification expands; powerful states experience rope while weak states experience tangled rope / snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereign_equality_vs_asymmetric_power, empirical, 'Whether sovereignty principle presumes equal or asymmetric state power').

omega_variable(
    historical_contingency_of_westphalia,
    'Is the Westphalian territorial sovereignty model the only institutional arrangement for collective self-determination, or one contingent solution among many?',
    'Historical: documentation of pre-Westphalian forms of collective organization (overlapping jurisdictions, city-states, imperial systems, ummah models). Comparative institutional: analysis of contemporary non-Westphalian experiments (EU supranational authority, ASEAN soft consensus, indigenous governance overlapping state boundaries)',
    'If Westphalian model is inevitable: mountain / piton classification is correct. If contingent: constraint is explicitly constructed; false summit detection is appropriate; alternative readings become materially possible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_contingency_of_westphalia, conceptual, 'Whether Westphalian sovereignty is inevitable or historically contingent').

omega_variable(
    committer_reading_underdetermination,
    'This constraint instantiates the ''sovereignty_primary'' reading of the border_normative_status kernel. What structural and empirical signals confirm that THIS reading (rather than ''freedom_primary'' or ''qualified_sovereignty'') is the appropriate analytical frame for the constraint at hand?',
    'Examination of the case context: which reading best explains observed state behavior, legal doctrine, and justificatory rhetoric? Assessment of whether the constraint exhibits properties of pure exclusion (sovereignty_primary), pure access rights (freedom_primary), or negotiated mixed regimes (qualified_sovereignty). Cross-reading validation: do the sibling readings produce structurally coherent alternative classifications for the same observables?',
    'If this reading is confirmed: sovereignty_primary constraint properly frames the classification. If an alternative reading better explains the observables: reauthoring as that reading may be more analytically appropriate. Perspectival gap analysis: do the five non-analytical perspectives align with sovereignty_primary framing, or do they suggest measurement under an alternative kernel reading?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_reading_underdetermination, conceptual, 'Confirmation that sovereignty_primary reading is appropriate vs. sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereignty_primary, 1648, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_westphalian_emergence_1648, sovereignty_primary, theater_ratio, 0, 0.3).
narrative_ontology:measurement(theater_nationalism_rise_1850, sovereignty_primary, theater_ratio, 1, 0.4).
narrative_ontology:measurement(theater_postcolonial_era_1960, sovereignty_primary, theater_ratio, 2, 0.48).
narrative_ontology:measurement(theater_modern_migration_pressure_2020, sovereignty_primary, theater_ratio, 3, 0.55).

% Extraction over time
narrative_ontology:measurement(extractiveness_westphalian_emergence_1648, sovereignty_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(extractiveness_nationalism_rise_1850, sovereignty_primary, base_extractiveness, 1, 0.45).
narrative_ontology:measurement(extractiveness_postcolonial_era_1960, sovereignty_primary, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(extractiveness_modern_migration_pressure_2020, sovereignty_primary, base_extractiveness, 3, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereignty_primary, identity_coordination).
narrative_ontology:boltzmann_floor_override(sovereignty_primary, 0.12).
narrative_ontology:affects_constraint(sovereignty_primary, freedom_primary).
narrative_ontology:affects_constraint(sovereignty_primary, qualified_sovereignty).
narrative_ontology:affects_constraint(sovereignty_primary, refugee_protection_regime).
narrative_ontology:affects_constraint(sovereignty_primary, labor_migration_governance).
narrative_ontology:affects_constraint(sovereignty_primary, citizenship_acquisition).

% DUAL FORMULATION NOTE:
% This constraint (sovereignty_primary) is the upstream reading in the border_normative_status kernel. Sibling readings (freedom_primary, qualified_sovereignty) represent alternative framings of the same underlying commitment structure. Each reading produces a distinct ε value and beneficiary/victim configuration. The readings are linked via reading_relations (coexists_with / influences) in cs_structure, not via affects_constraints. The network edges here point to dependent constraints (refugee_protection_regime, labor_migration_governance) that operate within the sovereignty-primary framework but could be respecified under alternative kernel readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sovereignty_primary, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
