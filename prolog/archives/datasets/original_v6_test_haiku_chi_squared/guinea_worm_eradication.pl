% ============================================================================
% CONSTRAINT STORY: guinea_worm_eradication
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_guinea_worm_eradication, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: guinea_worm_eradication
 *   human_readable: Global Guinea Worm Eradication Program
 *   domain: social/public_health/humanitarian
 *
 * SUMMARY:
 *   The Global Guinea Worm Eradication Program, led by The Carter Center
 *   since 1986, represents a successful international coordination mechanism
 *   for eliminating a parasitic disease through provision of safe drinking
 *   water, health education, and surveillance. The constraint is
 *   fundamentally coordinative rather than extractive: aligned incentives
 *   across endemic populations, international NGOs, national governments, and
 *   health institutions create a shared goal of disease elimination. The
 *   program exhibits minimal theater (functional outcomes dominate over
 *   performative reporting) and declining extractiveness as the disease nears
 *   eradication. The constraint's structure is a Rope from all major
 *   perspectives, with Scaffold properties emerging as the program approaches
 *   its sunset deadline. Endemic populations experience the constraint as
 *   access to safer water technology (mobile exit, low extraction). National
 *   governments experience it as harmonization of health governance
 *   (constrained exit, low extraction). International institutions experience
 *   it as temporary coordination infrastructure designed for transition to
 *   endemic-country ownership (sunset clause embedded in design). The
 *   analytical observer sees a pure coordination game with aligned incentives
 *   across all parties.
 *
 * KEY AGENTS:
 *   - Endemic community members (local level): Primary beneficiaries (powerless/mobile) — direct recipients of safe water access and health benefits
 *   - The Carter Center and partner NGOs: Primary coordinators (institutional/arbitrage) — institutional actors mobilizing resources and harmonizing protocols across borders
 *   - Endemic country governments: Institutional participants (organized/constrained) — sovereign actors with constrained exit due to health governance obligations
 *   - WHO and UNICEF: Coordination infrastructure providers (organized/constrained) — global health institutions building surveillance and water access systems with sunset logic
 *   - Analytical observer: Views constraint as global public health coordination mechanism (analytical/analytical) — sees aligned incentives and pure coordination game
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(guinea_worm_eradication, 0.12).
domain_priors:suppression_score(guinea_worm_eradication, 0.08).
domain_priors:theater_ratio(guinea_worm_eradication, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(guinea_worm_eradication, extractiveness, 0.12).
narrative_ontology:constraint_metric(guinea_worm_eradication, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(guinea_worm_eradication, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(guinea_worm_eradication, rope).
narrative_ontology:human_readable(guinea_worm_eradication, "Global Guinea Worm Eradication Program").
narrative_ontology:topic_domain(guinea_worm_eradication, "social/public_health/humanitarian").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(guinea_worm_eradication, endemic_populations).
narrative_ontology:constraint_beneficiary(guinea_worm_eradication, international_health_community).
narrative_ontology:constraint_beneficiary(guinea_worm_eradication, participating_governments).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENDEMIC COMMUNITY (ROPE) — Local villagers benefit directly from access to safe water sources and elimination of Guinea worm infection risk. Exit options are mobile: they can adopt provided well systems or water filters. Constraint is primarily coordinative (access to water technology). d≈0.25, f(d)≈0.10, σ=0.8 → χ≈0.01. Near-zero effective extraction; pure coordination benefit.
constraint_indexing:constraint_classification(guinea_worm_eradication, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: CARTER CENTER / INTERNATIONAL NGOS (ROPE) — Primary coordinating institution. Institutional power with arbitrage exit (can shift focus to other eradication programs). Benefits from demonstrated success in disease eradication, which increases donor confidence and institutional legitimacy. The constraint is a coordination mechanism: mobilizing resources, harmonizing health protocols across borders, training health workers. d≈0.10, f(d)≈-0.02, σ=1.2 → χ≈-0.00. Negligible extraction; net beneficiary through mission accomplishment.
constraint_indexing:constraint_classification(guinea_worm_eradication, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ENDEMIC COUNTRY GOVERNMENTS (ROPE) — Organized institutional actors with constrained exit (sovereignty requires health governance participation; international health obligations constrain exit). Benefit from disease elimination, improved population health metrics, and reduced healthcare burden. Experience the eradication program primarily as coordination mechanism: harmonizing with international standards, mobilizing domestic resources, enabling cross-border surveillance. d≈0.35, f(d)≈0.25, σ=1.0 → χ≈0.03. Very low effective extraction; genuine coordination game.
constraint_indexing:constraint_classification(guinea_worm_eradication, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: GLOBAL HEALTH INSTITUTIONS (SCAFFOLD) — Organized actors (WHO, UNICEF) experience the eradication program as temporary infrastructure with declining sunset logic. The constraint is a scaffolding mechanism for building local health capacity, disease surveillance systems, and water access infrastructure that will persist after eradication. theater_ratio=0.25 is low (functional, not performative). As eradication nears completion, the international coordination apparatus is structurally designed to transition to endemic-country ownership. d≈0.40, f(d)≈0.40, σ=1.1 → χ≈0.05. Low extraction; sunset mechanism embedded in design.
constraint_indexing:constraint_classification(guinea_worm_eradication, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — From civilizational/global perspective, the eradication program is a pure coordination mechanism: solving the collective action problem of disease elimination across borders through shared protocols, resource pooling, and aligned incentives. No significant extraction is observable. The constraint is the coordination infrastructure itself. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.08. Low extraction; symmetric coordination game.
constraint_indexing:constraint_classification(guinea_worm_eradication, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(guinea_worm_eradication_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(guinea_worm_eradication, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(guinea_worm_eradication, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(guinea_worm_eradication_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The program's primary mechanism is providing access to water technology and health information — genuine public goods with minimal value asymmetry. No single party extracts sustained value at others' expense. The beneficiaries (endemic populations) directly receive health improvements. The coordinators (Carter Center, NGOs) benefit from mission accomplishment and institutional legitimacy, but these are not extraction — they are legitimate coordination benefits. Governments benefit from improved health outcomes and reduced disease burden. The declining extractiveness trend (0.18→0.12→0.08) reflects increasing efficiency and technology diffusion, reducing coordination overhead. Suppression (0.08): Very low. Communities retain significant agency in adoption decisions. Water filter and well technologies are relatively accessible. Health education enables informed choice. No coercive barriers prevent participation (except poverty, which the program addresses directly). Theater ratio (0.25): Low. The program is substantially functional: reported cases correlate with actual eradication progress, surveillance data drives interventions, and water access improvements demonstrate concrete outcomes. Theater has decreased over time as monitoring becomes more rigorous and community-based verification strengthens.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is minimal and primarily temporal. All perspectives converge on Rope classification, which is unusual for a global social program. The slight variation (Scaffold for global institutions) reflects the sunset logic embedded in the program design — international coordination is structured as temporary support for building endemic-country capacity. This absence of major perspectival disagreement is diagnostic: when all structural positions (powerless beneficiary, institutional coordinator, national government, global institution, analytical observer) agree on a low-extraction, high-coordination classification, the constraint is likely a genuine public good coordination mechanism rather than a hybrid extraction-coordination system. The minor gap between Rope and Scaffold reflects different time horizons: local/biographical perspectives see Rope (immediate coordination); generational/institutional perspectives see Scaffold (temporary infrastructure with sunset).
 *
 * DIRECTIONALITY LOGIC:
 *   Endemic populations: Beneficiary + mobile → d≈0.25, f(d)≈0.10. Moderate beneficiary status with exit option; can adopt provided technologies. Carter Center / NGOs: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.02. Net beneficiary; arbitrage exit (can shift to other programs). Endemic governments: Beneficiary + constrained → d≈0.35, f(d)≈0.25. Constrained exit due to sovereignty and health governance obligations, but genuine coordination benefits. Global health institutions: Both + constrained → d≈0.40, f(d)≈0.40. Symmetric coordination game with constrained exit; benefit from capacity building alignment. Analytical observer: Symmetric → d≈0.50, f(d)≈0.65. Views constraint as equilibrium coordination mechanism with aligned incentives.
 *
 * MANDATROPHY ANALYSIS:
 *   PURE COORDINATION RESOLUTION: This constraint resolves potential mandatrophy by demonstrating that low extractiveness (0.12) across all perspectives is structural, not observational. The program achieves genuine coordination (Rope) by aligning incentives rather than creating asymmetric dependencies. No perspective misidentifies extraction as coordination or vice versa. All parties benefit from disease elimination; the constraint is the mechanism enabling collective achievement of a shared goal. The declining theater ratio and declining extractiveness trend confirm that the system is becoming more functionally pure over time, not accumulating hidden extraction. The Scaffold properties (sunset logic, declining oversight) reflect deliberate program design, not institutional degradation. This is a canonical example of a constraint where mandatrophy does not arise because no perspective has incentive to mislabel the coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endemic_country_political_will,
    'Will endemic countries maintain disease surveillance and water quality standards after international coordination support is withdrawn?',
    'Post-eradication monitoring of surveillance reporting rates, water access maintenance in remaining endemic regions, and government health budget allocation trends',
    'If YES: scaffold sunset succeeds, constraint transitions cleanly. If NO: residual disease rebounds, coordination failure, program reclassifies as snare of external dependency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endemic_country_political_will, empirical, 'Sustainability of endemic-country health infrastructure post-eradication').

omega_variable(
    funding_model_robustness,
    'Is the eradication program''s funding model dependent on the bounded goal (eradication deadline), or does it risk perpetuation as a low-extractiveness bureaucratic structure?',
    'Analysis of funding trends post-eradication declaration; tracking whether international health budgets maintain dedicated guinea worm surveillance or redirect to other priorities',
    'If deadline drives funding cessation: pure rope. If bureaucratic perpetuation occurs: constraint degrades to piton (degraded coordination persisting through inertia).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_model_robustness, empirical, 'Whether eradication goal terminus drives funding sunset or bureaucratic persistence').

omega_variable(
    behavioral_adoption_durability,
    'Do communities continue practicing water safety behaviors (boiling, filtering) after Guinea worm elimination removes the proximal threat?',
    'Longitudinal studies of water-use behaviors in declared eradication zones; correlation between health education content retention and disease elimination',
    'If behaviors persist: coordination achieved. If behaviors revert: constraint requires ongoing enforcement, reclassifying to snare or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_adoption_durability, empirical, 'Durability of behavioral change after disease elimination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(guinea_worm_eradication, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gworm_tr_t0, guinea_worm_eradication, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gworm_tr_t10, guinea_worm_eradication, theater_ratio, 10, 0.28).
narrative_ontology:measurement(gworm_tr_t20, guinea_worm_eradication, theater_ratio, 20, 0.22).

% Extraction over time
narrative_ontology:measurement(gworm_be_t0, guinea_worm_eradication, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(gworm_be_t10, guinea_worm_eradication, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(gworm_be_t20, guinea_worm_eradication, base_extractiveness, 20, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(guinea_worm_eradication, resource_allocation).
narrative_ontology:affects_constraint(guinea_worm_eradication, waterborne_disease_access_disparity).
narrative_ontology:affects_constraint(guinea_worm_eradication, international_health_institution_coordination).

% DUAL FORMULATION NOTE:
% The eradication program's success depends on upstream constraint (international health governance coordination) and downstream constraint (sustainable water access infrastructure). This constraint is the operational mechanism linking institutional coordination to endemic-population outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
