% ============================================================================
% CONSTRAINT STORY: israeli_settlement_expansion_legality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_israeli_settlement_expansion_legality, []).

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
 *   constraint_id: israeli_settlement_expansion_legality
 *   human_readable: Israeli Settlement Expansion Legality Frame
 *   domain: international_law/geopolitics/territorial_conflict
 *
 * SUMMARY:
 *   The Israeli settlement expansion legality constraint operates across
 *   multiple jurisdictions, legal frameworks, and power asymmetries, creating
 *   a diagnostic case of how the same structural phenomenon can be classified
 *   as six distinct constraint types from different observer positions. The
 *   constraint's core structure: Israeli law permits and facilitates
 *   settlement expansion in occupied Palestinian territory; international law
 *   (UN Resolutions 242, 2334; Geneva Convention IV; ICJ Advisories)
 *   prohibits it; enforcement asymmetry permits expansion despite
 *   prohibition. The extractiveness has increased monotonically from 0.35
 *   (1993, Oslo Accords framework, genuine legal ambiguity) to 0.62 (2024,
 *   consistent expansion despite continuous international prohibition).
 *   Theater ratio has similarly increased from 0.42 (early ambiguity period)
 *   to 0.65 (current performative UN resolutions + continued expansion). The
 *   constraint exhibits all three hallmarks of Tangled Rope: genuine
 *   coordination function (Israeli settlement provides territorial control
 *   and security depth, Palestinian Authority coordination manages population
 *   under occupation), significant asymmetric extraction (Palestinians
 *   displaced, two-state option eroded), and active enforcement (Israeli law,
 *   military security apparatus, judicial validation). The false natural law
 *   perspective (mountain) risks naturalizing what is a reversible
 *   institutional choice — the persistence of settlements reflects specific
 *   legal decisions and enforcement choices, not inevitability.
 *
 * KEY AGENTS:
 *   - Palestinian Displaced Populations: Primary victims (powerless/trapped) — structurally locked in occupation framework with no legal exit mechanism; land confiscation unidirectional
 *   - Palestinian Authority: Secondary actor (moderate/constrained) — coordinates population governance while constrained by Israeli security dependency and territorial erosion; experiences mixed coordination and extraction
 *   - Israeli Settlement Movement: Primary beneficiary (institutional/arbitrage) — experiences settlement framework as coordination mechanism enabling territorial consolidation; has arbitrage options (acceleration, retroactive legalization, boundary redefinition)
 *   - Israeli Government Executive: Co-beneficiary (institutional/mobile) — benefits from settlement expansion but also constrained by international pressure and domestic coalition dynamics; mobile but politically interdependent
 *   - International Legal Institutions (ICC, ICJ, UN): Organized mediators (organized/constrained) — coordinate international legal norms while constrained by state non-compliance and enforcement asymmetry; high theater ratio (resolutions issued, opinions stated, material non-enforcement)
 *   - Regional Powers (US, EU, Arab states): Stakeholders (powerful/mobile) — experience both coordination benefits (regional stability engagement) and extraction mechanisms (differential legal enforcement based on strategic interest); highest exit flexibility
 *   - Analytical Observer: Meta-perspective (analytical/analytical) — risks naturalizing institutional choices as inevitable features of territorial disputes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(israeli_settlement_expansion_legality, 0.62).
domain_priors:suppression_score(israeli_settlement_expansion_legality, 0.68).
domain_priors:theater_ratio(israeli_settlement_expansion_legality, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(israeli_settlement_expansion_legality, extractiveness, 0.62).
narrative_ontology:constraint_metric(israeli_settlement_expansion_legality, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(israeli_settlement_expansion_legality, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(israeli_settlement_expansion_legality, tangled_rope).
narrative_ontology:human_readable(israeli_settlement_expansion_legality, "Israeli Settlement Expansion Legality Frame").
narrative_ontology:topic_domain(israeli_settlement_expansion_legality, "international_law/geopolitics/territorial_conflict").

domain_priors:requires_active_enforcement(israeli_settlement_expansion_legality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(israeli_settlement_expansion_legality, israeli_settlement_movement).
narrative_ontology:constraint_beneficiary(israeli_settlement_expansion_legality, israeli_government_executive).
narrative_ontology:constraint_victim(israeli_settlement_expansion_legality, palestinian_displaced_populations).
narrative_ontology:constraint_victim(israeli_settlement_expansion_legality, international_legal_framework).
narrative_ontology:constraint_victim(israeli_settlement_expansion_legality, two_state_solution_viability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN DISPLACED POPULATIONS (SNARE) — Trapped within occupation framework with no legal exit mechanism. Extraction flows unidirectionally: land confiscation, settlement displacement, restricted movement. No coordination benefit; suppression is structural (military governance, property law asymmetry, geographic confinement). Maximum experience of pure extraction.
constraint_indexing:constraint_classification(israeli_settlement_expansion_legality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PALESTINIAN AUTHORITY & CIVIL SOCIETY (TANGLED ROPE) — Constrained by dependency on Israeli security cooperation, trade relationships, and humanitarian access, yet simultaneously must coordinate with Palestinian constituencies and international backers. Genuine coordination function (governing population, providing services) exists alongside asymmetric extraction (settlement encroachment reducing governing territory, resource asymmetry). Exit requires breaking with major regional powers; costs are high but not infinite.
constraint_indexing:constraint_classification(israeli_settlement_expansion_legality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ISRAELI SETTLEMENT MOVEMENT & GOVERNMENT (ROPE) — Experiences the constraint as coordination: settling territory requires legal frameworks, resource allocation, and security provision. Net beneficiary with arbitrage options (can accelerate settlements, offer retroactive legalization, redefine municipal boundaries). Extraction flows toward this agent, not away. Suppression of alternative framings (Israeli High Court challenges, international law citations) functions as coordination maintenance.
constraint_indexing:constraint_classification(israeli_settlement_expansion_legality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL LEGAL INSTITUTIONS (TANGLED ROPE) — Organized actors (ICC, ICJ, UN bodies) see genuine coordination function (establishing universal legal norms) alongside extraction mechanism (enforcement asymmetry: powerful states evade, weak states face prosecution; legitimacy theater in statements while realpolitik dominates outcomes). Constrained by dependence on state compliance and inability to compel powerful actors. High theater ratio reflects performative resolutions.
constraint_indexing:constraint_classification(israeli_settlement_expansion_legality, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL LEGAL FRAMEWORK AS FORMAL STRUCTURE (PITON) — UN Resolutions 242, 2334; ICJ Advisories; Geneva Conventions IV — the formal legal prohibitions persist through institutional inertia despite systematic non-enforcement and circumvention. The framework persists because dismantling it would be more costly than maintaining the performative structure. Theater ratio high: resolutions passed, advisory opinions issued, statements reaffirmed, while material practice contradicts the formal rule. Primary function (preventing territorial conquest) has atrophied; maintenance function (legitimacy theater) persists.
constraint_indexing:constraint_classification(israeli_settlement_expansion_legality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGIONAL POWERS & EXTERNAL STAKEHOLDERS (TANGLED ROPE) — US, EU, regional powers experience genuine coordination benefits (regional stability, trade relationships, security partnerships) alongside asymmetric extraction mechanisms (ability to leverage settlements as diplomatic pressure, differential enforcement of legal standards based on strategic interest). Mobile exit options but constrained by geopolitical interdependencies. Moderate experienced extraction.
constraint_indexing:constraint_classification(israeli_settlement_expansion_legality, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZING VIEW (MOUNTAIN) — Risk of naturalizing the constraint as immutable: 'territorial disputes are permanent features of international politics,' 'settlements are inevitable in occupation,' 'legal frameworks are powerless to prevent state action.' The analytical perspective risks collapsing contingent institutional arrangements (specific treaty violations, enforcement choices, political decisions) into laws of nature. The engine's false summit detector will flag this classification as naturalization of reversible institutional choices.
constraint_indexing:constraint_classification(israeli_settlement_expansion_legality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israeli_settlement_expansion_legality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(israeli_settlement_expansion_legality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(israeli_settlement_expansion_legality, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(israeli_settlement_expansion_legality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(israeli_settlement_expansion_legality, TR),
    TR >= 0.70.

:- end_tests(israeli_settlement_expansion_legality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high, increasing over interval. At t=0 (1993), genuine legal ambiguity existed — international law was contested, Israeli domestic law was the operative framework, and Palestinian state option seemed viable. Extraction at 0.35 reflected real coordination (mutual recognition, Oslo framework). By t=30 (2024), unidirectional extraction dominates: settlement expansion has consumed 60%+ of West Bank territory; two-state option is near-irreversible; international legal prohibition is explicit and continuous. The extractiveness trajectory reflects accumulation of extraction mechanisms despite constant international legal resistance. Suppression (0.68): High. Structural suppression mechanisms include military governance of occupied territory, asymmetric property law (Israeli settlers' property rights enforced, Palestinian property rights contested), restricted Palestinian movement and resource access (water, radio spectrum, airspace), political pressure on international enforcement bodies. Suppression is not total — Palestinian civil society exists, international advocacy functions, Israeli courts hear some challenges — but barriers are substantial and institutionalized. Theater ratio (0.65): Moderate-high. Theater manifests in: UN Resolutions passed unanimously while material non-enforcement continues; ICJ Advisories issued while implementing power withheld; Israeli High Court rulings ordering settlement demolitions that are rarely enforced; international conferences producing agreements not implemented. The constraint's legitimacy theater (legal framework maintained, rules stated, procedures performed) obscures enforcement reality (rules not applied asymmetrically). Theater has increased over interval as the gap between legal prohibition and material practice widens.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces radically different classifications across the seven perspectives. The Palestinian powerless perspective sees pure extraction (Snare) — unidirectional dispossession with no coordination benefit and maximum suppression. The Palestinian Authority moderate perspective sees mixed coordination and extraction (Tangled Rope) — genuine governance coordination exists alongside settlement extraction. The Israeli institutional beneficiary perspective sees pure coordination (Rope) — the constraint solves the real problem of territorial settlement and security provision. International legal institutions see coordination failure with enforcement theater (Tangled Rope) — they coordinate legal norms while unable to enforce them symmetrically. The international legal framework as formal structure sees a degraded ritual (Piton) — the legal prohibitions persist through institutional inertia despite non-enforcement. Regional powers with mobile exit options see moderate mixed extraction (Tangled Rope) — they benefit from regional stability engagement but suffer from enforcement asymmetry in selective application of legal standards. The analytical naturalizing perspective risks the false summit (Mountain) — treating territorial settlement as inevitable rather than contingent on specific enforcement choices. These perspectival gaps are structurally real: agents genuinely experience the same constraint type differently depending on their power position and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from structural position: beneficiary status, victim status, and exit options. Israeli settlement beneficiaries with arbitrage options (can relocate settlements, offer retroactive legalization, change municipal boundaries) have low d (0.15-0.25), producing negative or low effective extraction (chi). Palestinian victims with trapped exit (no legal mechanism to prevent displacement, no military option, no alternative territory) have high d (0.90+), producing maximum experienced extraction. Palestinian Authority actors constrained by Israeli dependency have moderate d (0.65-0.75), producing moderate chi. International institutional actors with constrained enforcement capacity have d around 0.70, producing moderate chi despite their institutional power (constrained exit reduces effective power). The sigmoid f(d) function maps these directionality values to experienced extractiveness: beneficiaries experience the constraint as coordination (negative chi appears as benefit); victims experience it as extraction (high chi); moderate actors experience it as mixed. The constraint's spatial scope (regional → global through international law) scales chi upward for international perspectives (σ(S)=1.1 for continental/global scope).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint avoids the coordination/extraction mandatrophy through explicit declaration of both genuine coordination (Israeli settlement provides territorial control and security coordination) and genuine asymmetric extraction (Palestinian territory loss, displacement, two-state option erosion). The Tangled Rope classification is stable across perspectives except where structural position produces alternative legitimate readings (Snare from powerless perspective, Rope from beneficiary perspective, Piton from framework perspective). The perspectival gap is diagnostically meaningful: the gap itself reveals the enforcement asymmetry. If the constraint classified as uniform Rope from all perspectives, the theater would be hidden (false positive for pure coordination). If it classified as uniform Snare from all perspectives, the coordination mechanisms (Palestinian governance, territorial control) would be invisible (false positive for pure extraction). The Tangled Rope claim with perspectival variation (Snare, Rope, Piton as legitimate alternative readings) captures the structural reality: the constraint is genuinely hybrid with real coordination alongside real extraction, and the perspectival gap reveals which agents perceive which component. The theater ratio increase (0.42 → 0.65) indicates that performative components (legal resolutions, advisories) have grown relative to functional components (actual enforcement of prohibitions), but the functional coordination components (settlement provision, territorial control) persist. This mixed function explains why enforcement alone fails to resolve the constraint — dismantling Israeli settlements requires replacing their coordination function (security depth, territorial consolidation), not just reversing the extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legality_definition_ambiguity,
    'What legal framework defines ''legality'' for settlement expansion — Israeli domestic law, international law, or a third hybrid?',
    'Explicit choice of governing law; comparison of classifications under each framework separately. If different frameworks yield different ε values and classifications, decompose into separate constraint stories.',
    'If Israeli law alone: ε ≈ 0.10 (coordination function dominates). If international law alone: ε ≈ 0.75 (pure extraction with theater). If hybrid: ε ≈ 0.62 (tangled rope, as claimed). The ambiguity in ''legality'' permits talking past the structural question.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legality_definition_ambiguity, conceptual, 'Which legal framework defines settlement legality').

omega_variable(
    coordination_vs_enforcement_asymmetry,
    'Does the constraint coordinate genuine interests (Israeli security, Palestinian governance, regional stability) or is coordination language a cover for enforcement asymmetry?',
    'Counterfactual analysis: if all parties had equal enforcement power, would the same legal framework (Geneva IV, UN 242) be chosen? If no, the coordination function is spurious (cover language). If yes, coordination is genuine.',
    'If spurious: constraint should reclassify as Snare (pure extraction with legitimacy theater). If genuine: Tangled Rope classification is correct. Current 0.62 ε assumes mixed function; if coordination is spurious, ε should rise to 0.75+.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_enforcement_asymmetry, conceptual, 'Whether coordination function is genuine or cover for enforcement asymmetry').

omega_variable(
    enforcement_equilibrium_stability,
    'Is the current enforcement pattern (asymmetric: settlements continue despite legal prohibitions) an equilibrium that can persist indefinitely, or does accumulating illegality create structural instability that must eventually resolve?',
    'Long-term trajectory analysis: does continued settlement expansion past critical territorial thresholds trigger enforcement escalation, negotiated settlement, or further legalization circumvention? Monitoring of ICJ/ICC effectiveness growth or continued decline.',
    'If stable equilibrium: constraint may be reclassified as Piton (theater maintaining a stable non-enforcement). If unstable: constraint remains Tangled Rope with high mandatrophy risk — the mixed function (coordination + extraction) cannot hold long-term, forcing eventual collapse toward pure types.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_equilibrium_stability, empirical, 'Whether enforcement asymmetry represents stable equilibrium or accumulating instability').

omega_variable(
    identity_lock_mechanism,
    'For Israeli settlers and supporters of settlement expansion, is opposition to settlements identity-locked (constituted through ideological/religious identity fused with territorial claim) or merely constrained (high-cost political exit)?',
    'Comparative analysis: do Israeli opponents of settlements show identity-transcendence (acknowledge legitimacy of Palestinian claims, accept territorial compromise) or identity-constraint (acknowledge Palestinian claims but cannot politically afford to reverse course)? Post-agreement case studies.',
    'If identity_locked dominates: the settlement expansion constraint is intertwined with Israeli identity politics (use identity_coordination type); classification perspective from Israeli institutional power remains trapped. If constrained dominates: identity_locked exit option is misattributed; revise to constrained or arbitrage based on actual exit costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether settlement support is identity-locked or constrained').

omega_variable(
    two_state_collapse_causality,
    'Is the two-state solution''s decreasing viability a direct effect of settlement expansion extracting territory below viability thresholds, or a confounding effect of other political factors (failed negotiations, security deterioration, international disengagement)?',
    'Counterfactual historical modeling: what would Palestinian state viability look like if settlement expansion had halted in 1993, 2000, 2008? Comparison with other territorial disputes where similar settlement patterns were or were not followed by political collapse.',
    'If direct causality (settlements alone drive collapse): the constraint extraction flow is toward destroying Palestinian state option — extreme extraction asymmetry, supports reclassification toward pure Snare. If confounding: extraction may be real but not sole driver of two-state collapse; ε may be lower than 0.62.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(two_state_collapse_causality, empirical, 'Causal role of settlement expansion in two-state solution viability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(israeli_settlement_expansion_legality, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(isel_tr_t0, israeli_settlement_expansion_legality, theater_ratio, 0, 0.42).
narrative_ontology:measurement(isel_tr_t15, israeli_settlement_expansion_legality, theater_ratio, 15, 0.55).
narrative_ontology:measurement(isel_tr_t30, israeli_settlement_expansion_legality, theater_ratio, 30, 0.65).
narrative_ontology:measurement(isel_tr_t5, israeli_settlement_expansion_legality, theater_ratio, 5, 0.47).
narrative_ontology:measurement(isel_tr_t20, israeli_settlement_expansion_legality, theater_ratio, 20, 0.6).
narrative_ontology:measurement(isel_tr_t10, israeli_settlement_expansion_legality, theater_ratio, 10, 0.51).

% Extraction over time
narrative_ontology:measurement(isel_be_t0, israeli_settlement_expansion_legality, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(isel_be_t15, israeli_settlement_expansion_legality, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(isel_be_t30, israeli_settlement_expansion_legality, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(isel_be_t5, israeli_settlement_expansion_legality, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(isel_be_t20, israeli_settlement_expansion_legality, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(isel_be_t10, israeli_settlement_expansion_legality, base_extractiveness, 10, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(israeli_settlement_expansion_legality, resource_allocation).
narrative_ontology:affects_constraint(israeli_settlement_expansion_legality, palestinian_authority_legitimacy).
narrative_ontology:affects_constraint(israeli_settlement_expansion_legality, two_state_solution_viability).
narrative_ontology:affects_constraint(israeli_settlement_expansion_legality, international_law_enforcement_asymmetry).

% DUAL FORMULATION NOTE:
% Settlement expansion legality is upstream of Palestinian state viability (settlements consume territory below viability threshold). It is also coupled to international law enforcement asymmetry — the same enforcement gap that permits settlements undermines ICC/ICJ credibility globally. These are structurally distinct constraints but causally linked. The decomposition boundary: if the observable changes from 'settlement legality under Israeli law' to 'settlement legality under international law' or 'settlement impact on two-state viability,' create separate stories with different ε values. Current story uses 'legality under both frameworks simultaneously' which produces tangled_rope; pure Israeli domestic law would produce Rope; pure international law would produce Snare.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(israeli_settlement_expansion_legality, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
