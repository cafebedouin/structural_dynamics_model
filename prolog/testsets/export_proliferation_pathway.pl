% ============================================================================
% CONSTRAINT STORY: export_proliferation_pathway
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_export_proliferation_pathway, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: export_proliferation_pathway
 *   human_readable: Export Proliferation Pathway for Predictive Surveillance Technology
 *   domain: technology_governance/surveillance_studies/export_control_policy
 *
 * SUMMARY:
 *   The export proliferation pathway describes how Geedge Corporation's
 *   existing commercial Great Firewall installations in authoritarian client
 *   states (Ethiopia, Kazakhstan, Myanmar, Pakistan) create infrastructure
 *   lock-in that enables future deployment of predictive surveillance
 *   technology. The constraint operates at two levels: the current-generation
 *   Tiangou Secure Gateway installations provide legitimate network security
 *   and content filtering functions while simultaneously establishing the
 *   technical foundation, institutional relationships, and procurement
 *   dependencies that will facilitate upgrade to next-generation predictive
 *   tools once developed. This pathway is structurally distinct from the
 *   predictive surveillance technology itself (modeled in
 *   predictive_surveillance_extractiveness) — it is the distribution
 *   mechanism, not the capability. The constraint exhibits tangled rope
 *   characteristics: genuine coordination benefits (standardized
 *   infrastructure, technology transfer, network security) coexist with
 *   asymmetric extraction (infrastructure lock-in that will amplify future
 *   surveillance capabilities). The pathway's extractiveness has increased
 *   over the measurement interval (0.32 → 0.48) as more countries have
 *   installed Tiangou systems and as the predictive capabilities being
 *   developed upstream have matured. Theater ratio remains relatively low
 *   (0.35) because the infrastructure is functionally operational, not
 *   performative — the systems do provide network security, even as they
 *   enable surveillance. Suppression has increased (0.48 → 0.62) as
 *   infrastructure lock-in deepens and as export control regimes have proven
 *   ineffective at preventing diffusion.
 *
 * KEY AGENTS:
 *   - Citizens in Recipient Countries: Primary victim (powerless/trapped) — trapped within national borders, no exit from surveillance infrastructure once deployed, maximum extraction
 *   - Civil Society Organizations: Secondary victim (moderate/constrained) — can relocate or shift tactics at cost, experience mixed extraction (infrastructure enables documentation while threatening operations)
 *   - Geedge Corporation: Primary beneficiary (institutional/arbitrage) — captures market lock-in and upgrade revenue, experiences constraint as pure coordination
 *   - Domestic Surveillance Apparatus: Secondary beneficiary (institutional/constrained) — benefits from technology development and export revenue, constrained by international scrutiny
 *   - Export Control Coalition: Organized actors (organized/mobile) — Wassenaar Arrangement, EU regulations, US Entity List attempting to close pathway through regulatory frameworks
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees irreducible hybrid of coordination and extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(export_proliferation_pathway, 0.48).
domain_priors:suppression_score(export_proliferation_pathway, 0.62).
domain_priors:theater_ratio(export_proliferation_pathway, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(export_proliferation_pathway, extractiveness, 0.48).
narrative_ontology:constraint_metric(export_proliferation_pathway, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(export_proliferation_pathway, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(export_proliferation_pathway, tangled_rope).
narrative_ontology:human_readable(export_proliferation_pathway, "Export Proliferation Pathway for Predictive Surveillance Technology").
narrative_ontology:topic_domain(export_proliferation_pathway, "technology_governance/surveillance_studies/export_control_policy").

domain_priors:requires_active_enforcement(export_proliferation_pathway).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(export_proliferation_pathway, authoritarian_client_states).
narrative_ontology:constraint_beneficiary(export_proliferation_pathway, geedge_corporation).
narrative_ontology:constraint_beneficiary(export_proliferation_pathway, domestic_surveillance_apparatus).
narrative_ontology:constraint_victim(export_proliferation_pathway, citizens_in_recipient_countries).
narrative_ontology:constraint_victim(export_proliferation_pathway, civil_society_organizations).
narrative_ontology:constraint_victim(export_proliferation_pathway, independent_media).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CITIZENS IN RECIPIENT COUNTRIES (SNARE) — Trapped within national borders with no exit from surveillance infrastructure. Once the pathway is established and predictive tools deployed, citizens face comprehensive monitoring with no alternatives. Maximum extraction: their behavioral data feeds systems designed to suppress dissent.
constraint_indexing:constraint_classification(export_proliferation_pathway, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL SOCIETY ORGANIZATIONS (TANGLED ROPE) — Constrained by operating environment but not entirely trapped. The export pathway creates genuine coordination benefits (standardized protocols enable cross-border advocacy documentation) while simultaneously enabling the surveillance that targets them. Can relocate operations or shift tactics at significant cost. Mixed extraction: the infrastructure both enables and threatens their work.
constraint_indexing:constraint_classification(export_proliferation_pathway, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: GEEDGE CORPORATION (ROPE) — Primary beneficiary with arbitrage exit options. The existing commercial pathway (Tiangou Secure Gateway installations in Ethiopia, Kazakhstan, Myanmar, Pakistan) creates infrastructure lock-in that guarantees market for next-generation predictive tools. Experiences the constraint as pure coordination: solving the legitimate problem of secure network infrastructure for client states while building upgrade pathway. Net beneficiary with minimal experienced extraction.
constraint_indexing:constraint_classification(export_proliferation_pathway, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DOMESTIC SURVEILLANCE APPARATUS (TANGLED ROPE) — Benefits from technology development and export revenue that funds domestic capabilities, but constrained by international scrutiny and export control regimes. The pathway enables coordination (technology refinement through deployment feedback) while creating compliance costs and reputational risks. Mixed position: extraction flows toward this actor but with significant friction.
constraint_indexing:constraint_classification(export_proliferation_pathway, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EXPORT CONTROL COALITION (SCAFFOLD) — Organized international actors (Wassenaar Arrangement, EU dual-use regulations, US Entity List) see the pathway as a temporary coordination failure with sunset logic. Current-generation exports (Great Firewall infrastructure) are not yet controlled, but predictive surveillance tools are increasingly subject to export restrictions. The coalition is building alternative frameworks (human rights due diligence requirements, end-use verification) that will close the pathway as they mature. Low effective extraction because the coalition has agency and sees a closure mechanism.
constraint_indexing:constraint_classification(export_proliferation_pathway, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the export pathway exhibits both genuine coordination function (technology diffusion enables infrastructure development in lower-income countries) and asymmetric extraction (the same infrastructure enables authoritarian control). The pathway is not a natural law (it depends on specific corporate strategies and regulatory gaps) but also not pure extraction (the infrastructure has legitimate uses). Tangled rope classification reflects the irreducible hybrid structure.
constraint_indexing:constraint_classification(export_proliferation_pathway, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(export_proliferation_pathway_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(export_proliferation_pathway, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(export_proliferation_pathway, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(export_proliferation_pathway, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(export_proliferation_pathway_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The pathway creates substantial extraction through infrastructure lock-in that will amplify future surveillance capabilities, but current-generation systems also provide genuine network security functions. The value reflects that extraction is real but not yet maximal — the predictive tools that will fully exploit the pathway are still under development. Suppression (0.62): High. Once infrastructure is installed, recipient countries face significant barriers to exit: technical migration costs, institutional procurement dependencies, training lock-in, and loss of sunk investment. Export control regimes have proven largely ineffective — technology diffuses through licensing agreements, component-level exports, and shell companies. Theater ratio (0.35): Low-moderate. The infrastructure is functionally operational, not performative. Tiangou systems do provide DDoS protection, content filtering, and network management — the surveillance function is layered on top of genuine utility, not a replacement for it. Theater has increased slightly over the interval as export control compliance processes have grown more elaborate without becoming more effective.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates classic tangled rope perspectival divergence. Geedge Corporation sees pure coordination (Rope) — they are solving the legitimate problem of network security infrastructure for client states while building a sustainable business model. The export control coalition sees a temporary problem with a sunset (Scaffold) — current-generation exports slip through regulatory gaps, but predictive tools will be controlled as frameworks mature. Citizens in recipient countries see pure extraction (Snare) — the infrastructure creates inescapable surveillance with no alternatives. Civil society organizations and the domestic surveillance apparatus both see tangled rope — genuine coordination benefits coexist with asymmetric extraction. The analytical observer confirms tangled rope at the civilizational level — the pathway exhibits irreducible hybrid structure that cannot be decomposed into pure coordination or pure extraction. The perspectival gap is not a measurement error; it reflects real differences in structural position relative to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Citizens in recipient countries are victims with trapped exit options, yielding high d and maximum experienced extraction — they cannot leave national borders and have no alternative to the surveillance infrastructure. Civil society organizations are victims with constrained exit options, yielding moderate-high d — they can relocate operations or shift tactics but at significant cost, and they also derive some benefit from standardized infrastructure for documentation. Geedge Corporation is a beneficiary with arbitrage exit options, yielding very low d and negative effective extraction — the company captures market lock-in with minimal constraints. The domestic surveillance apparatus is a beneficiary with constrained exit options (international scrutiny creates friction), yielding low-moderate d. The export control coalition has mobile exit options and organized power, yielding moderate d despite their regulatory role — they experience the constraint as a coordination problem they are actively solving. The analytical observer uses analytical exit options, yielding moderate-high d that reflects the constraint's hybrid structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that the same infrastructure pathway can be simultaneously: (1) a legitimate technology transfer mechanism that provides network security to lower-income countries (coordination function), and (2) an extraction mechanism that enables authoritarian surveillance and creates lock-in dependencies (extraction function). The tangled rope classification captures this irreducible duality. The constraint is not mislabeled coordination (it has genuine security benefits) and not mislabeled extraction (it has genuine surveillance costs). The mandatrophy resolution is structural: the pathway's dual function is a property of the infrastructure itself, not an artifact of measurement perspective. Export control regimes attempt to separate the functions (allow security infrastructure, block surveillance tools) but the technical reality is that the same infrastructure serves both purposes, and the lock-in created by current-generation deployments amplifies the extraction potential of future predictive tools.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    predictive_capability_threshold,
    'At what point does surveillance infrastructure transition from monitoring (current-generation) to prediction (next-generation), and does this threshold change the constraint''s classification?',
    'Technical analysis of deployed systems: behavioral prediction accuracy, pre-crime intervention capabilities, social graph modeling depth. Comparison of current Tiangou installations vs documented predictive systems.',
    'If threshold is low (current systems already predictive): extractiveness is higher than measured, pathway is already operational. If threshold is high (predictive capabilities require substantial upgrade): current pathway is preparatory infrastructure, not yet the full extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(predictive_capability_threshold, empirical, 'Threshold distinguishing monitoring from predictive surveillance').

omega_variable(
    export_control_effectiveness,
    'Do export control regimes actually prevent proliferation of dual-use surveillance technology, or do they merely create compliance theater while technology diffuses through alternative channels?',
    'Historical analysis of controlled vs uncontrolled technology diffusion rates; identification of circumvention pathways (shell companies, technology licensing, component-level exports); comparison of stated vs actual enforcement.',
    'If effective: scaffold perspective confirmed, pathway has genuine sunset. If ineffective: export controls are theater (piton), pathway persists regardless of regulatory framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(export_control_effectiveness, empirical, 'Whether export controls prevent or merely document proliferation').

omega_variable(
    infrastructure_lock_in_reversibility,
    'Can recipient countries migrate away from Geedge infrastructure once installed, or does the pathway create irreversible technical and institutional lock-in?',
    'Case studies of countries that attempted to switch surveillance vendors; analysis of interoperability barriers, training dependencies, data migration costs, and institutional capture of procurement processes.',
    'If reversible: suppression is lower than measured, victims have latent exit options. If irreversible: suppression is higher, pathway creates permanent dependency that amplifies extraction from future predictive tools.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_lock_in_reversibility, empirical, 'Reversibility of surveillance infrastructure lock-in').

omega_variable(
    legitimate_use_proportion,
    'What proportion of deployed Tiangou infrastructure serves legitimate network security functions vs surveillance functions, and does this proportion change as predictive capabilities are added?',
    'Traffic analysis of deployed systems; comparison of stated use cases (DDoS protection, content filtering for legal compliance) vs observed use cases (political content blocking, activist targeting); longitudinal tracking as systems are upgraded.',
    'If high legitimate use: coordination function is substantial, tangled rope classification is robust. If low legitimate use: coordination is cover story, constraint is closer to snare from more perspectives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimate_use_proportion, empirical, 'Proportion of legitimate vs surveillance use in deployed infrastructure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(export_proliferation_pathway, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(export_prolif_theater_t0, export_proliferation_pathway, theater_ratio, 0, 0.25).
narrative_ontology:measurement(export_prolif_theater_t3, export_proliferation_pathway, theater_ratio, 3, 0.28).
narrative_ontology:measurement(export_prolif_theater_t6, export_proliferation_pathway, theater_ratio, 6, 0.32).
narrative_ontology:measurement(export_prolif_theater_t10, export_proliferation_pathway, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(export_prolif_extract_t0, export_proliferation_pathway, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(export_prolif_extract_t3, export_proliferation_pathway, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(export_prolif_extract_t6, export_proliferation_pathway, base_extractiveness, 6, 0.44).
narrative_ontology:measurement(export_prolif_extract_t10, export_proliferation_pathway, base_extractiveness, 10, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(export_prolif_suppress_t0, export_proliferation_pathway, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(export_prolif_suppress_t3, export_proliferation_pathway, suppression_requirement, 3, 0.54).
narrative_ontology:measurement(export_prolif_suppress_t6, export_proliferation_pathway, suppression_requirement, 6, 0.59).
narrative_ontology:measurement(export_prolif_suppress_t10, export_proliferation_pathway, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(export_proliferation_pathway, global_infrastructure).

% DUAL FORMULATION NOTE:
% This constraint is downstream of both predictive_surveillance_extractiveness (the capability being proliferated) and chip_constraint_bottleneck (the hardware dependency that shapes deployment patterns). The export pathway is structurally distinct from the surveillance technology itself — it models the distribution mechanism and infrastructure lock-in, not the predictive capability. The pathway's extractiveness (0.48) reflects the lock-in and suppression dynamics, while the upstream predictive surveillance constraint's extractiveness reflects the surveillance capability itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(export_proliferation_pathway, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
