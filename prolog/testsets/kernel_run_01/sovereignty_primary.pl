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
 *   human_readable: Territorial Border Control as State Sovereignty Prerogative
 *   domain: international_law/political_philosophy/migration_studies
 *
 * SUMMARY:
 *   Territorial borders are constitutive of state sovereignty under this
 *   reading — admission control is a core prerogative subject only to narrow
 *   international law constraints (non-refoulement, treaty obligations). This
 *   constraint is one reading of the contested kernel
 *   'border_normative_status'. The reading instantiates a specific normative
 *   framework: the political community has the right to determine membership
 *   boundaries, and this right is grounded in self-governance principles
 *   rather than derived from broader cosmopolitan obligations. The constraint
 *   exhibits extraction and coordination simultaneously: it coordinates
 *   political self-determination (beneficiary: political community) while
 *   extracting from excluded migrants whose livelihood access depends on
 *   border permeability. The reading's structural position differs sharply
 *   from the freedom_of_movement_primary reading, which treats mobility as a
 *   fundamental human capacity and borders as derivative restrictions
 *   requiring justification. Under sovereignty_primary, the burden of
 *   justification is reversed: freedom of movement must be justified as an
 *   exception to the default sovereignty prerogative. This reversal produces
 *   different victim sets, different beneficiary analysis, and different
 *   calculus of acceptable extraction.
 *
 * KEY AGENTS:
 *   - Political Community / State Authority: Primary beneficiary (institutional/arbitrage) — exercises collective self-determination through membership control; experiences border constraint as enabling rather than limiting
 *   - Excluded Non-Citizens: Primary victim (powerless/trapped) — denied entry without legal standing to contest; bears maximum suppression and extraction
 *   - Refugees with Treaty Protection: Secondary victim (moderate/constrained) — protected by non-refoulement but constrained by narrow persecution definition; experience mixed coordination and extraction
 *   - Wealthy Nation-States (Global North): Secondary beneficiary (powerful/arbitrage) — extract material benefit through labor market segmentation, agricultural protection, climate externality export; coordinate selectively through bilateral agreements
 *   - Border Enforcement Workers: Secondary victim (moderate/constrained) — implement border control under state authority; bear moral and psychological cost of enforcement
 *   - Transnational Migration Advocacy Coalitions: Organized agents (organized/constrained) — perceive sunset pathway through regional free movement protocols; exert counter-pressure on state authority
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable law of political order
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereignty_primary, 0.52).
domain_priors:suppression_score(sovereignty_primary, 0.68).
domain_priors:theater_ratio(sovereignty_primary, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereignty_primary, extractiveness, 0.52).
narrative_ontology:constraint_metric(sovereignty_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sovereignty_primary, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(sovereignty_primary, "Territorial Border Control as State Sovereignty Prerogative").
narrative_ontology:topic_domain(sovereignty_primary, "international_law/political_philosophy/migration_studies").

domain_priors:requires_active_enforcement(sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereignty_primary, '87e62bc5-90c7-4628-8ee7-83423d2142e7').
narrative_ontology:cs_created_at('87e62bc5-90c7-4628-8ee7-83423d2142e7', '').
narrative_ontology:cs_kernel_codification('87e62bc5-90c7-4628-8ee7-83423d2142e7', fixed_text).
narrative_ontology:cs_authority_grounding('87e62bc5-90c7-4628-8ee7-83423d2142e7', lineage).
narrative_ontology:cs_interpretation_layer_present('87e62bc5-90c7-4628-8ee7-83423d2142e7').
narrative_ontology:cs_kernel_id(sovereignty_primary, border_normative_status).
narrative_ontology:cs_reading_relation('87e62bc5-90c7-4628-8ee7-83423d2142e7', freedom_of_movement_primary, influences).
narrative_ontology:cs_reading_relation('87e62bc5-90c7-4628-8ee7-83423d2142e7', managed_migration_hybrid, coexists_with).
narrative_ontology:cs_axiom('87e62bc5-90c7-4628-8ee7-83423d2142e7', foundational, political_community_self_determination_via_membership).
narrative_ontology:cs_axiom_status(political_community_self_determination_via_membership, holdable).
narrative_ontology:cs_axiom('87e62bc5-90c7-4628-8ee7-83423d2142e7', foundational, admission_control_necessity_for_self_governance).
narrative_ontology:cs_axiom_status(admission_control_necessity_for_self_governance, holdable).
narrative_ontology:cs_reference_frame('87e62bc5-90c7-4628-8ee7-83423d2142e7', westphalian_sovereign_state_system).
narrative_ontology:cs_drift_state('87e62bc5-90c7-4628-8ee7-83423d2142e7', contemporary_transnational_era, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereignty_primary, political_community_self_determination).
narrative_ontology:constraint_beneficiary(sovereignty_primary, state_institutional_authority).
narrative_ontology:constraint_victim(sovereignty_primary, excluded_migrants_outside_treaty_scope).
narrative_ontology:constraint_victim(sovereignty_primary, border_enforcement_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED NON-CITIZEN (SNARE) — Faces absolute exclusion from territory with no legal standing to contest the decision. Suppression is maximal: no right of entry, no internal appeal, no alternative forum. Geographic mobility is theoretically possible but practically constrained by global border regime and limited migration pathways. The constraint extracts: border closure prevents livelihood access, family reunification, safety. No coordination benefit to the excluded agent — pure extraction enforced through visa denial, deportation, and exclusion.
constraint_indexing:constraint_classification(sovereignty_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REFUGEE WITH TREATY PROTECTION (TANGLED ROPE) — Constrained by non-refoulement obligations and refugee convention, the state coordinates asylum function (genuine coordination benefit: safe haven for those meeting criteria) alongside extraction (narrow interpretation of 'persecution', high evidentiary burdens, liminal status with restricted rights). The agent experiences mixed extraction and genuine protection. Exit options are constrained but not eliminated — some pathway to legal status exists within the coordination framework.
constraint_indexing:constraint_classification(sovereignty_primary, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: POLITICAL COMMUNITY / STATE AUTHORITY (ROPE) — Experiences border control as pure coordination: the constraint enables self-determination by allowing the political community to maintain boundaries, select members, and preserve social cohesion. No extraction from this perspective — the beneficiary is the sovereign right to determine membership. The state has complete arbitrage: borders are universally recognized as within sovereign prerogative. This perspective instantiates the reading: borders = coordinated self-governance mechanism.
constraint_indexing:constraint_classification(sovereignty_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: WEALTHY NATION-STATES (TANGLED ROPE) — Extract significant material and political benefit from border control: labor market segmentation (cheaper labor through visas), agricultural subsidies protected from competition, climate externalities exported to less-developed states. Simultaneously coordinate within supranational frameworks (EU free movement, USMCA labor standards, bilateral trade agreements). Effective extraction χ is moderate because these states face reciprocal constraints from other powerful states and treaty obligations. Arbitrage is high — they can negotiate migration frameworks on favorable terms.
constraint_indexing:constraint_classification(sovereignty_primary, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: TRANSNATIONAL MIGRATION ADVOCACY COALITIONS (SCAFFOLD) — Organized agents (UNHCR, international NGOs, labor unions in destination countries) see border control as a temporary institutional arrangement with emerging sunset pathways: regional free movement protocols (EU model), bilateral labor agreements, climate migration frameworks. These coalitions experience suppression as high but perceive agency and alternative pathways. Theater is moderate — advocacy and litigation are functional mechanisms for rule-clarification and gradual boundary expansion.
constraint_indexing:constraint_classification(sovereignty_primary, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal perspective, this reading sees territorial borders as an immutable structural feature of political order: no state system exists without borders, and borders require admission control. This perspective risks naturalizing what may be a contingent institutional arrangement. The engine's false summit detector will identify whether this is genuine natural law (mathematically or logically required) or a naturalized political choice.
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

:- end_tests(sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The state extracts material benefits from border control through labor market segmentation (access to lower-wage migrant workers under temporary visa regimes), agricultural protection (exclusion of foreign competition), and externality export (climate impacts borne by less-developed states). However, the extraction is not total because international treaty obligations (refugee convention, non-refoulement, family reunification) constrain absolute discretion. The rise in extractiveness over the interval (0.35 → 0.52) reflects increasing asymmetry: wealthy states have constructed increasingly restrictive asylum definitions and border enforcement mechanisms while maintaining selective labor migration pathways that maximize extraction benefits. Suppression (0.68): High. Barriers to entry are structural (visa requirements, deportation authority, geographic isolation) and legal (state enforcement). But suppression is not at the maximum (0.95+) because treaty obligations create narrow pathways (asylum, family reunification) and some states permit limited migration. Theater ratio (0.45): Moderate-low. The constraint's legitimacy narrative is relatively functional — sovereignty is broadly accepted as the grounding principle, and border enforcement is widely recognized as legitimate state action. However, some performative content exists: rhetorical claims about border security effectiveness often exceed actual capacity, and humanitarian framing (protecting refugees, family reunification) sometimes masks extraction mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap here is extreme. The political community (rope) experiences border control as enabling self-governance — the constraint solves a coordination problem. The excluded non-citizen (snare) experiences the identical structural mechanism as pure extraction with no coordination benefit. The refugee with treaty protection (tangled rope) experiences both: genuine protection coordinated by the constraint alongside constrained access that extracts through high evidentiary burdens. Wealthy nation-states (tangled rope) experience extraction benefits (labor segmentation, agricultural protection) alongside coordination constraints (reciprocal treaty obligations to other powerful states). The analytical observer (mountain) risks naturalizing this entire structure as a law of political organization rather than a contingent institutional arrangement. The reading instantiates one specific normative frame — sovereignty prerogative — that produces this specific perspectival configuration. The freedom_of_movement_primary reading would reverse the beneficiary-victim assignment and produce different classifications from identical structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) captures how much each agent is targeted by versus benefits from the constraint. The political community has d ≈ 0.05 (full beneficiary): sovereignty enables this agent's primary goal (self-determination). Excluded non-citizens have d ≈ 0.95 (full target): the constraint exists precisely to exclude them. Refugees with treaty protection have d ≈ 0.70 (primarily target with limited recognition): they are the constraint's secondary target, but treaty obligations create partial beneficiary status through protection pathways. Wealthy nation-states have d ≈ 0.25 (partial beneficiary): they benefit from extraction mechanisms but face reciprocal constraints. Border enforcement workers have d ≈ 0.80 (target): they implement the constraint under state authority and bear enforcement costs. The analytical observer has d ≈ 0.72 (neutral analytic position). Directionality then feeds into the sigmoid f(d) to produce experienced extractiveness (χ). The beneficiary's low d produces low/negative χ (they experience the constraint as enabling). The target's high d produces high χ (they experience intense extraction). This produces the radical perspectival gap: the same ε and suppression yield entirely different χ values depending on d.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (misdirection of truth by apparent natural law) is high risk here. The mountain perspective claims that borders are an immutable feature of political order — no state system exists without territorial boundaries. This is empirically true of all modern states, but it may be logically contingent rather than naturally necessary. The constraint does not resolve mandatrophy; it documents it. The resolution requires examining whether: (1) borders are logically required for political self-determination, or (2) political self-determination could be achieved through alternative mechanisms (cultural autonomy, economic integration, transnational federalism). If (2), then the mountain classification is a false summit — the 'naturalness' of borders masks a political choice. The tangled rope classification is more robust: it acknowledges both the genuine coordination function (enabling political communities to maintain boundaries) and the genuine extraction (enabling economic exploitation and exclusion). The reading's strength is that it explicitly identifies beneficiaries and victims, making the extraction visible rather than naturalizing it as law. The reading's weakness is that it may assume political community boundaries are themselves natural rather than socially constructed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_vs_political_community_identity,
    'Is the ''political community'' that benefits from self-determination coterminous with the territorial state, or does it include transnational diasporas, future generations, and non-citizen residents whose life chances depend on border policy?',
    'Philosophical analysis of membership criteria; empirical study of how border communities (immigrants, refugees, border workers) define their own political community membership; intergenerational analysis of climate migration and territorial displacement',
    'If coterminous with state citizenship: beneficiary group is narrow, extraction high. If broader: beneficiary group includes partially overlapping populations with victims, creating more complex tangled rope structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_vs_political_community_identity, conceptual, 'Whether political community identity is coterminous with territorial state').

omega_variable(
    sovereignty_grounding_choice,
    'Is state territorial sovereignty a natural outcome of political organization (this reading''s assumption) or a contingent institutional choice that emerged from European nation-state consolidation and could be differently configured (freedom_of_movement reading''s assumption)?',
    'Historical analysis of pre-nation-state political organization; identification of alternative legitimacy frameworks (cosmopolitan, transnational, bioregional); analysis of whether current border regimes are optimal for stated purposes or historical artifacts',
    'If natural/immutable: mountain classification holds. If contingent/revisable: constraint becomes tangled rope or even snare from the analytical perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_grounding_choice, conceptual, 'Whether sovereignty is natural law or contingent institutional choice').

omega_variable(
    treaty_obligation_sufficiency,
    'Do current international law constraints (non-refoulement, refugee convention, family reunification protocols) adequately protect migrant welfare, or do they function primarily as performance legitimacy for extraction regimes?',
    'Empirical analysis of asylum approval rates, implementation of non-refoulement obligations, effectiveness of family reunification pathways; comparison of treaty text to actual state practice; longitudinal study of how states interpret obligation scope during crisis periods',
    'If adequate protection: tangled rope classification confirmed (genuine coordination function alongside extraction). If performative: snare classification becomes more salient from excluded migrant perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treaty_obligation_sufficiency, empirical, 'Whether treaty obligations provide meaningful protection or perform legitimacy').

omega_variable(
    alternative_sovereignty_frameworks,
    'Could political self-determination be achieved through mechanisms other than territorial border control (e.g., cultural autonomy, economic integration with political voice, transnational federalism, bioregional governance)?',
    'Comparative institutional analysis of current border-lite frameworks (EU, Schengen, ECOWAS); experiments with cosmopolitan democracy; analysis of historical periods with different sovereignty configurations; modeling of alternative coordination mechanisms for cultural/political self-governance',
    'If alternative frameworks viable: sovereignty_primary reading forecloses alternatives without logical necessity. If not viable: reading''s claim that borders are necessary for self-determination is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_sovereignty_frameworks, conceptual, 'Whether territorial borders are necessary for political self-determination').

omega_variable(
    beneficiary_asymmetry_temporal_scale,
    'At what temporal scale (immediate, biographical, generational, civilizational) does the beneficiary-victim relationship flip? Does sustained border closure eventually harm the political community through demographic collapse, labor shortage, or isolation?',
    'Demographic analysis of aging societies and migration dependency; economic modeling of long-term effects of labor market segmentation; study of historical cases where border closure caused internal instability; analysis of brain drain and capital flight patterns',
    'If scale is long (civilizational): constraint may reclassify from coordination to extraction at generational/civilizational horizons. If scale is immediate: coordination function holds across all time horizons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_asymmetry_temporal_scale, empirical, 'Temporal scale at which beneficiary-victim relationship reverses').

omega_variable(
    reading_coexistence_empirical,
    'Can the sovereignty_primary reading and freedom_of_movement_primary reading coexist in the same institutional framework, or do they logically foreclose one another?',
    'Analysis of actual dual-regime frameworks (Schengen internal free movement + external border control; Australian states with free internal movement + international border control); philosophical analysis of whether mobility rights and sovereignty are logically incompatible; examination of whether apparent coexistence masks hidden prioritization',
    'If coexist: reading relationship is ''coexists_with''. If foreclose: relationship is ''forecloses''. This determines whether the two readings can be held simultaneously within one institutional framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_empirical, conceptual, 'Whether sovereignty and freedom of movement readings logically coexist').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereignty_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereignty_primary, theater_ratio, 0, 0.38).
narrative_ontology:measurement(sove_tr_t20, sovereignty_primary, theater_ratio, 20, 0.42).
narrative_ontology:measurement(sove_tr_t40, sovereignty_primary, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereignty_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sove_be_t20, sovereignty_primary, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(sove_be_t40, sovereignty_primary, base_extractiveness, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereignty_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(sovereignty_primary, refugee_protection_obligation).
narrative_ontology:affects_constraint(sovereignty_primary, family_reunification_pathways).
narrative_ontology:affects_constraint(sovereignty_primary, managed_migration_labor_selection).
narrative_ontology:affects_constraint(sovereignty_primary, transnational_diaspora_rights).

% DUAL FORMULATION NOTE:
% This constraint (sovereignty_primary) is part of a constraint family decomposed from the contested kernel 'border_normative_status'. The sibling reading 'freedom_of_movement_primary' has a different ε (likely 0.62+, snare-class) because it treats the identical border enforcement mechanism as pure extraction without coordination function. The managed_migration_hybrid reading has intermediate ε (0.45-0.55) treating borders as jointly optimizable. Each reading is a structurally distinct constraint with its own base metrics, beneficiary/victim structure, and classification. They share a common observable (border control mechanisms) but instantiate it differently through normative framing. Decomposition is justified by ε-invariance: the normative frame changes what counts as extraction vs coordination, producing materially different ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sovereignty_primary, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
