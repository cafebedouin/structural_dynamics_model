% ============================================================================
% CONSTRAINT STORY: disability_identity_politics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_disability_identity_politics, []).

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
 *   constraint_id: disability_identity_politics
 *   human_readable: Disability Identity Politics Constraint
 *   domain: social/political/identity
 *
 * SUMMARY:
 *   Disability identity politics refers to the constraint through which
 *   disabled people's access to material resources (income support, workplace
 *   accommodations, healthcare, legal protections) becomes contingent on
 *   adopting and performing a unified disability identity aligned with
 *   dominant disability movement narratives. This creates a fundamental
 *   structural tension: the disability movement coordinates genuine
 *   collective action that has produced real victories (the ADA,
 *   accessibility standards, community infrastructure, legal protections),
 *   simultaneously extracting conformity from disabled people whose
 *   disability experience, identity, or political commitments diverge from
 *   movement-approved narratives. The constraint operates through multiple
 *   overlapping mechanisms: formal gatekeeping (disability organizations
 *   controlling access to services and information), informal social pressure
 *   (stigmatization of disabled people who refuse movement identity as
 *   'self-hating' or 'not really disabled'), internalized cognitive locks
 *   (disabled people whose identity is fused with movement narratives, making
 *   exit from the identity unthinkable), and institutional coupling (benefit
 *   systems that require adopting medical/legal disability categories that
 *   reflect movement-negotiated definitions rather than lived experience).
 *   The constraint exhibits what appears to be all six DR types depending on
 *   structural position, making it a rich exemplar of how identity-based
 *   social movements can simultaneously coordinate and extract.
 *
 * KEY AGENTS:
 *   - Disabled People Seeking Resources (powerless/identity_locked or constrained) — Face material dependency on movement gatekeeping for benefits, accommodations, legal protections. Identity-locked agents experience the constraint as unchangeable because their self-concept is constituted through disability movement identity; constrained agents see high costs to refusing movement alignment but could theoretically exit. Primary victims.
 *   - Disability Movement Leadership (institutional/arbitrage) — NGOs, large advocacy organizations, movement intellectuals who benefit from unified disability identity; can exit the constraint (move between movements, reframe identity) with minimal cost. Primary beneficiaries.
 *   - Grassroots Disabled Activists (organized/constrained) — Disabled people participating in movement organizing who experience both coordination benefits and extraction pressures. Can theoretically exit to form alternative coalitions but face coordination barriers.
 *   - Disabled People with Intersecting Identities (powerless/constrained) — Disabled people whose other identity commitments (racial, religious, gender, class, immigrant status) conflict with dominant movement narratives; forced to choose between accessing disability resources and maintaining other identity commitments. Secondary victims with particularly high suppression.
 *   - Alternative Disability Framework Movements (organized/mobile) — Neurodiversity communities, disability justice networks, intersectional disability collectives building parallel structures with explicit intent to reduce dependence on dominant movement gatekeeping.
 *   - Medical-Legal Classification System (institutional/arbitrage) — Government bureaucracies, medical certification authorities whose benefit allocation mechanisms require conforming to legal disability categories; benefits from stable category definitions but experiences its own gatekeeping as performative and inadequate.
 *   - Analytical Observer (analytical/analytical) — Risks naturalizing identity politics dynamics as inevitable features of any disability movement rather than contingent outcomes of specific institutional arrangements.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(disability_identity_politics, 0.52).
domain_priors:suppression_score(disability_identity_politics, 0.68).
domain_priors:theater_ratio(disability_identity_politics, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(disability_identity_politics, extractiveness, 0.52).
narrative_ontology:constraint_metric(disability_identity_politics, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(disability_identity_politics, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(disability_identity_politics, tangled_rope).
narrative_ontology:human_readable(disability_identity_politics, "Disability Identity Politics Constraint").
narrative_ontology:topic_domain(disability_identity_politics, "social/political/identity").

domain_priors:requires_active_enforcement(disability_identity_politics).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(disability_identity_politics, disability_movement_leadership).
narrative_ontology:constraint_beneficiary(disability_identity_politics, institutional_disability_advocates).
narrative_ontology:constraint_victim(disability_identity_politics, disabled_people_refusing_movement_identity).
narrative_ontology:constraint_victim(disability_identity_politics, disabled_people_with_intersecting_identities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISABLED PERSON SEEKING RESOURCES (SNARE) — Access to disability benefits, workplace accommodations, healthcare, and legal protections requires adopting and performing the dominant disability identity. The person is identity-locked: their self-concept as a disabled person is constituted through the movement narrative, making exit unthinkable from within their identity frame. High suppression from structural barriers (bureaucratic requirements, funding gatekeeping, institutional gatekeeping) combined with internalized cognitive locks. Maximum experienced extraction because the agent faces both material barriers and identity-based inability to conceive of exit.
constraint_indexing:constraint_classification(disability_identity_politics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: DISABLED PERSON RESISTING MOVEMENT IDENTITY (SNARE) — Faces material barriers to accessing accommodations and services if they refuse to align with movement narratives. The refusal carries high costs: loss of community, reduced access to disability-specific services, stigmatization as 'not really disabled' or 'self-hating.' Exit is structurally possible (can refuse the identity) but extractively costly (loses material resources and social support). This perspective shows pure extraction without identity lock — the agent sees the constraint clearly but cannot leave for material reasons.
constraint_indexing:constraint_classification(disability_identity_politics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DISABLED PERSON WITHIN THE MOVEMENT (TANGLED ROPE) — Experiences genuine coordination benefits (community solidarity, collective advocacy, accessibility victories, legal protections) alongside asymmetric extraction (pressure to conform to movement narratives, emotional labor of representing the movement, constrained to articulate only movement-approved identity expressions). The constraint simultaneously solves the collective action problem of organizing dispersed disabled people AND extracts conformity from those whose disability experience doesn't match dominant narratives. Moderate experienced extraction because exit options exist (can move between disability subcommunities) but carry real costs.
constraint_indexing:constraint_classification(disability_identity_politics, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL DISABILITY ADVOCATES (ROPE) — NGOs, legal advocacy organizations, and institutional actors see the constraint as coordination: unified disability identity enables coalition-building, litigation strategy, legislative advocacy, and resource allocation. They benefit from a stable, cohesive movement identity that can negotiate with government and employers. The constraint solves their collective action problem. Low effective extraction because these actors experience the mechanism primarily as coordination value; they gain arbitrage options through standardized disability categories.
constraint_indexing:constraint_classification(disability_identity_politics, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: GRASSROOTS DISABLED ACTIVISTS (TANGLED ROPE) — Organized agents who benefit from movement identity for collective action but also experience extraction through homogenization pressures. Can theoretically exit (form alternative coalitions) but face coordination barriers — the existing disability movement is the dominant coordinating force, and exit carries real costs to organizing capacity. Experience both the coordination function (collective power) and the extraction mechanism (normalization of disability identity) simultaneously. Organized power level differentiates this from individual disabled people in perspective 3.
constraint_indexing:constraint_classification(disability_identity_politics, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: MEDICAL-LEGAL CLASSIFICATION SYSTEM (PITON) — Governmental disability bureaucracies, medical certification processes, and legal disability categories represent a degraded coordination mechanism. Originally designed to allocate benefits to those meeting medical criteria, the system now functions largely as a performative gatekeeping ritual: disabled people must perform medical disability identity to access resources, but the medical categories poorly capture actual disability experience or need for accommodation. The theater persists through institutional inertia despite widespread recognition of its dysfunction. Low extractiveness from this perspective because the institutional system experiences itself as inadequate but cannot be reformed without destabilizing benefit allocation.
constraint_indexing:constraint_classification(disability_identity_politics, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ALTERNATIVE DISABILITY FRAMEWORKS (SCAFFOLD) — Neurodiversity movements, disability justice frameworks, and intersectional disability collectives represent emerging alternative coordination mechanisms that see the dominant disability identity politics as temporary. These movements are building parallel structures (peer support, accessible organizing, intersectional resource sharing) with explicit sunset logic: as alternative frameworks mature, they reduce dependence on dominant movement gatekeeping. Mobile exit options and organized power level indicate this is experienced as a solvable coordination problem with a path out.
constraint_indexing:constraint_classification(disability_identity_politics, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (NATURAL LAW VIEW / FALSE SUMMIT RISK) — From a civilizational perspective, one might argue that any collective identity politics requires unified self-presentation, and disabled people choosing to access resources through movement frames is simply the inevitable cost of organizing across difference. The constraint appears as a natural feature of identity-based social movements. However, the structural data reveals this as a false summit: the constraint is not inevitable but contingent on specific institutional arrangements (benefit allocation tied to medical category, movement gatekeeping of legitimacy, resource scarcity creating zero-sum competition for movement visibility). The mountain classification naturalizes what are actually political choices.
constraint_indexing:constraint_classification(disability_identity_politics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(disability_identity_politics_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(disability_identity_politics, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(disability_identity_politics, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(disability_identity_politics, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(disability_identity_politics, TR),
    TR >= 0.70.

:- end_tests(disability_identity_politics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts conformity and identity alignment from disabled people in exchange for access to material resources. The extractiveness is not at snare-level severity because: (a) genuine coordination benefits exist (movement has produced real legislative victories, accessibility infrastructure, community support), and (b) disabled people retain some agency in how they navigate the constraint (can selectively align with movement narratives, maintain private identity divergence, seek alternative resources). The measurement shows rising extractiveness over the interval (0.38 → 0.52) as the movement has matured and professionalized — as disability advocacy organizations have become larger, more institutionalized, and more gatekeeping-capable. Suppression (0.68): High. Significant barriers to exit include: (a) structural barriers (disabled people's material dependency on movement-controlled resources like benefits, accommodations, and service infrastructure), (b) social barriers (stigmatization, loss of community, reduced peer support), and (c) cognitive barriers (for identity-locked agents, exit would require reconstructing their identity). Suppression has increased over the interval (0.55 → 0.68) as alternative resources have not scaled fast enough to provide genuine exit options. Theater ratio (0.58): Moderate-high. The constraint functions partly through performative mechanisms: (a) performing alignment with movement narratives to access resources (whether or not the narratives match actual experience), (b) formal gatekeeping rituals that serve as signals of movement legitimacy rather than substantive resource allocation mechanisms, (c) institutional theater of formal disability categories in benefit systems that don't map well to lived experience. Theater has increased modestly (0.42 → 0.58) as the movement has professionalized and as institutional coupling with government benefit systems has deepened.
 *
 * PERSPECTIVAL GAP:
 *   The gap reveals the core structural tension: the disability movement genuinely coordinates collective action that produces wins (ADA compliance, accessible transportation, legal protections) while simultaneously making those wins conditional on adopting movement-approved identity. For disabled people whose experience or politics align with movement narratives, this is experienced as rope (pure coordination) or tangled rope (mixed). For disabled people whose experience diverges from movement narratives — disabled people who prioritize individualism over identity politics, disabled people with religious commitments that conflict with secular movement culture, disabled people whose disability is invisible or conditionally present and who resist the 'always-disabled' identity, disabled people in non-Western cultures with different disability conceptualizations, disabled people with intersecting identities that create conflicts — the same structural mechanism is experienced as snare (pure extraction). The beneficiary gap is equally stark: institutional disability advocates experience low extraction because they have the power and exit options to shape movement narratives and benefit from coordinated action. Powerless disabled people experience high extraction because they lack the power to reshape narratives and face material dependency on movement-controlled resources.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from the agent's structural relationship to the constraint: their power level, exit options, and whether they are beneficiaries or victims. Powerless disabled people seeking resources are victims with high d (0.85-0.95): trapped agents experience maximum extraction, identity-locked agents show slightly lower d (0.89) because their structural mobility is theoretically higher even though they cannot perceive it. Institutional disability advocates are beneficiaries with low d (0.15-0.20): they benefit from the constraint, have arbitrage exit options, so experience low or negative effective extraction. Organized disabled activists are victims with moderate d (0.50-0.60): they benefit from the movement's coordination but also bear conformity costs; their ability to theoretically exit to alternative coalitions reduces their d compared to powerless agents. The organizing principle is consistent: agents who benefit and can exit experience low d → negative χ. Agents who bear costs and cannot exit experience high d → high χ. The effective extractiveness each agent experiences is χ = ε × f(d), which produces the perspectival gap: the same base extractiveness (0.52) maps to χ ≈ 0.75 (snare) for d=0.85 victims, χ ≈ 0.40 (rope) for d=0.50 moderate agents, and χ ≈ -0.08 (institutional benefit) for d=0.15 beneficiaries. This is why the constraint appears as snare/tangled rope/rope from different perspectives — the perspectives differ not in their observation of ε but in their position in the extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through indexical classification: the constraint is simultaneously all of rope, tangled rope, snare, scaffold, and piton depending on the agent's structural position. There is no single 'correct' classification — the constraint genuinely is a Rope from the perspective of institutional disability advocates (it solves their coordination problem), genuinely is a Snare from the perspective of powerless disabled people with identity-locked exit options, and genuinely is a Tangled Rope from the perspective of disabled people within the movement who experience both coordination and extraction. The analytical observer's natural law classification (mountain) is revealed as a false summit by the presence of identifiable beneficiaries (disability movement leadership) and the omega variables documenting the institutional contingency of the constraint. The mandatrophy teaches that unified disability identity is not an inevitable law of disability movements but a contingent institutional product shaped by specific arrangements (centralized movement organizations, bureaucratic benefit systems, relative scarcity of alternative resources). The resolution is structural: alternative frameworks that provide genuine resource alternatives (neurodiversity services, disability justice infrastructure, peer-run support systems) reduce the constraint's force by removing the zero-sum choice between accessing resources and refusing movement identity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_constrained_distinction,
    'Is the binding mechanism identity-based (the disabled person''s self-concept is constituted through movement identity, making exit unthinkable) or structurally material (high costs to exit, but exit is conceivable)?',
    'Longitudinal interviews with people who have exited disability movement communities; analysis of how exit narratives describe the experience (identity transformation vs. cost-benefit calculation); comparison of post-exit identity stability across exit type',
    'If primarily identity-locked: the constraint''s power comes from internal cognitive capture rather than external barriers; different intervention strategies (identity reframing support, alternative identity communities). If primarily constrained: material barrier reduction (increased disability service providers, income support independent of movement alignment) would reduce the constraint''s force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_distinction, empirical, 'Whether binding is cognitive identity-lock or structural constraint').

omega_variable(
    movement_narrative_homogeneity,
    'How much genuine heterogeneity exists within ''the disability movement,'' and how much of the experienced constraint comes from actual gatekeeping vs. from disabled people self-censoring to fit perceived expectations?',
    'Content analysis of disability movement spaces (subreddits, forums, NGO statements) coding for narrative diversity; surveys of disabled people asking what identities/experiences they believe are ''acceptable'' to voice in movement spaces vs. what is actually tolerated; comparison of stated movement positions vs. actual organizational resource allocation',
    'If high gatekeeping: constraint is extraction mechanism; reducing gatekeeping requires structural change to movement institutions. If high self-censoring: constraint is partly internalized performance; changing expectations (communication campaigns, leadership diversity) might reduce suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(movement_narrative_homogeneity, empirical, 'Actual gatekeeping vs. anticipated conformity pressure').

omega_variable(
    resource_contingency_mechanism,
    'How tightly is access to material resources (benefits, accommodations, legal protections) actually contingent on adopting movement identity vs. simply requiring documentation of disability status?',
    'Audit study: disabled people seeking accommodations/benefits with different identity presentations (aligned vs. misaligned with movement narratives); analysis of benefit denial rates and documented reasons; interviews with benefits administrators about informal gatekeeping practices',
    'If tight contingency: the constraint is enforced through bureaucratic discretion and formal requirements; legal reform could reduce contingency. If loose: the constraint is primarily social/cultural; resource access is technically available but psychologically mediated by internalized expectations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_contingency_mechanism, empirical, 'Actual resource contingency on identity alignment').

omega_variable(
    alternative_framework_maturity,
    'Do alternative disability frameworks (neurodiversity, disability justice, intersectional models) have sufficient institutional capacity to provide resources and services currently gatekept by dominant movement, or are they aspirational rather than functional alternatives?',
    'Comparative audit: disabled people trying to access services through alternative frameworks vs. dominant movement pathways; measurement of time-to-resource, resource adequacy, and framework capacity; longitudinal tracking of alternative framework institutional growth',
    'If functionally mature: scaffold classification is accurate, sunset is realistic; alternative frameworks can reduce constraint force. If aspirational: scaffold is premature; the constraint persists because no genuine alternative coordination mechanism exists yet.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_framework_maturity, empirical, 'Alternative frameworks'' functional capacity vs. aspirational status').

omega_variable(
    collective_action_necessity,
    'Is the unified disability identity genuinely necessary to solve the collective action problem of organizing dispersed disabled people, or is it an unnecessary homogenization that serves movement leadership interests?',
    'Historical case analysis: successful disability advocacy campaigns examining which identity elements were causally necessary vs. performative; cross-movement comparison (disability vs. other identity movements) on identity flexibility vs. coalition breadth; game-theoretic analysis of collective action requirements',
    'If necessary: the constraint is Rope (pure coordination); reducing it would weaken advocacy effectiveness. If unnecessary: the constraint is Snare or Tangled Rope; alternative coordination mechanisms could achieve advocacy goals with less identity conformity pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_necessity, conceptual, 'Whether unified identity is necessary for collective action').

omega_variable(
    natural_law_vs_contingent_arrangement,
    'Is the constraint a natural feature of any disability rights movement (inevitable consequence of organizing across difference) or a contingent product of specific institutional arrangements (benefit allocation systems, movement structure, resource scarcity)?',
    'Comparative analysis of disability movements across different institutional contexts (countries with different benefit systems, historical periods with different movement structures); identification of contextual variables that correlate with constraint severity; thought experiment testing what would change if institutional context changed',
    'If natural law: constraint is immutable; efforts should focus on managing extraction rather than eliminating it. If contingent: constraint could be restructured through institutional change (decoupling benefits from movement identity, building alternative coordination mechanisms).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_contingent_arrangement, conceptual, 'Natural feature of disability movements vs. contingent institutional product').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(disability_identity_politics, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(disab_tr_t0, disability_identity_politics, theater_ratio, 0, 0.42).
narrative_ontology:measurement(disab_tr_t10, disability_identity_politics, theater_ratio, 10, 0.52).
narrative_ontology:measurement(disab_tr_t20, disability_identity_politics, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(disab_be_t0, disability_identity_politics, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(disab_be_t10, disability_identity_politics, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(disab_be_t20, disability_identity_politics, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(disab_su_t0, disability_identity_politics, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(disab_su_t10, disability_identity_politics, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(disab_su_t20, disability_identity_politics, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(disability_identity_politics, identity_coordination).
narrative_ontology:affects_constraint(disability_identity_politics, accessibility_standards_capture).
narrative_ontology:affects_constraint(disability_identity_politics, disability_benefits_bureaucracy).
narrative_ontology:affects_constraint(disability_identity_politics, intersectional_identity_conflicts).

% DUAL FORMULATION NOTE:
% Disability identity politics is downstream of structural factors (economic scarcity of accommodations, concentration of disability services in movement-aligned organizations) but represents a distinct constraint focused on identity conformity rather than resource scarcity itself. The network relationships identify constraints that structurally influence this one: accessibility standards capture (how disability movement leadership controls which access standards are prioritized), disability benefits bureaucracy (institutional gatekeeping through medical categories), and intersectional identity conflicts (how disabled people with multiple marginalized identities experience the constraint differently).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(disability_identity_politics, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
