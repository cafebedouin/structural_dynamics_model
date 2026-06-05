% ============================================================================
% CONSTRAINT STORY: border_normative_status__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__sovereignty_primary, []).

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
    narrative_ontology:cs_story_uid/2,
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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: border_normative_status__sovereignty_primary
 *   human_readable: Border Authority and Sovereignty (Sovereignty-Primary Reading)
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint instantiates the sovereignty-primary reading of the
 *   contested border normative status kernel. The reading claims that
 *   territorial boundaries are legitimate instruments through which a
 *   collective exercises foundational self-determination authority, and that
 *   states therefore retain the right to exclude non-members without
 *   extraordinary justification. The constraint operates as a tangled rope:
 *   it genuinely coordinates membership and collective decision-making (the
 *   rope function) while simultaneously extracting from excluded migrants and
 *   displaced populations (the asymmetric extraction). The
 *   sovereignty-primary reading treats border enforcement as a legitimate
 *   state function, making exclusion an ordinary exercise of authority rather
 *   than a violation requiring justification. This contrasts sharply with the
 *   freedom-primary reading (freedom of movement as fundamental right) and
 *   the qualified-sovereignty reading (border control authority constrained
 *   by proportionality and rights obligations). The structural delta
 *   specified in the kernel contest is realized in this story: excluded
 *   migrants enter the victim set; border enforcement becomes legitimate
 *   state function; displacement is treated as externality or non-issue
 *   rather than as a claim on collective resources. The measurement
 *   trajectory shows rising suppression (enforcement infrastructure
 *   maturation) and rising extractiveness (as alternative pathways to
 *   membership are foreclosed), indicating that the constraint's coercive
 *   mechanisms have intensified over the interval.
 *
 * KEY AGENTS:
 *   - Excluded Migrants: Primary victims (powerless/trapped) — face absolute prohibition on entry; no exit alternatives; maximum experienced extraction
 *   - Displaced Populations: Primary victims (powerless/trapped) — stateless, climate-displaced, conflict-displaced persons become chronic victims; generational transmission of exclusion
 *   - State Institutional Actors: Primary beneficiaries (institutional/arbitrage) — gain sovereignty authority to self-govern; can choose enforcement level and exceptions; low experienced extraction
 *   - Citizen Members: Mixed beneficiaries/secondary victims (moderate/constrained) — benefit from public goods and welfare access but bear labor market distortion and moral hazard costs; constrained exit options
 *   - Transnational Capital: Mixed relationship (powerful/mobile) — benefits from labor pool immobilization and enforcement of contracts; experiences extraction through constrained worker mobility; can arbitrage across jurisdictions
 *   - International Rights Regime: Institutional actor (institutional/arbitrage) — persists formally while operative force is neutralized by sovereignty-primary framing; sees its own authority as degraded (piton perspective)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional choice as prerequisite for democracy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, 0.58).
domain_priors:suppression_score(border_normative_status__sovereignty_primary, 0.72).
domain_priors:theater_ratio(border_normative_status__sovereignty_primary, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_normative_status__sovereignty_primary, "Border Authority and Sovereignty (Sovereignty-Primary Reading)").
narrative_ontology:topic_domain(border_normative_status__sovereignty_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_normative_status__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__sovereignty_primary, 'ce66c6bc-75fd-43fd-839f-9e9daeb0ddba').
narrative_ontology:cs_kernel_codification('ce66c6bc-75fd-43fd-839f-9e9daeb0ddba', formalized).
narrative_ontology:cs_authority_grounding('ce66c6bc-75fd-43fd-839f-9e9daeb0ddba', lineage).
narrative_ontology:cs_interpretation_layer_present('ce66c6bc-75fd-43fd-839f-9e9daeb0ddba').
narrative_ontology:cs_reading_relation('ce66c6bc-75fd-43fd-839f-9e9daeb0ddba', border_normative_status__freedom_primary, forecloses).
narrative_ontology:cs_reading_relation('ce66c6bc-75fd-43fd-839f-9e9daeb0ddba', border_normative_status__qualified_sovereignty, coexists_with).
narrative_ontology:cs_axiom('ce66c6bc-75fd-43fd-839f-9e9daeb0ddba', foundational, collective_self_determination_requires_bounded_membership).
narrative_ontology:cs_axiom_status(collective_self_determination_requires_bounded_membership, holdable).
narrative_ontology:cs_axiom_grounding('ce66c6bc-75fd-43fd-839f-9e9daeb0ddba', collective_self_determination_requires_bounded_membership, deontological).
narrative_ontology:cs_axiom('ce66c6bc-75fd-43fd-839f-9e9daeb0ddba', foundational, state_authority_to_exclude_is_foundational_not_derivative).
narrative_ontology:cs_axiom_status(state_authority_to_exclude_is_foundational_not_derivative, holdable).
narrative_ontology:cs_axiom_grounding('ce66c6bc-75fd-43fd-839f-9e9daeb0ddba', state_authority_to_exclude_is_foundational_not_derivative, conventional).
narrative_ontology:cs_reference_frame('ce66c6bc-75fd-43fd-839f-9e9daeb0ddba', westphalian_sovereignty_model).
narrative_ontology:cs_drift_state('ce66c6bc-75fd-43fd-839f-9e9daeb0ddba', contemporary_mass_migration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ce66c6bc-75fd-43fd-839f-9e9daeb0ddba', '').
narrative_ontology:cs_kernel_id(border_normative_status__sovereignty_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, state_institutional_actors).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, citizen_members).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, displaced_populations).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, human_rights_compliance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED MIGRANT (SNARE) — Faces absolute prohibition on entry enforced by state monopoly on legitimate violence. No exit from the exclusion regime. Suppression is total: alternatives (asylum, family reunification, labor mobility) are foreclosed by the reading's foundational premise that states have authority to exclude. Experiences maximum extractiveness.
constraint_indexing:constraint_classification(border_normative_status__sovereignty_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DISPLACED POPULATIONS (SNARE) — Climate refugees, conflict-displaced persons, and stateless populations become chronic victims when sovereignty-primary reading denies any cosmopolitan obligation to receive. The constraint treats displacement as an externality of state border authority rather than as a claim on collective resources. Generational horizon captures intergenerational transmission of statelessness and exclusion.
constraint_indexing:constraint_classification(border_normative_status__sovereignty_primary, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: STATE INSTITUTIONAL ACTOR (ROPE) — The state experiences this constraint as a pure coordination mechanism for collective self-determination. Border authority is the infrastructure for any collective to persist and self-govern. The state's exit option is arbitrage: it can choose which international agreements to sign, which enforcement level to maintain, which exceptions to permit. Low experienced extraction because the constraint subsidizes state autonomy.
constraint_indexing:constraint_classification(border_normative_status__sovereignty_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CITIZEN MEMBERS / CONSTRAINED BENEFICIARIES (TANGLED ROPE) — Citizens benefit from border-mediated access to state welfare, public goods, and democratic participation. But they also bear costs: labor market distortion, reduced migration options, potential conscription or border service obligations, and moral hazard of displacement responsibility avoidance. The constraint mixes genuine coordination (defining the demos) with extraction (excluding outside labor supply, shifting welfare costs).
constraint_indexing:constraint_classification(border_normative_status__sovereignty_primary, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TRANSNATIONAL CAPITAL (TANGLED ROPE) — Capital is mobile; firm headquarters can relocate, supply chains can bypass high-cost jurisdictions. But capital also benefits from border stability (predictable labor supply, enforcement of contracts, exclusion of competitors' labor). Capital experiences extraction via immobilized labor pools and constrained worker mobility, but benefits from state coordination of enforcement and exclusion mechanisms.
constraint_indexing:constraint_classification(border_normative_status__sovereignty_primary, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL RIGHTS REGIME (PITON) — The regime of human rights law, refugee conventions, and migration protocols persists formally while the sovereignty-primary reading largely neutralizes its operative force. Border enforcement continues under sovereignty framing while rights language becomes performative decoration. Theater ratio is moderate-high because the regime goes through the motions of review and appeals while state discretion is legally unreviewable.
constraint_indexing:constraint_classification(border_normative_status__sovereignty_primary, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some form of bounded community is structurally necessary for any collective self-governance to function. Borders appear as a natural prerequisite for democracy and social cooperation — you cannot have 'the people' without boundaries of membership. This perspective risks naturalizing what the structural data reveals as a contingent institutional choice: the sovereignty-primary reading naturalizes border authority as inherent to legitimate statehood, but alternative readings (freedom-primary, qualified-sovereignty) show that borders can be reconfigured without losing democratic legitimacy.
constraint_indexing:constraint_classification(border_normative_status__sovereignty_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__sovereignty_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(border_normative_status__sovereignty_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(border_normative_status__sovereignty_primary, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(border_normative_status__sovereignty_primary, TR),
    TR >= 0.70.

:- end_tests(border_normative_status__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from excluded migrants (absolute prohibition, no exit) and displaced populations (treated as externalities). But the extraction is not maximal (Snare would be ε ≥ 0.66) because the constraint also performs genuine coordination: defining the demos, enabling collective decision-making, and distributing public goods to members. The citizen perspective (tangled_rope) captures the mixed nature — citizens benefit from membership coordination but also bear costs. The measurement trajectory (0.42 → 0.58 over the interval) reflects intensification of exclusion mechanisms as border enforcement technology improves and alternative pathways are foreclosed. Suppression (0.72): High. State monopoly on legitimate violence, legal prohibitions on unauthorized entry, detention and deportation infrastructure, and foreclosure of asylum/family reunification pathways all constitute suppression. The measurement trajectory (0.58 → 0.72) reflects maturation of enforcement capacity. Theater ratio (0.55): Moderate. The sovereignty-primary reading includes legitimate legal justification for border authority (treaties, domestic legislation, state rights frameworks), reducing the pure performative content. However, the international rights regime components (refugee law, human rights review) become largely theatrical under this reading — their operative force is neutralized by sovereignty primacy. The measurement trajectory (0.38 → 0.55) reflects increasing performative content as rights mechanisms proliferate but lack enforcement teeth.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The excluded migrant sees snare (absolute extraction with no exit). The displaced population sees generational snare (intergenerational transmission of statelessness). The state sees rope (pure coordination of self-determination). Citizen members see tangled_rope (mixed coordination and extraction). Transnational capital sees tangled_rope (benefits from labor immobilization but mobile itself). The international rights regime sees piton (its authority is formally preserved but functionally inert under sovereignty-primary override). The analytical observer risks seeing mountain (borders as natural prerequisite for democracy) — a false summit that naturalizes the reading's choice. The gap between state perspective (rope) and victim perspectives (snare) is the largest in the constraint corpus, reflecting the fundamental disagreement between the sovereignty-primary and freedom-primary readings over whether the constraint is primarily coordinative or extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness is derived from the agent's structural position relative to the constraint. The state benefits from border authority (beneficiary + arbitrage exit → low d → negative f(d) → low effective extraction experienced). Excluded migrants are pure targets (victim + trapped exit → high d → high f(d) → high effective extraction experienced). Citizen members are mixed (partial beneficiary through membership, partial victim through labor market costs + constrained exit → moderate d → moderate f(d) → moderate effective extraction). Capital is mobile (can arbitrage but also benefits from immobilized labor → d ≈ 0.55 → moderate effective extraction). The international rights regime has formal authority but operational constraint (arbitrage capacity undermined by sovereignty override → d ≈ 0.45 → low-to-moderate effective extraction). The analytical observer's natural-law perspective derives d from the civilizational power atom (d ≈ 0.73 → high f(d) → high effective extraction) but this is a false summit — the engine will detect beneficiaries (state, citizens) and reclassify.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that the tangled_rope classification is correct relative to the sovereignty-primary reading's foundational commitments, but the reading itself is a contested kernel position. The constraint is genuinely mixed — it coordinates membership while extracting from non-members — which is the tangled_rope signature. However, the constraint is also a candidate for false-summit detection: the analytical observer's mountain perspective naturalizes what is actually a chosen reading of a contested kernel. The naturalizing move occurs when border authority is framed as 'prerequisite for democracy' or 'inherent to collective governance' rather than as 'our chosen reading of how to balance self-determination against mobility rights.' The false-summit detector should flag this when beneficiaries are declared (state, citizens) on a mountain-classified perspective. The mandatrophy itself is not internal to this story — it is distributed across the kernel contest: the freedom-primary reading would classify borders as snare (pure extraction); the qualified-sovereignty reading would apply additional constraints (proportionality, rights compliance) modifying the extraction term.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_kernel_contest,
    'Which reading of border normative status is justified: sovereignty-primary, freedom-primary, or qualified-sovereignty?',
    'Comparative institutional analysis of migration outcomes, welfare effects, and democracy quality under each reading''s enforcement regime; longitudinal data on displacement flows and asylum recognition rates; normative justification audits against foundational commitments of each reading.',
    'If freedom-primary is justified: border-normative-status reclassifies globally to rope or snare-for-state-perspective-only. If qualified-sovereignty justified: reclassifies to tangled-rope with different beneficiary/victim sets and enforcement constraints. If sovereignty-primary holds: current classification stands but false-summit detection flags the naturalizing move.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Kernel contest: which normative reading of border authority is justified').

omega_variable(
    refugee_overflow_threshold,
    'At what scale of displacement do sovereignty-primary border regimes encounter a structural limitation — exclusion becomes materially untenable?',
    'Historical analysis of refugee crises (Syrian displacement, Afghan resettlement, Rohingya crisis, Ukraine, Palestinian displacement); threshold analysis of when neighboring states cannot physically enforce borders against exodus; studies of informal migration channels and trafficking networks that emerge when formal borders become absolute.',
    'If threshold is easily exceeded: the constraint''s suppression cannot be sustained at scale — it reduces to theater (piton classification for high-volume regimes). If threshold is high: the constraint''s suppression is genuinely implementable and snare classification holds for excluded migrants.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(refugee_overflow_threshold, empirical, 'Physical/material limits on exclusion enforcement at scale').

omega_variable(
    citizen_preference_heterogeneity,
    'Do citizens genuinely prefer zero-migration equilibrium, or does the sovereignty-primary framing obscure heterogeneous citizen preferences (employers want labor mobility, humanitarian-minded citizens want asylum access)?',
    'Survey evidence on citizen preferences for immigration policy by socioeconomic position, occupation, and value orientation; analysis of political coalition-building around border policy; revealed preferences through voting and direct democracy instruments (referenda on migration).',
    'If heterogeneous: the reading''s beneficiary identification (undifferentiated citizen members) oversimplifies; some citizen coalitions are actually victims. If homogeneous: the rope perspective for the state is more accurate. Changes distribution of power within citizen groups and affects whether citizen perspective should be tangled-rope or disaggregated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(citizen_preference_heterogeneity, empirical, 'Heterogeneity of citizen preferences on migration and border policy').

omega_variable(
    historical_legitimacy_of_borders,
    'Are current borders legitimate expressions of collective self-determination, or artifacts of colonial imposition and power asymmetry?',
    'Genealogical analysis of border origins; mapping of colonial boundary imposition vs self-determined border establishment; analysis of post-colonial state legitimacy claims grounded in inherited colonial borders vs. in indigenous self-determination.',
    'If colonial artifacts: the sovereignty-primary reading''s foundational premise (borders are instruments of self-determination) is undermined for post-colonial states — the constraint becomes extraction mechanism disguised as legitimacy claim. If self-determined: the reading''s legitimacy basis is strengthened. Affects whether false-summit detection is triggered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_legitimacy_of_borders, conceptual, 'Whether current borders represent genuine collective self-determination or colonial imposition').

omega_variable(
    alternative_membership_boundaries,
    'Can collective self-determination operate with permeable or identity-based membership boundaries rather than territorial exclusion?',
    'Comparative case studies of non-territorial membership systems (professional guilds, religious communities, diaspora networks); analysis of whether digital/virtual communities can sustain collective decision-making without territorial boundaries; theoretical analysis of what ''democracy'' requires in terms of bounded membership.',
    'If permeable boundaries are viable: sovereignty-primary reading forecloses legitimate alternatives unnecessarily — the reading''s core claim (territory is necessary for self-determination) is overspecified. If territorial boundaries are necessary: the reading''s natural-law perspective gains empirical support, though still remains naturalizing a chosen institutional form.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_membership_boundaries, conceptual, 'Structural necessity of territorial boundaries for collective self-determination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__sovereignty_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(border_sov_tr_t0, border_normative_status__sovereignty_primary, theater_ratio, 0, 0.38).
narrative_ontology:measurement(border_sov_tr_t10, border_normative_status__sovereignty_primary, theater_ratio, 10, 0.48).
narrative_ontology:measurement(border_sov_tr_t20, border_normative_status__sovereignty_primary, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(border_sov_be_t0, border_normative_status__sovereignty_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(border_sov_be_t10, border_normative_status__sovereignty_primary, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(border_sov_be_t20, border_normative_status__sovereignty_primary, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(border_sov_su_t0, border_normative_status__sovereignty_primary, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(border_sov_su_t10, border_normative_status__sovereignty_primary, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(border_sov_su_t20, border_normative_status__sovereignty_primary, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__sovereignty_primary, identity_coordination).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, border_normative_status__freedom_primary).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, border_normative_status__qualified_sovereignty).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, exclusion_mechanism_legitimacy).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, asylum_refugee_obligation).

% DUAL FORMULATION NOTE:
% The border_normative_status kernel decomposes into three constraint stories representing three readings: sovereignty_primary (this file), freedom_primary, and qualified_sovereignty. Each story has its own ε value, beneficiary/victim structure, and classification. They are linked as siblings in the kernel contest, not as alternative measurements of a single constraint. The ε-invariance principle applies: the readings have structurally distinct victim sets (sovereignty-primary includes excluded migrants as victims; freedom-primary would not) and different foundational premises, making them genuinely distinct constraints rather than observational variants.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
