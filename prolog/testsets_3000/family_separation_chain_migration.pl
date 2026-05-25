% ============================================================================
% CONSTRAINT STORY: family_separation_chain_migration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_separation_chain_migration, []).

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
 *   constraint_id: family_separation_chain_migration
 *   human_readable: Family Separation Chain Migration Constraint
 *   domain: immigration/migration_policy
 *
 * SUMMARY:
 *   Family separation in chain migration enforcement creates a structural
 *   trap where migration-seeking families are deliberately separated at
 *   border enforcement points to deter continued migration attempts and to
 *   fracture social networks that facilitate chain migration. The constraint
 *   operates through three mechanisms: (1) physical separation (detention
 *   facilities for children, deportation of parents), (2) bureaucratic
 *   obscuration (family tracking systems that lose data, processing delays
 *   that extend separation indefinitely), and (3) psychological conditioning
 *   (the threat of separation is used to deter migration attempts and to
 *   extract compliance with deportation). The separation persists because it
 *   concentrates extraction on the most vulnerable agents (children,
 *   separated parents) while diffusing responsibility across multiple
 *   institutions (ICE, detention operators, immigration courts). The
 *   constraint exhibits both snare properties (high suppression, high
 *   extraction, minimal coordination) and tangled rope properties (some
 *   legitimate deterrence function exists alongside systematic extraction).
 *   The theater ratio reflects that immigration courts maintain performative
 *   due process while actual case review is severely degraded by backlog and
 *   expedited proceedings.
 *
 * KEY AGENTS:
 *   - Separated Children: Primary victims (powerless/trapped) — no exit, no agency, maximum extraction (childhood, family bonds, development)
 *   - Detained Migrants: Primary victims (powerless/trapped) — physically confined, legally prohibited exit, extracted labor and hope
 *   - Family Reunification Seekers: Secondary victims (moderate/constrained) — face high costs and delays in legal pathways; coordinated through bureaucracy while extracted through processing
 *   - Enforcement Bureaucracy (ICE, CBP): Primary beneficiary (institutional/arbitrage) — gains authority, resources, and political legitimacy from separation mechanism; can adjust enforcement intensity based on political winds
 *   - Detention Operators: Secondary beneficiary (institutional/arbitrage) — private and public detention facilities derive revenue and capacity from separated migrant population
 *   - Nativist Political Movements: Tertiary beneficiary (organized/constrained) — leverage family separation threat as deterrence narrative and political mobilization tool; constrained by humanitarian opposition and occasional court reversals
 *   - Immigration Court System: Institutional observer (institutional/arbitrage) — maintains formal judicial legitimacy while actual case review is degraded; piton classification reflects performative function
 *   - Humanitarian Coalition: Organized opposition (organized/constrained) — legal aid, nonprofits, advocacy groups building alternative pathways; constrained by political opposition and judicial capacity limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_separation_chain_migration, 0.58).
domain_priors:suppression_score(family_separation_chain_migration, 0.72).
domain_priors:theater_ratio(family_separation_chain_migration, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_separation_chain_migration, extractiveness, 0.58).
narrative_ontology:constraint_metric(family_separation_chain_migration, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(family_separation_chain_migration, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_separation_chain_migration, snare).
narrative_ontology:human_readable(family_separation_chain_migration, "Family Separation Chain Migration Constraint").
narrative_ontology:topic_domain(family_separation_chain_migration, "immigration/migration_policy").

domain_priors:requires_active_enforcement(family_separation_chain_migration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_separation_chain_migration, enforcement_bureaucracy).
narrative_ontology:constraint_beneficiary(family_separation_chain_migration, detention_operators).
narrative_ontology:constraint_beneficiary(family_separation_chain_migration, nativist_political_movements).
narrative_ontology:constraint_victim(family_separation_chain_migration, separated_children).
narrative_ontology:constraint_victim(family_separation_chain_migration, detained_migrants).
narrative_ontology:constraint_victim(family_separation_chain_migration, family_reunification_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SEPARATED CHILD (SNARE) — Powerless agent with no exit. Physical confinement, legal prohibitions on family contact, dependence on state care, and complete absence of alternatives. Maximally trapped. The extraction is existential: childhood, family bonds, psychological development, identity formation. Suppression is total — no mechanism exists for the child to contest or escape the separation.
constraint_indexing:constraint_classification(family_separation_chain_migration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIGRANT IN DETENTION (SNARE) — Physically confined, legally prohibited from exit, economically dependent on state provision, geographically isolated. The constraint extracts labor (work details), time (waiting cycles), hope (family separation threat), and dignity. Suppression is structural: detention itself is the suppression mechanism. Exit exists only through compliance with deportation or through judicial intervention that rarely succeeds.
constraint_indexing:constraint_classification(family_separation_chain_migration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: FAMILY REUNIFICATION SEEKER (TANGLED ROPE) — Can theoretically exit through legal immigration pathways, but faces immense costs: years of processing, legal fees, income loss during separation, documentation barriers, and the threat that seeking family unity through legal channels triggers deportation investigation. The constraint coordinates family reunification (legitimate state function) while extracting through bureaucratic processing delays and documentation requirements. Both genuine coordination (we do process family petitions) and asymmetric extraction (the process is deliberately slow and costly).
constraint_indexing:constraint_classification(family_separation_chain_migration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: BORDER ENFORCEMENT AGENCY (ROPE) — Experiences the constraint as coordination of immigration control. The family separation mechanism coordinates deterrence (potential migrants see the cost) and enforcement (removes 'anchoring' family members from the pool). The agency has arbitrage capacity: it can vary enforcement intensity, processing times, and family separation protocols based on political winds. Net beneficiary of the constraint — resources, authority, and legitimacy flow to this agent.
constraint_indexing:constraint_classification(family_separation_chain_migration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: IMMIGRATION COURT SYSTEM (PITON) — Maintains performative legitimacy while actual functional judicial review of family separation cases is severely degraded. Court appearances are scheduled theater with minimal real deliberation; case backlogs exceed five years; judges acknowledge the system is broken while perpetuating its procedures. The constraint persists through institutional inertia — the court system maintains separation authority despite acknowledging it should not use it. Theater ratio is high because the judicial form (due process appearance) masks the extraction content (systematic separation).
constraint_indexing:constraint_classification(family_separation_chain_migration, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: HUMANITARIAN COALITION (SCAFFOLD) — Organized agents (nonprofits, legal aid, advocacy groups) see the constraint as a temporary policy arrangement with a sunset: family reunification executive orders, legislative changes to immigration law, and international pressure are gradually building alternative pathways. The coalition has agency through litigation, political advocacy, and direct services. Exit path is visible but constrained by political opposition. Theater is moderate because legal challenges have produced some actual policy shifts, indicating the mechanism is not purely performative.
constraint_indexing:constraint_classification(family_separation_chain_migration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, all nation-states inherently separate families at borders; enforcement of territorial sovereignty requires some family separation; the constraint is an inescapable feature of statecraft. However, the structural data contradicts this naturalization: family separation is not an inherent feature of immigration enforcement. Many states maintain family unity while enforcing borders. The engine will flag this as a false summit — naturalizing a contingent policy choice as immutable law.
constraint_indexing:constraint_classification(family_separation_chain_migration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_separation_chain_migration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(family_separation_chain_migration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(family_separation_chain_migration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_separation_chain_migration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(family_separation_chain_migration, TR),
    TR >= 0.70.

:- end_tests(family_separation_chain_migration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts childhood, family stability, and psychological well-being from separated children and parents. The measurement trajectory (0.42 → 0.58 over interval) reflects that enforcement intensity and processing delays have increased over time, raising effective extraction. However, extractiveness is not at the snare maximum (0.70+) because some separated families eventually achieve reunification through legal channels, and political opposition has produced temporary policy reversals. Suppression (0.72): High. Structural barriers are total: physical confinement for children, legal prohibitions on family contact during detention, deportation threat for parents, documentation barriers for family reunification petitions, and processing delays that extend separation for years. Suppression mechanisms operate across multiple registers (physical, legal, bureaucratic, psychological). Theater ratio (0.61): Moderate-high. Immigration court proceedings maintain the appearance of judicial review (case filing, hearings, appeals) while actual due process is degraded by case backlog, limited time per hearing, and expedited proceedings. The performative element has increased (0.48 → 0.61) as political pressure has mounted, requiring the system to maintain appearance of legitimacy while perpetuating separation. The theater reflects that formal procedures are followed (theater presence) while meaningful review is absent (theater content).
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival divergence appears across all dimensions. The powerless victims (separated children, detained migrants) perceive snare at biographical horizon with trapped exit — the constraint appears unchangeable within their lifespan. The moderate family reunification seekers perceive tangled rope — they see both genuine state functions (processing) and extraction (delay and cost). The institutional enforcement agency perceives rope — they see the mechanism as serving coordination functions (deterrence, enforcement). The immigration court perceives piton — the formal procedures exist but are largely performative. The humanitarian coalition perceives scaffold — they see the constraint as temporary, with viable alternative pathways emerging. The civilizational observer risks perceiving mountain — embedding the constraint in the nature of statecraft itself. The perspectival structure reveals that the constraint's legitimacy depends on this divergence: different observers can defend different readings simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position relative to the extraction flow. Separated children and detained migrants are full targets (d ≈ 0.95): powerless agents, trapped exit, victims of extraction, experience maximum f(d) ≈ 1.42. Family reunification seekers are partial targets (d ≈ 0.65): moderate power, constrained exit, mixed beneficiary/victim status (they benefit from family reunification processes but are extracted through delays and costs), experience f(d) ≈ 1.00. The enforcement bureaucracy is a beneficiary (d ≈ 0.05): institutional power, arbitrage exit, primary beneficiary status, experience f(d) ≈ -0.12 (negative effective extraction — the constraint subsidizes this agent). Nativist political movements have mixed directionality (d ≈ 0.40): organized power, constrained exit (political opponents can reverse their policies), secondary beneficiary status through deterrence narrative, experience f(d) ≈ 0.40. Directionality overrides are unnecessary — the automatic derivation captures the structural relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is NOT RESOLVED. The constraint exhibits both genuine coordination functions (family separation does deter migration chain, does enforce border control) and systematic extraction (extraction from the most vulnerable agents in ways that exceed what enforcement coordination requires). The classification should be tangled_rope, not snare, IF we accept that deterrence is a legitimate state function and that some extraction overhead is acceptable. However, the omega variables reveal that alternative enforcement methods may achieve equivalent deterrence without family separation, which would confirm snare classification and invalidate the tangled_rope defense. The mandatrophy resolution depends on empirically resolving whether family separation is a necessary enforcement tool or whether it is pure extraction masquerading as deterrence. Current analysis: mandatrophy_resolved: false. The snare classification is defensible (high extraction, high suppression, minimal genuine coordination), but it is not settled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_vs_extraction_intent,
    'Is family separation primarily a deterrence mechanism (legitimate enforcement tool) or primarily an extraction mechanism (punishment of migration)?',
    'Policy document analysis; correlation between stated enforcement priorities and actual separation rates; comparison of separation rates across administrations with different stated deterrence vs extraction intent',
    'If primarily deterrence: classification shifts toward tangled_rope (coordination + extraction). If primarily extraction: classification confirmed as snare. Intent ambiguity is precisely what allows the constraint to persist — both framings are partially defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_vs_extraction_intent, conceptual, 'Whether family separation is deterrent policy or punitive extraction').

omega_variable(
    suppression_internalization_trajectory,
    'Do separated families internalize the suppression (believe family reunification is impossible, abandon legal pathways) or maintain resistance (continue seeking reunification, build political pressure)?',
    'Longitudinal tracking of separated families post-reunification: employment outcomes, political participation, documented mental health recovery vs persistence; migration intention surveys of families with separated members',
    'If internalized: suppression value is understated (effective suppression exceeds structural measure because it persists after release). If resisted: suppression is structural only, not cognitive, and could be dismantled by policy change. Distinguishes structural snare from cognitively captured snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_trajectory, empirical, 'Whether separated families internalize suppression or maintain resistance').

omega_variable(
    alternative_enforcement_sufficiency,
    'Can immigration enforcement achieve equivalent deterrence and control without family separation (e.g., through expedited deportation, electronic monitoring, documentation requirements alone)?',
    'Comparative policy analysis: deterrence rates and enforcement outcomes in jurisdictions without family separation vs those with; experimental variation in enforcement methods; cost-benefit analysis of separation vs alternatives',
    'If alternatives are sufficient: family separation is pure extraction (snare confirmed), not necessity. If alternatives are insufficient: classification shifts toward tangled_rope (deterrence coordination with extraction overhead). This omega directly tests whether the constraint is defensible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_enforcement_sufficiency, empirical, 'Whether alternative enforcement methods achieve equivalent outcomes without separation').

omega_variable(
    identity_locked_caregiver_role,
    'Are separated parents identity-locked to the caregiver role such that family reunification would require identity transformation, not just policy change?',
    'Qualitative analysis of separated parent narratives; assessment of whether parents who achieve reunification report identity reformation as a requirement; comparison of reunification success rates between parents who maintain caregiver identity vs those who transform identity',
    'If identity-locked: exit_options should shift from ''trapped'' to ''identity_locked'' for some agents, changing biographical time classification from mountain to rope. This reveals whether the constraint''s power is structural (physical separation) or cognitive (identity fusion). High prevalence of identity locking would indicate the constraint is partially self-sustaining through psychological capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_caregiver_role, empirical, 'Whether separated parents are identity-locked to caregiver role').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_separation_chain_migration, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fsm_tr_t0, family_separation_chain_migration, theater_ratio, 0, 0.48).
narrative_ontology:measurement(fsm_tr_t3, family_separation_chain_migration, theater_ratio, 3, 0.55).
narrative_ontology:measurement(fsm_tr_t6, family_separation_chain_migration, theater_ratio, 6, 0.61).

% Extraction over time
narrative_ontology:measurement(fsm_be_t0, family_separation_chain_migration, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fsm_be_t3, family_separation_chain_migration, base_extractiveness, 3, 0.51).
narrative_ontology:measurement(fsm_be_t6, family_separation_chain_migration, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_separation_chain_migration, enforcement_mechanism).
narrative_ontology:affects_constraint(family_separation_chain_migration, immigration_detention_regime).
narrative_ontology:affects_constraint(family_separation_chain_migration, asylum_processing_bottleneck).
narrative_ontology:affects_constraint(family_separation_chain_migration, documentation_access_trap).

% DUAL FORMULATION NOTE:
% Family separation chain migration is downstream of broader immigration enforcement architecture. The upstream constraint is immigration detention regime (which creates the physical separation mechanism); the sideways constraints are asylum processing bottleneck (which creates delays that extend separation) and documentation access trap (which creates barriers to family reunification petitions). Each downstream constraint has its own extractiveness values reflecting its specific structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
