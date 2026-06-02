% ============================================================================
% CONSTRAINT STORY: verona_civic_peace
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_verona_civic_peace, []).

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
 *   constraint_id: verona_civic_peace
 *   human_readable: Verona Civic Peace: Factional Coordination Under Threat
 *   domain: medieval_political/urban_governance
 *
 * SUMMARY:
 *   The Verona Civic Peace (established early 14th century under della Scala
 *   rule) presents a structural constraint that coordinates multi-faction
 *   urban coexistence while concentrating coercive and extractive power in
 *   the signoria. The constraint exhibits the full taxonomy of DR
 *   classification depending on observer position. For the merchant guilds
 *   and della Scala, the peace solves the genuine coordination problem of
 *   maintaining trade networks and preventing internecine feuding that would
 *   exhaust all factions. For minor magnate families, the peace constrains
 *   private military ambitions but provides predictable arbitration and land
 *   security. For the popular classes, the peace imposes suppression through
 *   legal enforcement (podestà courts, guild monopolies) and economic
 *   dependency (guild-controlled labor markets). The constraint's theater
 *   ratio (0.58) reflects performative elements: oath-taking ceremonies,
 *   formal trials, and ecclesiastical legitimation that convey justice while
 *   serving della Scala's political interests. The extractiveness trajectory
 *   (0.35 → 0.52 over 50 years) shows accumulation: initial coordination
 *   gains degrade as della Scala uses the peace machinery to consolidate
 *   monopoly power and extract rents through fines, confiscations, and
 *   appointments.
 *
 * KEY AGENTS:
 *   - Della Scala Signoria: Primary beneficiary (powerful/mobile) — monopolizes coercive capacity and arbitration authority; extracts through fines, confiscations, appointments; maintains hegemony through threat credibility
 *   - Merchant Guilds (Arti Maggiori): Secondary beneficiary (institutional/arbitrage) — benefit from stable trade environment and guild privilege; minimize extraction cost through council participation and price-setting authority
 *   - Minor Magnate Families: Constrained victim (moderate/constrained) — lose private military independence and face threat of exile; gain predictable dispute resolution and land security; moderate extraction through forced arbitration and Scala taxation
 *   - Popular Classes (Popolo): Primary victim (powerless/trapped) — trapped by guild-controlled labor markets and economic dependency; face suppression through legal enforcement; minimal coordination benefit; maximum extraction through taxes, fines, and forced labor obligations
 *   - Podestà Bureaucracy: Institutional actor (institutional/arbitrage) — performs justice functions that legitimate della Scala rule; maintains inertia despite functional degradation; benefits from appointments and prestige
 *   - Church and Confraternities: Organized mediator (organized/constrained) — provide legitimation rituals and social stability services; constrained by della Scala's control of appointments; benefit from guaranteed land holdings and tithe collection
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent power concentration as immutable feature of state formation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(verona_civic_peace, 0.52).
domain_priors:suppression_score(verona_civic_peace, 0.65).
domain_priors:theater_ratio(verona_civic_peace, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(verona_civic_peace, extractiveness, 0.52).
narrative_ontology:constraint_metric(verona_civic_peace, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(verona_civic_peace, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(verona_civic_peace, tangled_rope).
narrative_ontology:human_readable(verona_civic_peace, "Verona Civic Peace: Factional Coordination Under Threat").
narrative_ontology:topic_domain(verona_civic_peace, "medieval_political/urban_governance").

domain_priors:requires_active_enforcement(verona_civic_peace).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(verona_civic_peace, merchant_guilds).
narrative_ontology:constraint_beneficiary(verona_civic_peace, della_scala_signoria).
narrative_ontology:constraint_victim(verona_civic_peace, minor_families).
narrative_ontology:constraint_victim(verona_civic_peace, popular_classes).
narrative_ontology:constraint_victim(verona_civic_peace, civic_freedoms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POPOLO / MINOR FAMILIES (SNARE) — Trapped within the city walls and dependent on guild-controlled labor markets. The civic peace constrains factional violence but mandates submission to della Scala authority. Cannot exit without abandoning livelihood and protection. Maximum suppression: armed enforcement and economic dependency. Minimal coordination benefit — the peace serves the dominant faction's interest in monopolizing coercive power.
constraint_indexing:constraint_classification(verona_civic_peace, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MINOR MAGNATE FAMILIES (TANGLED ROPE) — Constrained by della Scala's military superiority and threat of exile or property confiscation. But also benefit from the peace — it prevents the internecine feuding that would exhaust their resources. Moderate extraction: forced submission to Scala arbitration and restricted private military. Genuine coordination function: the peace settles competing claims over land, water rights, and succession. Active enforcement required: Scala police and podestà courts.
constraint_indexing:constraint_classification(verona_civic_peace, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MERCHANT GUILDS (ROPE) — Primary beneficiary. The civic peace enables commerce by suppressing factional violence and providing predictable dispute resolution. Guilds arbitrage between della Scala (security provider) and the popolo (labor force). Minimal extraction experienced — guilds see the constraint as solving their genuine coordination problem: stable trade requires stable factionalism. Low suppression from the guilds' perspective: they participate in the podestà councils and influence policy.
constraint_indexing:constraint_classification(verona_civic_peace, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: DELLA SCALA SIGNORIA (TANGLED ROPE) — Powerful but constrained by the need to maintain the peace fiction without appearing tyrannical. Genuine coordination function: managing the competing claims of magnate families. Significant extraction: the peace gives della Scala monopoly on violence and arbitration, generating revenue through fines, confiscations, and appointments. Mobile exit option because della Scala can migrate (exile, relocation of power base) or dissolve the peace if control erodes. The constraint requires active enforcement through the podestà system and threat credibility.
constraint_indexing:constraint_classification(verona_civic_peace, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: PODESTÀ BUREAUCRACY (PITON) — The formal legal machinery of the civic peace. Theater ratio high (0.58): podestà courts perform justice rituals, oath-taking ceremonies, and formal arbitration that convey legitimacy but often serve della Scala's political interests rather than genuine conflict resolution. The bureaucracy persists through institutional inertia — it is the visible mechanism of peace, maintained because alternatives (direct Scala tyranny or descent into civil war) are worse. Primary function has attenuated: the real power lies in della Scala's threat capacity, not the court's wisdom.
constraint_indexing:constraint_classification(verona_civic_peace, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: CHURCH AND CONFRATERNITIES (TANGLED ROPE) — Organized actors with genuine coordination function (spiritual legitimation of the peace, mediation of disputes, poor relief to buy social stability). Constrained by della Scala's ability to revoke privileges and control appointments. Benefit from the peace (predictable land holdings, tithe collection, political influence). Experience moderate extraction: Scala appropriates some ecclesiastical resources and demands political loyalty framed as spiritual authority. Active enforcement: threatening excommunication or removal of clerics disloyal to the peace.
constraint_indexing:constraint_classification(verona_civic_peace, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, some concentration of enforcement capacity is inherent to any multi-faction urban system. The peace appears as an immutable property of state formation: you cannot have both equality between armed factions and stability. However, structural data contradicts this mountain classification — the specifics of Verona's peace (della Scala monopoly, guild privilege, popular suppression) are contingent institutional arrangements, not natural law. False summit: the naturalizing move mistakes a particular power concentration for a necessary feature of peace itself.
constraint_indexing:constraint_classification(verona_civic_peace, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(verona_civic_peace_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(verona_civic_peace, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(verona_civic_peace, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(verona_civic_peace, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(verona_civic_peace, TR),
    TR >= 0.70.

:- end_tests(verona_civic_peace_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Della Scala captures significant rents through monopolized arbitration and threat credibility, but much of this is legitimate first-mover reward for providing genuine coordination service (suppressing faction violence that would paralyze trade). The trajectory from 0.35 to 0.52 reflects extraction accumulation as della Scala uses the peace machinery to consolidate power beyond the initial coordination function. The base value reflects the mixture: genuine coordination need explains why no faction exits entirely; della Scala's accumulation explains why extraction increases over time. Suppression (0.65): Moderate-high. Structural barriers to exit are significant: the popolo cannot exit the city without abandoning economic livelihood; minor families face military and property loss through exile threats; guild membership is required for trade participation. But suppression is not total (0.90+) because some escape routes exist (migration to other cities, rural retreat, hidden economic activity). Theater ratio (0.58): Moderate. The podestà courts and ceremonial legitimation create performative content, but the underlying coercive structure is real and functional. Unlike pure piton constraints (0.70+), the Verona peace does coordinate genuine problems — the theater is a supplement to real enforcement, not a substitute for function.
 *
 * PERSPECTIVAL GAP:
 *   The constraint manifests maximum perspectival divergence across power positions. The della Scala signoria (powerful/mobile/institutional) classifies as Tangled Rope — they genuinely coordinate factional settlement while accumulating hegemonic power. The merchant guilds (institutional/arbitrage) classify as Rope — they experience coordination benefit with minimal extraction cost. The minor families (moderate/constrained) classify as Tangled Rope — caught between coordination benefit (dispute settlement) and extraction cost (lost autonomy). The popolo (powerless/trapped) classify as Snare — pure extraction with suppression maintained through legal enforcement and economic dependency. The podestà (institutional/arbitrage) classify as Piton — the formal legal machinery persists as legitimating theater despite functional degradation. The church (organized/constrained) classify as Tangled Rope — genuine coordination function (spiritual legitimation, poor relief) alongside constrained extraction (threat to clerical appointments). The analytical observer (analytical/analytical) risks classifying as Mountain — naturalizing the concentration of power as immutable feature of urban stability — but the structural data reveals this as a false summit: Venice and Florence maintained stability with distributed power, proving the Verona model is contingent.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's classification derives from its structural relationship (beneficiary/victim + exit options + power level) to the constraint. Della Scala as beneficiary with mobile exit options (can migrate, consolidate, or dissolve) experiences low directionality (d ≈ 0.22): they benefit from the constraint and retain agency. This produces low effective extraction (χ) despite moderate base extraction (ε = 0.52) — the formula χ = ε × f(d) × σ(S) dampens extraction for mobile beneficiaries. Merchant guilds as institutional beneficiaries with arbitrage options experience even lower d (≈ 0.12): they capture benefits while maintaining trade alternatives. Minor families as moderate-power victims with constrained exit (threat of exile, property loss, but some military capacity) experience moderate d (≈ 0.58): they are partially trapped but retain structural position and power. The popolo as powerless victims trapped by economic dependency experience maximum d (≈ 0.92): all of their structural position is occupied by the constraint. The podestà bureaucracy as institutional actors with arbitrage (can serve successor regimes) experience low d (≈ 0.20): they benefit from appointments and prestige regardless of which faction dominates. The analytical observer experiences d ≈ 0.73 (high but not maximum, reflecting observer position rather than engagement).
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY RECOGNITION: Verona's civic peace should be decomposed into at least two structurally distinct constraints with different ε values: (1) faction_violence_coordination (ε ≈ 0.15, Rope) — the genuine coordination problem of suppressing internecine feuding that all factions benefit from solving, and (2) della_scala_hegemony (ε ≈ 0.68, Snare) — the monopolization of coercive power and extraction of rents through the peace machinery. These constraints are linked via network.affects_constraints: the hegemony is downstream of and parasitic on the coordination function. The current story conflates them into tangled_rope, which is analytically correct for the composite system but obscures the decomposition. However, for corpus completeness, the tangled rope classification is retained here to demonstrate the hybrid case where coordination and extraction are genuinely intertwined in a single institutional arrangement (unlike the BGS case where they decompose cleanly). The mandatrophy is resolved by recognizing that della Scala's perspective (Tangled Rope) reflects the genuine hybridity: they do solve coordination while extracting, and the two functions cannot be separated without destroying the system. The popolo's perspective (Snare) is not contradicted — they experience pure extraction because the coordination benefit has negative value for them (they are constrained by the peace machinery itself, not liberated by it).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equilibrium_versus_hegemony,
    'Does the civic peace represent genuine multi-faction equilibrium or unilateral della Scala hegemony disguised as consent?',
    'Historical analysis of treaty enforcement asymmetries; comparison of penalties imposed on della Scala agents vs minor families; examination of council voting patterns and whether minority factions could block majority decisions',
    'If equilibrium: classification shifts toward Rope (coordination with balanced enforcement). If hegemony: classification shifts toward Snare (rule by threat with fiction of consent). Current assessment assumes significant hegemonic element (tangled rope with high extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equilibrium_versus_hegemony, empirical, 'Whether the peace is equilibrium or hegemonic suppression').

omega_variable(
    alternative_coordination_feasibility,
    'Could Verona''s factions maintain city stability through distributed, non-hierarchical coordination (rotating councils, mutual defense pacts, merchant-led arbitration) without della Scala hegemony?',
    'Comparative analysis of Italian cities: Venice''s council system, Florence''s guild republic, other Lombard communes; examination of whether Verona tried non-hierarchical models before della Scala consolidation',
    'If feasible: della Scala peace is contingent arrangement masquerading as necessity (false summit confirmed). If infeasible: the mountain perspective gains credibility — some hierarchy may be inherent. Current assessment: Venice and Florence models suggest feasibility, supporting false summit diagnosis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_coordination_feasibility, empirical, 'Whether non-hierarchical coordination could maintain stability').

omega_variable(
    popular_class_identity_lock,
    'Do the popolo accept della Scala rule as legitimate (identity-locked) or resist it structurally but are materially trapped?',
    'Analysis of popular revolts and their framing; examination of religious and civic rituals celebrating della Scala; comparison of overt resistance to structural barriers to exit',
    'If identity-locked: suppression measure understates the binding mechanism — the popolo carry the constraint with them even if they escape physically. If trapped: suppression is structural (economic dependency, physical barriers). Classification implications: identity-locked suggests higher theater ratio; trapped suggests higher actual extraction beneath the performative layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(popular_class_identity_lock, conceptual, 'Whether popular-class acceptance is internalized or material').

omega_variable(
    della_scala_succession_fragility,
    'How dependent is the peace on della Scala''s particular dynasty vs the hegemonic structure itself?',
    'Historical analysis of succession crises and civil unrest; examination of whether peace persisted through changes in Scala leadership; comparison with successor regimes',
    'If dependent on dynasty: peace has sunset clause built in (Scaffold perspective gains credibility). If structural: peace persists independent of leadership and must be analyzed as permanent hegemony. Current assessment: multiple Scala signori maintained the system, suggesting structural rather than personality-dependent, but succession fragility is measurable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(della_scala_succession_fragility, empirical, 'Whether peace depends on dynasty or hegemonic structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(verona_civic_peace, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vero_tr_t0, verona_civic_peace, theater_ratio, 0, 0.42).
narrative_ontology:measurement(vero_tr_t25, verona_civic_peace, theater_ratio, 25, 0.51).
narrative_ontology:measurement(vero_tr_t50, verona_civic_peace, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(vero_be_t0, verona_civic_peace, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vero_be_t25, verona_civic_peace, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(vero_be_t50, verona_civic_peace, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(verona_civic_peace, enforcement_mechanism).
narrative_ontology:affects_constraint(verona_civic_peace, faction_violence_coordination).
narrative_ontology:affects_constraint(verona_civic_peace, della_scala_hegemony).

% DUAL FORMULATION NOTE:
% The Verona Civic Peace conflates two structurally distinct constraints: (1) faction_violence_coordination (ε ≈ 0.15, Rope) — the genuine shared problem of preventing internecine feuding, and (2) della_scala_hegemony (ε ≈ 0.68, Snare) — the monopolization and rent extraction. This story presents the composite tangled_rope classification showing their interdependence. Decomposed stories would show pure coordination (Rope) and pure extraction (Snare) separately, linked via affects_constraints to reveal the parasitic relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(verona_civic_peace, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
