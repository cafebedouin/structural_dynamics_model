% ============================================================================
% CONSTRAINT STORY: republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_republican_reading, []).

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
 *   constraint_id: republican_reading
 *   human_readable: Republican Legitimacy: Authority Grounded in Popular Consent and Delegation
 *   domain: political_theory/constitutional_law/sovereignty
 *
 * SUMMARY:
 *   The republican reading of sovereign legitimacy holds that governmental
 *   authority is legitimate only when grounded in the consent of the governed
 *   and exercised through delegated representatives accountable to popular
 *   will. This reading emerged historically as a challenge to hereditary
 *   monarchy and divine right doctrines, positioning the electorate as the
 *   ultimate source of legitimacy. The constraint exhibits tangled-rope
 *   structure: it genuinely coordinates collective self-governance (citizens
 *   participate in selecting leaders, rule of law replaces arbitrary will)
 *   while simultaneously extracting power from those it nominally empowers
 *   (representatives accumulate control, electoral systems concentrate voice,
 *   participation mechanisms are performative). The increasing theater ratio
 *   (0.35→0.58 across the interval) reflects the growing gap between
 *   republican legitimacy doctrine and actual delegation mechanisms — as
 *   electoral systems mature, they develop more sophisticated theater
 *   (marketing, polling, mass media persuasion) that performs 'popular
 *   consent' while concentrating decision-making authority. The measurement
 *   trajectory shows extractiveness rising steadily as the constraint matures
 *   from insurgent challenge to hereditary authority (early phase: high
 *   ideological novelty, genuine delegatory intent) to institutionalized
 *   system (mature phase: performative ritualism, accumulated elite power,
 *   voter apathy).
 *
 * KEY AGENTS:
 *   - Electorate (theoretically sovereign): Nominal source of legitimacy but structurally constrained by participation barriers, information asymmetries, and collective action problems; nominal beneficiary experiencing actual victimization through suppression of voice
 *   - Delegated Representatives: Primary beneficiaries (institutional/arbitrage) — capture regulatory power, career advancement, and elite status while maintaining nominal accountability to voters
 *   - Hereditary Claimants / Excluded Nobility: Primary victims of this reading (institutional/constrained) — lose hereditary claim to executive power but retain wealth and cultural capital; must negotiate with republican structure
 *   - Disenfranchised Populations (by property, gender, citizenship, literacy): Trapped victims (powerless/trapped) — formally included as 'the people' but structurally excluded from participation; bear suppression of actual voice
 *   - Democratic Movement / Reform Organizations: Organized agents (organized/mobile) — see republican mechanism as transitional scaffold toward deeper participation; work to reduce suppression and theater
 *   - Constitutional Courts and Judiciary: Institutional actors (institutional/constrained) — enforce republican legitimacy doctrine through judicial review while accumulating interpretive power insulated from electoral accountability
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the contingent republican reading as a universal principle rather than as one contested legitimacy claim among many
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(republican_reading, 0.35).
domain_priors:suppression_score(republican_reading, 0.42).
domain_priors:theater_ratio(republican_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(republican_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(republican_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(republican_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(republican_reading, tangled_rope).
narrative_ontology:human_readable(republican_reading, "Republican Legitimacy: Authority Grounded in Popular Consent and Delegation").
narrative_ontology:topic_domain(republican_reading, "political_theory/constitutional_law/sovereignty").

domain_priors:requires_active_enforcement(republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(republican_reading, formalized).
narrative_ontology:cs_authority_grounding(republican_reading, extraction).
narrative_ontology:cs_interpretation_layer_present(republican_reading).
narrative_ontology:cs_kernel_id(republican_reading, sovereign_legitimacy).
narrative_ontology:cs_reading_relation(republican_reading, monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation(republican_reading, constitutional_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom(republican_reading, foundational, popular_consent_necessary_for_legitimacy).
narrative_ontology:cs_axiom_status(popular_consent_necessary_for_legitimacy, holdable).
narrative_ontology:cs_axiom(republican_reading, foundational, hereditary_succession_incompatible_with_republicanism).
narrative_ontology:cs_axiom_status(hereditary_succession_incompatible_with_republicanism, holdable).
narrative_ontology:cs_reference_frame(republican_reading, popular_sovereign_legitimacy).
narrative_ontology:cs_drift_state(republican_reading, contemporary_liberal_democracy, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(republican_reading, electorate_as_sovereign).
narrative_ontology:constraint_beneficiary(republican_reading, delegated_representatives).
narrative_ontology:constraint_victim(republican_reading, hereditary_claimants).
narrative_ontology:constraint_victim(republican_reading, excluded_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISENFRANCHISED CITIZEN (SNARE) — Formally possesses the source of legitimacy (is part of 'the people') but structurally excluded from meaningful participation through property requirements, literacy tests, citizenship restrictions, or engineered apathy. Bears the cost of republican legitimacy claims while trapped in systems that deny actual delegatory power. Maximum suppression — cannot exit the nation, cannot exit non-participation.
constraint_indexing:constraint_classification(republican_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORDINARY CITIZEN WITH LIMITED VOICE (TANGLED ROPE) — Theoretically the source of all legitimacy under republican doctrine, but structurally constrained by information barriers, vote dilution, gerrymandering, two-party compression, and collective action problems. Genuine benefits from delegatory system (checks on monarchy, rule of law, representation ideology) coexist with asymmetric extraction: representatives capture regulatory rents, special interests hijack delegation, exit through emigration is costly. Mixed experience — some coordination (popular will does influence policy), significant extraction (delegated power accumulates away from base).
constraint_indexing:constraint_classification(republican_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REPRESENTATIVE LEGISLATOR (ROPE) — Benefits from delegation structure while bearing some accountability constraint. Has arbitrage options: can exit to private sector, judicial roles, or administrative positions while retaining elite status. Experiences the constraint primarily as coordination mechanism: representing constituents enables career advancement, legislative efficacy, and power consolidation. Extraction toward legislators is constrained by periodic elections and constituent pressure (nominal suppression), but suppression often fails in practice (voter apathy, gerrymandering, information asymmetry).
constraint_indexing:constraint_classification(republican_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: HEREDITARY ELITE / NOBILITY (TANGLED ROPE) — Foremost victim of republican legitimacy doctrine when institutionalized against hereditary succession. Excluded from the core mechanism (cannot inherit executive power), but often retains substantial wealth, land, and cultural capital. May maintain coordination function (tradition, institutional stability, cultural continuity) while being extracted from: denied political supremacy, subordinated to elected representatives, pressured by egalitarian ideology. Constrained exit — cannot simply leave the nation with their institutional position intact; must negotiate with republican structure or emigrate entirely.
constraint_indexing:constraint_classification(republican_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL DEMOCRATIC MOVEMENT (SCAFFOLD) — Organized actors (transnational civil society, international institutions, diaspora networks) see republican legitimacy as a transitional scaffold: replacing hereditary authority with popular consent is a temporary coordination problem with a generational sunset. Democratic norms, constitutional courts, and electoral infrastructure are tools for building accountability until more direct forms of participation mature. Suppression declines as digital participation and decentralized authorization expand. Low theater ratio for this perspective — the mechanism is transparently about power transfer, not ceremonial performance.
constraint_indexing:constraint_classification(republican_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: CONSTITUTIONAL RITUALISM (PITON) — The performative maintenance of 'popular sovereignty' language in constitutional texts and civic ceremonies masks actual power consolidation by elites. The electorate is symbolically constituted as the source of legitimacy but functionally excluded from ongoing governance through bureaucracy, monetary policy, judicial activism, and administrative law. Constitutional references to 'we the people' persist through institutional inertia long after the mechanism has degraded. High theater ratio (0.58+) — the constraint is maintained through ritual invocation of 'the will of the people' rather than through functional delegation.
constraint_indexing:constraint_classification(republican_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: NATURAL LAW SKEPTIC (MOUNTAIN) — From a universal/civilizational perspective, this reading risks treating popular consent as an immutable principle of legitimate governance rather than as a historically contingent institutional arrangement. The claim that 'authority requires popular consent' appears as a self-evident truth of political order, obscuring that it is a constructed reading of sovereignty that benefits particular classes. This perspective sees republican legitimacy as a natural law of governance, but the structural data reveals this as a false summit — the constraint is actively enforced, benefits are asymmetrically distributed, and suppression mechanisms are substantial.
constraint_indexing:constraint_classification(republican_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(republican_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(republican_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(republican_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(republican_reading, TR),
    TR >= 0.70.

:- end_tests(republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The republican reading extracts from ordinary citizens who are theoretically sovereign but practically excluded from governance, while extracting less from representatives who nominally serve at the pleasure of the electorate but actually consolidate power. The base value reflects the genuine coordination function (popular input does influence policy direction, rule of law does constrain arbitrary authority) coexisting with asymmetric capture (special interests, elite networks, bureaucratic autonomy). Suppression (0.42): Moderate-high. Participation barriers (registration requirements, geographic inconvenience, information costs) combined with engineered apathy (two-party compression, scandal cycles, manufactured controversy) create substantial suppression. Exit options are severely limited — one cannot exit the nation costlessly or exit 'the people' status. Theater ratio (0.58): Moderate-high and rising. The performative content has increased over time as electoral systems mature: mass media campaigns, polling, political marketing, and constitutional symbolism ('we the people') perform 'popular consent' while actual decision-making authority accumulates in bureaucracies, central banks, and executive agencies. Claimed type (tangled rope) reflects the genuine coordination of popular legitimacy doctrine combined with asymmetric extraction of power from the electorate.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives diverge sharply on whether the republican reading constitutes genuine delegation or performative theater. The disenfranchised citizen sees pure extraction (Snare): theoretically sovereign but trapped in suppression. The ordinary voter sees mixed experience (Tangled Rope): some genuine coordination, significant extraction. The representative sees primarily coordination (Rope): serving constituents enables power and career. The hereditary elite sees extraction from their position (Tangled Rope): excluded from succession but retaining substantial capital. The democratic movement sees temporary coordination problem (Scaffold) with a sunset: electoral mechanisms will eventually mature toward direct participation, reducing theater. Constitutional ritualism sees degraded performance (Piton): the mechanism has atrophied into pure symbolic invocation. The natural law skeptic risks seeing immutable principle (Mountain) where the data reveals contingent construction. This perspectival spread — six distinct types from one constraint — demonstrates why single-position analysis fails for contested readings of sovereignty.
 *
 * DIRECTIONALITY LOGIC:
 *   The primary directionality tension in this constraint runs between the electorate (nominal beneficiary, actual victim) and representatives (nominal servants, actual beneficiaries). The electorate's structural position is paradoxical: they are declared the source of all legitimacy (d should be ~0.0, full beneficiary) but experience high extraction through suppression and exclusion (d should be ~0.9, full target). This paradox IS the core of the constraint — the divergence between rhetorical positioning and structural reality. Representatives experience positive directionality: they benefit from delegation (d ≈ 0.2-0.3, partial beneficiary) while maintaining nominal accountability (suppression is constrained by electoral cycles). Hereditary claimants experience inverted directionality: previously beneficiaries of hereditary succession, they are now victims of republican exclusion (d ≈ 0.8). The analytical observer risks collapsing this directionality structure by naturalizing republican legitimacy as a universal principle rather than as a contested reading grounded in specific power distributions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_genuineness_threshold,
    'At what level of voter participation, information access, and actual policy influence does ''popular consent'' transition from performative to genuine delegation?',
    'Cross-national comparison: participation rates, policy responsiveness to median voter, media independence indices, literacy and information access. Identification of threshold beyond which representative claims break down.',
    'If threshold is high (>70% participation, high policy responsiveness): many republics fail to achieve genuine delegation, reclassifying from Rope/Tangled Rope to Snare or Piton. If threshold is low: performative systems maintain legitimacy at lower structural integrity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_genuineness_threshold, empirical, 'Threshold for genuineness of popular consent in delegation').

omega_variable(
    hereditary_exclusion_necessity,
    'Does the legitimacy of republican popular consent *require* the structural exclusion of hereditary succession, or is the exclusion contingent to particular historical implementations?',
    'Constitutional theory analysis: whether hereditary office-holding with popular confirmation exists as a coherent middle position, or whether all historical cases collapse into either pure hereditary succession or pure electoral legitimacy.',
    'If hereditary element is incompatible with republican legitimacy: reading forecloses the monarchical and constitutional_hybrid readings entirely. If compatible: reading coexists with siblings — different parties can hold both without logical contradiction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hereditary_exclusion_necessity, conceptual, 'Whether republican legitimacy logically requires hereditary exclusion').

omega_variable(
    delegation_depth_recursion,
    'Does the democratic principle that authority derives from popular consent extend recursively to delegated authorities (bureaucrats, judges, central banks), or does delegation terminate at the electoral tier?',
    'Constitutional interpretation across systems: analysis of whether executive appointments, judicial selection, and administrative rule-making are subjected to popular accountability mechanisms. Empirical assessment of whether recursive delegation is attempted and whether it survives institutional pressures.',
    'If recursive delegation is required: many actual republics fail the constraint (extractiveness increases, suppression rises). If delegation terminates at electoral tier: constraint is satisfied even with opaque bureaucratic execution. Theater ratio interpretation depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegation_depth_recursion, conceptual, 'Whether delegation recursively applies to all governance authorities').

omega_variable(
    reading_contest_stability,
    'Is the republican reading stable as THE legitimate grounding for authority, or is it persistently contested by monarchical and hybrid readings that retain institutional power and social legitimacy?',
    'Historical trajectory analysis: periods of republican ascendance vs resurgence of hereditary claims; institutional durability of electoral mechanisms vs recurrence of authoritarian reversion; ideological coherence of republican doctrine across societies.',
    'If reading is stable: beneficiaries (electorate, representatives) maintain structural advantage; victims (hereditary claimants) are permanently subordinated. If persistently contested: the constraint itself oscillates; extractiveness and suppression vary cyclically as different readings gain institutional dominance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_stability, empirical, 'Institutional stability of the republican reading against competing legitimacy claims').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(republican_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(repu_tr_t0, republican_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(repu_tr_t50, republican_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement(repu_tr_t100, republican_reading, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(repu_be_t0, republican_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(repu_be_t50, republican_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(repu_be_t100, republican_reading, base_extractiveness, 100, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(republican_reading, identity_coordination).
narrative_ontology:affects_constraint(republican_reading, monarchical_reading).
narrative_ontology:affects_constraint(republican_reading, constitutional_hybrid_reading).
narrative_ontology:affects_constraint(republican_reading, electoral_system_capture).
narrative_ontology:affects_constraint(republican_reading, representative_accountability).

% DUAL FORMULATION NOTE:
% The republican reading is one component of the contested sovereign_legitimacy kernel. The monarchical_reading and constitutional_hybrid_reading are separate constraint stories instantiating alternative readings of the same kernel. All three stories should link through network.affects_constraints to show the constraint family structure. This story (republican_reading) forecloses the pure monarchical reading but coexists with the hybrid reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
