% ============================================================================
% CONSTRAINT STORY: imperial_religious_property_seizure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_religious_property_seizure, []).

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
 *   constraint_id: imperial_religious_property_seizure
 *   human_readable: Imperial Religious Property Seizure and Dispossession
 *   domain: political_economy/religious_institutional_power
 *
 * SUMMARY:
 *   Imperial religious property seizure represents a foundational extraction
 *   mechanism by which centralizing empires consolidate power by claiming or
 *   redirecting wealth accumulated by competing institutional actors
 *   (religious institutions, temples, monasteries, pilgrimage networks). The
 *   constraint operates through explicit authorization (legal edict declaring
 *   seizure), theological legitimation (doctrinal justification), and
 *   military enforcement (suppression of resistance). Religious institutions
 *   are structurally vulnerable: they accumulate substantial property and
 *   revenue streams, they organize populations outside direct imperial
 *   control, and they claim authority that rivals the crown's. The seizure
 *   resolves this rivalry through expropriation. The constraint exhibits the
 *   full range of DR classifications depending on observer position: the
 *   dispossessed community experiences pure extraction (snare), the crown
 *   experiences pure coordination (rope), the nobility experiences mixed
 *   benefits and coercion (tangled rope), the competing religious institution
 *   provides legitimation theater (piton), and the analytical observer risks
 *   naturalizing the seizure as an inevitable feature of empire formation
 *   (mountain). The measurements show extractiveness rising from 0.45 to 0.72
 *   over the interval, indicating that initial seizures (45% extractive, with
 *   significant theatrical justification) escalate into systematic
 *   expropriation (72% extractive) as the crown's confidence grows and
 *   alternative theological justifications accumulate.
 *
 * KEY AGENTS:
 *   - Dispossessed Religious Community: Primary victim (powerless/trapped) — faces permanent loss of sacred property and institutional assets; no legal recourse or resistance capacity
 *   - Religious Institutional Leadership: Secondary victim (moderate/constrained) — clergy and administrators lose endowments, livelihood, and institutional autonomy; constrained between capitulation, flight, and suppression
 *   - Imperial Treasury: Primary beneficiary (institutional/arbitrage) — captures all proceeds from seizures; experiences the mechanism as pure coordination for legitimate fiscal purposes
 *   - Crown-Allied Nobility: Secondary beneficiary (powerful/mobile) — receives portions of seized property as reward for loyalty and enforcement; experiences mixed coordination and extraction
 *   - Competing Imperial Religious Authority: Institutional legitimator (institutional/arbitrage) — provides theological endorsement of seizures; maintains institutional position through performative authorization
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent political choice as immutable feature of empire
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_religious_property_seizure, 0.68).
domain_priors:suppression_score(imperial_religious_property_seizure, 0.75).
domain_priors:theater_ratio(imperial_religious_property_seizure, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_religious_property_seizure, extractiveness, 0.68).
narrative_ontology:constraint_metric(imperial_religious_property_seizure, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(imperial_religious_property_seizure, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_religious_property_seizure, snare).
narrative_ontology:human_readable(imperial_religious_property_seizure, "Imperial Religious Property Seizure and Dispossession").
narrative_ontology:topic_domain(imperial_religious_property_seizure, "political_economy/religious_institutional_power").

domain_priors:requires_active_enforcement(imperial_religious_property_seizure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_religious_property_seizure, imperial_treasury).
narrative_ontology:constraint_beneficiary(imperial_religious_property_seizure, crown_allied_nobility).
narrative_ontology:constraint_victim(imperial_religious_property_seizure, religious_institutions).
narrative_ontology:constraint_victim(imperial_religious_property_seizure, faith_communities).
narrative_ontology:constraint_victim(imperial_religious_property_seizure, dispossessed_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPOSSESSED COMMUNITY (SNARE) — Religious communities and their adherents face expropriation of sacred lands, shrine properties, and institutional assets with no legal recourse, military resistance, or viable exit. The constraint appears as irreversible loss. Suppression is maximal: organized resistance is prohibited, legal challenge is foreclosed, relocation is impossible (sacred land cannot be abandoned). High experienced extraction with no coordination benefit.
constraint_indexing:constraint_classification(imperial_religious_property_seizure, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RELIGIOUS INSTITUTIONAL LEADERSHIP (SNARE) — Clergy and institutional administrators face seizure of endowments, monasteries, temples, and pilgrim revenue streams. Exit options are severely constrained: they can flee (losing all influence and livelihood), capitulate (losing institutional autonomy and sacred authority), or resist (risking execution or permanent suppression). Significant extraction with minimal coordination function — the crown negotiates spoils, not partnership.
constraint_indexing:constraint_classification(imperial_religious_property_seizure, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: IMPERIAL TREASURY (ROPE) — The crown experiences the seizure as pure coordination: channeling religious wealth toward state functions (military, administration, infrastructure). No cost to the empire; sole beneficiary. Exit options abundant: the seizure can be expanded, consolidated, or relaxed depending on fiscal need. The constraint appears as a coordination mechanism for legitimate public purpose.
constraint_indexing:constraint_classification(imperial_religious_property_seizure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CROWN-ALLIED NOBILITY (TANGLED ROPE) — Local nobility may receive portions of seized properties as reward for loyalty and enforcement. They experience mixed coordination (rewarding allies) and extraction (from religious institutions). Exit options are mobile but costly: refusing the reward signals disloyalty; accepting it ties them to the crown's religious policy. Moderate extraction but with genuine coordination benefit.
constraint_indexing:constraint_classification(imperial_religious_property_seizure, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: COMPETING IMPERIAL RELIGIOUS AUTHORITY (PITON) — A state-sanctioned or rival religious institution may formally endorse the seizures as theologically justified (heresy punishment, idolatry suppression, doctrinal correction). The endorsement is performative: it provides legitimacy theater rather than genuine theological function. The constraint persists through institutional inertia — the authorization ritual continues long after its justificatory power has eroded.
constraint_indexing:constraint_classification(imperial_religious_property_seizure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal historical perspective, religious property seizure is an immutable feature of imperial consolidation: all empires extract from competing power centers, and religious institutions inevitably accumulate property that centralized states perceive as a rival fund. The constraint appears as a natural law of empire. However, this perspective risks naturalizing a contingent political choice — other empires have coexisted with religious wealth through different institutional arrangements.
constraint_indexing:constraint_classification(imperial_religious_property_seizure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_religious_property_seizure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(imperial_religious_property_seizure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(imperial_religious_property_seizure, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_religious_property_seizure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(imperial_religious_property_seizure, TR),
    TR >= 0.70.

:- end_tests(imperial_religious_property_seizure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The seizure captures 45-72% of religious institutional wealth directly, with no compensation or alternative provision. The escalation from 0.45 to 0.72 reflects that initial seizures target the most visible and defenseless institutions first, with increasing confidence and scope as resistance fails to materialize. Suppression (0.75): Very high. Organized resistance to seizure is prohibited (threat of execution); legal challenge is foreclosed (crown authority is supreme); theological objection is delegitimized (state-approved doctrine reframes resistance as heresy); relocation is impossible (sacred properties cannot be abandoned). Theater ratio (0.55): Moderate-high. Initial seizures employ substantial theological justification (heresy suppression, idolatry correction, doctrinal correction), with the theatrical component declining as the crown becomes more brazen. By measurement point 10, the theater ratio remains elevated (0.55) despite declining justification — the authorization ritual persists through institutional inertia even as its persuasive power erodes. This signals a drift toward piton characteristics over very long timescales.
 *
 * PERSPECTIVAL GAP:
 *   The original research group sees coordination (Rope) — they are solving the legitimate problem of communicating findings. The open science coalition sees a temporary problem with a sunset (Scaffold) — arXiv and registered reports are building alternative pathways. The journal editorial system sees its own degraded ritual (Piton) — peer review persists through inertia, not function. Replication groups see mixed coordination and extraction (Tangled Rope) — the system both enables and constrains their work. The field's epistemic reliability sees pure extraction (Snare) — premature claims contaminate the literature with no self-correction mechanism. The civilizational analytical observer risks seeing an immutable natural law (Mountain) — verification lag is inherent to science — but the structural data reveals this as a false summit: the contingent institutional arrangements (career incentives, funding concentration, publication bias) are not laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are determined by structural position relative to the seizure extraction flow. The dispossessed community is purely targeted (d ≈ 0.95): they lose property without compensation or benefit. Religious leadership is primarily targeted with minor constrained exit (d ≈ 0.85): they face loss of institutional control and livelihood but can flee or capitulate, at extreme cost. The imperial treasury is purely beneficiary (d ≈ 0.05): they capture proceeds without bearing costs. Crown-allied nobility are mixed beneficiaries with mobile exit (d ≈ 0.45): they receive portions of seized wealth but must enforce the mechanism and could be abandoned by the crown. Competing religious authority is beneficiary with arbitrage exit (d ≈ 0.25): they gain relative advantage through the crown's targeting of rivals, but could themselves be seized if they lose royal favor. These d values feed the sigmoid f(d) to produce experienced extractiveness chi, which is then scaled by scope σ(S) to determine effective extraction. Trapped agents with d ≈ 0.95 experience maximum chi; arbitrage beneficiaries with d ≈ 0.05 experience near-zero or negative chi.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by the perspectival method: this constraint has no single 'correct' type. Each perspective reveals a genuine structural feature. The snare classification (from the powerless agent's view) is not 'more correct' than the rope classification (from the beneficiary's view) — they reflect different structural positions in the same extractive mechanism. The resolution of apparent contradiction is that the analytical observer must recognize this is not a single constraint viewed from multiple angles but rather a relationship between agents with fundamentally asymmetric structural interests. The mountain perspective is a false summit (naturalized contingency) and can be rejected. The piton perspective reveals that institutional theological legitimation is degraded and performative. The analytical insight is that the constraint's classification depends entirely on the observer's structural position — there is no view-independent fact about whether this is a snare or rope, only the fact that different agents experience it radically differently because they occupy different positions in the extraction flow.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_justification_sincerity,
    'Is the theological justification for seizure (heresy, idolatry, doctrinal correction) genuine crown belief or pure legitimation theater?',
    'Historical analysis of the crown''s actions toward other religious institutions, consistency of stated doctrine with enforcement patterns, private vs. public justifications revealed in correspondence',
    'If genuine: the constraint includes a coordination function (suppressing actual doctrinal competition) and should reclassify from pure snare toward tangled rope. If theater: the constraint is pure extraction disguised as theological duty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_justification_sincerity, empirical, 'Whether theological justification reflects genuine crown belief or pure theater').

omega_variable(
    fiscal_necessity_vs_predation,
    'Is the seizure driven by genuine imperial fiscal need (war, infrastructure) or opportunistic predation on accumulated religious wealth?',
    'Temporal correlation analysis: seizures coinciding with military campaigns and building projects vs seizures during periods of fiscal stability; examination of alternative revenue sources available to the crown; comparison of religious property seized vs total imperial revenue',
    'If driven by necessity: extraction reflects legitimate state consolidation (higher f(d) weighting for powerless agents as collateral damage of necessary statecraft). If predatory: extraction is pure rent-seeking (maximum f(d) applying to intentional targeting).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_necessity_vs_predation, empirical, 'Whether seizure is driven by fiscal necessity or opportunistic predation').

omega_variable(
    religious_population_alternative_provision,
    'Does the crown provide alternative institutional support for religious practice (state temples, approved clergy stipends, protected pilgrim access) after seizure, or is the deprivation total?',
    'Documentation of post-seizure religious institutional arrangements: whether crown funds replacement temples, pays clergy, permits worship; comparison of pre- and post-seizure religious practice accessibility and freedom',
    'If alternative provision exists: suppression is moderate (religious function continues, if constrained). If total deprivation: suppression is maximal (religious institutions are eliminated, not replaced).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(religious_population_alternative_provision, empirical, 'Whether crown provides alternative institutional support for religious practice').

omega_variable(
    identity_locked_clergy_participation,
    'Do some clergy actively collude in seizures (endorsing as doctrinal correction) because their professional and spiritual identity has fused with the crown''s authority, even though their institutional interests are harmed?',
    'Historical analysis of clergy testimony and actions: comparing clergy who resist vs. collaborate; examining whether collaborating clergy maintain their institutional position or are eventually purged; tracking whether clergy maintain independent theological positions or adopt crown-mandated doctrine',
    'If identity-locked clergy participation is significant: some perspectives should include identity_locked exit options (clergy who cannot exit because their identity is fused with crown authority). If minimal: clergy resistance/collaboration reflects material incentives alone (use trapped/constrained/mobile exits).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_clergy_participation, empirical, 'Whether clergy collude due to identity fusion with crown authority').

omega_variable(
    rival_religious_institution_beneficiary_status,
    'Does a rival religious institution genuinely benefit from the seizure of a competing tradition''s property, or does the seizure harm religious institutions broadly?',
    'Examination of whether state-favored religious institutions receive seizure proceeds, whether they are protected from future seizures, whether crown ultimately turns on all religious institutions including those initially favored',
    'If rival institution benefits durably: the constraint includes real coordination (crown consolidating power through religious hierarchy). If rival eventually faces seizure: the constraint is pure extraction (all religious institutions are interim targets, with sequencing providing temporary beneficiary status).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rival_religious_institution_beneficiary_status, empirical, 'Whether rival religious institution durably benefits or is eventually targeted').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_religious_property_seizure, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(imprs_tr_t0, imperial_religious_property_seizure, theater_ratio, 0, 0.35).
narrative_ontology:measurement(imprs_tr_t5, imperial_religious_property_seizure, theater_ratio, 5, 0.48).
narrative_ontology:measurement(imprs_tr_t10, imperial_religious_property_seizure, theater_ratio, 10, 0.55).
narrative_ontology:measurement(imprs_tr_t15, imperial_religious_property_seizure, theater_ratio, 15, 0.62).

% Extraction over time
narrative_ontology:measurement(imprs_be_t0, imperial_religious_property_seizure, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(imprs_be_t5, imperial_religious_property_seizure, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(imprs_be_t10, imperial_religious_property_seizure, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(imprs_be_t15, imperial_religious_property_seizure, base_extractiveness, 15, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_religious_property_seizure, enforcement_mechanism).
narrative_ontology:affects_constraint(imperial_religious_property_seizure, religious_institutional_autonomy).
narrative_ontology:affects_constraint(imperial_religious_property_seizure, faith_community_demographic_stability).
narrative_ontology:affects_constraint(imperial_religious_property_seizure, imperial_fiscal_consolidation).

% DUAL FORMULATION NOTE:
% Imperial religious property seizure is a specific instantiation of a broader imperial consolidation constraint. Decomposition into separate stories reflects different measurement axes: the seizure mechanism (this story, ε=0.68) vs. the long-term religious institutional autonomy constraint (downstream, ε varies by tradition) vs. the fiscal consolidation mechanism (upstream coordination, ε ≤ 0.35). The seizure story focuses on the expropriation mechanism; downstream stories address the persistence of religious institutions under constraint; upstream stories address imperial revenue consolidation as a coordination problem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imperial_religious_property_seizure, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
