% ============================================================================
% CONSTRAINT STORY: institutional_legitimacy_without_consent
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_legitimacy_without_consent, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: institutional_legitimacy_without_consent
 *   human_readable: Institutional Legitimacy Without Explicit Consent
 *   domain: political/institutional
 *
 * SUMMARY:
 *   The constraint of institutional legitimacy without explicit consent is
 *   foundational to political order. It describes the structural gap between
 *   the claimed basis of authority (popular sovereignty, rule of law, natural
 *   law, divine right, or performance legitimacy) and the empirical reality
 *   that most populations did not explicitly authorize their government and
 *   cannot easily exit it. This constraint enables state capacity — taxation,
 *   military mobilization, law enforcement, and public goods provision —
 *   while simultaneously suppressing alternatives to that authority through a
 *   combination of legal prohibition, enforcement capacity, and
 *   internalization of legitimacy narratives. The constraint operates across
 *   all six DR types from different perspectives, revealing deep tensions
 *   between coordination necessity and extractive concentration. The rising
 *   measurements across the interval (extractiveness 0.42→0.58, suppression
 *   0.55→0.68, theater 0.48→0.64) suggest that institutional legitimacy is
 *   increasingly maintained through performative mechanisms rather than
 *   genuine consent, while suppression requirements are rising despite (or
 *   because of) expanded democratic theater. This diagnostic pattern points
 *   toward Piton degradation: the legitimacy apparatus is becoming
 *   increasingly theatrical even as its extraction mechanism hardens.
 *
 * KEY AGENTS:
 *   - Institutional Authority-Holders: Primary beneficiary (institutional/arbitrage) — extract legitimacy and compliance capacity from the constraint; can exit at low cost through emigration or position-seeking in alternative states
 *   - Conscripted Citizens: Primary victim (powerless/trapped) — legally bound by jurisdiction they did not explicitly choose; face maximum suppression with minimal exit options; experience extraction as non-negotiable
 *   - Organized Dissent Groups: Secondary victim (moderate/constrained) — experience mixed coordination benefit and extractive suppression; face career/legal penalties for challenging legitimacy narratives
 *   - Democratic Reform Movements: Organized secondary actor (organized/constrained) — experience the constraint as temporary (Scaffold perspective); see democratic expansion and participatory governance as sunset mechanisms
 *   - Post-National Alternative Structures: Institutional secondary actor (institutional/arbitrage) — exist outside the nation-state constraint; offer comparison cases for alternative coordination mechanisms
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable features of governance; false summit diagnosis applies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_legitimacy_without_consent, 0.58).
domain_priors:suppression_score(institutional_legitimacy_without_consent, 0.68).
domain_priors:theater_ratio(institutional_legitimacy_without_consent, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_legitimacy_without_consent, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_legitimacy_without_consent, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(institutional_legitimacy_without_consent, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_legitimacy_without_consent, tangled_rope).
narrative_ontology:human_readable(institutional_legitimacy_without_consent, "Institutional Legitimacy Without Explicit Consent").
narrative_ontology:topic_domain(institutional_legitimacy_without_consent, "political/institutional").

domain_priors:requires_active_enforcement(institutional_legitimacy_without_consent).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(institutional_legitimacy_without_consent, '59a9b3da-2794-40c0-b687-38fd782d7b25').
narrative_ontology:cs_kernel_codification('59a9b3da-2794-40c0-b687-38fd782d7b25', formalized).
narrative_ontology:cs_authority_grounding('59a9b3da-2794-40c0-b687-38fd782d7b25', extraction).
narrative_ontology:cs_interpretation_layer_present('59a9b3da-2794-40c0-b687-38fd782d7b25').
narrative_ontology:cs_reading_relation('59a9b3da-2794-40c0-b687-38fd782d7b25', legitimacy_from_performance, coexists_with).
narrative_ontology:cs_reading_relation('59a9b3da-2794-40c0-b687-38fd782d7b25', legitimacy_from_tradition, coexists_with).
narrative_ontology:cs_reading_relation('59a9b3da-2794-40c0-b687-38fd782d7b25', legitimacy_from_divine_right, forecloses).
narrative_ontology:cs_axiom('59a9b3da-2794-40c0-b687-38fd782d7b25', foundational, legitimacy_requires_consent).
narrative_ontology:cs_axiom_status(legitimacy_requires_consent, holdable).
narrative_ontology:cs_axiom_grounding('59a9b3da-2794-40c0-b687-38fd782d7b25', legitimacy_requires_consent, deontological).
narrative_ontology:cs_axiom('59a9b3da-2794-40c0-b687-38fd782d7b25', foundational, governance_possible_without_continuous_explicit_consent).
narrative_ontology:cs_axiom_status(governance_possible_without_continuous_explicit_consent, holdable).
narrative_ontology:cs_axiom_grounding('59a9b3da-2794-40c0-b687-38fd782d7b25', governance_possible_without_continuous_explicit_consent, empirically_contingent).
narrative_ontology:cs_axiom('59a9b3da-2794-40c0-b687-38fd782d7b25', secondary, state_authority_derives_from_people).
narrative_ontology:cs_axiom_status(state_authority_derives_from_people, overridden).
narrative_ontology:cs_axiom_grounding('59a9b3da-2794-40c0-b687-38fd782d7b25', state_authority_derives_from_people, deontological).
narrative_ontology:cs_reference_frame('59a9b3da-2794-40c0-b687-38fd782d7b25', popular_sovereignty_doctrine).
narrative_ontology:cs_drift_state('59a9b3da-2794-40c0-b687-38fd782d7b25', contemporary_democratic_skepticism, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('59a9b3da-2794-40c0-b687-38fd782d7b25', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_legitimacy_without_consent, institutional_authority_holders).
narrative_ontology:constraint_beneficiary(institutional_legitimacy_without_consent, coordination_beneficiaries).
narrative_ontology:constraint_victim(institutional_legitimacy_without_consent, dissident_populations).
narrative_ontology:constraint_victim(institutional_legitimacy_without_consent, excluded_groups).
narrative_ontology:constraint_victim(institutional_legitimacy_without_consent, individual_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSCRIPTED CITIZEN (SNARE) — Legally bound by a state system they did not explicitly authorize. Exit is materially impossible (no valid exit mechanism within the constraint) or requires abandonment of property, citizenship, and social bonds. Born into the jurisdiction; citizenship is assigned, not chosen. No alternative coordination available within reach. Maximum experienced extraction: all costs of governance, all enforcement power directed at behavioral compliance, minimal reciprocal right to exit or refuse consent.
constraint_indexing:constraint_classification(institutional_legitimacy_without_consent, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZED DISSENT GROUP (TANGLED ROPE) — Experiences genuine coordination benefit (public goods: infrastructure, law enforcement, social insurance) alongside asymmetric extraction (restricted speech, surveillance, selective enforcement against dissent). Can theoretically exit (migrate to another state) but at high cost (capital loss, social severance, legal barriers). Constrained exit combined with real mixed benefits and burdens.
constraint_indexing:constraint_classification(institutional_legitimacy_without_consent, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL AUTHORITY (ROPE) — Experiences the constraint as pure coordination. Legitimacy (perceived or actual) enables state capacity: taxation, military, regulatory power, and public goods provision. Authority holders can exit (emigrate with assets, seek position in alternative states) with minimal friction. The constraint solves the collective action problem of political coordination. From this view, the constraint is coordination-focused: how to govern a population without needing explicit consent from every individual for every action. Suppression appears necessary overhead rather than extraction.
constraint_indexing:constraint_classification(institutional_legitimacy_without_consent, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DEMOCRATIC REFORM MOVEMENT (SCAFFOLD) — Sees institutional legitimacy without explicit consent as a temporary coordination failure being resolved by democratic mechanisms (electoral franchise, constitutional amendment, participatory governance). The constraint has a sunset: as democratic norms mature and expand (wider suffrage, local participation, consent-based institutional redesign), the extraction mechanism weakens. Reform agents experience constrained exit (cannot fully exit the national jurisdiction while advocating for change) but see a structural exit path (institutional transformation). Theater ratio is moderate: democratic procedures create performative legitimacy-manufacturing, but also enable genuine consent-gathering mechanisms.
constraint_indexing:constraint_classification(institutional_legitimacy_without_consent, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: POST-NATIONAL SKEPTIC (PITON) — Views national-state legitimacy as a degraded institutional form. The constraint once served genuine coordination functions (collective defense, infrastructure provision, mutual aid) at a scale (nation-state) where these were optimal. But as technology enables alternate coordination mechanisms (diaspora networks, transnational governance, supra-state institutions), the nation-state's legitimacy apparatus persists through inertia. The legitimacy-manufacturing theater (flags, anthems, national narratives, mandatory education) maintains the constraint despite atrophied function. Theater_ratio high because the performative maintenance of national identity exceeds the constraint's actual coordination work.
constraint_indexing:constraint_classification(institutional_legitimacy_without_consent, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NATURAL LAW THEORIST (MOUNTAIN) — From a universal civilizational perspective, some form of authority coordination without continuous explicit consent is a necessary feature of any stable governance system. Humans are not atomized agents endlessly renegotiating consent; coordination at scale requires delegation of authority to decision-making bodies that operate without constant plebiscites. This is not extraction but an immutable structural feature of political life. The constraint appears as a natural law: governance without explicit universal consent is logically necessary. However, the structural data flags this as a false summit: identifiable beneficiaries (authority-holders) exist, and suppression is active rather than passive. The 'natural law' framing naturalizes what is actually a contingent institutional choice.
constraint_indexing:constraint_classification(institutional_legitimacy_without_consent, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_legitimacy_without_consent_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_legitimacy_without_consent, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_legitimacy_without_consent, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_legitimacy_without_consent, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_legitimacy_without_consent, TR),
    TR >= 0.70.

:- end_tests(institutional_legitimacy_without_consent_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant compliance and cognitive labor from populations (tax burden, mandatory education instilling legitimacy narratives, legal constraints on dissent, surveillance) while providing genuine public goods (defense, infrastructure, law and order). The extraction is not maximal because populations do receive reciprocal benefits — public goods are genuinely provided. The value reflects rising extraction over the interval as enforcement infrastructure expands and theater increases. Suppression (0.68): High. The constraint operates through substantial suppression mechanisms: legal prohibition of secession, enforcement of tax collection, criminalization of alternative authority claims, surveillance and control of information, psychological suppression through education and narrative. Suppression is active (requiring ongoing enforcement investment) rather than passive (inherent barriers). The rising trajectory suggests suppression is intensifying, possibly because legitimacy through explicit consent is weakening. Theater ratio (0.64): Moderate-high. Democratic legitimacy-manufacturing creates substantial theatrical activity: elections, constitutional preambles, patriotic education, civic ceremonies. However, beneath the theater exists genuine governance function (public goods provision, dispute resolution, collective security). The theater is increasing over the interval while underlying consent satisfaction may be declining, pointing to Piton dynamics: theatrical legitimacy substituting for substantive legitimacy-building.
 *
 * PERSPECTIVAL GAP:
 *   The six-perspective dispersion reveals that institutional legitimacy-without-consent is not a single unified constraint but a bundle of different constraints with different extraction mechanics. The Rope perspective (authority) and Snare perspective (powerless conscript) are almost opposite experiences of the same institutional structure. This gap is not resolvable by averaging or splitting the difference — it is the core structural reality that must be reported. The analytical observer's false mountain diagnosis (perspective 6) is critical: it reveals how 'legitimacy without consent is necessary' naturalizes what is actually a contingent institutional choice. This naturalization is the primary mechanism through which suppression is sustained: if the constraint is a law of nature rather than a constructed arrangement, then suppressing alternatives to it appears necessary rather than extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (χ) is computed from base_properties (ε = 0.58, σ(S) = 1.0 for national scope) multiplied by f(d), the sigmoid function of directionality. Beneficiaries with arbitrage exit (authority-holders, d ≈ 0.15) get f(d) ≈ -0.01, producing χ ≈ -0.006 (negative extraction, net benefit, coordination). Powerless conscripts with trapped exit (d ≈ 0.95) get f(d) ≈ 1.42, producing χ ≈ 0.82 (high snare). Organized dissent with constrained exit (d ≈ 0.60) gets f(d) ≈ 0.80, producing χ ≈ 0.46 (tangled rope boundary). Reform movements see chi modulated by their vision of sunset: effective χ drops as they see institutional exit paths. Post-national skeptics experience lower chi because they perceive arbitrage alternatives (decentralized coordination, diaspora networks) that reduce the constraint's grip. The mechanism is transparent in the directionality derivation: beneficiaries experience low d regardless of base_properties; victims experience high d. This is how the same institutional structure appears as six radically different constraint types.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by fully inhabiting its contradiction: institutional legitimacy without explicit consent IS both coordination and extraction, depending entirely on the agent's position. The constraint cannot be reduced to a single type because it genuinely serves both functions. Authority-holders need legitimacy-without-consent to coordinate large populations efficiently (genuine Rope coordination). Conscripts need exit options to make the constraint nonextractive (genuine Snare without exit). Reform movements need institutional pathways to convert suppression into representation (genuine Scaffold dynamic). The mandatrophy is resolved not by choosing a type but by reporting the full presheaf: the constraint is all six types simultaneously, and the gap between perspectives is the diagnostic signal that justifies the framework's existence. Any simpler classification (choosing one type) would suppress essential information: it would hide the exploitation suffered by conscripts, or the coordination necessity recognized by authorities, or the institutional degradation visible to post-national observers. The six perspectives are not different opinions about one fact — they are six different structural facts about the constraint's operation at different power levels and time horizons.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_source_ambiguity,
    'Does institutional legitimacy derive from explicit or implicit consent, historical legitimacy, performance, or imposed authority?',
    'Historical analysis of institutional origins; surveys of subjective legitimacy perception; correlation between legitimacy perception and enforcement requirements; comparison of voluntary compliance rates across institutions with different origin narratives.',
    'If legitimacy primarily derives from explicit consent: the constraint is performative (Piton), and sunset logic applies. If derived from imposed authority: the constraint is extractive (Snare). If derived from historical legitimacy plus performance: the constraint is mixed (Tangled Rope). If derived from perceived natural necessity: the constraint is a genuine mountain — but this requires no active suppression to sustain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, empirical, 'Whether institutional legitimacy derives from explicit consent or other sources').

omega_variable(
    alternative_coordination_sufficiency,
    'Could alternative decentralized coordination mechanisms (network organizations, polycentric governance, diaspora networks, platform-based administration) provide equivalent public goods to nation-states without the legitimacy-without-consent mechanism?',
    'Case studies of alternative coordination structures (Rojava, city-states, supra-national institutions, decentralized autonomous organizations); measurement of public goods provision quality, coverage, sustainability, and scalability; cost-benefit analysis of alternative mechanisms at various population scales.',
    'If alternatives are sufficient: the nation-state legitimacy constraint is contingent (Piton becoming Scaffold as alternatives mature). If alternatives are insufficient: the constraint is structurally necessary (genuine Rope or Mountain). If alternatives create different extraction patterns: the constraint is not universal but context-dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_sufficiency, empirical, 'Whether decentralized alternatives could replace nation-state public goods provision').

omega_variable(
    suppression_necessity_ambiguity,
    'How much of measured suppression is necessary overhead for any governance system, and how much is extractive overhead specific to legitimacy-without-consent?',
    'Comparison of suppression requirements (police resources, legal enforcement, surveillance spending) across institutional forms with different consent mechanisms (representative democracy vs. authoritarian, federal vs. centralized, direct democracy pockets vs. delegated governance); measurement of suppression scaling with legitimacy perception.',
    'If suppression scales independently of legitimacy perception: it is extractive overhead (high chi, Snare/Tangled Rope). If suppression scales inversely with legitimacy perception: it is coordination cost (lower chi, Rope). If suppression is constant regardless of institutional form: it is a natural law overhead (Mountain base extraction floor).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_necessity_ambiguity, empirical, 'Whether suppression is necessary governance overhead or extractive overhead').

omega_variable(
    exit_availability_temporal_drift,
    'Has the availability of exit options (migration, alternative citizenship, digital exile, parallel governance) increased or decreased over the measurement interval?',
    'Historical data on visa availability, dual citizenship rates, digital governance adoption, cost of international migration, regulatory barriers to nomadism, supra-national institution membership; correlation with suppression and extractiveness trends.',
    'If exit options increasing: constraint is degrading (Piton or Scaffold as exit becomes more viable). If exit options decreasing: constraint is hardening (Snare, suppression rising). If exit options stable: constraint structure is stable regardless of external change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_availability_temporal_drift, empirical, 'Temporal trend in availability of genuine exit options').

omega_variable(
    explicit_consent_manufacturing_distinction,
    'How much of ''democratic legitimacy'' represents genuine explicit consent (binding referenda, opt-in governance), and how much is manufactured consent (electoral choice among constrained options, mandatory participation framing as voluntary)?',
    'Comparison of actual voter choice freedom across jurisdictions; analysis of participation rates as proxy for consent (low participation = low perceived legitimacy); examination of institutional design choices (compulsory vs. voluntary voting, range of viable policy options, exit or opt-out mechanisms).',
    'If explicit consent mechanisms are genuine and expand: the constraint is moving toward Rope or Scaffold. If consent mechanisms are manufactured and expanding in theater without expanding in substance: the constraint is becoming Piton. If explicit consent is impossible at state scale: the constraint remains Mountain or Snare depending on whether necessity or extraction is dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(explicit_consent_manufacturing_distinction, conceptual, 'Whether democratic legitimacy represents genuine or manufactured consent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_legitimacy_without_consent, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(instleg_tr_t0, institutional_legitimacy_without_consent, theater_ratio, 0, 0.48).
narrative_ontology:measurement(instleg_tr_t3, institutional_legitimacy_without_consent, theater_ratio, 3, 0.56).
narrative_ontology:measurement(instleg_tr_t6, institutional_legitimacy_without_consent, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(instleg_be_t0, institutional_legitimacy_without_consent, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(instleg_be_t3, institutional_legitimacy_without_consent, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(instleg_be_t6, institutional_legitimacy_without_consent, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(instleg_su_t0, institutional_legitimacy_without_consent, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(instleg_su_t3, institutional_legitimacy_without_consent, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(instleg_su_t6, institutional_legitimacy_without_consent, suppression_requirement, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_legitimacy_without_consent, enforcement_mechanism).
narrative_ontology:affects_constraint(institutional_legitimacy_without_consent, democratic_legitimacy_manufacture).
narrative_ontology:affects_constraint(institutional_legitimacy_without_consent, exit_prohibition_legal_framework).
narrative_ontology:affects_constraint(institutional_legitimacy_without_consent, patriotic_education_narrative_capture).
narrative_ontology:affects_constraint(institutional_legitimacy_without_consent, surveillance_infrastructure_asymmetry).

% DUAL FORMULATION NOTE:
% Institutional legitimacy-without-consent is upstream of more specific coordination and extraction mechanisms. This constraint families contains four sibling stories: (1) the formal legal structure of exit prohibition; (2) the education and narrative system that manufactures perceived legitimacy; (3) the surveillance and enforcement infrastructure that sustains suppression; (4) the democratic theater that creates performances of consent. Each sibling has its own ε value reflecting different empirical tractability. Declare each in separate JSON files linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_legitimacy_without_consent, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
