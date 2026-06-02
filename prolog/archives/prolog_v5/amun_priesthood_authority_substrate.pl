% ============================================================================
% CONSTRAINT STORY: amun_priesthood_authority_substrate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_amun_priesthood_authority_substrate, []).

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
 *   constraint_id: amun_priesthood_authority_substrate
 *   human_readable: Amun Priesthood Authority Substrate at Karnak
 *   domain: ancient_religion/institutional_authority
 *
 * SUMMARY:
 *   The Amun priesthood at Karnak accumulated land, ritual centrality, and
 *   operational authority over approximately 1200 years (late 18th Dynasty
 *   through Late Period, c. 1350-660 BCE) until it functioned as a
 *   quasi-independent institutional authority structurally parallel to
 *   pharaonic court. The constraint exemplifies how interpretive-accretion
 *   authority structures can accumulate operational power that becomes a
 *   binding constraint on the kernel-bearing authority. The priesthood
 *   derived its power from administering the Ma'at-Amun framework—the
 *   cosmological and political narrative that legitimated pharaonic rule.
 *   This created a mutual-hostage relationship: pharaonic legitimacy depended
 *   on priestly recognition and ritual validation; priestly authority
 *   depended on pharaonic land grants and continuous ritual sponsorship. The
 *   constraint demonstrates the tangled_rope structure in institutional
 *   authority itself: both pharaoh and priesthood have genuine coordination
 *   interests (maintaining religious legitimation system), yet both extract
 *   costs from the other (pharaoh loses direct land control and executive
 *   autonomy; priesthood remains dependent on pharaonic resource allocation).
 *   The peasantry, beneath both institutions, experiences the system as
 *   snare—dual extraction claims that are structurally indivisible and backed
 *   by both religious sanction and coercive enforcement. The constraint
 *   accumulated extractiveness over its interval: early New Kingdom
 *   priesthood was powerful but not yet autonomous (ε ≈ 0.28); by mid-New
 *   Kingdom the priesthood had acquired approximately 10-15% of arable land
 *   and significant administrative autonomy (ε ≈ 0.45-0.58); by Late Period
 *   the priesthood effectively controlled temple lands equivalent to state
 *   revenues and exercised veto power over pharaonic religious policy (ε ≈
 *   0.68). The theater ratio rose from ~0.32 (functional religious practice)
 *   to ~0.55 (increasingly performative ritual justifying administrative
 *   authority) as the practical coordination function was displaced by
 *   authority maintenance ritual.
 *
 * KEY AGENTS:
 *   - Amun Priesthood (Karnak Temple Complex): Primary beneficiary (institutional/arbitrage) — accumulates land grants, administrative autonomy, and ritual gatekeeping authority; experiences constraint as pure coordination (maintaining Ma'at-Amun framework)
 *   - Pharaonic Dynasty (Kernel Authority): Primary constraining victim (institutional/constrained) — legitimacy depends on priestly validation; loses direct control over lands granted to priesthood and must negotiate religious policy; experiences constraint as tangled_rope (coordinates with priesthood while being constrained by it)
 *   - Egyptian Peasantry: Secondary victim (powerless/trapped) — subject to dual extraction claims (pharaonic taxation and priestly temple obligations); no alternative social positions or geographic mobility; bears maximum extraction cost
 *   - Temple Administrative Apparatus: Secondary beneficiary (powerful/constrained) — administrators (both priestly and secular) control resource allocation, scheduling, and estate management; gatekeeping positions generate administrative rents; dependent on pharaonic sponsorship and priestly approval
 *   - Reform-Movement Factions (Late New Kingdom): Organized actors attempting authority redistribution (organized/constrained) — recognize the constraint as remediable through institutional restructuring; attempt to impose sunset clauses on land grants and administrative autonomy
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements (priesthood authority) as inevitable consequence of religious systems; false summit candidate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(amun_priesthood_authority_substrate, 0.58).
domain_priors:suppression_score(amun_priesthood_authority_substrate, 0.65).
domain_priors:theater_ratio(amun_priesthood_authority_substrate, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(amun_priesthood_authority_substrate, extractiveness, 0.58).
narrative_ontology:constraint_metric(amun_priesthood_authority_substrate, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(amun_priesthood_authority_substrate, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(amun_priesthood_authority_substrate, tangled_rope).
narrative_ontology:human_readable(amun_priesthood_authority_substrate, "Amun Priesthood Authority Substrate at Karnak").
narrative_ontology:topic_domain(amun_priesthood_authority_substrate, "ancient_religion/institutional_authority").

domain_priors:requires_active_enforcement(amun_priesthood_authority_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(amun_priesthood_authority_substrate, amun_priesthood).
narrative_ontology:constraint_beneficiary(amun_priesthood_authority_substrate, pharaonic_dynasty).
narrative_ontology:constraint_victim(amun_priesthood_authority_substrate, egyptian_peasantry).
narrative_ontology:constraint_victim(amun_priesthood_authority_substrate, pharaonic_executive_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EGYPTIAN PEASANTRY (SNARE) — Bears extraction through dual tribute claims (pharaonic and priestly). No exit from agricultural land-base. The joint pharaoh-priesthood system extracts maximum surplus; peasants experience this as immutable natural order enforced through both divine sanction and state violence. Suppression is structural: land ownership, corvée obligation, and religious prohibition of questioning divine hierarchy.
constraint_indexing:constraint_classification(amun_priesthood_authority_substrate, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PHARAONIC AUTHORITY (TANGLED ROPE) — Coordinates religious legitimation through Amun priesthood (genuine coordination function: Ma'at-Amun framework legitimates rule) while extracting concessions: land grants to priesthood reduce pharaonic direct control, priestly approval becomes constraint on pharaonic religious policy. Exit is constrained by dynastic succession and the necessity of religious legitimation — a pharaoh cannot rule without priestly validation. Benefits from the coordination (legitimacy transfer) but bears costs (land loss, authority dilution). Active enforcement required: pharaoh must continuously negotiate, grant lands, sponsor festivals to maintain priestly cooperation.
constraint_indexing:constraint_classification(amun_priesthood_authority_substrate, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AMUN PRIESTHOOD (ROPE) — Experiences the constraint as pure coordination: administering the Ma'at-Amun framework that legitimates pharaonic rule is their primary function. Benefits from land grants, ritual sponsorship, and operational autonomy in religious matters. Exit options are arbitrage-class: priesthood can shift allegiance among competing dynastic claimants, can expand or contract ritual support, can interpret divine will to favor or constrain pharaonic initiatives. The priesthood sees themselves as coordinating a sacred system, not extracting from it — their extraction of resources from peasants is justified as necessary for maintaining cosmic order.
constraint_indexing:constraint_classification(amun_priesthood_authority_substrate, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: TEMPLE ADMINISTRATIVE APPARATUS (TANGLED ROPE) — Coordinates the operational system for collecting, storing, and distributing temple resources while extracting administrative prerogatives and resource control. Temple administrators (both priestly and secular) benefit from their gatekeeping role over agricultural surplus, shrine maintenance, and ritual scheduling. Constrained by dependence on pharaonic sponsorship and priestly approval. The apparatus experiences the constraint as mixed: genuine coordination problem (how to manage vast estates and ritual calendar) combined with opportunity for administrative rent-seeking.
constraint_indexing:constraint_classification(amun_priesthood_authority_substrate, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: OFFICIAL PRIESTHOOD RITUAL (PITON) — By Late Period, the elaborate rituals performed at Karnak become increasingly performative theater: the gods are believed to be physically sustained by ritual offering, but the practical administrative work of managing the priesthood and estates increasingly drives the ritual schedule rather than cosmic necessity. The ritual apparatus persists through institutional inertia and because it justifies resource concentration. Theater ratio reflects the gap between mythological justification (cosmic order maintenance) and operational reality (estate administration). The ritual is degraded not in failure but in function displacement — it has become theater maintaining the authority structure rather than mechanism sustaining the cosmos.
constraint_indexing:constraint_classification(amun_priesthood_authority_substrate, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale, the constraint appears immutable: all complex hierarchical societies require religious legitimation of authority, and religious institutions necessarily accumulate power in the process of providing that legitimation. This perspective risks naturalizing what is contingent: the specific form of the Amun priesthood's authority (land accumulation, administrative autonomy, ritual gatekeeping) is not inherent to religious systems but emerged from historical contingencies (Akhenaten's Aten experiment weakened traditional priesthoods, New Kingdom military expansion generated surplus for temple acquisition). The engine's false summit detection will flag this perspective as naturalizing a constructed institutional arrangement.
constraint_indexing:constraint_classification(amun_priesthood_authority_substrate, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: REFORM-MOVEMENT FACTION (SCAFFOLD) — Late New Kingdom attempts to constrain priesthood authority (Ramses III's efforts to limit Amun land grants, later administrative reforms) represent a scaffold perspective: the constraint is recognized as temporary dysfunction remediable through institutional restructuring. These reformers see the pharaonic-priestly imbalance as a solvable coordination problem with finite sunset. The reforms have sunset logic: if successful redistribution of authority were achieved, the tangled_rope would degrade to pure rope (coordination without asymmetric extraction). The reforms ultimately fail (priestly authority continues rising through Late Period), making them an abortive scaffold — the sunset was anticipated but not reached.
constraint_indexing:constraint_classification(amun_priesthood_authority_substrate, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(amun_priesthood_authority_substrate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(amun_priesthood_authority_substrate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(amun_priesthood_authority_substrate, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(amun_priesthood_authority_substrate, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(amun_priesthood_authority_substrate, TR),
    TR >= 0.70.

:- end_tests(amun_priesthood_authority_substrate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting the asymmetric resource extraction from peasantry (snare-level) combined with the constrained mutual extraction between pharaoh and priesthood (tangled_rope-level). The value reflects the interval endpoint (750 years into the constraint's accumulation). Early extractiveness was lower (0.28) because priesthood authority was still emerging; as land accumulation and administrative autonomy increased, extractiveness rose to 0.58 by 500 years in. Suppression (0.65): High. Multiple suppression mechanisms reinforce the constraint: (1) Structural: peasants lack exit options through land ownership, geographic mobility is restricted by state control, corvée obligations are legally codified. (2) Institutional: pharaonic military and bureaucratic enforcement of tax collection and temple obligations. (3) Ideological: Ma'at-Amun framework presents the system as cosmically necessary; religious texts emphasize peasants' duty to support divine sustenance through offerings. The suppression does not reach maximum (0.85+) because some exit mechanisms exist (migration to frontier areas, participation in non-sanctioned cults, occasional resistance) and because the system's own legitimation narrative creates internal contradictions that fuel reform movements. Theater ratio (0.48): Moderate. The constraint exhibits genuine coordination function (Ma'at-Amun framework does coordinate political legitimation) alongside growing performative theater (elaborate rituals increasingly justified not as cosmic necessity but as demonstration of pharaonic piety). The theater ratio has risen from 0.32 to 0.55 over the interval, indicating displacement of functional coordination by ritual performance. The 0.48 endpoint value represents the point where functionality and performance are roughly balanced—functional enough to maintain the system's legitimacy narrative, performative enough that the ritual apparatus could be significantly streamlined without affecting actual coordination outcomes.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence across power asymmetry. The priesthood (institutional/arbitrage exit) perceives rope: genuine coordination function (Ma'at-Amun legitimation), voluntary resource transfer justified by cosmic necessity, beneficiary status. The pharaoh (institutional/constrained exit) perceives tangled_rope: benefits from legitimation (necessity) but constrained by priestly approval and land loss (costs); cannot exit without institutional collapse; active enforcement required to maintain balance. The peasantry (powerless/trapped exit) perceives snare: pure extraction with no coordination benefit, suppression structural and ideological, no exit except death or flight. The temple apparatus (powerful/constrained) perceives tangled_rope: coordinates administrative function while gatekeeping resource flows; benefits from rent-seeking but dependent on pharaonic sponsorship. The reform faction (organized/constrained) perceives scaffold: views the imbalance as remediable through authority restructuring; assumes sunset logic (if pharaonic reforms succeed, constraint converts from tangled_rope to rope). The analytical observer (analytical/analytical) risks false summit (mountain): naturalizing the priesthood's authority as inevitable feature of religious systems, obscuring the contingent policy choices that enabled accumulation. The perspectival gaps between institutional actors (priesthood vs pharaoh) emerge through directionality differences: priesthood's beneficiary+arbitrage position (d ≈ 0.10) produces rope-classification; pharaoh's constrained+mixed-victim position (d ≈ 0.45) produces tangled_rope-classification; same institutional power level but opposite exit-option and beneficiary/victim status.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) maps their power level + exit options + beneficiary/victim status to a position in [0.0, 1.0], where 0.0 = full beneficiary (constraint subsidizes them) and 1.0 = full target (constraint extracts from them). The sigmoid function f(d) transforms d to an effective power modifier that scales extractiveness. Amun Priesthood: beneficiary + institutional power + arbitrage exit → d ≈ 0.10 (low, near beneficiary end). They set policy boundaries (what counts as proper Ma'at), control resource distribution, and can shift allegiance. Pharaonic Authority: mixed (benefits from legitimation, constrained by priestly veto) + institutional power + constrained exit → d ≈ 0.45 (moderate, closer to symmetric). They need priestly cooperation and cannot walk away, but they retain ultimate enforcement power. Egyptian Peasantry: victims + powerless + trapped exit → d ≈ 0.92 (high, near target end). They bear extraction from both institutions, have no exit option (land-bound, corvée-obligated), and have no political power. The sigmoid f(d) converts these d values to multipliers: low d produces f(d) < 0.65 (beneficiary discount or institutional prestige effect); moderate d produces f(d) ≈ 0.65-0.85 (balanced experience); high d produces f(d) > 1.0 (amplified extraction effect). Combined with base extractiveness (0.58) and scope modifier (national ≈ 1.0), the effective extraction chi varies by position: priesthood experiences χ ≈ 0.58 × 0.40 × 1.0 ≈ 0.23 (negative effective extraction, experiences system as beneficial); pharaoh experiences χ ≈ 0.58 × 0.75 × 1.0 ≈ 0.44 (moderate positive, constrained); peasantry experiences χ ≈ 0.58 × 1.28 × 1.0 ≈ 0.74 (high positive, pure extraction). This explains why the same constraint produces different classification types from different positions: the structural extraction flow is real and unidirectional (toward priesthood and pharaoh from peasants), but the experience depends on where you stand in the flow.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy—the apparent logical impossibility of a constraint being simultaneously 'coordination' (rope) and 'extraction' (snare)—by demonstrating that both classification types are correct from different observational positions. The constraint IS coordination from the priesthood's perspective: administering the Ma'at-Amun framework solves a real institutional problem (how to legitimate pharaonic authority), and the priesthood genuinely coordinates this function. The constraint IS extraction from the peasantry's perspective: they bear costs (agricultural surplus, labor obligations, ritual duties) with no direct benefit. The constraint IS tangled_rope from the pharaoh's perspective: it coordinates legitimation (genuine benefit) while constraining executive authority (genuine cost). The mandatrophy resolution shows that the six-type taxonomy avoids both false-unity (insisting there is one 'correct' classification) and false-relativism (treating all perspectives as equally valid). Instead: (1) the constraint's objective properties (ε=0.58, suppression=0.65) are fixed; (2) the classification depends on the observer's structural position (beneficiary/victim status, power level, exit options); (3) the divergence between perspectives reveals the structure that a single-position analysis would hide. The false summit perspective (mountain classification from civilizational analytical view) reveals the constraint's legitimating narrative naturalizes what is actually contingent institutional arrangement. The analytical observer sees the constraint as immutable law of religious systems, but the structural data shows it is remediable through political choice (pharaonic reform could restructure land grants, administrative autonomy could be constrained, the Amun priesthood's authority is reversible). The mandatrophy is fully resolved by the presheaf representation: six valid types, each from a specific (P, T, E, S) position, each with justified classification logic, no contradiction because the types are perspectival, not mutually exclusive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimation_necessity_vs_extraction,
    'Is the priesthood''s authority accumulation an inevitable consequence of providing religious legitimation, or a contingent historical outcome that could have been otherwise?',
    'Comparative institutional analysis: examination of other ancient states'' religious authority structures (Mesopotamian temples, Hittite priesthoods, later Islamic waqf systems) to identify whether authority accumulation is structurally inevitable or contingent on specific choices (land-grant policies, ritual centralization, absence of competing authorities)',
    'If inevitable: constraint approaches mountain status (inherent to religious legitimation systems). If contingent: constraint is tangled_rope sustained by policy choices and remains remediable through authority restructuring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimation_necessity_vs_extraction, empirical, 'Whether priesthood authority accumulation is inevitable or contingent').

omega_variable(
    dual_extraction_mechanism,
    'Do pharaonic and priestly extraction claims operate as independent mechanisms or as unified system where peasants experience them as indivisible?',
    'Analysis of tax/tribute documents and temple records to determine whether peasants paid dual distinct taxes or a single bundled obligation. Examination of resistance patterns and peasant coping strategies to identify whether they distinguish pharaonic vs priestly claims or treat the system as unified extraction.',
    'If independent: constraint decomposes into two separate snare constraints (pharaonic and priestly), each with different ε values. If unified: single snare constraint with higher effective suppression because the dual-claim system is less escapable than either alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dual_extraction_mechanism, empirical, 'Whether pharaonic and priestly extraction claims operate independently or as unified system').

omega_variable(
    mutual_hostage_stability,
    'Does the mutual-dependence relationship between pharaoh and priesthood create equilibrium (stable tangled_rope) or precarious zero-sum competition (latent snare)?',
    'Historical trajectory analysis: examination of cases where pharaonic-priestly balance shifts (Akhenaten''s break with Amun, Ramses III''s attempted reforms, Late Period priestly dominance). Identification of whether imbalances are self-correcting (equilibrium) or ratcheting (one-way power transfer).',
    'If equilibrium: tangled_rope classification holds — both institutions have structural incentive to maintain the balance. If ratcheting: the constraint conceals underlying snare (priesthood gradually captures pharaonic authority, or pharaoh subordinates priesthood to symbolic status). Current historical evidence shows ratcheting toward priestly dominance, suggesting the tangled_rope is unstable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mutual_hostage_stability, empirical, 'Whether pharaonic-priestly mutual dependence creates stable equilibrium').

omega_variable(
    peasant_alternative_narratives,
    'Did peasants internalize the Ma''at-Amun framework as legitimate divine necessity, or perceive it as coercive authority justified through religious theater?',
    'Examination of peasant-generated sources (rare but present in magical texts, graffiti, oral-tradition fragments recovered through ethnographic analogy) and indirect evidence from resistance patterns (flight, non-compliance, participation in non-sanctioned cults). Analysis of whether peasant religious participation shows identification with the system or constraint-driven compliance.',
    'If internalized: suppression has strong identity-lock component, making the snare more stable and the constraint closer to cultural natural law. If coercive: suppression is purely structural, the constraint is recognized as extraction, and the snare is vulnerable to delegitimation if religious authority fails.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(peasant_alternative_narratives, empirical, 'Whether peasants internalized the legitimating framework or perceived it as theater').

omega_variable(
    administrative_efficiency_versus_extraction,
    'Did the priesthood''s centralization of administrative functions actually improve resource allocation efficiency (genuine coordination benefit), or serve primarily to enable resource concentration (extraction justification)?',
    'Comparative analysis of temple estates'' productivity vs. pharaonic state farms and private holdings in same period. Examination of whether centralized administration reduced transaction costs or created administrative overhead that disproportionately benefited the priesthood.',
    'If efficiency-enhancing: the coordination component of tangled_rope is genuine and substantial. If primarily extractive: the coordination component is thin, and the constraint is closer to snare with coordination theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrative_efficiency_versus_extraction, empirical, 'Whether priesthood centralization improved administrative efficiency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(amun_priesthood_authority_substrate, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(amun_tr_t0, amun_priesthood_authority_substrate, theater_ratio, 0, 0.32).
narrative_ontology:measurement(amun_tr_t250, amun_priesthood_authority_substrate, theater_ratio, 250, 0.4).
narrative_ontology:measurement(amun_tr_t500, amun_priesthood_authority_substrate, theater_ratio, 500, 0.48).
narrative_ontology:measurement(amun_tr_t750, amun_priesthood_authority_substrate, theater_ratio, 750, 0.55).

% Extraction over time
narrative_ontology:measurement(amun_be_t0, amun_priesthood_authority_substrate, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(amun_be_t250, amun_priesthood_authority_substrate, base_extractiveness, 250, 0.45).
narrative_ontology:measurement(amun_be_t500, amun_priesthood_authority_substrate, base_extractiveness, 500, 0.58).
narrative_ontology:measurement(amun_be_t750, amun_priesthood_authority_substrate, base_extractiveness, 750, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(amun_priesthood_authority_substrate, identity_coordination).
narrative_ontology:affects_constraint(amun_priesthood_authority_substrate, pharaonic_succession_legitimacy).
narrative_ontology:affects_constraint(amun_priesthood_authority_substrate, temple_labor_extraction).
narrative_ontology:affects_constraint(amun_priesthood_authority_substrate, agricultural_surplus_distribution).

% DUAL FORMULATION NOTE:
% The Amun priesthood constraint is upstream of pharaonic legitimacy (the priesthood's authority depends on validating pharaonic claims) and downstream of agricultural surplus (the priesthood depends on surplus extraction through dual tax mechanisms). This story represents the priesthood as institutional actor; separate stories model pharaonic succession constraints and peasant labor extraction to capture their distinct ε values and measurement profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(amun_priesthood_authority_substrate, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
