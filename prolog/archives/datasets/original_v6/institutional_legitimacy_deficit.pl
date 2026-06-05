% ============================================================================
% CONSTRAINT STORY: institutional_legitimacy_deficit
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_legitimacy_deficit, []).

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
 *   constraint_id: institutional_legitimacy_deficit
 *   human_readable: Institutional Legitimacy Deficit
 *   domain: institutional/political/governance
 *
 * SUMMARY:
 *   Institutional legitimacy deficit describes the structural gap between
 *   institutional claims to democratic authority and the actual distribution
 *   of power within decision-making processes. This constraint operates
 *   across all institutional contexts — governments, corporations,
 *   universities, NGOs — wherever formal procedures and rhetorical
 *   commitments to participation coexist with concentrated power. The deficit
 *   creates extractive dynamics: excluded populations bear the costs of
 *   institutional decisions while having no meaningful voice in them, yet the
 *   institution's legitimating narrative claims to represent 'the public
 *   interest' or 'all stakeholders.' The constraint exhibits all six DR types
 *   depending on observer position: it appears as snare (powerless agents
 *   trapped with no exit), tangled rope (mid-level participants experiencing
 *   mixed coordination and extraction), rope (incumbent leaders securing
 *   power through legitimacy mechanisms), scaffold (reform coalitions working
 *   to expand actual participation), piton (procedural rituals maintaining
 *   appearance of legitimacy through inertia), and false-naturalized mountain
 *   (treating power asymmetries as inevitable). The theater ratio trend (0.42
 *   → 0.68) reflects the layering of legitimacy mechanisms: as direct power
 *   becomes more visible and contested, institutions respond by multiplying
 *   procedural theaters (public consultations, advisory boards, stakeholder
 *   engagement, equity task forces) whose primary function is to absorb
 *   dissent while preserving actual decision-making structures. The
 *   extractiveness trend (0.35 → 0.58) reflects the escalation of the
 *   constraint: initial legitimacy deficits create excluded populations;
 *   excluded populations mobilize for voice; institutions respond with
 *   procedural expansion rather than power-sharing; the gap between form and
 *   substance grows wider, requiring more theater to maintain appearance;
 *   extraction increases as the energy cost of manufacturing consent rises.
 *
 * KEY AGENTS:
 *   - Excluded Populations: Primary victims (powerless/trapped) — bear costs of institutional decisions with no meaningful decision-making power; systematically prevented from exiting or voicing dissent
 *   - Incumbent Power Holders: Primary beneficiaries (institutional/arbitrage) — maintain power through legitimacy mechanisms; have high exit options and ability to restructure institutions
 *   - Mid-Level Institutional Participants: Secondary actors (moderate/constrained) — face coordination needs (service delivery, resource allocation) alongside participation in power structures they don't control; career and status dependent on institutional continuity
 *   - Reform Coalitions and Accountability Movements: Organized actors (organized/mobile) — mobilize to expand actual participation and transparency; see legitimacy deficit as a solvable institutional design problem with clear sunset mechanisms
 *   - Procedural Legitimacy Rituals: Institutional mechanisms (institutional/constrained) — public consultations, advisory boards, stakeholder engagement processes that persist through inertia despite documented failure to shift power
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent power arrangements as inevitable features of governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_legitimacy_deficit, 0.58).
domain_priors:suppression_score(institutional_legitimacy_deficit, 0.65).
domain_priors:theater_ratio(institutional_legitimacy_deficit, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_legitimacy_deficit, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_legitimacy_deficit, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(institutional_legitimacy_deficit, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_legitimacy_deficit, tangled_rope).
narrative_ontology:human_readable(institutional_legitimacy_deficit, "Institutional Legitimacy Deficit").
narrative_ontology:topic_domain(institutional_legitimacy_deficit, "institutional/political/governance").

domain_priors:requires_active_enforcement(institutional_legitimacy_deficit).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_legitimacy_deficit, incumbent_power_holders).
narrative_ontology:constraint_beneficiary(institutional_legitimacy_deficit, institutional_administrators).
narrative_ontology:constraint_victim(institutional_legitimacy_deficit, excluded_populations).
narrative_ontology:constraint_victim(institutional_legitimacy_deficit, institutional_trust).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED POPULATIONS (SNARE) — Populations systematically excluded from meaningful participation in institutional decision-making face extraction with minimal coordination benefit. They cannot exit the institutional system without bearing severe costs (loss of services, legal penalties, social isolation). The constraint operates through deliberate suppression of alternative pathways and narrative denial of the exclusion itself. Maximum experienced extraction from those with no structural voice.
constraint_indexing:constraint_classification(institutional_legitimacy_deficit, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-LEVEL PARTICIPANTS (TANGLED ROPE) — Career professionals and community leaders within institutions face genuine coordination needs (coordinating resources, maintaining service delivery) but also experience asymmetric extraction through opaque decision-making and manufactured consent. They have some agency and access to information, but exit costs are high (career damage, loss of status). Experiences both the coordination function and the extraction mechanism.
constraint_indexing:constraint_classification(institutional_legitimacy_deficit, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT POWER HOLDERS (ROPE) — Institutional leadership experiences the legitimacy constraint primarily as a coordination mechanism: maintaining consistency in rules, messaging, and authority structures. They have high exit options (can relocate, restructure, capture alternative institutions). The constraint secures their position without appearing coercive — legitimacy as cover for authority. Net beneficiary position with maximal agency.
constraint_indexing:constraint_classification(institutional_legitimacy_deficit, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORM COALITIONS (SCAFFOLD) — Organized reform movements, transparency advocates, and accountability-focused coalitions view the legitimacy deficit as a temporary institutional failure with a sunset clause. Participatory governance, open-data mandates, and truth commissions represent alternative pathways. The scaffold classification reflects the coalition's agency and clarity of exit mechanism: institutional design can shift from opaque to transparent through deliberate reform. Theater is elevated because reform rhetoric often substitutes for substantive change, but the coalition's mobilization provides genuine agency.
constraint_indexing:constraint_classification(institutional_legitimacy_deficit, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: PROCEDURAL RITUALS (PITON) — Public consultations, advisory boards, and stakeholder engagement processes persist as institutional theater despite their well-documented failure to shift power dynamics. These procedures maintain appearance of legitimacy while preserving the actual decision-making structure unchanged. Theater ratio high because the functional coordination value has atrophied; the rituals persist through institutional inertia and because alternatives aren't fully established. The constraint is maintained by ceremonial repetition, not by structural necessity.
constraint_indexing:constraint_classification(institutional_legitimacy_deficit, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some legitimacy deficit is claimed to be inherent to complex governance: perfect transparency is impossible, complete participation is impractical, power asymmetries are inevitable. This perspective naturalizes the deficit as an immutable feature of human organization. However, comparative institutional analysis and historical shifts in legitimacy mechanisms reveal this as false naturalization — legitimacy deficits vary dramatically across institutional types and time periods. The mountain classification indicates the risk of treating contingent power arrangements as laws of nature.
constraint_indexing:constraint_classification(institutional_legitimacy_deficit, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_legitimacy_deficit_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_legitimacy_deficit, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_legitimacy_deficit, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_legitimacy_deficit, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_legitimacy_deficit, TR),
    TR >= 0.70.

:- end_tests(institutional_legitimacy_deficit_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant benefit for incumbent power holders through concentrated authority while imposing costs on excluded populations. However, extraction is not maximal because: (a) incumbents must maintain legitimacy performance, which requires some responsiveness to excluded populations; (b) reform coalitions have successfully shifted some domains toward greater transparency; (c) excluded populations retain some agency through protest and alternative institutions. Suppression (0.65): Moderate-high. The constraint operates through multiple suppression mechanisms: structural barriers to participation (time, expertise, access), social penalties for dissent, narrative denial of exclusion, and co-option of dissenting voices into procedural theaters. The suppression is not total — information about power asymmetries is available, organizing is possible — but the costs of challenging the system are high. Theater ratio (0.68): High. Institutional legitimacy increasingly relies on procedural theater rather than actual power-sharing. Public consultations, diversity initiatives, stakeholder committees, and participatory processes proliferate as the legitimacy deficit deepens — each new procedure is presented as addressing the problem, while actual decision-making structures remain concentrated. Theater rises over the interval as institutions respond to visibility of power asymmetries by multiplying appearance-management mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — institutional decision-making concentrated in the hands of power holders while requiring rhetorical commitment to serving 'the public' — classifies as six different types depending on structural position. The perspectival gap reveals that the institution's legitimacy claim is doing critical work: it allows incumbent power holders to experience the concentration of authority as coordination (rope), while excluded populations experience it as extraction (snare). The gap is not about disagreement on facts — both positions observe the same power asymmetry — but about structural position relative to the extraction mechanism. The reform coalition sees the gap as solvable (scaffold), while the procedural ritual sees it as managed through theater (piton). The analytical observer risks falsely naturalizing the structure as inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures the structural position of each agent in the extraction flow. Excluded populations with no meaningful exit (trapped) and positioned as victims of the constraint derive high d (0.92+), producing maximum f(d) ≈ 1.42. Incumbent power holders positioned as beneficiaries with arbitrage exit options derive low d (~0.05), producing negative f(d) ≈ -0.12. Mid-level participants with constrained exit and mixed structural position derive moderate d (~0.55), producing moderate f(d) ≈ 0.75. Reform coalitions with mobile exit and organized power derive lower-moderate d (~0.40), producing f(d) ≈ 0.40. The procedural rituals (piton classification) are not agents in the derivation chain — they are mechanisms the institution uses to manage the legitimacy deficit. Their classification reflects theater_ratio exceeding 0.70 and loss of primary function, not through directionality calculation. The analytical observer with analytical exit derives moderate d (~0.72), producing f(d) ≈ 1.15.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: This constraint resolves the mandatrophy by clarifying that institutional legitimacy deficits are structurally tangled_rope at the base level (genuine coordination functions coexist with asymmetric extraction), but appear as different types from different structural positions. The snare perspective captures the victim-side reality; the rope perspective captures the beneficiary-side reality; the scaffold captures the reform possibility; the piton captures the institutional decay pattern. None of these is 'the real type' — each is a legitimate reading of the same constraint structure from a different position. The analytical observer's mountain classification is false naturalization (the deficit is not inherent to governance, as shown by historical variation in institutional legitimacy). The mandatrophy is resolved by the presheaf structure: the constraint's true classification IS the full map of perspectives showing how power positions determine institutional experience.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_deficit_measurement,
    'What observable distinguishes a genuine institutional legitimacy deficit from the normal gap between ideal and actual governance?',
    'Comparative analysis of institutions with high public trust vs. low public trust; tracking whether procedural changes correlate with trust recovery or remain cosmetic',
    'If legitimacy deficit is universal: constraint reclassifies from tangled_rope toward mountain. If deficit is concentrated in specific institutional types: constraint decomposes into separate stories per domain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_deficit_measurement, empirical, 'How to distinguish actual legitimacy deficit from normal governance friction').

omega_variable(
    participation_capture_vs_genuine,
    'Does formal inclusion of excluded populations in institutional processes constitute genuine power-sharing or merely absorb dissent through participation theater?',
    'Analysis of decision outcomes: do policies shift when excluded groups participate in consultation vs. when they are excluded? Does participation reduce or merely channel protest?',
    'If genuine: some legitimacy deficit resolves through expanded participation (tangled_rope → rope). If theater: expanded participation deepens the snare by legitimating decisions that excluded groups had no actual power over.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(participation_capture_vs_genuine, empirical, 'Whether formal participation constitutes genuine power-sharing').

omega_variable(
    institutional_exit_feasibility,
    'Can excluded populations realistically exit or alternative-institution around the constraint (local governance, mutual aid, exit to other jurisdictions), or is the trapped classification accurate?',
    'Historical analysis of successful institutional alternatives; cost-benefit analysis of exit vs. participation in contested institutions',
    'If exit feasible: powerless agents reclassify toward constrained or mobile; snare classification weakens. If exit systematically prevented: snare classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_exit_feasibility, empirical, 'Whether powerless agents have realistic institutional exit options').

omega_variable(
    reform_sunset_actualization,
    'Do reform movements and transparency initiatives actually shift institutional decision-making structures, or do they degrade into piton-style rituals?',
    'Longitudinal analysis of reform initiatives: power distribution before/after reforms; tracking whether procedural changes outlast initial reform period',
    'If reforms actualize: scaffold classification valid — legitimacy deficit has real sunset mechanism. If reforms degrade: scaffold is aspirational; the constraint reclassifies toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_sunset_actualization, empirical, 'Whether institutional reforms produce sustained power-sharing or degrade to ritual').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_legitimacy_deficit, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legit_tr_t0, institutional_legitimacy_deficit, theater_ratio, 0, 0.42).
narrative_ontology:measurement(legit_tr_t15, institutional_legitimacy_deficit, theater_ratio, 15, 0.61).
narrative_ontology:measurement(legit_tr_t30, institutional_legitimacy_deficit, theater_ratio, 30, 0.68).
narrative_ontology:measurement(legit_tr_t45, institutional_legitimacy_deficit, theater_ratio, 45, 0.65).

% Extraction over time
narrative_ontology:measurement(legit_be_t0, institutional_legitimacy_deficit, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legit_be_t15, institutional_legitimacy_deficit, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(legit_be_t30, institutional_legitimacy_deficit, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(legit_be_t45, institutional_legitimacy_deficit, base_extractiveness, 45, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_legitimacy_deficit, enforcement_mechanism).
narrative_ontology:affects_constraint(institutional_legitimacy_deficit, democratic_deficit).
narrative_ontology:affects_constraint(institutional_legitimacy_deficit, regulatory_capture).
narrative_ontology:affects_constraint(institutional_legitimacy_deficit, institutional_inertia).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
