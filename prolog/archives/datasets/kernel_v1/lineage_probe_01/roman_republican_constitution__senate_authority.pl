% ============================================================================
% CONSTRAINT STORY: roman_republican_constitution__senate_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_roman_republican_constitution__senate_authority, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: roman_republican_constitution__senate_authority
 *   human_readable: Senate Authority in the Roman Republic: Auctoritas and Unelected Permanence
 *   domain: political/historical/constitutional
 *
 * SUMMARY:
 *   The Roman Senate's authority derived not from formal legal power but from
 *   auctoritas — a prestige-based weight that steered the Republic's finance,
 *   war-making, and religious interpretation for nearly five centuries (5th
 *   century BCE to 1st century BCE). This reading instantiates one
 *   constitutional framing of the Republic: that its foundational principle
 *   was the permanence and accumulated wisdom of the Senate, which could
 *   guide magistrates and popular assemblies through moral suasion rather
 *   than command. The constraint exhibits both genuine coordination (the
 *   Senate did solve problems of inter-magistrate conflict and temporal
 *   continuity that annual magistracies could not) and asymmetric extraction
 *   (the Senate's control over financial decisions and religious
 *   interpretation created barriers to agency for magistrates and assemblies
 *   acting against its consensus). The senatorial order benefited from
 *   permanence and unaccountability; magistrates and popular bodies
 *   experienced suppression through prestige-based veto and resource denial
 *   rather than explicit prohibition. Extractiveness accumulated over the
 *   interval as the Senate's financial and religious control intensified,
 *   peaking in the late Republic when senatorial dominance provoked the
 *   Gracchi and populares movements.
 *
 * KEY AGENTS:
 *   - Senatorial Order: Primary beneficiary (institutional/arbitrage) — permanent council extracting deference and policy guidance through auctoritas without formal accountability
 *   - Magistracies (consuls, praetors, etc.): Primary victims (moderate/constrained) — annual offices formally holding executive power but constrained by Senate financial control and prestige-based veto
 *   - Popular Assemblies: Secondary victims (moderate/constrained) — formally sovereign in legislation and magistrate election, but the Senate's control of interpretation and resource allocation limited effective agency
 *   - Tribunes of the Plebs: Organized victims (organized/constrained) — sacrosanct veto power offset by Senate's financial and religious dominance; gradual extractive concessions over centuries
 *   - Plebeian Factions and Popular Movements: Organized challengers (organized/constrained) — Gracchi and populares movements extracted concessions through sustained resistance to senatorial control
 *   - Republic's Formal Institutions: Apparatus (institutional/arbitrage) — magistracies, assemblies, and procedural rules persisted as legitimating theater through which Senate rule actually operated
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing senatorial authority as an architectural necessity rather than contingent historical framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roman_republican_constitution__senate_authority, 0.52).
domain_priors:suppression_score(roman_republican_constitution__senate_authority, 0.58).
domain_priors:theater_ratio(roman_republican_constitution__senate_authority, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roman_republican_constitution__senate_authority, extractiveness, 0.52).
narrative_ontology:constraint_metric(roman_republican_constitution__senate_authority, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(roman_republican_constitution__senate_authority, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(roman_republican_constitution__senate_authority, tangled_rope).
narrative_ontology:human_readable(roman_republican_constitution__senate_authority, "Senate Authority in the Roman Republic: Auctoritas and Unelected Permanence").
narrative_ontology:topic_domain(roman_republican_constitution__senate_authority, "political/historical/constitutional").

domain_priors:requires_active_enforcement(roman_republican_constitution__senate_authority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(roman_republican_constitution__senate_authority, 'c7c18efb-b68b-48f7-9509-d69061eb53c3').
narrative_ontology:cs_kernel_codification('c7c18efb-b68b-48f7-9509-d69061eb53c3', distributed).
narrative_ontology:cs_authority_grounding('c7c18efb-b68b-48f7-9509-d69061eb53c3', lineage).
narrative_ontology:cs_interpretation_layer_present('c7c18efb-b68b-48f7-9509-d69061eb53c3').
narrative_ontology:cs_reading_relation('c7c18efb-b68b-48f7-9509-d69061eb53c3', roman_republican_constitution__crisis_machinery, coexists_with).
narrative_ontology:cs_reading_relation('c7c18efb-b68b-48f7-9509-d69061eb53c3', roman_republican_constitution__legal_codification_twelve_tables, coexists_with).
narrative_ontology:cs_reading_relation('c7c18efb-b68b-48f7-9509-d69061eb53c3', roman_republican_constitution__magistracies_and_collegiality, coexists_with).
narrative_ontology:cs_reading_relation('c7c18efb-b68b-48f7-9509-d69061eb53c3', roman_republican_constitution__popular_assemblies_and_tribunate, coexists_with).
narrative_ontology:cs_axiom('c7c18efb-b68b-48f7-9509-d69061eb53c3', foundational, senate_permanence_foundational).
narrative_ontology:cs_axiom_status(senate_permanence_foundational, holdable).
narrative_ontology:cs_axiom_grounding('c7c18efb-b68b-48f7-9509-d69061eb53c3', senate_permanence_foundational, conventional).
narrative_ontology:cs_axiom('c7c18efb-b68b-48f7-9509-d69061eb53c3', foundational, auctoritas_not_command).
narrative_ontology:cs_axiom_status(auctoritas_not_command, holdable).
narrative_ontology:cs_axiom_grounding('c7c18efb-b68b-48f7-9509-d69061eb53c3', auctoritas_not_command, conventional).
narrative_ontology:cs_reference_frame('c7c18efb-b68b-48f7-9509-d69061eb53c3', senate_stewardship_legitimacy).
narrative_ontology:cs_drift_state('c7c18efb-b68b-48f7-9509-d69061eb53c3', late_republic_crisis, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('c7c18efb-b68b-48f7-9509-d69061eb53c3', '').
narrative_ontology:cs_kernel_id(roman_republican_constitution__senate_authority, roman_republican_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(roman_republican_constitution__senate_authority, senatorial_order).
narrative_ontology:constraint_victim(roman_republican_constitution__senate_authority, magistracies_and_assemblies).
narrative_ontology:constraint_victim(roman_republican_constitution__senate_authority, plebeian_agencies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MAGISTRATE OPPOSING SENATE (SNARE) — The annual magistrate who attempts policy against senatorial consensus faces absolute suppression through prestige-based veto and financial starvation. No legal recourse exists; the constraint operates through reputation and resource denial rather than formal law. Maximum experienced extraction — the magistrate is trapped within the constitutional order yet wholly subordinated to unelected permanence.
constraint_indexing:constraint_classification(roman_republican_constitution__senate_authority, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: POPULAR ASSEMBLY AND TRIBUNES (TANGLED ROPE) — The assemblies formally possessed sovereign legislative power and the tribunes held sacrosanct veto authority, yet the Senate's financial control and prestige overwhelmed these formal powers. Genuine coordination exists: the Senate communicates war strategy, directs resource allocation, and shapes religious interpretation through which the assemblies and tribunes govern. But the coordination is asymmetric — the Senate extracts policy deference without formal accountability. Constrained exit: assemblies could theoretically resist, but financial starvation and social ostracism exact heavy costs.
constraint_indexing:constraint_classification(roman_republican_constitution__senate_authority, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SENATORIAL ORDER (ROPE) — The permanent council experiences the constraint as pure coordination: steering finance, war, and religion through auctoritas (weight of authority) rather than legal command. The Senate benefits from its permanence and accumulated prestige, which allows it to guide magistrates and assemblies without holding formal power. Arbitrage exit: senators could lose position through disgrace but retain social standing; institutional continuity is assured. Net beneficiary — extraction flows toward the Senate, not away from it.
constraint_indexing:constraint_classification(roman_republican_constitution__senate_authority, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PLEBEIAN ORGANIZED RESISTANCE (TANGLED ROPE) — Over centuries, plebeian organizations (tribunes, assemblies, eventually the Gracchi and populares movements) achieved coordination with the Senate while simultaneously extracting concessions. The constraint functioned as hybrid: the Senate needed plebeian military participation and tax compliance, creating genuine negotiating space. But the Senate's control of finance and religious interpretation meant concessions were always constrained. Organized agents with sustained pressure gradually shifted extraction ratios, showing that institutional entrenchment is not immobile — it yields to organized persistence over generational time.
constraint_indexing:constraint_classification(roman_republican_constitution__senate_authority, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REPUBLIC'S FORMAL CONSTITUTIONAL APPARATUS (PITON) — At civilizational scope, the formal magistracies, assemblies, and tribunes appear largely performative. The Republic's written and unwritten rules prescribed legislative sovereignty and procedural rights, yet the Senate's auctoritas rendered these procedures theater. The constraint persists through institutional inertia — the formal apparatus continues to function because it is the legitimating ritual through which the Senate actually rules. Theater ratio is moderate (0.35) because the formal procedures did have real function (elections did select magistrates, assemblies did debate), but their functional scope was bounded by senatorial override through prestige and financial control.
constraint_indexing:constraint_classification(roman_republican_constitution__senate_authority, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational universal perspective, the Senate's authority represents an immutable principle: any republic of sufficient complexity requires a permanent deliberative body to coordinate policy across magistracies and time horizons. The Republic's constitutional framework can take many forms — crisis machinery, legal codification, collegial magistracies, popular assemblies — but all presume a coordinating center. The Senate IS that center; its auctoritas is not a construction but an architectural necessity. However, this perspective risks naturalizing what is actually a contingent historical outcome — one reading among several equally defensible readings of the Republic's foundational commitments.
constraint_indexing:constraint_classification(roman_republican_constitution__senate_authority, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(roman_republican_constitution__senate_authority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(roman_republican_constitution__senate_authority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(roman_republican_constitution__senate_authority, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(roman_republican_constitution__senate_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(roman_republican_constitution__senate_authority, TR),
    TR >= 0.70.

:- end_tests(roman_republican_constitution__senate_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The Senate extracted sustained deference from magistrates and assemblies through a combination of financial control, religious interpretation authority, and prestige-based pressure. However, the extraction was not absolute — magistrates retained formal power to act independently (at high cost), and assemblies did pass legislation against Senate preferences occasionally. The measurement trajectory shows accumulation from 0.35 (early Republic, when senatorial dominance was less developed) to 0.52 (late Republic, when senatorial control of finance and religious interpretation was near-total), driven by the Gracchi crisis. Suppression (0.58): Moderate-high. Suppression operates through prestige and financial starvation rather than legal prohibition. A magistrate could formally act against Senate consensus but faced career destruction, ostracism, or prosecution after term-end. Assemblies could pass legislation but financial control meant its implementation depended on Senate cooperation. This is suppression without explicit command — all the more effective for being framed as 'advice' or 'divine will.' Theater ratio (0.35): Low-moderate. The formal procedures — elections, assemblies, legislation, magistracies — retained genuine function throughout the interval. The Senate did not simply decree outcomes; it shaped them through procedural pressure and persuasion. The relative low theater reflects that the constraint operated through structural power (finance, permanence) rather than pure ritual performance.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the senatorial (Rope) and victim perspectives (Snare/Tangled Rope) reflects the tension between auctoritas as coordination mechanism and auctoritas as suppression instrument. From inside the senatorial order, guiding policy through accumulated prestige is solving genuine coordination problems. From outside, this guidance is enforced compliance masked as advice. Neither perspective is false — they map to real structural asymmetries in agency and benefit. The piton classification (Republic's formal institutions) captures that the constitutional procedures continued to operate even as their functional scope narrowed — elections still happened, assemblies still deliberated, but the Senate's authority rendered many outcomes foreordained.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from the agent's structural position relative to Senate authority. The senatorial order as beneficiary with arbitrage-level exit options derives low d (around 0.10), producing negative or near-zero χ — they experience the constraint as beneficial coordination. Magistrates and assemblies as victims with constrained exit (high cost to resist, but exit possible) derive moderate-high d (around 0.70), producing χ ≥ 0.50 — they experience substantial extraction. Organized plebeian movements over generational time derive moderate-high d but with increasing leverage over time, visible in measurement trajectory of suppression rising (enforcement intensity increasing as Senate felt threatened). The analytical perspective derives d from the universality of the observation — the structural lesson learned from studying this constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    auctoritas_vs_potestas_distinction,
    'Is auctoritas (weight, prestige, influence) fundamentally different from potestas (formal legal power), or is the distinction a rhetorical cover for actual power exercised through prestige rather than law?',
    'Structural analysis of cases where Senate recommendations were formally rejected by magistrates or assemblies; frequency, outcomes, and post-rejection consequences. Does the distinction hold empirically or does ''prestige-based veto'' collapse into command?',
    'If genuine distinction: Senate authority is primarily coordination (Rope from Senate perspective, Tangled Rope from constrained perspectives). If rhetorical cover: Senate operates as de facto law-maker (closer to Snare from victim perspectives). Classification shifts from hybrid coordination-extraction to extraction-dominant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(auctoritas_vs_potestas_distinction, empirical, 'Structural reality of auctoritas vs potestas distinction').

omega_variable(
    plebeian_agency_and_concessions,
    'Did plebeian organizations (tribunes, assemblies, Gracchi movement) extract genuine concessions from the Senate through organized resistance, or did the Senate grant concessions strategically to maintain system stability?',
    'Historical trajectory of plebeian rights: land redistribution, veto power, access to magistracies, religious authority. Comparison of concession timing with periods of plebeian mobilization vs. quiescence. Analysis of Senate discourse on plebeian rights — necessity vs. grace?',
    'If genuinely extracted through organized resistance: constraint is Tangled Rope with upward mobility potential over generational time. If strategically granted: constraint is Snare with managed pressure-relief valves. Shapes long-term classification and exit options for organized agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plebeian_agency_and_concessions, empirical, 'Agency and leverage of plebeian organized movements').

omega_variable(
    constitutional_kernel_identity_contest,
    'Which reading of the Republic''s constitution is the foundational one — the Senate''s auctoritas, the crisis machinery (dictatorship), the legal codification (Twelve Tables), the magistracies'' collegiality, or the popular assemblies'' sovereignty?',
    'Comparative historical analysis of which claim was invoked to justify action across different periods. Which reading survived intact when others were suspended or reformed? Which reading was restored after interruption? Phylogenetic stability across regime changes.',
    'This is a conceptual omega that routes through the kernel structure itself (Rule 2). The reading declared here (senate_authority) is one defensible reading, not the unique correct reading. Other readings (crisis_machinery, legal_codification, magistracies_and_collegiality, popular_assemblies_and_tribunate) are equally valid constitutional framings. The constraint story anchors on Senate authority; other stories will anchor on the others. No reading foreclosed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_kernel_identity_contest, conceptual, 'Kernel reading contest: which constitutional principle is foundational?').

omega_variable(
    senatorial_extraction_intentionality,
    'Did the Senate deliberately construct and maintain the auctoritas system to extract deference from magistrates and assemblies, or did the system emerge organically from accumulated prestige and institutional inertia?',
    'Analysis of Senate discourse on its own role: explicit claims about guiding finance, war, and religion. Comparison with deliberate constitutional design (e.g., patrician/plebeian distinction). Longitudinal examination of whether the Senate modified its own authority claims in response to challenges.',
    'If deliberate extraction: constraint is closer to Snare (intentional asymmetric extraction). If organic emergence: constraint is closer to Tangled Rope (unintentional coordination + extraction hybrid). Affects narrative framing but not mathematical classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(senatorial_extraction_intentionality, empirical, 'Intentionality of senatorial extraction mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(roman_republican_constitution__senate_authority, 0, 450).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roma_tr_t0, roman_republican_constitution__senate_authority, theater_ratio, 0, 0.25).
narrative_ontology:measurement(roma_tr_t300, roman_republican_constitution__senate_authority, theater_ratio, 300, 0.32).

% Extraction over time
narrative_ontology:measurement(roma_be_t0, roman_republican_constitution__senate_authority, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(roma_be_t150, roman_republican_constitution__senate_authority, base_extractiveness, 150, 0.48).
narrative_ontology:measurement(roma_be_t300, roman_republican_constitution__senate_authority, base_extractiveness, 300, 0.52).
narrative_ontology:measurement(roma_be_t450, roman_republican_constitution__senate_authority, base_extractiveness, 450, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(roma_su_t0, roman_republican_constitution__senate_authority, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(roma_su_t150, roman_republican_constitution__senate_authority, suppression_requirement, 150, 0.5).
narrative_ontology:measurement(roma_su_t300, roman_republican_constitution__senate_authority, suppression_requirement, 300, 0.55).
narrative_ontology:measurement(roma_su_t450, roman_republican_constitution__senate_authority, suppression_requirement, 450, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(roman_republican_constitution__senate_authority, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(roman_republican_constitution__senate_authority, 0.18).
narrative_ontology:affects_constraint(roman_republican_constitution__senate_authority, roman_republican_constitution__crisis_machinery).
narrative_ontology:affects_constraint(roman_republican_constitution__senate_authority, roman_republican_constitution__legal_codification_twelve_tables).
narrative_ontology:affects_constraint(roman_republican_constitution__senate_authority, roman_republican_constitution__magistracies_and_collegiality).
narrative_ontology:affects_constraint(roman_republican_constitution__senate_authority, roman_republican_constitution__popular_assemblies_and_tribunate).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Roman Republican constitution kernel. Each reading (senate_authority, crisis_machinery, legal_codification, magistracies_and_collegiality, popular_assemblies_and_tribunate) is a separate constraint story with its own ε value and structural logic. They share a kernel but instantiate different constitutional theories. Linkage via network.affects_constraints documents the kernel family. The Senate authority reading emphasizes permanence and auctoritas as the foundational principle; the crisis machinery reading emphasizes emergency suspension and restoration; the legal codification reading emphasizes written law and citizen knowledge; the magistracies reading emphasizes distributed power and collegiality; the popular sovereignty reading emphasizes assembly veto and tribunes' protection. Each reading produces different extractiveness profiles and beneficiary/victim structures. The constraint family is fully specified when all five stories are generated and cross-linked.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
