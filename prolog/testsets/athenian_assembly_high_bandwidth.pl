% ============================================================================
% CONSTRAINT STORY: athenian_assembly_high_bandwidth
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_athenian_assembly_high_bandwidth, []).

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
 *   constraint_id: athenian_assembly_high_bandwidth
 *   human_readable: Athenian Assembly as High-Bandwidth Outer Container
 *   domain: ancient_politics/athenian_democracy
 *
 * SUMMARY:
 *   The Athenian ekklesia represents an inverted case to anchored
 *   institutional fixity: a high-bandwidth outer container with minimal
 *   constraint on kernel revision through standard amendment procedure.
 *   Multiple laws and decrees could be passed in a single meeting; the
 *   Assembly met frequently (at least 40 times per year by some estimates)
 *   with no formal upper limit on legislative pace. This constraint exhibits
 *   structural tension between the coordination benefits of rapid collective
 *   decision-making and the absorption capacity of nested institutional
 *   containers (military command, allied state relationships, religious
 *   practice, treasury administration). The constraint is simultaneously: (1)
 *   a genuine coordination mechanism enabling democratic governance at
 *   unprecedented scale and speed (rope perspective from citizen body), (2)
 *   an asymmetric extraction system subordinating non-citizens to continuous
 *   policy changes without voice (tangled_rope for metics, snare for enslaved
 *   persons), (3) a degraded institutional arrangement where formal
 *   subordination of military and religious authority masks actual
 *   operational independence (piton for military command and religious
 *   establishment), and (4) a historically contingent configuration suited to
 *   a specific phase of imperial expansion that becomes pathological as
 *   complexity increases (scaffold from analytical perspective). The
 *   constraint's theater ratio is comparatively low (0.35) because
 *   enforcement is direct and material rather than performative — the
 *   Assembly's decisions are executed rapidly through existing mechanisms
 *   rather than mediated by symbolic ritual.
 *
 * KEY AGENTS:
 *   - Male Citizens (Mature): Primary beneficiary (powerful/mobile) — coordinating body; experience the constraint as enabling democratic governance and collective power
 *   - Naval Elite and Trierarchs: Secondary beneficiary (powerful/arbitrage) — benefit from resource mobilization and strategic coordination
 *   - Military Commanders (Strategos and Generals): Institutional actor (organized/constrained) — formally subordinate to Assembly but operationally independent; constraint becomes piton as bandwidth exceeds absorption capacity
 *   - Religious Authorities and Priesthoods: Institutional actor (institutional/arbitrage) — nominally consulted but functionally subordinate; maintain performative legitimacy while political actors exercise authority
 *   - Metics (Foreign Residents): Moderate victim (moderate/constrained) — subject to continuous tax and obligation changes, no political voice, but benefit from commercial and military coordination
 *   - Enslaved Persons: Primary victim (powerless/trapped) — no exit option, subject to continuous reassignment and extraction via Assembly decisions affecting household and military organization
 *   - Allied States and Tributaries: Victim (organized/constrained) — benefit from Athenian military alliance and commercial access but suffer from rapid and sometimes contradictory policy changes affecting tribute and military demands
 *   - Women: Structural victim (powerless/trapped) — entirely excluded from Assembly participation and formal political decision-making
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(athenian_assembly_high_bandwidth, 0.52).
domain_priors:suppression_score(athenian_assembly_high_bandwidth, 0.68).
domain_priors:theater_ratio(athenian_assembly_high_bandwidth, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(athenian_assembly_high_bandwidth, extractiveness, 0.52).
narrative_ontology:constraint_metric(athenian_assembly_high_bandwidth, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(athenian_assembly_high_bandwidth, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(athenian_assembly_high_bandwidth, tangled_rope).
narrative_ontology:human_readable(athenian_assembly_high_bandwidth, "Athenian Assembly as High-Bandwidth Outer Container").
narrative_ontology:topic_domain(athenian_assembly_high_bandwidth, "ancient_politics/athenian_democracy").

domain_priors:requires_active_enforcement(athenian_assembly_high_bandwidth).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(athenian_assembly_high_bandwidth, male_citizens).
narrative_ontology:constraint_beneficiary(athenian_assembly_high_bandwidth, military_commanders).
narrative_ontology:constraint_beneficiary(athenian_assembly_high_bandwidth, religious_authorities).
narrative_ontology:constraint_victim(athenian_assembly_high_bandwidth, metics).
narrative_ontology:constraint_victim(athenian_assembly_high_bandwidth, enslaved_persons).
narrative_ontology:constraint_victim(athenian_assembly_high_bandwidth, women).
narrative_ontology:constraint_victim(athenian_assembly_high_bandwidth, allied_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENSLAVED PERSONS (SNARE) — No exit option; subject to continuous reassignment and extraction via Assembly decisions. The constraint coordinates Athenian household and war economies while extracting labor at maximum rate. Theater is low because enforcement is immediate and material, not performative. Maximum suppression and extractiveness from the perspective of those with zero voice and zero exit.
constraint_indexing:constraint_classification(athenian_assembly_high_bandwidth, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: METICS (TANGLED ROPE) — Structurally mobile (can emigrate, though costly) but subject to continuous Assembly-mandated obligations: taxes, military service, residence restrictions. The system genuinely coordinates commercial life and resident alien integration with Athenian institutions, but extraction runs asymmetrically toward the citizen body. Suppression is high (legal disability, no political voice) but not total (some property rights, some legal protections). Extractiveness is moderate because metics do benefit from the commercial and military coordination that the Assembly's high-bandwidth decisions enable.
constraint_indexing:constraint_classification(athenian_assembly_high_bandwidth, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MALE CITIZENS (ROPE) — The constraint is primarily a coordination mechanism from this perspective. The Assembly's high-bandwidth decision rate solves the collective action problem of governing a growing empire and defending against external threats. Citizens experience the constraint as enabling — it coordinates civic participation, religious observance, and military obligation at unprecedented scale. Exit is theoretically possible (emigration, withdrawal) but carries reputational cost. Extraction runs toward the beneficiary group; experienced as coordination rather than coercion.
constraint_indexing:constraint_classification(athenian_assembly_high_bandwidth, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: ALLIED STATES AND TRIBUTARIES (TANGLED ROPE) — The Assembly's high-bandwidth decision-making produces rapid shifts in imperial policy, military demand, and tribute assessment. Allied states experience genuine coordination through the Athenian system (legal framework, military alliance, trade access) but also heavy extraction through unilateral Assembly decisions about tribute and military mobilization. Constrained exit — full withdrawal carries military and economic penalties, but passive defiance or foot-dragging is possible. The high bandwidth creates instability: today's policy is tomorrow's revision, forcing rapid adaptation.
constraint_indexing:constraint_classification(athenian_assembly_high_bandwidth, tangled_rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: RELIGIOUS AUTHORITIES AND FESTIVALS (PITON) — The Assembly's bandwidth creates coordination theater around religious observance: festivals, sacrifices, and oracular consultation are nominally decisive but functionally subordinate to military and political necessity. When Assembly pace conflicts with religious calendar, politics wins. The religious establishment maintains performative authority (Assembly consults oracles, honors traditional festivals) while actual authority lies with military and political actors. Theater ratio is high; extractiveness is low because religious authorities have largely retreated to symbolic maintenance of legitimacy rather than substantive power. The institutional arrangement persists through inertia: Assembly continues to invoke religious legitimacy while ignoring religious constraints.
constraint_indexing:constraint_classification(athenian_assembly_high_bandwidth, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: MILITARY COMMANDERS (PITON) — High-bandwidth Assembly decisions create formal subordination of military command to democratic authority. Generals must implement rapidly changing directives, but their operational capacity lags behind decision-making pace. The constraint becomes performatively binding (Assembly asserts control, generals nominally obey) while functionally degraded: when pace of Assembly decisions exceeds operational absorption capacity, commanders develop workarounds (strategic delay, field initiative, informal authority). Theater is moderate-to-high; extractiveness is low because the formal constraint is increasingly moot — operational reality has decoupled from Assembly mandate through practical necessity. The institutional arrangement persists because it maintains democratic legitimacy while permitting military effectiveness.
constraint_indexing:constraint_classification(athenian_assembly_high_bandwidth, piton,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: NAVAL ELITE AND TRIERARCHS (ROPE) — Wealthy citizens who fund and command naval forces experience the high-bandwidth Assembly as coordination mechanism serving their interests. The constraint enables rapid mobilization, rapid resource allocation, and rapid strategic adjustment. Exit options are substantial (reduced funding, relocation, withdrawal from service) but elite actors benefit from the system's ongoing operation. Effective extraction runs toward this group through public honor and political influence. The constraint appears as genuine coordination from their position.
constraint_indexing:constraint_classification(athenian_assembly_high_bandwidth, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (SCAFFOLD) — From a long view, the Athenian Assembly's high-bandwidth decision-making is a temporary institutional configuration suited to a specific historical window: imperial expansion and military competition in the eastern Mediterranean (480–380 BCE). The constraint functions well when inner containers (military command, allied relationships, treasury) can absorb the rate of change. As complexity increases (larger empire, more tributaries, longer supply lines), the bandwidth that was adaptive becomes pathological. The transition is evident in later accounts: by the 4th century, Assembly dysfunction and frequent reversal of decisions become notable problems. The scaffold interpretation sees the high-bandwidth phase as a transitional institutional form with an implicit sunset — as scale increases beyond absorption capacity, the system moves toward either (a) delegation of decision authority to subordinate bodies (strategos gaining power), (b) formalization of constraints (stricter amendment procedures), or (c) imperial collapse (reduction of scale). The constraint is neither permanent nor pathological — it is a time-bound coordination solution.
constraint_indexing:constraint_classification(athenian_assembly_high_bandwidth, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(athenian_assembly_high_bandwidth_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(athenian_assembly_high_bandwidth, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(athenian_assembly_high_bandwidth, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(athenian_assembly_high_bandwidth, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(athenian_assembly_high_bandwidth, TR),
    TR >= 0.70.

:- end_tests(athenian_assembly_high_bandwidth_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint's bandwidth enables rapid extraction from subjugated and non-citizen populations through Assembly decisions affecting labor, residence, obligation, and wealth. But extractiveness is not maximal because the system is genuinely coordinating a polity at scale — the bandwidth serves real coordination needs for a growing empire managing military and commercial relationships. Extractiveness increases over the 100-year interval (0.38 → 0.52) as the scale of the empire expands, creating more opportunities for Assembly decisions to extract from non-citizens and allied states. Suppression (0.68): High. Multiple layers: enslaved persons have zero exit and zero voice; metics have legal disabilities and tax obligations; women are entirely excluded; allied states face military consequences for defection. Suppression operates through legal disability, military enforcement, and the absence of alternative institutional channels for grievance. Theater ratio (0.35): Low-to-moderate. The constraint operates through direct material enforcement rather than performative ritual. Assembly decisions are immediately executed (military mobilization, tax collection, legal judgment) without requiring extensive symbolic legitimation. The low theater reflects that the Assembly's authority is instantiated in direct action, not in ceremony. Theater ratio increases slightly over time (0.28 → 0.38) as the system matures and the maintenance of democratic legitimacy requires more explicit performance of popular sovereignty despite narrowing actual participation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. Male citizens perceive rope (coordination enabling collective self-governance); metics perceive tangled_rope (genuine coordination coupled with asymmetric extraction); enslaved persons perceive snare (extraction with no coordination benefit); military and religious authorities perceive piton (nominal subordination that is functionally degraded as their actual authority exceeds formal mandate); allied states perceive tangled_rope with deteriorating stability (initial coordination benefit declining as Assembly decisions become increasingly extractive); the analytical observer perceives scaffold (a historically contingent coordination solution with an implicit sunset as scale and complexity exceed bandwidth absorption capacity). The perspectival gaps reflect real structural differences in exit options, beneficiary status, and institutional position. No single classification captures the constraint from all perspectives; the presheaf structure reveals the constraint's nature more clearly than any unified type.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position and exit options. Male citizens (beneficiary + mobile) experience low d and negative effective extraction — the constraint subsidizes them through coordination and political power. Metics (victim + constrained) experience moderate-high d — they bear costs (taxes, restrictions, vulnerability to policy changes) but retain some exit option (emigration, though costly). Enslaved persons (victim + trapped) experience maximum d (0.95) — no exit option, no beneficiary status, all costs. Military commanders (institutional + constrained) experience moderate d — formally subordinate but operationally independent, so the apparent extraction (subordination) is partly illusory. Religious authorities (institutional + arbitrage) experience low d — they benefit from institutional legitimacy even as political authority supersedes theirs. Allied states (victim + constrained) experience high d — they bear extraction through tribute and military demand with limited exit (defection carries military cost). The canonical d values are modified in all cases by the specific power atoms and exit options that characterize each perspective's structural position within the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that classification type depends entirely on observation position. The same structural phenomenon — the Assembly's high-bandwidth decision-making — classifies as rope, tangled_rope, snare, piton, and scaffold depending on who is observing and what interests are at stake. The analytical observer risks misclassifying the entire system as mountain (democratic governance is a natural law of human social organization) or as permanent rope (democracy always coordinates without extracting). The structural data contradicts both: the extraction toward non-citizens, the suppression of women, and the instability of allied relationships are constitutive features of the constraint, not accidental. The mandatrophy is resolved by accepting that all six types are legitimate readings of different structural relationships within the same institutional container. The constraint is not 'really' one type — it is a presheaf where each perspective reveals distinct features. The challenge for policy is to recognize which of these features are necessary coordination costs (and thus acceptable) and which are independent extraction mechanisms (and thus candidates for removal or reform).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bandwidth_absorption_capacity_mismatch,
    'At what rate of Assembly decisions does the operational capacity of military command, allied state adaptation, and treasury administration reach saturation?',
    'Analysis of decision reversal rates, military effectiveness decline, and allied state defection/rebellion timing in relation to Assembly meeting frequency and decision volume',
    'If saturation occurs at high frequency (current observed rate): bandwidth is pathological, and the constraint degrades to snare or piton. If saturation occurs at higher frequency: bandwidth is functional for observed scale, and the constraint remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bandwidth_absorption_capacity_mismatch, empirical, 'Assembly decision rate vs. operational absorption capacity').

omega_variable(
    extraction_or_coordination_in_metic_suppression,
    'Is the suppression of metics (legal disability, tax burden, lack of political voice) structural consequence of coordination at large scale, or independent extraction mechanism designed to concentrate power?',
    'Comparison with non-democratic coordination systems (Persian satrapies, Sparta) at similar scale; analysis of whether metic restrictions would persist if extracted explicitly via formal taxation vs. labor obligation; counterfactual: what would coordinate-without-extraction look like?',
    'If structural consequence: suppression is coordination cost, and the constraint is pure rope from aggregate perspective. If independent extraction: suppression is orthogonal mechanism, and constraint is snare component disguised in coordination framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_or_coordination_in_metic_suppression, conceptual, 'Whether metic suppression is structural coordination cost or independent extraction').

omega_variable(
    high_bandwidth_pathology_vs_contingency,
    'Is the documented military and administrative dysfunction in later Athenian history (4th century) caused by Assembly bandwidth exceeding absorption capacity, or by loss of hegemonic stability and shifting external threat environment?',
    'Controlled comparison: Assembly decision frequency vs. contemporaneous military effectiveness; analysis of whether performance decline tracks bandwidth increase or external stability decline; examination of systems with similar bandwidth but lower scale',
    'If bandwidth-driven: the constraint has an inherent pathology threshold, and the analytical observer''s scaffold perspective is structural. If contingent on external factors: the constraint''s dysfunction is not intrinsic, and the system could have sustained high bandwidth under different conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(high_bandwidth_pathology_vs_contingency, empirical, 'Causation of later Athenian institutional dysfunction').

omega_variable(
    women_exclusion_necessity,
    'Is the exclusion of women from the Assembly a necessary structural feature of high-bandwidth coordination, or an orthogonal extraction mechanism?',
    'Comparison with later democratic systems that maintain high decision bandwidth while including women; analysis of whether women''s exclusion correlates with bandwidth or with pre-democratic kinship structures; examination of female-inclusive coordination systems at comparable scales',
    'If necessary: women''s exclusion is coordination cost of the system, and the constraint is tangled_rope for women (structured extraction justified by coordination need). If orthogonal: women''s exclusion is independent suppression mechanism, and the constraint is pure snare for women.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(women_exclusion_necessity, conceptual, 'Whether women''s exclusion is structural necessity or independent extraction').

omega_variable(
    allied_state_defection_threshold,
    'What level of Assembly decision frequency and reversal rate triggers allied state defection or rebellion?',
    'Historical analysis of allied state behavior correlation with Assembly decision rate and policy reversals; modeling of defection cost vs. instability cost for allied states',
    'If threshold is low (currently observed rate exceeds it in cases of rebellion): the constraint is unsustainably extractive for tributaries, and bandwidth is pathological. If threshold is high: the system can sustain current bandwidth while maintaining alliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allied_state_defection_threshold, empirical, 'Allied state tolerance for Assembly decision rate and reversal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(athenian_assembly_high_bandwidth, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(athasm_theater_t0, athenian_assembly_high_bandwidth, theater_ratio, 0, 0.28).
narrative_ontology:measurement(athasm_theater_t25, athenian_assembly_high_bandwidth, theater_ratio, 25, 0.31).
narrative_ontology:measurement(athasm_theater_t50, athenian_assembly_high_bandwidth, theater_ratio, 50, 0.35).
narrative_ontology:measurement(athasm_theater_t75, athenian_assembly_high_bandwidth, theater_ratio, 75, 0.38).

% Extraction over time
narrative_ontology:measurement(athasm_extract_t0, athenian_assembly_high_bandwidth, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(athasm_extract_t25, athenian_assembly_high_bandwidth, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(athasm_extract_t50, athenian_assembly_high_bandwidth, base_extractiveness, 50, 0.52).
narrative_ontology:measurement(athasm_extract_t75, athenian_assembly_high_bandwidth, base_extractiveness, 75, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(athenian_assembly_high_bandwidth, enforcement_mechanism).
narrative_ontology:affects_constraint(athenian_assembly_high_bandwidth, athenian_ostracism_bandwidth_brake).
narrative_ontology:affects_constraint(athenian_assembly_high_bandwidth, metic_legal_disability_extraction).
narrative_ontology:affects_constraint(athenian_assembly_high_bandwidth, allied_state_tribute_instability).

% DUAL FORMULATION NOTE:
% The Athenian Assembly's high bandwidth is downstream of the founding democratic constraint (citizen sovereignty) but represents a distinct structural configuration. Alternative coordination systems (oligarchy, tyranny) could exercise collective decision-making at lower bandwidth. The constraint family links the founding democratic constraint with its high-bandwidth institutional instantiation and the secondary constraints (ostracism as bandwidth brake, metic status as extraction layer, tributary instability as bandwidth pathology) that emerge from or are shaped by the high-bandwidth core.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(athenian_assembly_high_bandwidth, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
