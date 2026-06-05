% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__indigenous_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__indigenous_continuity_reading, []).

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
 *   constraint_id: territorial_legitimacy__indigenous_continuity_reading
 *   human_readable: Territorial Legitimacy via Indigenous Continuity and Anti-Colonial Self-Determination (Nakba Reading)
 *   domain: international_law/political_sovereignty/decolonization
 *
 * SUMMARY:
 *   This constraint instantiates one specific reading of the contested kernel
 *   of territorial legitimacy: legitimacy derives from continuous indigenous
 *   habitation, self-determination rights, and anti-colonial decolonization
 *   principles. Under this reading, the 1948 establishment of the Israeli
 *   state represents Nakba (catastrophe) for the Palestinian indigenous
 *   population — not a legal partition but a colonial dispossession. The
 *   constraint models the structural mechanisms by which this reading is
 *   suppressed, excluded from mainstream institutional discourse, and
 *   rendered materially impossible through territorial confinement, legal
 *   exclusion, and military enforcement. The constraint exhibits
 *   characteristics of a snare from the perspective of the indigenous
 *   Palestinian population (structurally trapped, no exit options, pure
 *   extraction) and a snare from the perspective of the international legal
 *   system (trapped in a legitimacy contradiction it cannot resolve). From
 *   the Israeli state's perspective, the constraint appears as rope (solving
 *   a security and institutional coordination problem). The indigenous
 *   continuity reading directly forecloses the partition reading's core
 *   premise — that 1948 represented a legitimate legal allocation rather than
 *   dispossession — within a single ethical framework. However, both readings
 *   coexist in contemporary discourse held by different parties. The
 *   constraint's extractiveness has increased over 76 years (from 0.35 in
 *   1948 to 0.68 in 2004) as settlements expanded, territorial fragmentation
 *   deepened, and the right of return became materially less feasible.
 *   Suppression has intensified correspondingly (from 0.62 to 0.85),
 *   reflecting the escalation of military enforcement and legal exclusion
 *   mechanisms. The theater ratio has risen (from 0.35 to 0.62), indicating
 *   that increasingly, international consensus around the two-state solution
 *   functions performatively while material facts on the ground contradict
 *   it.
 *
 * KEY AGENTS:
 *   - Indigenous Palestinian population: Primary victim (powerless/trapped) — structurally dispossessed, militarily confined, legally excluded from 78% of historic territory, right of return systematically denied
 *   - Palestinian Authority and diaspora actors: Secondary victim (moderate/constrained) — face constrained choices between recognition of status quo (autonomy + occupation) or sustained principled rejection (siege + pressure)
 *   - Israeli state institutional apparatus: Primary beneficiary (institutional/arbitrage) — controls territory, resources, military force; has exit options but experiences the constraint as protective coordination rather than extractive
 *   - International legal/diplomatic system: Trapped actor (powerful/mobile yet snared) — institutionally obligated to enforce partition reading yet unable to resolve the legitimacy contradiction with indigenous continuity reading; trapped in compliance costs and unresolvable disputes
 *   - Two-state solution consensus: Degraded institutional apparatus (institutional/arbitrage) — maintains ritual (peace negotiations, diplomatic consensus) while material conditions erode the viability of the framework itself
 *   - Analytical observer: Risks naturalizing dispossession as immutable historical fact rather than contingent institutional outcome maintained by continuous suppression
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, 0.68).
domain_priors:suppression_score(territorial_legitimacy__indigenous_continuity_reading, 0.85).
domain_priors:theater_ratio(territorial_legitimacy__indigenous_continuity_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__indigenous_continuity_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy__indigenous_continuity_reading, "Territorial Legitimacy via Indigenous Continuity and Anti-Colonial Self-Determination (Nakba Reading)").
narrative_ontology:topic_domain(territorial_legitimacy__indigenous_continuity_reading, "international_law/political_sovereignty/decolonization").

domain_priors:requires_active_enforcement(territorial_legitimacy__indigenous_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__indigenous_continuity_reading, '5cde5827-28dc-41bc-9ac2-90fc061e160d').
narrative_ontology:cs_kernel_codification('5cde5827-28dc-41bc-9ac2-90fc061e160d', formalized).
narrative_ontology:cs_authority_grounding('5cde5827-28dc-41bc-9ac2-90fc061e160d', extraction).
narrative_ontology:cs_interpretation_layer_present('5cde5827-28dc-41bc-9ac2-90fc061e160d').
narrative_ontology:cs_reading_relation('5cde5827-28dc-41bc-9ac2-90fc061e160d', territorial_legitimacy__partition_reading, forecloses).
narrative_ontology:cs_reading_relation('5cde5827-28dc-41bc-9ac2-90fc061e160d', territorial_legitimacy__security_necessity_reading, coexists_with).
narrative_ontology:cs_axiom('5cde5827-28dc-41bc-9ac2-90fc061e160d', foundational, continuous_indigenous_habitation_primacy).
narrative_ontology:cs_axiom_status(continuous_indigenous_habitation_primacy, holdable).
narrative_ontology:cs_axiom_grounding('5cde5827-28dc-41bc-9ac2-90fc061e160d', continuous_indigenous_habitation_primacy, deontological).
narrative_ontology:cs_axiom('5cde5827-28dc-41bc-9ac2-90fc061e160d', secondary, decolonization_reversibility_principle).
narrative_ontology:cs_axiom_status(decolonization_reversibility_principle, holdable).
narrative_ontology:cs_axiom_grounding('5cde5827-28dc-41bc-9ac2-90fc061e160d', decolonization_reversibility_principle, deontological).
narrative_ontology:cs_reference_frame('5cde5827-28dc-41bc-9ac2-90fc061e160d', pre_1948_indigenous_palestinian_territorial_continuity).
narrative_ontology:cs_drift_state('5cde5827-28dc-41bc-9ac2-90fc061e160d', contemporary_post_2004, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('5cde5827-28dc-41bc-9ac2-90fc061e160d', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, settler_colonial_state_apparatus).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, indigenous_palestinian_population).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, territorial_self_determination_right).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS PALESTINIAN POPULATION (SNARE) — Structurally trapped by military occupation, legal dispossession, and territorial confinement. The constraint extracts Palestinian land, historical claim, and right of return while offering no exit. Suppression operates through military enforcement, legal exclusion from property ownership in 78% of territory, and denial of political representation. No coordination function exists from this perspective — pure extraction backed by coercive state apparatus.
constraint_indexing:constraint_classification(territorial_legitimacy__indigenous_continuity_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PALESTINIAN AUTHORITY & DIASPORA ACTORS (TANGLED ROPE) — Face constrained exits: recognize the territorial status quo and gain limited autonomy, or maintain principled rejection and suffer sustained military/economic pressure. Some institutional coordination functions exist (Oslo accords coordination, humanitarian administration) alongside massive asymmetric extraction (Israeli control of water, airspace, borders). The constraint offers minimal genuine benefit while extracting political sovereignty.
constraint_indexing:constraint_classification(territorial_legitimacy__indigenous_continuity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ISRAELI STATE INSTITUTIONAL APPARATUS (ROPE) — Experiences the constraint as coordination of security and state consolidation. From this perspective, territorial control solves a genuine coordination problem (security, state viability, resource access). The state benefits from the legitimacy framework that treats 1948 as legal partition rather than dispossession. Has full exit options (can revise territorial claims, recognize Palestinian sovereignty) but perceives the constraint as protective rather than extractive. Net beneficiary with institutional power.
constraint_indexing:constraint_classification(territorial_legitimacy__indigenous_continuity_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL LEGAL/DIPLOMATIC SYSTEM (SNARE) — Despite global institutional power and mobility, the system is trapped in a legitimacy contradiction: it simultaneously enforces the partition reading (UN 181, 1948 borders recognized in law) and must grapple with the indigenous continuity reading's claim that partition itself was illegitimate. The constraint extracts inconsistency and compliance costs without resolving the fundamental dispute. No genuine coordination function — the international system's role is co-opted by the beneficiary state.
constraint_indexing:constraint_classification(territorial_legitimacy__indigenous_continuity_reading, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: TWO-STATE SOLUTION CONSENSUS (PITON) — The institutional consensus around a 'two-state solution' is largely theatrical at this point: it has been the official position of the UN, the US, the EU, and the Palestinian Authority since the 1990s, yet continues to lose material viability (settlement expansion, territorial fragmentation, demographic changes). The apparatus persists through diplomatic ritual and institutional inertia despite the erosion of its structural basis. Theater ratio is high (continuous peace negotiations with no binding enforcement or mechanism). This perspective sees the constraint as a degraded institution maintained for legitimacy rather than function.
constraint_indexing:constraint_classification(territorial_legitimacy__indigenous_continuity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE SUMMIT CANDIDATE) — From a civilizational perspective, one might argue that territorial dispossession and displacement are irreversible facts: 75+ years of settlement, demographic change, institutional entrenchment, and geopolitical commitment make the indigenous continuity claim 'unrealistic.' This perspective risks naturalizing the outcome of coercive dispossession as an immutable state of affairs. However, the structural data reveals this as a false summit: the constraint depends on continuous enforcement, institutional legitimacy claims, and suppression mechanisms — it is not an immutable law but a contingent institutional arrangement. The engine's false summit detector applies here.
constraint_indexing:constraint_classification(territorial_legitimacy__indigenous_continuity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(territorial_legitimacy__indigenous_continuity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(territorial_legitimacy__indigenous_continuity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(territorial_legitimacy__indigenous_continuity_reading, TR),
    TR >= 0.70.

:- end_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts Palestinian territorial rights, self-determination capacity, demographic continuity, and property claims while offering minimal benefit to the victim population. The extraction is not maximal (0.85+) because some Palestinian institutional actors (PA, NGOs, diaspora communities) retain limited agency and coordinate partial governance functions. The measurement trajectory (0.35→0.68) reflects settlement expansion and territorial accumulation over time. Suppression (0.85): High and rising. Suppression operates through military occupation, legal exclusion from land ownership/residency in majority of territory, administrative detention, movement restrictions, denial of political representation in land allocated to Israeli control, and exclusion from the legitimacy framework itself (the partition reading treats Palestinian dispossession as a legal necessity rather than a rights violation). The rising trajectory (0.62→0.85) reflects intensification of enforcement mechanisms and expansion of settlements. Theater ratio (0.62): Moderate-high. Significant performative elements exist at the international level: the two-state solution consensus is formally endorsed by all major actors (UN, US, EU, PA) yet simultaneously contradicted by material actions (settlement expansion, annexation moves, territorial fragmentation). Institutional language emphasizes 'peace process' while structural conditions move away from viable partition. The rising theater ratio suggests increasing divergence between institutional rhetoric and material facts.
 *
 * PERSPECTIVAL GAP:
 *   The indigenous continuity reading produces a pronounced perspectival gap between the beneficiary state's experience (rope — security coordination, legitimate state consolidation) and the victim population's experience (snare — pure extraction, no exit). The international system occupies a structural trap: it must simultaneously enforce the partition reading (which treats 1948 as legally legitimate) and grapple with the indigenous continuity reading's claim that partition itself was illegitimate dispossession. The analytical observer risks collapsing into the false summit perspective — treating 75 years of settlement and institutional entrenchment as immutable facts rather than continuous outcomes of active suppression and enforcement. The piton perspective (two-state solution) shows how institutional consensus can persist through ritual and inertia even as its material basis erodes. No perspective sees genuine mutual benefit or coordination — the constraint is extractive across all readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is computed from structural position: the indigenous Palestinian population is both a victim (high d, near 1.0) and powerless (no exit options beyond analytical), producing the highest experienced extractiveness (f(d) ≈ 1.42 in legacy terms). The Israeli state apparatus is a beneficiary (low d, near 0.15) with institutional power and arbitrage exit options, producing low or negative experienced extractiveness. The international system is nominally powerful but structurally trapped in the legitimacy contradiction — it experiences high extractiveness from being forced to endorse a reading (partition) while unable to defend it against the indigenous continuity reading's logical challenge. Directionality overrides are not needed — the structural data directly supports the d values derived from beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by clarifying that the indigenous continuity reading produces a snare classification consistently across victim and beneficiary perspectives, distinguished primarily by the beneficiary's experience of the constraint as coordination (rope) rather than extraction. The mandatrophy does not arise from ambiguity about type — the snare classification is robust — but from the competing legitimacy groundings (omegas) that determine whether the constraint is itself justified or illegitimate. The false summit candidate perspective (analytical observer naturalizing dispossession as immutable) exemplifies the oracle gap: the observer's native context (assuming state legitimacy, treating 75-year-old facts as immutable) prevents recognition of the constraint structure that cross-position analysis reveals. Resolving the mandatrophy requires engaging the omega variables: which legitimacy grounding takes precedence, and does reversibility matter for historical wrongs beyond a certain threshold?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_grounding_competing,
    'Which legitimacy grounding takes precedence: continuous indigenous habitation, or international legal partition and recognition achieved through decolonization process?',
    'Doctrinal analysis of international law hierarchy (jus cogens principles of self-determination vs. uti possidetis stability principles); historical examination of whether the 1948 partition process satisfied anti-colonial decolonization standards; comparative case law (India-Pakistan, Cyprus, Ireland, Palestine, other post-colonial partitions)',
    'If indigenous continuity grounding prevails: constraint reclassifies as institutionally illegitimate at all perspectives except those naturalizing it (mountain). If partition grounding prevails: constraint reclassifies toward rope/tangled_rope as a legitimate if contested allocation. If hierarchy is indeterminate: the ambiguity itself becomes the constraint (unresolvable legal conflict).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_grounding_competing, conceptual, 'Competing legitimacy groundings — indigenous continuity vs. international partition').

omega_variable(
    right_of_return_reversibility,
    'Is the right of return for 1948-displaced Palestinian refugees structurally reversible after 75+ years of settlement and demographic change, or does it represent an irreversible historical outcome that the legitimacy framework must accommodate rather than reverse?',
    'Historical feasibility analysis of return scenarios (absorption capacity, property restoration, demographic impact); examination of comparable post-conflict return processes (Rwanda, Bosnia, others); legal analysis of whether ''irreversibility'' can override jus cogens self-determination principles',
    'If return is reversible: legitimacy framework centers on restoration of indigenous Palestinian self-determination in historic Palestine territory. If return is irreversible: legitimacy framework must shift toward compensation, co-existence, or partition accommodation. This directly affects whether the snare classification persists or transitions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(right_of_return_reversibility, empirical, 'Whether Palestinian right of return is structurally reversible after 75+ years').

omega_variable(
    partition_process_legitimacy,
    'Did the 1948 partition process (UN 181, Mandate dissolution, state declaration) constitute a legitimate anti-colonial decolonization, or did it impose territorial division on an indigenous population without their consent, thereby perpetuating colonial extraction under a new form?',
    'Historical and doctrinal analysis: Did Palestinian Arabs consent to partition? Were indigenous representation and self-determination respected in the partition process? Comparison with other UN-supervised decolonizations (India, Indonesia, etc.) and their legitimacy criteria; examination of whether partition can satisfy anti-colonial decolonization principles when one party experiences it as dispossession',
    'If partition was legitimate: the indigenous continuity reading''s core premise is undermined; the constraint reframes as a border/resource dispute between two legitimate states. If partition was illegitimate: the indigenous continuity reading''s claim that 1948 was Nakba (catastrophe) rather than legal partition becomes constitutive of the legitimacy framework itself. This is the highest-impact omega.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(partition_process_legitimacy, conceptual, 'Whether 1948 partition process satisfied anti-colonial decolonization legitimacy standards').

omega_variable(
    settler_colonial_vs_immigration,
    'Is Israeli state formation and settlement patterns best characterized as settler-colonialism (structured dispossession and replacement of indigenous population), or as immigration and nation-building by a diaspora population with historical claim, within a framework that did not fully recognize indigenous Palestinian self-determination?',
    'Comparative analysis of settler-colonial typologies (North America, South Africa, Australia) against Israeli settlement patterns; examination of stated settler intent (kibbutz ideology, official state rhetoric, settlement planning); analysis of whether replacement dynamics (Palestinian displacement correlating with Jewish immigration) constitute structural settler-colonialism or result from military conflict and separate policy domains',
    'If settler-colonialism: the legitimacy framework centers on decolonization and indigenous restoration. If immigration/nation-building: legitimacy becomes a conflict between two competing self-determination claims. The snare classification depends partly on this assessment — pure extraction (snare) vs. contested allocation between groups with historical claims (tangled_rope or partition_reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_colonial_vs_immigration, conceptual, 'Whether state formation and settlement patterns constitute settler-colonialism or immigration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__indigenous_continuity_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_indig_theater_1948, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(terr_indig_theater_1978, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement(terr_indig_theater_2004, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 56, 0.62).

% Extraction over time
narrative_ontology:measurement(terr_indig_extract_1948, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(terr_indig_extract_1978, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(terr_indig_extract_2004, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 56, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(terr_indig_suppress_1948, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(terr_indig_suppress_1978, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(terr_indig_suppress_2004, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 56, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__indigenous_continuity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy__partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy__security_necessity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, right_of_return_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, settlement_legality_framework).

% DUAL FORMULATION NOTE:
% The territorial legitimacy kernel decomposes into three distinct constraint stories with different ε values and legitimacy groundings. This story (indigenous_continuity_reading, ε=0.68) models the snare structure when continuity-based indigenous self-determination is the operative legitimacy standard. The partition_reading (ε≈0.35-0.45, expected Rope/Tangled Rope) models allocation as a legal process. The security_necessity_reading (ε≈0.55-0.70, expected Tangled Rope/Snare) models the constraint when security requirements override prior claims. Each reading has distinct beneficiaries, victims, and suppressions. They are not three perspectives on one constraint but three different constraints arising from three different legitimacy groundings applied to the same territorial dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
