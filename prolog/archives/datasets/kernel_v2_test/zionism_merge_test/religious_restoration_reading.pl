% ============================================================================
% CONSTRAINT STORY: religious_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_religious_restoration_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: religious_restoration_reading
 *   human_readable: Religious Zionist Interpretation: Divine Promise and Messianic Process
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   The religious Zionist interpretation of Israeli territorial control
 *   post-1967 frames the occupation of the West Bank, Gaza, and Golan Heights
 *   as fulfillment of divine covenant and acceleration of messianic
 *   redemption. This reading emerged as a distinct theological-political
 *   framework after the Six-Day War, when religious Zionist thinkers
 *   (particularly Rabbi Tzvi Yehuda Kook and the Gush Emunim movement)
 *   reinterpreted territorial expansion as a sign of divine favor and a
 *   religious obligation to settle the land. The constraint operates through:
 *   (1) theological mandate that makes territorial concessions religiously
 *   illegitimate, (2) settlement enterprise as practical implementation of
 *   divine command, (3) political coalition structure that gives religious
 *   parties disproportionate influence, (4) legal framework that privileges
 *   Jewish settlement over Palestinian rights. The constraint exhibits rising
 *   extractiveness (0.45→0.78) and suppression (0.55→0.82) over the 50-year
 *   interval as the settlement project expanded and the theological framing
 *   became more entrenched in Israeli politics. Theater ratio remains
 *   relatively low (0.35) because the theological commitment is genuine for
 *   adherents — this is not performative religion but sincere belief driving
 *   political action, though the political consequences (resource extraction,
 *   territorial control) are substantial.
 *
 * KEY AGENTS:
 *   - Palestinian Population: Primary victim (powerless/trapped) — bears maximum extraction through land confiscation, movement restrictions, legal subordination under military occupation justified by theological claims
 *   - Religious Zionist Movement: Primary beneficiary (institutional/arbitrage) — captures state resources for settlement enterprise, political influence through coalition structure, theological legitimation for territorial expansion
 *   - Settlement Enterprise: Secondary beneficiary (institutional/arbitrage) — receives state subsidies, military protection, legal privileges; implements theological mandate through territorial facts on the ground
 *   - Secular Israeli Citizens: Mixed position (moderate/constrained) — benefit from state security coordination but bear costs of diplomatic isolation, military burden, budget allocation to religious institutions
 *   - Religious Zionist Settlers: Identity-locked agents (moderate/identity_locked) — structurally mobile but functionally trapped by theological identity; exit would require abandoning messianic role
 *   - International Diplomatic Community: Constrained coordinator (institutional/constrained) — cannot exit regional stability coordination but extraction occurs through diplomatic capital spent, humanitarian costs, erosion of international law norms
 *   - Analytical Observer (within tradition): Sees pure coordination (analytical/analytical) — the tradition's own premises define extraction out of existence
 *   - Analytical Observer (cross-traditional): Sees tangled rope (analytical/analytical) — genuine coordination for religious Zionist community plus substantial extraction from occupied population
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(religious_restoration_reading, 0.78).
domain_priors:suppression_score(religious_restoration_reading, 0.82).
domain_priors:theater_ratio(religious_restoration_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(religious_restoration_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(religious_restoration_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(religious_restoration_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(religious_restoration_reading, rope).
narrative_ontology:human_readable(religious_restoration_reading, "Religious Zionist Interpretation: Divine Promise and Messianic Process").
narrative_ontology:topic_domain(religious_restoration_reading, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(religious_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(religious_restoration_reading, '560c004c-b842-47e8-8ffc-c823ea454de5').
narrative_ontology:cs_kernel_codification('560c004c-b842-47e8-8ffc-c823ea454de5', fixed_text).
narrative_ontology:cs_authority_grounding('560c004c-b842-47e8-8ffc-c823ea454de5', lineage).
narrative_ontology:cs_interpretation_layer_present('560c004c-b842-47e8-8ffc-c823ea454de5').
narrative_ontology:cs_reading_relation('560c004c-b842-47e8-8ffc-c823ea454de5', religious_restoration_reading__national_liberation_reading, influences).
narrative_ontology:cs_reading_relation('560c004c-b842-47e8-8ffc-c823ea454de5', religious_restoration_reading__settler_colonial_reading, coexists_with).
narrative_ontology:cs_axiom('560c004c-b842-47e8-8ffc-c823ea454de5', foundational, divine_territorial_mandate).
narrative_ontology:cs_axiom_status(divine_territorial_mandate, holdable).
narrative_ontology:cs_axiom_grounding('560c004c-b842-47e8-8ffc-c823ea454de5', divine_territorial_mandate, theological).
narrative_ontology:cs_axiom('560c004c-b842-47e8-8ffc-c823ea454de5', foundational, messianic_acceleration_through_settlement).
narrative_ontology:cs_axiom_status(messianic_acceleration_through_settlement, holdable).
narrative_ontology:cs_axiom_grounding('560c004c-b842-47e8-8ffc-c823ea454de5', messianic_acceleration_through_settlement, theological).
narrative_ontology:cs_axiom('560c004c-b842-47e8-8ffc-c823ea454de5', secondary, territorial_concession_theological_prohibition).
narrative_ontology:cs_axiom_status(territorial_concession_theological_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('560c004c-b842-47e8-8ffc-c823ea454de5', territorial_concession_theological_prohibition, deontological).
narrative_ontology:cs_reference_frame('560c004c-b842-47e8-8ffc-c823ea454de5', pre_1967_religious_zionist_restraint).
narrative_ontology:cs_drift_state('560c004c-b842-47e8-8ffc-c823ea454de5', post_1967_territorial_expansion, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('560c004c-b842-47e8-8ffc-c823ea454de5', '').
narrative_ontology:cs_kernel_id(religious_restoration_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(religious_restoration_reading, religious_zionist_movement).
narrative_ontology:constraint_beneficiary(religious_restoration_reading, settlement_enterprise).
narrative_ontology:constraint_beneficiary(religious_restoration_reading, territorial_maximalist_coalition).
narrative_ontology:constraint_victim(religious_restoration_reading, palestinian_population).
narrative_ontology:constraint_victim(religious_restoration_reading, secular_zionist_tradition).
narrative_ontology:constraint_victim(religious_restoration_reading, regional_stability).
narrative_ontology:constraint_vindicates(religious_restoration_reading, divine_promise_doctrine).
narrative_ontology:constraint_vindicates(religious_restoration_reading, messianic_acceleration_theology).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN POPULATION (SNARE) — Trapped by military occupation justified through theological claims. No exit options: cannot leave (geographic/economic constraints), cannot negotiate (theological mandate non-negotiable), cannot resist (military asymmetry). Experiences maximum extraction: land confiscation, movement restrictions, resource control, legal subordination. The theological framing makes the constraint appear immutable from this position — not a political dispute but a divine decree.
constraint_indexing:constraint_classification(religious_restoration_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SECULAR ISRAELI CITIZEN (TANGLED ROPE) — Constrained by the political coalition structure that empowers religious parties. Benefits from state security apparatus and national identity coordination, but bears costs: military service defending settlements, budget allocation to religious institutions, diplomatic isolation, erosion of democratic norms. Can exit through emigration (constrained by economic/social costs) but cannot exit the political structure while remaining. Mixed coordination and extraction: the state provides genuine security coordination while embedding theological extraction.
constraint_indexing:constraint_classification(religious_restoration_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RELIGIOUS ZIONIST MOVEMENT (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: theological framework aligns political action with divine will, mobilizes constituency, justifies territorial claims, secures state resources for settlement enterprise. Arbitrage exit: can shift between religious and nationalist framings as politically expedient. Low effective extraction — the constraint channels resources toward this agent.
constraint_indexing:constraint_classification(religious_restoration_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL DIPLOMATIC COMMUNITY (TANGLED ROPE) — Constrained by the theological framing's resistance to secular negotiation frameworks. Benefits from regional stability coordination mechanisms (treaties, aid frameworks) but extraction occurs through: diplomatic capital spent managing conflict, humanitarian costs, erosion of international law norms. Cannot exit (regional stability is a collective good) but also cannot effectively coordinate when one party claims divine mandate.
constraint_indexing:constraint_classification(religious_restoration_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: RELIGIOUS ZIONIST SETTLER (SNARE) — Identity-locked rather than structurally trapped. Could physically relocate (has state support, economic resources, legal protections) but identity is constituted through the settlement project. Exit would require abandoning not just location but theological identity — ceasing to be an agent of messianic redemption. Experiences the constraint as divine obligation, but the identity lock creates functional entrapment: the theological frame makes alternatives literally unthinkable. This perspective instantiates the oracle gap — the settler cannot see the extraction structure from within the identity frame.
constraint_indexing:constraint_classification(religious_restoration_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 6: RELIGIOUS ZIONIST ANALYTICAL (ROPE) — From within the theological framework's own premises, the constraint is pure coordination: aligning political action with divine will, fulfilling covenant obligations, accelerating messianic redemption. The framework sees territorial control as religious duty, not extraction. This is the claimed_type perspective — the reading's self-understanding. The analytical observer within this tradition cannot see the extraction because the tradition's epistemic premises define extraction out of existence.
constraint_indexing:constraint_classification(religious_restoration_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 7: CROSS-TRADITIONAL ANALYTICAL (TANGLED ROPE) — From outside the theological framework, the constraint exhibits both genuine coordination (mobilizes constituency, provides meaning structure, solves collective action problems for the religious Zionist community) and substantial extraction (territorial dispossession, legal subordination, resource transfer from occupied population). The theological framing is not mere cover — it genuinely coordinates — but it also enables and justifies extraction that secular frameworks would render illegitimate.
constraint_indexing:constraint_classification(religious_restoration_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(religious_restoration_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(religious_restoration_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(religious_restoration_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(religious_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(religious_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. The constraint extracts substantially from the Palestinian population (land, resources, mobility, legal rights) and moderately from secular Israeli citizens (budget allocation, diplomatic costs, democratic erosion). The extraction has accumulated over 50 years as settlement expansion and legal subordination deepened. The value reflects that this is not total extraction (some coordination functions exist) but substantial asymmetric transfer. Suppression (0.82): Very high. The constraint suppresses alternatives through: military occupation preventing Palestinian exit, theological mandate preventing Israeli territorial concessions, coalition structure preventing secular political reform, international law erosion preventing external intervention. Suppression has intensified as the settlement project became irreversible and the theological framing became politically entrenched. Theater ratio (0.35): Moderate-low. The theological framing is NOT primarily performative — religious Zionist adherents genuinely believe in divine mandate and messianic acceleration. The theater component exists (political leaders invoke theology strategically, some settlers are motivated by economic incentives rather than theology) but is not dominant. The low theater ratio distinguishes this from a piton — the constraint's function has not atrophied into pure performance.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence driven by epistemic frame. From within the religious Zionist tradition (Perspective 6), the constraint is pure coordination — aligning political action with divine will, fulfilling covenant obligations. The tradition's own analytical instruments cannot detect extraction because the epistemic premises define territorial control as religious duty rather than political choice. From the Palestinian position (Perspective 1), the constraint is pure extraction — a snare with no exit, justified by theological claims that make negotiation impossible. From the cross-traditional analytical position (Perspective 7), the constraint is tangled rope — genuine coordination for the religious Zionist community (mobilizes constituency, provides meaning, solves collective action problems) combined with substantial extraction from the occupied population. The secular Israeli position (Perspective 2) experiences mixed coordination and extraction within the same national framework. The identity-locked settler (Perspective 5) instantiates the oracle gap: structurally mobile but functionally trapped by theological identity, unable to see the extraction structure from within the frame. The gap between Perspective 6 (rope from within tradition) and Perspective 7 (tangled rope from outside) is the core measurement — it reveals that single-position analysis fails when the observer's epistemic frame is itself part of the constraint structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position. Palestinian population: victims + trapped exit → d approaches 1.0 → maximum effective extraction. Religious Zionist movement: beneficiaries + arbitrage exit → d approaches 0.0 → negative effective extraction (subsidy). Secular Israeli citizens: mixed beneficiary/victim + constrained exit → d ≈ 0.4-0.5 → moderate effective extraction. Religious Zionist settlers: beneficiaries but identity_locked → d ≈ 0.3 (lower than structural position suggests because identity lock creates functional entrapment that the derivation chain reads as partial victim status). International community: mixed coordination/extraction + constrained exit → d ≈ 0.5 → moderate effective extraction. The analytical observer within the tradition experiences d ≈ 0.0 (pure coordination from within the epistemic frame); the cross-traditional analytical observer experiences d ≈ 0.4 (recognizes both coordination and extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that the classification depends on the observer's epistemic frame and structural position. The religious Zionist tradition's self-understanding (rope) is not false — it genuinely coordinates for adherents — but it is incomplete. The tradition cannot see the extraction because its epistemic premises define extraction out of existence: if territorial control is divine mandate, then dispossession of the prior population is not extraction but fulfillment of covenant. The cross-traditional analytical observer sees both functions: genuine coordination (the theology mobilizes, provides meaning, solves collective action problems for the religious Zionist community) and substantial extraction (territorial dispossession, legal subordination, resource transfer). The tangled rope classification from the cross-traditional position is not a compromise between rope and snare — it is the recognition that both functions coexist in the same structure. The mandate (divine promise) has not outlived its function for adherents, but the function includes extraction that the mandate's framing renders invisible. This is not mandatrophy (function outlived) but mandatrophy's opposite: the mandate's function is precisely to naturalize the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint one reading of the contested kernel ''Zionist legitimacy basis''? What structural elements would change under sibling readings (national liberation, settler-colonial)?',
    'Comparison across readings: national_liberation_reading would frame territorial claims as decolonization from Ottoman/British rule and self-determination; settler_colonial_reading would frame the same territorial control as displacement and extraction. The disagreement is located in the legitimacy grounding: divine mandate vs. national sovereignty vs. colonial structure.',
    'If readings are genuinely distinct constraints (different ε values): decomposition is correct. If readings are observer-dependent views of one constraint: the kernel framework is misapplied. The religious reading''s ε (0.78) is substantially higher than a national-liberation reading would produce (~0.35) because theological mandate removes negotiability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether this is one reading of a contested kernel or a distinct constraint').

omega_variable(
    theological_mandate_vs_political_strategy,
    'Is the theological framing a genuine epistemic commitment or a strategic legitimation device for territorial expansion?',
    'Historical analysis: correlation between theological intensity and territorial opportunity; comparison of theological claims in periods of strength vs. weakness; examination of whether theological mandates are negotiable when politically expedient.',
    'If genuine commitment: the constraint is identity_locked for adherents and the extraction is a side effect of theological obligation. If strategic device: the constraint is a snare with theological cover, and the extraction is the primary function. Mixed case (both): tangled_rope classification is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_mandate_vs_political_strategy, empirical, 'Whether theological framing is epistemic commitment or strategic legitimation').

omega_variable(
    messianic_acceleration_falsifiability,
    'What empirical conditions would falsify the messianic acceleration theology? If territorial control does not produce messianic redemption, does the theology adjust or persist?',
    'Longitudinal tracking of theological claims: do failed predictions (e.g., Oslo Accords as divine punishment, disengagement from Gaza as theological crisis) lead to framework revision or to auxiliary hypothesis generation? Comparison with other messianic movements'' response to disconfirmation.',
    'If falsifiable and responsive: the theology is an empirical claim subject to revision (lower theater_ratio). If unfalsifiable or immunized through auxiliary hypotheses: the theology is performative (higher theater_ratio, piton trajectory).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_acceleration_falsifiability, empirical, 'Whether messianic acceleration theology is falsifiable or immunized').

omega_variable(
    coalition_power_threshold,
    'At what threshold of Palestinian organization does the powerless/trapped classification shift to organized/constrained, changing the constraint''s stability?',
    'Historical analysis of Palestinian resistance movements: First Intifada (1987-1993) shifted Israeli policy despite power asymmetry; Second Intifada (2000-2005) led to unilateral disengagement. Critical mass appears to be sustained mobilization + international attention + economic cost to occupation.',
    'If threshold is reachable: the snare has a structural exit path through coalition formation. If threshold is unreachable under current suppression levels: the trapped classification is stable. The religious framing raises the threshold by making territorial concessions theologically illegitimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_power_threshold, empirical, 'Threshold for Palestinian coalition power to shift constraint dynamics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(religious_restoration_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(relrest_theater_1967, religious_restoration_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(relrest_theater_1977, religious_restoration_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(relrest_theater_1987, religious_restoration_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(relrest_theater_1997, religious_restoration_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(relrest_theater_2007, religious_restoration_reading, theater_ratio, 40, 0.34).
narrative_ontology:measurement(relrest_theater_2017, religious_restoration_reading, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(relrest_extract_1967, religious_restoration_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(relrest_extract_1977, religious_restoration_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(relrest_extract_1987, religious_restoration_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(relrest_extract_1997, religious_restoration_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(relrest_extract_2007, religious_restoration_reading, base_extractiveness, 40, 0.76).
narrative_ontology:measurement(relrest_extract_2017, religious_restoration_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(relrest_suppress_1967, religious_restoration_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(relrest_suppress_1977, religious_restoration_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(relrest_suppress_1987, religious_restoration_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(relrest_suppress_1997, religious_restoration_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(relrest_suppress_2007, religious_restoration_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(relrest_suppress_2017, religious_restoration_reading, suppression_requirement, 50, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(religious_restoration_reading, identity_coordination).
narrative_ontology:affects_constraint(religious_restoration_reading, national_liberation_reading).
narrative_ontology:affects_constraint(religious_restoration_reading, settler_colonial_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'Zionist legitimacy basis' kernel. The religious_restoration_reading, national_liberation_reading, and settler_colonial_reading are structurally distinct constraints (different ε values, different beneficiary/victim structures) that compete for legitimacy in the same political space. They are not observer-dependent views of one constraint but distinct constraints linked by their common kernel. The religious reading's ε (0.78) is higher than the national-liberation reading's would be (~0.35) because theological mandate removes negotiability; the settler-colonial reading's ε would be higher still (~0.85) because it frames the entire structure as extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
