% ============================================================================
% CONSTRAINT STORY: soviet_constitution_1936__party_state_duality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_soviet_constitution_1936__party_state_duality, []).

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
 *   constraint_id: soviet_constitution_1936__party_state_duality
 *   human_readable: Party-State Duality: Constitutional Invisibility of the Command Apparatus (1936)
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   The 1936 Soviet Constitution presents a profound structural gap between
 *   its written text and the actual location of power. The document describes
 *   a federal state with autonomous republics, a bicameral legislature,
 *   elected soviets, autonomous ministries, and an independent judiciary — a
 *   constitutional structure that, taken literally, appears to distribute
 *   power across state institutions. Yet the Communist Party apparatus, which
 *   actually exercises power, is barely mentioned in the text. The party
 *   operated through parallel channels outside the constitutional map: the
 *   Politburo, Central Committee, party discipline within soviets and
 *   ministries, and cadre appointments that overrode formal electoral or
 *   bureaucratic procedures. Citizens and officials navigating by the written
 *   constitution would find their understanding of power fundamentally
 *   misaligned with reality. This constraint examines the extraction produced
 *   by this gap: the party benefits from plausible deniability and
 *   international legitimacy, while those attempting to operate within
 *   constitutional bounds are trapped by a false map. The timing is crucial:
 *   the constitution was adopted by acclamation in December 1936 while the
 *   Great Purge accelerated — the most rights-rich text published as rights
 *   were being maximally violated.
 *
 * KEY AGENTS:
 *   - Communist Party Apparatus: Primary beneficiary (institutional/arbitrage) — exercises undescribed power, maintains plausible deniability, controls cadre appointments and enforcement
 *   - Constitutional Navigators: Primary victim (powerless/trapped) — citizens and officials attempting to operate within written law; no exit from the state; written rules provide no guide to actual authority
 *   - Formal State Institutions: Secondary actor (organized/constrained) — soviets, ministries, courts formally described but actually subordinated to party directives; constrained but also derive legitimacy from constitutional text
 *   - International Legal System: Tertiary actor (institutional/arbitrage) — foreign governments, international bodies; read the constitution as legitimate governance document; maintained this reading through diplomatic courtesy despite understanding the duality
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing the party-state duality as an inevitable feature of revolutionary governance rather than a contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(soviet_constitution_1936__party_state_duality, 0.68).
domain_priors:suppression_score(soviet_constitution_1936__party_state_duality, 0.75).
domain_priors:theater_ratio(soviet_constitution_1936__party_state_duality, 0.88).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(soviet_constitution_1936__party_state_duality, extractiveness, 0.68).
narrative_ontology:constraint_metric(soviet_constitution_1936__party_state_duality, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(soviet_constitution_1936__party_state_duality, theater_ratio, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(soviet_constitution_1936__party_state_duality, snare).
narrative_ontology:human_readable(soviet_constitution_1936__party_state_duality, "Party-State Duality: Constitutional Invisibility of the Command Apparatus (1936)").
narrative_ontology:topic_domain(soviet_constitution_1936__party_state_duality, "political/constitutional").

domain_priors:requires_active_enforcement(soviet_constitution_1936__party_state_duality).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(soviet_constitution_1936__party_state_duality, 'c5e0ceb9-bc83-4c0a-aede-f1248a1454d0').
narrative_ontology:cs_kernel_codification('c5e0ceb9-bc83-4c0a-aede-f1248a1454d0', formalized).
narrative_ontology:cs_authority_grounding('c5e0ceb9-bc83-4c0a-aede-f1248a1454d0', extraction).
narrative_ontology:cs_interpretation_layer_present('c5e0ceb9-bc83-4c0a-aede-f1248a1454d0').
narrative_ontology:cs_reading_relation('c5e0ceb9-bc83-4c0a-aede-f1248a1454d0', soviet_constitution_1936__federal_fiction, coexists_with).
narrative_ontology:cs_reading_relation('c5e0ceb9-bc83-4c0a-aede-f1248a1454d0', soviet_constitution_1936__rights_catalog_facade, coexists_with).
narrative_ontology:cs_reading_relation('c5e0ceb9-bc83-4c0a-aede-f1248a1454d0', soviet_constitution_1936__terror_coincidence, coexists_with).
narrative_ontology:cs_axiom('c5e0ceb9-bc83-4c0a-aede-f1248a1454d0', foundational, party_apparatus_structural_supremacy).
narrative_ontology:cs_axiom_status(party_apparatus_structural_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('c5e0ceb9-bc83-4c0a-aede-f1248a1454d0', party_apparatus_structural_supremacy, empirically_contingent).
narrative_ontology:cs_axiom('c5e0ceb9-bc83-4c0a-aede-f1248a1454d0', foundational, constitutional_invisibility_as_extraction_mechanism).
narrative_ontology:cs_axiom_status(constitutional_invisibility_as_extraction_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('c5e0ceb9-bc83-4c0a-aede-f1248a1454d0', constitutional_invisibility_as_extraction_mechanism, deontological).
narrative_ontology:cs_reference_frame('c5e0ceb9-bc83-4c0a-aede-f1248a1454d0', constitutional_state_with_invisible_command_structure).
narrative_ontology:cs_drift_state('c5e0ceb9-bc83-4c0a-aede-f1248a1454d0', end_of_1936_purge_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('c5e0ceb9-bc83-4c0a-aede-f1248a1454d0', '').
narrative_ontology:cs_kernel_id(soviet_constitution_1936__party_state_duality, soviet_constitution_1936).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(soviet_constitution_1936__party_state_duality, communist_party_apparatus).
narrative_ontology:constraint_victim(soviet_constitution_1936__party_state_duality, constitutional_navigators).
narrative_ontology:constraint_victim(soviet_constitution_1936__party_state_duality, formal_state_institutions).
narrative_ontology:constraint_victim(soviet_constitution_1936__party_state_duality, soviet_citizens).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SOVIET CITIZENS & FORMAL STATE ACTORS (SNARE) — Those attempting to navigate the state apparatus via the 1936 Constitution operate from a false map. The document describes soviets, ministries, and courts as decision-making bodies, but power is actually exercised through undescribed party channels. The trap is total: no exit from the state itself, and the written rules provide no guidance to actual authority. Maximum extraction — the citizen's compliance with written law cannot protect them because law-making power is invisible.
constraint_indexing:constraint_classification(soviet_constitution_1936__party_state_duality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMMUNIST PARTY APPARATUS (ROPE) — The party experiences the constitutional text as a coordination mechanism that solves the problem of ruling while maintaining plausible deniability. The constitution announces the existence of soviets and ministries (which genuinely exist and perform certain functions), while the party operates through parallel channels outside the text. The apparatus benefits from this arrangement: it exercises power without formal accountability, coordinates state action through trusted cadres, and presents an internationally respectable document to the world. Net beneficiary — extraction runs toward the party, not away.
constraint_indexing:constraint_classification(soviet_constitution_1936__party_state_duality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: SOVIETS, MINISTRIES, COURTS (TANGLED ROPE) — These institutions are constrained by the party's parallel command structure but also benefit from the constitutional text's formal legitimation. A ministry formally exercises authority over its domain (coordination function) while actually executing party directives from outside the written structure (extraction function). The institutions cannot exit the party's rule, but they also have agency within delegated boundaries and derive legitimacy from being written into the constitution. Mixed experience: partly autonomous, partly extractive.
constraint_indexing:constraint_classification(soviet_constitution_1936__party_state_duality, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL LEGAL SYSTEM (PITON) — The 1936 Constitution is read by foreign governments and international bodies as a genuine legal framework: it contains federalism, rights enumeration, and formal separation of powers. The document performs legitimacy for an international audience while bearing no functional relationship to actual Soviet governance. The international reading persists through diplomatic inertia and formal courtesies — the text is cited as proof of Soviet legality even by actors who understand the party-state duality. Theater ratio very high: the constitution is almost entirely performative for international consumption.
constraint_indexing:constraint_classification(soviet_constitution_1936__party_state_duality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / INSTITUTIONAL NECESSITY VIEW (MOUNTAIN) — From a civilizational perspective, any revolutionary state facing hostile external pressure must maintain a parallel command structure outside constitutional constraints. The gap between written law and actual power is an irreducible feature of revolutionary governance — constitutional form is incompatible with revolutionary enforcement. This perspective sees the party-state duality as a natural law of transitional politics. However, the structural data reveals this as a false summit: the duality is not inevitable but contingently produced by the choice to maintain both the communist party as a separate institution AND a constitutional state form simultaneously.
constraint_indexing:constraint_classification(soviet_constitution_1936__party_state_duality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(soviet_constitution_1936__party_state_duality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(soviet_constitution_1936__party_state_duality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(soviet_constitution_1936__party_state_duality, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(soviet_constitution_1936__party_state_duality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(soviet_constitution_1936__party_state_duality, TR),
    TR >= 0.70.

:- end_tests(soviet_constitution_1936__party_state_duality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The party extracts substantial benefit from the constitutional arrangement: it rules without formal accountability, maintains an international-respectable facade, coordinates state action through trusted cadres, and suppresses alternative power centers. The extraction is not total (some coordination benefit exists — the constitution does enable certain state functions), but the asymmetry is severe. Suppression (0.75): Very high. The suppression operates at multiple levels: (1) textual suppression — the party apparatus is barely described; (2) coercive suppression — exit from the state is impossible; (3) cognitive suppression — those trained to read constitutions as law-books cannot easily recognize that actual power lies outside the text. Theater ratio (0.88): Very high. The constitution is substantially performative: it announces formal procedures (election of soviets, judicial independence, federalism) that function as theater masking party control. The text stages a constitutional state while power operates through undescribed channels. The performance is nearly complete — 88% of the constitutional machinery is designed for show rather than governance. Baseline theater (0.80 at interval start) reflects the constitutional text's inherent performativity; the rise to 0.88 reflects intensifying gap as the purge demonstrates that constitutional protections are illusory.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a profound perspectival gap between the party apparatus and those attempting to navigate by the constitution. The party sees coordination (Rope) — the constitution solves their problem of ruling while maintaining legitimacy. Constitutional navigators see pure extraction (Snare) — they face a trap with no exit and no guide. The formal state institutions occupy a middle position (Tangled Rope) — they are constrained by party directives but also enabled by constitutional legitimacy. The international observer sees performative legitimacy (Piton) — the constitution is maintained as theater despite known irrelevance to actual governance. The civilizational analytical view risks seeing an inevitable institutional law (Mountain) — but the false summit detector fires because identifiable beneficiaries (the party) exist and the duality is contingent, not necessary.
 *
 * DIRECTIONALITY LOGIC:
 *   The party apparatus derives d ≈ 0.05 (full beneficiary, arbitrage exit) — they experience negative effective extraction, benefiting from the constraint. Constitutional navigators derive d ≈ 0.95 (full victim, trapped exit) — they experience maximum extraction, bearing the cost of the false map with no escape route. Formal state institutions derive d ≈ 0.55 (mixed victim-beneficiary, constrained exit) — they are subordinated to party control but also derive legitimacy from the constitution, experiencing tangled costs and benefits. The international observer derives d ≈ 0.72 (analytical position, observing performance) — they see the constraint from outside, understanding it as theater but maintaining diplomatic engagement with the text. The directionality gap between beneficiary (d=0.05) and trapped victim (d=0.95) is maximal, producing the snare classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing the coordination function (the constitution does coordinate certain state functions) from the extraction function (the party extracts power through undescribed channels). The snare classification holds because the extraction so completely dominates the coordination that the coordinated functions become tools of extraction. A citizen complying with written law is actually ensuring their own entrapment — the constitutional machinery coordinates compliance while the party apparatus controls the stakes. The mandatrophy is resolved by recognizing that in a snare, the coordination function is entirely subordinated to extraction: constitutional rules exist to make victims more efficiently controllable, not to distribute power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_of_suppression,
    'Was the suppression of the party apparatus from the constitutional text a deliberate architectural choice or an implicit artifact of how Bolshevik governance evolved?',
    'Analysis of drafting debates, Bukharin''s theoretical statements on the constitution, and comparison with earlier Soviet constitutions (1918, 1924) to determine whether the duality was consciously constructed or inherited',
    'If deliberate: the constraint is a snare by design — the party intentionally created a false map to extract compliance. If implicit: the constraint is a piton — the text simply failed to describe power as it had evolved. Intentionality shifts the beneficiary''s culpability and the victim''s interpretive error from structural trap to navigational failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_of_suppression, conceptual, 'Whether suppression of party apparatus from constitution was deliberate or implicit').

omega_variable(
    alternative_reading_viability,
    'Could a Soviet citizen, party official, or foreign observer in 1936 have recognized the party-state duality from the constitutional text alone, or was the suppression truly opaque to contemporary readers?',
    'Contemporaneous interpretations: legal scholarship from 1936-1937, official commentary on the constitution, diplomatic cables describing foreign understanding, party communications to cadres about how to navigate the dual structure',
    'If viability high: the suppression is less total — the duality was legible to informed readers, and victims bear some responsibility for navigating by the written text. If viability low: suppression was near-total deception, and extraction is maximized — the constitution operated as a trap, not merely a incomplete map.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_reading_viability, empirical, 'Whether party-state duality was legible to contemporary readers').

omega_variable(
    false_summit_diagnosis,
    'Is the party-state duality presented as an immutable feature of communist governance (natural law) or a contingent institutional arrangement (snare)?',
    'Contrast the mountain perspective''s claim (revolutionary governance inherently requires hidden command structure) against the snare perspective''s claim (the duality is a choice to maintain separate institutions). The false summit detector fires if the constitution benefits identifiable agents (the party) while being naturalized as inevitable law.',
    'If mountain: the constraint is treated as a governing law of transitional states — all such states must have dual power structures. If snare: the constraint is a specific artifact of Leninist party organization — alternative revolutionary constitutions exist with more integrated governance structures. Diagnosis determines whether the party-state duality generalizes or is unique to Soviet model.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_diagnosis, conceptual, 'False summit detection: naturalization of contingent institutional duality').

omega_variable(
    victim_agency_in_compliance,
    'To what extent did Soviet citizens and state officials actively maintain the constitutional fiction through voluntary compliance, versus being coerced into treating the written text as authoritative?',
    'Analysis of state discourse practices: official invocation of constitutional provisions in public contexts (speeches, decrees, education), party guidance to cadres on navigating dual structure, evidence of citizens internalizing the written rules as legitimate despite knowing of party override',
    'If high voluntary maintenance: victims partially co-produce the snare through their own compliance behaviors — the trap is collaborative. If high coercion: the snare is imposed against resistant subjects — the trap is total extraction. The balance determines whether the classification should shift toward tangled_rope (some coordination benefit to maintaining the fiction) or stay at snare (pure extraction through deception).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_agency_in_compliance, empirical, 'Victim agency in maintaining constitutional fiction versus coercion').

omega_variable(
    party_apparatus_visibility_asymmetry,
    'Is the party apparatus truly invisible in the 1936 Constitution, or is it described through other textual channels (preamble, party role in soviets, references to party discipline)?',
    'Close textual analysis of the constitution: enumeration of all mentions of the Communist Party, its functions, and its relationship to state organs; comparison with 1918 and 1924 constitutions which more explicitly named party supremacy; analysis of what counts as ''description'' versus ''suppression''',
    'If truly invisible: the snare classification is correct — the apparatus is actively omitted. If implicitly present: the classification should shift toward tangled_rope — the duality is described indirectly (party members in soviet leadership), and victims have textual resources to infer the structure. The omega documents whether ''barely mentioned'' in the source material reflects actual constitutional text or historiographical interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(party_apparatus_visibility_asymmetry, empirical, 'Degree of explicit versus implicit party apparatus visibility in constitutional text').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(soviet_constitution_1936__party_state_duality, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sovi_tr_t0, soviet_constitution_1936__party_state_duality, theater_ratio, 0, 0.8).
narrative_ontology:measurement(sovi_tr_t1, soviet_constitution_1936__party_state_duality, theater_ratio, 1, 0.88).

% Extraction over time
narrative_ontology:measurement(sovi_be_t0, soviet_constitution_1936__party_state_duality, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(sovi_be_t1, soviet_constitution_1936__party_state_duality, base_extractiveness, 1, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sovi_su_t0, soviet_constitution_1936__party_state_duality, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(sovi_su_t1, soviet_constitution_1936__party_state_duality, suppression_requirement, 1, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(soviet_constitution_1936__party_state_duality, enforcement_mechanism).
narrative_ontology:affects_constraint(soviet_constitution_1936__party_state_duality, soviet_constitution_1936__federal_fiction).
narrative_ontology:affects_constraint(soviet_constitution_1936__party_state_duality, soviet_constitution_1936__rights_catalog_facade).
narrative_ontology:affects_constraint(soviet_constitution_1936__party_state_duality, soviet_constitution_1936__terror_coincidence).

% DUAL FORMULATION NOTE:
% The 1936 Soviet Constitution kernel decomposes into multiple constraint stories, each instantiating a different reading of the same text. The party_state_duality reading (this story) examines the extraction produced by the constitutional text's suppression of the party apparatus. The federal_fiction reading examines the contradiction between federalism and centralized party control. The rights_catalog_facade reading examines the coincidence of rights enumeration and their violation. The terror_coincidence reading examines the temporal synchrony of constitutional adoption and purge intensification. Each reading has its own extractiveness value, beneficiary/victim structure, and perspectives. They are linked through the kernel but represent structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
