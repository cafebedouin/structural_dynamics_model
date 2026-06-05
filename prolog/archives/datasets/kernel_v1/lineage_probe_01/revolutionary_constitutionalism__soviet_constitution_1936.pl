% ============================================================================
% CONSTRAINT STORY: revolutionary_constitutionalism__soviet_constitution_1936
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_soviet_constitution_1936, []).

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
 *   constraint_id: revolutionary_constitutionalism__soviet_constitution_1936
 *   human_readable: The Stalin Constitution of 1936: Revolutionary Constitutionalism as Totalitarian Facade
 *   domain: political/legal/constitutional_law
 *
 * SUMMARY:
 *   The Stalin Constitution of 1936 represents revolutionary
 *   constitutionalism as totalitarian facade — a text that enumerates
 *   extensive rights while systematically concealing that actual power runs
 *   entirely through the Communist Party apparatus, which the constitution
 *   never mentions constraining. The constraint operates at maximum
 *   extractiveness and suppression: the nominal guarantees of work,
 *   education, housing, freedom of speech, and assembly are rendered
 *   meaningless by the principle that all rights exist 'in accordance with
 *   the interests of the working people and in the interests of strengthening
 *   the socialist state,' a formulation that reserves absolute interpretive
 *   authority to the party. The constitution creates no independent mechanism
 *   for adjudicating conflicts between citizens and state; no court can
 *   strike down party decisions as unconstitutional; no assembly of citizens
 *   can constrain party power through constitutional amendment. The
 *   constraint demonstrates how a legal text can simultaneously serve three
 *   functions: (1) international legitimacy (presenting the Soviet Union as a
 *   constitutional state), (2) internal theater (creating the appearance of
 *   constraint while maintaining absolute party control), and (3) total
 *   suppression (ensuring that any appeal to constitutional rights is met
 *   with secret police enforcement of party discipline). The constitutional
 *   guarantees are not merely unenforced; they are strategically enumerated
 *   to create the false impression of constraint, making their violation less
 *   visible and more deniable than naked dictatorship would be.
 *
 * KEY AGENTS:
 *   - Communist Party Apparatus: Primary beneficiary (institutional/arbitrage) — gains international legitimacy and internal control by operating behind constitutional facade; extracts total power without constitutional constraint
 *   - Soviet Populace: Primary victim (powerless/trapped) — nominally granted rights but systematically suppressed by party apparatus unmentioned and unconstrained by constitution; no exit option
 *   - Constitutional Legitimacy: Abstract victim (powerless/trapped) — the concept of constitutional constraint is violated so thoroughly that the very notion of constitutional government is degraded through systematic misuse
 *   - Soviet Legal Intelligentsia: Secondary victims (powerful/constrained) — tasked with administering a constitution they must know is a facade; extract professional benefit while suppressing knowledge of contradiction
 *   - International Observers: Tertiary observers (analytical/constrained) — excluded from actual power structure; can only judge from text and observe performance; their analytical leverage is degraded
 *   - Historical Comparative Frame: Analytical position (analytical/analytical) — enables recognition that the Soviet choice is contingent, not inevitable; other revolutionary regimes made different constitutional choices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(revolutionary_constitutionalism__soviet_constitution_1936, 0.92).
domain_priors:suppression_score(revolutionary_constitutionalism__soviet_constitution_1936, 0.95).
domain_priors:theater_ratio(revolutionary_constitutionalism__soviet_constitution_1936, 0.88).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(revolutionary_constitutionalism__soviet_constitution_1936, extractiveness, 0.92).
narrative_ontology:constraint_metric(revolutionary_constitutionalism__soviet_constitution_1936, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(revolutionary_constitutionalism__soviet_constitution_1936, theater_ratio, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(revolutionary_constitutionalism__soviet_constitution_1936, snare).
narrative_ontology:human_readable(revolutionary_constitutionalism__soviet_constitution_1936, "The Stalin Constitution of 1936: Revolutionary Constitutionalism as Totalitarian Facade").
narrative_ontology:topic_domain(revolutionary_constitutionalism__soviet_constitution_1936, "political/legal/constitutional_law").

domain_priors:requires_active_enforcement(revolutionary_constitutionalism__soviet_constitution_1936).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(revolutionary_constitutionalism__soviet_constitution_1936, '18add361-b81f-4731-a0b7-e34cf39800ae').
narrative_ontology:cs_kernel_codification('18add361-b81f-4731-a0b7-e34cf39800ae', fixed_text).
narrative_ontology:cs_authority_grounding('18add361-b81f-4731-a0b7-e34cf39800ae', extraction).
narrative_ontology:cs_interpretation_layer_present('18add361-b81f-4731-a0b7-e34cf39800ae').
narrative_ontology:cs_reading_relation('18add361-b81f-4731-a0b7-e34cf39800ae', revolutionary_constitutionalism__french_constitution_1791, coexists_with).
narrative_ontology:cs_reading_relation('18add361-b81f-4731-a0b7-e34cf39800ae', revolutionary_constitutionalism__us_constitution, coexists_with).
narrative_ontology:cs_axiom('18add361-b81f-4731-a0b7-e34cf39800ae', foundational, nominal_rights_as_concealment).
narrative_ontology:cs_axiom_status(nominal_rights_as_concealment, holdable).
narrative_ontology:cs_axiom_grounding('18add361-b81f-4731-a0b7-e34cf39800ae', nominal_rights_as_concealment, instrumental).
narrative_ontology:cs_axiom('18add361-b81f-4731-a0b7-e34cf39800ae', foundational, party_apparatus_supremacy).
narrative_ontology:cs_axiom_status(party_apparatus_supremacy, overridden).
narrative_ontology:cs_axiom_grounding('18add361-b81f-4731-a0b7-e34cf39800ae', party_apparatus_supremacy, deontological).
narrative_ontology:cs_reference_frame('18add361-b81f-4731-a0b7-e34cf39800ae', party_dictatorship_concealed_as_constitutional_socialism).
narrative_ontology:cs_drift_state('18add361-b81f-4731-a0b7-e34cf39800ae', post_1936_stalinist_consolidation, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('18add361-b81f-4731-a0b7-e34cf39800ae', '').
narrative_ontology:cs_kernel_id(revolutionary_constitutionalism__soviet_constitution_1936, revolutionary_constitutionalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(revolutionary_constitutionalism__soviet_constitution_1936, communist_party_apparatus).
narrative_ontology:constraint_victim(revolutionary_constitutionalism__soviet_constitution_1936, soviet_populace).
narrative_ontology:constraint_victim(revolutionary_constitutionalism__soviet_constitution_1936, constitutional_legitimacy).
narrative_ontology:constraint_victim(revolutionary_constitutionalism__soviet_constitution_1936, rule_of_law).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SOVIET CITIZEN (SNARE) — Nominally granted rights by the constitution (work, education, housing, speech, assembly). In structural reality, every right is constrained by 'in accordance with the interests of the working people' and enforced by the party apparatus, which the constitution never mentions. The citizen is trapped with no exit option and no mechanism to claim constitutional protection. The constitution's enumerated rights are theater; actual governance runs through party discipline and secret police. Maximum extraction and suppression experienced by the powerless agent.
constraint_indexing:constraint_classification(revolutionary_constitutionalism__soviet_constitution_1936, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMMUNIST PARTY APPARATUS (ROPE) — The apparatus benefits from the constitution's facade: it gains international legitimacy as a constitutional state while actual power runs entirely through party channels, unrestricted by the text. The party experiences the constitution as coordination mechanism — it coordinates the appearance of legality with the reality of party dictatorship. No extraction is experienced; instead, the apparatus extracts benefit from others. The constitution appears as a pure coordination tool that enables the party to present itself as constitutional while maintaining absolute control.
constraint_indexing:constraint_classification(revolutionary_constitutionalism__soviet_constitution_1936, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: SOVIET LEGAL INTELLIGENTSIA (TANGLED ROPE) — Constitutional lawyers and judges occupy a structurally ambiguous position. They are tasked with administering the constitution as if it were a real legal constraint, producing scholarly commentary that treats the text as meaningful. This gives them limited agency and some professional status. But they bear the cost of cognitive dissonance: the text they study is systematically violated by the party apparatus they serve. They cannot exit without losing their position, yet they cannot fully commit to the fiction. The constraint extracts their intellectual labor while suppressing their ability to note the contradiction.
constraint_indexing:constraint_classification(revolutionary_constitutionalism__soviet_constitution_1936, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL LIBERAL OBSERVERS (PITON) — From the liberal democratic perspective, the 1936 Constitution appears as a performance: it mimics the form of liberal constitutionalism (enumerated rights, separation of powers language) while maintaining totalitarian substance. The observation is degraded because the observers are excluded from the actual power structure and can only judge from text and propaganda. Their classification as a constraint degrades from Snare (if they had power to enforce compliance) to Piton (they can only comment and observe, with minimal actual leverage). The theatrical performance is maintained because it serves the regime's international positioning.
constraint_indexing:constraint_classification(revolutionary_constitutionalism__soviet_constitution_1936, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / REVOLUTIONARY CONTRADICTION (MOUNTAIN) — From a civilizational view, the constraint appears as an immutable structural feature of revolutionary constitutionalism itself: the gap between revolutionary principle (rights for the people) and revolutionary practice (dictatorship of the party) is inherent to all revolutions, not specific to Stalin. This perspective risks naturalizing what is actually a contingent choice — the Soviet regime's decision to use the constitution as facade rather than attempting to institutionalize constraints on party power (as the French republic attempted, or as the US constitutional system partially achieves). The analytical observer's mountain classification is a false summit, naturalizating the regime's strategic choice as inevitable.
constraint_indexing:constraint_classification(revolutionary_constitutionalism__soviet_constitution_1936, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: HISTORICAL ANALYST / CONTINGENT CHOICE (SNARE) — The 1936 Constitution is not inevitable. Other revolutionary regimes made different choices: the French 1791 Constitution attempted (unsuccessfully) to establish real constraints; the US Constitution achieved partial institutional durability. The Soviet choice to write extensive nominal rights while maintaining totalitarian suppression is a specific strategic decision, not a law of nature. The constitution's beneficiary (party apparatus) and victim (everyone else) are clearly identifiable. The extractiveness is not a feature of revolutionary constitutionalism in general but of Stalinist governance specifically. From this perspective, the constraint is a deliberate Snare, not a tragic inevitability.
constraint_indexing:constraint_classification(revolutionary_constitutionalism__soviet_constitution_1936, snare,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(revolutionary_constitutionalism__soviet_constitution_1936_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(revolutionary_constitutionalism__soviet_constitution_1936, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(revolutionary_constitutionalism__soviet_constitution_1936, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(revolutionary_constitutionalism__soviet_constitution_1936, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(revolutionary_constitutionalism__soviet_constitution_1936, TR),
    TR >= 0.70.

:- end_tests(revolutionary_constitutionalism__soviet_constitution_1936_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.92): Maximal. The party apparatus extracts total power while maintaining the appearance of constitutional constraint. The citizen nominally retains rights but all are subordinated to 'the interests of the socialist state' — meaning party determination. The extraction increases from 0.72 to 0.92 over the interval (0-5 years post-1936) because the party gains confidence in the facade and reduces even the minimal restraint or ambiguity that might have existed in the constitution's first year. Suppression (0.95): Maximal. The constitution mentions no mechanism to constrain the party; the secret police and party discipline are the actual enforcement apparatus. Citizens have no appeal to constitutional authority; any attempt to invoke constitutional rights is met with suppression disguised as party discipline. The suppression is constitutionally invisible — the document provides no vocabulary for naming or challenging it. Theater ratio (0.88): High. The entire constitutional apparatus is performative. Constitutional conferences are staged. Constitutional amendments go through ritual motions while party decisions remain unchanged. Judicial proceedings follow constitutional forms while following party orders. Legal scholarship engages with constitutional text while knowing it is divorced from reality. The theater increases from 0.78 to 0.88 because the regime becomes more confident in its performance, reducing even the minimal reality-checking that early uncertainty might have forced. Claimed type (Snare): The constraint is pure extraction — the party extracts total power while suppressing all alternatives and all appeals to constitutional constraint. The beneficiary is identifiable and concentrated (party apparatus); the victims are dispersed (entire populace); no genuine coordination function exists. The constitution is not Tangled Rope (which would require some genuine coordination benefit) but pure Snare with maximal theater.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates complete perspectival divergence. The party apparatus sees Rope — the constitution as a coordination mechanism enabling the appearance of legitimacy. The Soviet citizen sees Snare — total extraction behind nominal guarantees. The legal intelligentsia sees Tangled Rope — professional benefit mixed with enforced complicity in a fiction. International observers see Piton — a degraded performance without leverage to enforce real constraint. The civilizational analytical observer risks seeing Mountain — the constitutional gap as inherent to revolutionary governance. But the historical comparative analyst sees Snare — the gap as a specific Stalinist choice, not inevitable. The perspectival gap reveals that the Mountain classification is false summit: the constraint is not an immutable law of revolutionary constitutionalism but a contingent strategic choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies radically by perspective. For the powerless citizen (trapped/powerless), d approaches 1.0 — the entire extraction flows away from them, toward the party apparatus. The sigmoid f(d) at d≈0.95 yields ~1.42, indicating maximum experienced extractiveness. For the party apparatus (arbitrage/institutional), d approaches 0.0 — they are pure beneficiary with exit option (they can modify the constitution or abandon it entirely). The sigmoid f(d) at d≈0.05 yields ~-0.12, indicating the apparatus experiences negative extraction (they extract from others while bearing no cost themselves). For the legal intelligentsia (constrained/powerful), d≈0.70 — they are constrained but not trapped, with some professional agency. The sigmoid yields ~1.10, indicating they experience moderately high extraction through cognitive dissonance and complicity. For international observers (analytical/constrained), d≈0.72 — they are outside the extraction flow but constrained by lack of leverage. The sigmoid yields ~1.15, indicating their analytical perspective experiences the constraint as an extractive gap between text and reality that they cannot resolve. The directionality overrides are not needed; the structural derivation from beneficiary/victim and exit options produces the correct d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exceeds ε > 0.70 (extractiveness 0.92) and therefore requires mandatrophy resolution. The mandatrophy asks: Is this Snare truly extraction or misidentified coordination? The structural data resolves this decisively: (1) Beneficiary is identifiable and concentrated (party apparatus) rather than distributed. (2) Victims are identifiable and extensive (entire populace). (3) No mechanism exists for victims to claim coordination benefits or enforce rights. (4) The constitution creates appearance of constraint while maintaining total suppression. (5) All perspectives except the beneficiary's experience pure or near-pure extraction. The mandatrophy is resolved by observing that genuine coordination would require some mechanism for disputes between parties to be adjudicated by constitutional authority — courts, assemblies, or amendment processes. The 1936 Constitution provides none of these mechanisms; all are subordinated to party will. Therefore, the constraint is genuinely Snare, not misidentified Tangled Rope. The classification is not aspirational or uncertain; it is structurally confirmed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    facade_functionality_ambiguity,
    'Does the constitution''s facade function as genuine international legitimacy-building (enabling Soviet diplomatic recognition and trade), or is it merely decorative propaganda with no causal effect on state behavior?',
    'Comparative analysis of Soviet diplomatic acceptance pre- and post-1936; correlation between constitutional text and actual foreign policy recognition; historical counterfactual of whether similar regime policies would have been possible without constitutional facade',
    'If facade is functionally legitimizing: the constitution is Tangled Rope from the party''s perspective (genuine coordination benefit mixed with pure extraction from citizens). If merely decorative: it is pure Snare for all perspectives (no coordination function, only theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(facade_functionality_ambiguity, empirical, 'Whether the constitution''s facade provides genuine international legitimacy or is purely decorative').

omega_variable(
    kernel_reading_contest,
    'Is the 1936 Constitution a reading of ''revolutionary constitutionalism as such'' (universal structure inherent to all revolutions), or a specific instantiation of ''Stalinist strategic choice'' (contingent decision among alternatives)?',
    'Comparative constitutional analysis: examine French 1791, US 1787, and later Soviet constitutions (1977, 1993) to identify whether the gap between nominal and actual constraint is universal to revolutions or specific to Stalinist totalitarianism. Examine internal Soviet debate about constitutional design: did Bolshevik leaders consider but reject constraint mechanisms?',
    'If universal (Mountain perspective): the constraint is inherent to revolutionary constitution-writing; Snare classification is perspectival misidentification of immutable structure. If contingent (Snare perspective): the Mountain classification is false summit; the regime chose this structure and could have chosen differently.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the constitutional facade is inherent to revolutionary constitutionalism or a contingent Stalinist choice').

omega_variable(
    soviet_citizen_coordination_benefit,
    'Do Soviet citizens derive any genuine coordination benefit from the constitution''s guarantees, or is the entire constraint structure pure extraction masquerading as coordination?',
    'Analysis of how citizens actually used constitutional provisions (workplace grievance mechanisms, housing rights claims, etc.); documentation of rare cases where constitutional text was invoked successfully; comparison of citizen welfare and rights access in constitutionalized vs. purely authoritarian regimes',
    'If coordination benefit exists: perspective shifts from Snare to Tangled Rope for powerless agent (mixed extraction and coordination). If no benefit: confirms Snare; the entire constitutional framework is theater concealing pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(soviet_citizen_coordination_benefit, empirical, 'Whether Soviet citizens gained any genuine coordination benefit from constitutional provisions').

omega_variable(
    reading_relations_foreclosure,
    'Does the Soviet reading''s core claim (constitutional text as systematic facade concealing totalitarian power) logically foreclose the US reading (constitutional text as durable framework absorbing transformation)?',
    'Examine whether both readings can be held within a single analytical framework that distinguishes constitutional types by institutional design features (entrenchment mechanisms, amendment processes, review powers) and historical path dependence. Determine whether the readings compete (different regimes made different choices) or whether one''s success proves the other''s logical impossibility.',
    'If foreclosed: the readings make contradictory claims about what constitutions can do; they cannot coexist. If coexist: different regimes made different institutional choices; both readings remain live.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_relations_foreclosure, conceptual, 'Whether the Soviet facade reading forecloses the US durability reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(revolutionary_constitutionalism__soviet_constitution_1936, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sovcon_tr_t0, revolutionary_constitutionalism__soviet_constitution_1936, theater_ratio, 0, 0.78).
narrative_ontology:measurement(sovcon_tr_t2, revolutionary_constitutionalism__soviet_constitution_1936, theater_ratio, 2, 0.84).
narrative_ontology:measurement(sovcon_tr_t5, revolutionary_constitutionalism__soviet_constitution_1936, theater_ratio, 5, 0.88).

% Extraction over time
narrative_ontology:measurement(sovcon_be_t0, revolutionary_constitutionalism__soviet_constitution_1936, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(sovcon_be_t2, revolutionary_constitutionalism__soviet_constitution_1936, base_extractiveness, 2, 0.82).
narrative_ontology:measurement(sovcon_be_t5, revolutionary_constitutionalism__soviet_constitution_1936, base_extractiveness, 5, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(sovcon_su_t0, revolutionary_constitutionalism__soviet_constitution_1936, suppression_requirement, 0, 0.88).
narrative_ontology:measurement(sovcon_su_t2, revolutionary_constitutionalism__soviet_constitution_1936, suppression_requirement, 2, 0.92).
narrative_ontology:measurement(sovcon_su_t5, revolutionary_constitutionalism__soviet_constitution_1936, suppression_requirement, 5, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(revolutionary_constitutionalism__soviet_constitution_1936, enforcement_mechanism).
narrative_ontology:affects_constraint(revolutionary_constitutionalism__soviet_constitution_1936, french_constitution_1791).
narrative_ontology:affects_constraint(revolutionary_constitutionalism__soviet_constitution_1936, us_constitution).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the revolutionary_constitutionalism kernel. The French 1791 and US Constitution are structurally distinct constraints with different ε values (French: ~0.55 Tangled Rope; US: ~0.15 Rope). The network relationship is one of sibling readings competing for interpretive authority over what revolutionary constitutionalism produces, not a causal dependency. The Soviet reading 'influences' both siblings by demonstrating that revolutionary constitutionalism can sustain maximal extraction if institutional design explicitly rejects constraint mechanisms — this creates pressure on the French and US readings to explain why they made different choices.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
