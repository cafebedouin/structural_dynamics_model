% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__revisionist_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__revisionist_zionism_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jewish_territorial_claim__revisionist_zionism_reading
 *   human_readable: Revisionist Zionist Maximalist Territorial Claim ('Iron Wall' Doctrine)
 *   domain: political/historical/settler_colonialism
 *
 * SUMMARY:
 *   This constraint instantiates the Revisionist Zionist reading of the
 *   contested jewish_territorial_claim kernel: Jabotinsky's 1923 'Iron Wall'
 *   doctrine and its paramilitary successors, which claimed sovereignty over
 *   both banks of the Jordan as a non-negotiable national right and held that
 *   Arab acceptance was to be compelled by demonstrated, unbreachable
 *   military force rather than sought through consent or negotiated
 *   partition. This is a maximalist reading distinct from political Zionism's
 *   negotiated-majority framing, labor Zionism's
 *   settlement-and-institution-building framing, and cultural Zionism's
 *   non-sovereign spiritual-center framing — those are separate constraints
 *   (siblings in the same kernel), not alternative measurements of this one.
 *   The extraction and suppression trajectories rise steadily across the
 *   interval as paramilitary capacity (Irgun, later Lehi) matured from
 *   doctrine into organized armed enforcement, culminating in the 1948 war
 *   and the political consolidation (Herut) of the maximalist claim into an
 *   enduring political-territorial program that outlived the immediate
 *   Mandate-era context.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, 0.81).
domain_priors:suppression_score(jewish_territorial_claim__revisionist_zionism_reading, 0.87).
domain_priors:theater_ratio(jewish_territorial_claim__revisionist_zionism_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__revisionist_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__revisionist_zionism_reading, "Revisionist Zionist Maximalist Territorial Claim ('Iron Wall' Doctrine)").
narrative_ontology:topic_domain(jewish_territorial_claim__revisionist_zionism_reading, "political/historical/settler_colonialism").

domain_priors:requires_active_enforcement(jewish_territorial_claim__revisionist_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__revisionist_zionism_reading, '15b80289-0ce7-4631-8c75-3ea2d81d3db5').
narrative_ontology:cs_kernel_codification('15b80289-0ce7-4631-8c75-3ea2d81d3db5', distributed).
narrative_ontology:cs_authority_grounding('15b80289-0ce7-4631-8c75-3ea2d81d3db5', practice).
narrative_ontology:cs_interpretation_layer_present('15b80289-0ce7-4631-8c75-3ea2d81d3db5').
narrative_ontology:cs_reading_relation('15b80289-0ce7-4631-8c75-3ea2d81d3db5', jewish_territorial_claim__political_zionism_reading, influences).
narrative_ontology:cs_reading_relation('15b80289-0ce7-4631-8c75-3ea2d81d3db5', jewish_territorial_claim__labor_zionism_reading, influences).
narrative_ontology:cs_reading_relation('15b80289-0ce7-4631-8c75-3ea2d81d3db5', jewish_territorial_claim__cultural_zionism_reading, forecloses).
narrative_ontology:cs_axiom('15b80289-0ce7-4631-8c75-3ea2d81d3db5', foundational, arab_consent_not_a_prerequisite).
narrative_ontology:cs_axiom_status(arab_consent_not_a_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('15b80289-0ce7-4631-8c75-3ea2d81d3db5', arab_consent_not_a_prerequisite, instrumental).
narrative_ontology:cs_axiom('15b80289-0ce7-4631-8c75-3ea2d81d3db5', foundational, military_force_as_primary_sequential_mechanism).
narrative_ontology:cs_axiom_status(military_force_as_primary_sequential_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('15b80289-0ce7-4631-8c75-3ea2d81d3db5', military_force_as_primary_sequential_mechanism, instrumental).
narrative_ontology:cs_axiom('15b80289-0ce7-4631-8c75-3ea2d81d3db5', secondary, territorial_maximalism_both_banks_nonnegotiable).
narrative_ontology:cs_axiom_status(territorial_maximalism_both_banks_nonnegotiable, overridden).
narrative_ontology:cs_axiom_grounding('15b80289-0ce7-4631-8c75-3ea2d81d3db5', territorial_maximalism_both_banks_nonnegotiable, conventional).
narrative_ontology:cs_reference_frame('15b80289-0ce7-4631-8c75-3ea2d81d3db5', irredentist_national_maximalism).
narrative_ontology:cs_drift_state('15b80289-0ce7-4631-8c75-3ea2d81d3db5', post_1948_statehood, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('15b80289-0ce7-4631-8c75-3ea2d81d3db5', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, irgun_and_successor_militias).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, settler_land_claimants_east_and_west_bank).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arab_population).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, transjordanian_arab_population).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, labor_zionist_settlement_project).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__revisionist_zionism_reading, national_self_determination_via_force_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jabotinsky's movement and its political heirs articulate the claim that Jewish sovereignty over both banks of the Jordan is a national right requiring no Arab consent, and that only an 'Iron Wall' of Jewish military force, presented to Arab society as unbreachable, can compel acceptance of Jewish statehood. They set the doctrine, organize paramilitary capacity (Irgun, later Lehi splinters, and post-1948 political successors), and treat negotiation as premature until the wall is established.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement, agenda_setter,
    organized, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement, beneficiary).

% Paramilitary organizations that operationalize the Iron Wall doctrine through armed action against British administration, Arab communities, and rival Zionist factions favoring restraint. They gain organizational purpose, recruitment legitimacy, and post-independence political capital (culminating in electoral power) from the doctrine's persistence.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, irgun_and_successor_militias, agenda_setter,
    organized, biographical, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__revisionist_zionism_reading, irgun_and_successor_militias, beneficiary).

% Jewish settlers whose claims to land on both banks of the Jordan are legitimated and expanded by the maximalist doctrine. They benefit from ideological cover for territorial acquisition beyond what negotiated partition would have granted, but are also bound to defend indefensible-without-force positions.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, settler_land_claimants_east_and_west_bank, beneficiary,
    moderate, generational, constrained, regional).

% The Arab population of Palestine is treated by the doctrine as an obstacle whose consent is structurally unnecessary and whose resistance is to be broken by demonstrated force rather than negotiated with. They bear displacement, land loss, and military suppression; the doctrine explicitly denies them a veto or a negotiating seat until they capitulate to the wall.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arab_population, payer,
    powerless, generational, trapped, regional).

% Under the 'both banks' claim, the population east of the Jordan (governed at the time under British Mandate arrangements later becoming Transjordan) is claimed as part of the territory requiring Jewish sovereignty. They have no representation in the doctrine's formation and their existing political arrangements are treated as illegitimate obstacles to the claim.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, transjordanian_arab_population, payer,
    powerless, generational, trapped, regional).

% The dominant Labor Zionist institutions (Yishuv leadership, Haganah, later the state apparatus) bear reputational, diplomatic, and security costs imposed by Revisionist maximalism and its paramilitary enforcement — international condemnation, British crackdowns affecting all Zionist factions, and internal civil conflict (e.g. the Altalena affair). Their gradualist, negotiation-oriented strategy is repeatedly destabilized by unilateral Revisionist military action they did not authorize.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, labor_zionist_settlement_project, payer,
    organized, generational, constrained, regional).

% The Mandate administration is the nominal sovereign the doctrine treats as an obstacle to be outlasted or attacked rather than negotiated with in good faith; its own governance preferences carry no weight in the doctrine's calculus and it becomes a direct military target of enforcement actions.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, british_mandatory_authority, excluded,
    institutional, biographical, constrained, regional).

% Scholars examine Jabotinsky's writings (notably 'The Iron Wall', 1923) and the subsequent trajectory of Revisionist and Herut/Likud politics to assess whether the doctrine functioned as a coherent strategic theory of forced accommodation or as ideological cover for territorial maximalism irrespective of Arab welfare.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, historians_of_zionism, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__revisionist_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Within the Revisionist movement itself, the doctrine coordinates otherwise fractious factions and paramilitary cells around a single, unambiguous strategic theory: that peace can only follow demonstrated Jewish military supremacy, never precede it. This solves an internal coordination problem — it gives disparate militant groups a shared theory of victory and sequencing.
% TRANSFER_FUNCTION: The doctrine transfers land, political voice, and physical security away from Palestinian and Transjordanian Arab populations toward the Jewish national project's most militant faction, and additionally transfers strategic control and diplomatic risk away from moderate Zionist institutions (who did not choose this sequencing) onto the entire Zionist project's international standing.
% ABSENT_VOICES: Palestinian and Transjordanian Arabs are categorically excluded from the doctrine's deliberation — the doctrine's own logic holds that their voice is irrelevant until they are militarily compelled to accept an outcome already decided. Moderate Zionist voices favoring negotiated accommodation (Weizmann, later Ben-Gurion's more pragmatic phases) are present but structurally overridden whenever Revisionist paramilitary action unilaterally changes facts on the ground.
% DISAPPEARANCE_RATIONALE: Without the Iron Wall doctrine and its paramilitary enforcement apparatus, the trajectory of Zionist state-building plausibly shifts toward negotiated partition models advanced by Labor Zionism and cultural Zionists, with different borders, different treatment of the Transjordan claim, and a substantially different relationship between the eventual state and its Arab population — the doctrine's removal changes both territory and method, not merely rhetoric.
% FOUNDING_PROBLEM: The perceived problem: Arab society would never voluntarily accept large-scale Jewish immigration and eventual Jewish sovereignty over Palestine (and, in the maximalist version, Transjordan), no matter how the offer was framed or how limited the initial claim; therefore acceptance would have to be compelled by an unambiguous demonstration of Jewish military strength before any negotiation could be meaningful.
% FOUNDING_PROBLEM_CORROBORATION: Jabotinsky and Revisionist successors attest the problem was real and remains real, citing continued regional rejection of Jewish sovereignty. Independent historians (including some sympathetic to Zionism broadly, e.g. Anita Shapira, Benny Morris in his later work) attest that the 'no possible Arab consent' premise was never rigorously tested against less maximalist offers, and that the doctrine's persistence past 1948 — as territorial maximalism toward the West Bank and beyond — reflects continued ideological commitment rather than a live unresolved security problem, since the state achieved recognized sovereignty over the pre-1967 borders without requiring Transjordan's incorporation. Palestinian and international legal scholarship attests the founding problem as framed presupposes the illegitimacy of prior Arab claims rather than describing an external constraint.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__revisionist_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__revisionist_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__revisionist_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_territorial_claim__revisionist_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__revisionist_zionism_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__revisionist_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__revisionist_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.55→0.81) because the doctrine's core mechanism — compel first, negotiate never or only after capitulation — structurally forecloses any settlement that returns value to the Arab population; the transfer is asymmetric by design, not an incidental byproduct of coordination. Suppression is authored highest of all metrics (0.87 at interval end) because the doctrine's entire theory of change is coercive: it does not claim consent will emerge, it claims consent is unnecessary and force will substitute for it. Theater ratio stays low throughout (0.10→0.22) because the doctrine's violence was substantially operational rather than symbolic — actual land seizure, actual armed conflict, actual demographic displacement, not primarily performative compliance activity. Accessibility collapse is moderate (0.4) rather than extreme because, unlike a mountain, alternative political strategies (negotiated partition, binationalism) remained live and contested within the broader Zionist movement throughout the period — the doctrine did not eliminate alternatives so much as compete with and eventually help displace them. Resistance is very high (0.88) reflecting both Arab armed and political resistance and internal Zionist resistance (Haganah discipline against Irgun unilateralism, the Altalena affair).
 *
 * DIRECTIONALITY LOGIC:
 *   The Revisionist movement and its paramilitary arms are the clear structural beneficiaries and agenda-setters: they author the doctrine, control the enforcement apparatus, and derive organizational and later electoral capital from it — d sits near the full-beneficiary end for these seats. Palestinian and Transjordanian Arab populations are the doctrine's explicit targets by its own stated logic (their consent is declared unnecessary and their resistance the thing to be broken) — d sits near the full-target end, amplified by trapped exit options and regional scope. The labor Zionist mainstream is a partial victim despite being nominally 'on the same side': it bears diplomatic and security costs imposed by unilateral Revisionist action it did not choose and often actively opposed, which is why it is coded as payer despite organized power and constrained (not trapped) exit — it could and did resist internally (Haganah crackdowns on Irgun), but could not fully insulate itself from the consequences.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Arab society will never consent, therefore force must substitute for consent) is authored as contested rather than resolved-dead or straightforwardly-live: the state achieved internationally recognized sovereignty in 1948 without incorporating Transjordan, which the doctrine had held to be non-negotiable, suggesting the maximalist premise was falsified in its own terms even as diluted or transformed versions of it (Greater Israel ideology, post-1967 settlement politics) persisted institutionally. This is the mandatrophy signature: a doctrine whose stated founding condition was substantially superseded by events continued to generate political and territorial claims on inertial ideological authority rather than continued necessity — the classification prevents collapsing this into either 'pure coordination that solved a real problem' or 'pure inertial theater'; it was substantially extractive coordination (the tangled_rope reading) precisely because it retained enforcement teeth and beneficiary capture long after its founding premise's empirical basis had eroded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    iron_wall_strategic_theory_vs_ideological_cover,
    'Was the Iron Wall doctrine a genuine (if morally troubling) strategic theory of how peace could be achieved through demonstrated deterrence, or was ''compelling acceptance'' primarily a legitimating cover story for territorial maximalism that would have been pursued regardless of any theory of Arab psychology?',
    'Close reading of Jabotinsky''s private correspondence and internal Revisionist movement debates versus public doctrine statements; comparison of doctrine''s predicted sequencing (force then negotiation) against actual Revisionist behavior when negotiation opportunities arose.',
    'If genuine strategic theory, the coordination function (a shared theory of victory for a fragmented militant movement) is more substantial and the tangled_rope classification''s coordination half is better supported. If primarily cover, the constraint tips toward snare — coordination function nearly absent, extraction the entire content.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iron_wall_strategic_theory_vs_ideological_cover, conceptual, 'Whether Iron Wall doctrine was genuine strategic theory or ideological cover for maximalism.').

omega_variable(
    both_banks_claim_naturalness_vs_construction,
    'Was the claim to both banks of the Jordan an inherent extension of Jewish national self-determination as the Revisionist movement understood it, or a specifically constructed maximalist position adopted for strategic/political reasons within the Zionist movement''s internal competition for legitimacy?',
    'Comparative analysis of Revisionist platform evolution (1923 founding vs. later Herut platforms) against material changes in territorial opportunity (e.g., British partition proposals, war outcomes) to see whether the claim tracked opportunity or remained doctrinally fixed.',
    'If the claim tracked opportunity, it functioned more as opportunistic extraction than principled doctrine, reinforcing the tangled_rope/snare boundary question. If doctrinally fixed regardless of opportunity, it supports reading the claim as a sincerely held (if extreme) ideological commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(both_banks_claim_naturalness_vs_construction, empirical, 'Whether the both-banks maximalist claim was fixed doctrine or opportunistic positioning.').

omega_variable(
    post_1948_persistence_and_zombie_founding_problem,
    'Given that the state achieved recognized sovereignty in 1948 without the Transjordan claim, does the doctrine''s later ideological descendants (Greater Israel movements, certain settlement politics) represent the same founding problem in a new form, or an entirely different problem (post-1967 security/territorial politics) retroactively legitimated by borrowing Revisionist rhetoric?',
    'Genealogical tracing of explicit doctrinal citation and organizational continuity between pre-1948 Revisionist/Irgun figures and post-1967 territorial maximalist movements, versus independent origination of post-1967 claims from different circumstances.',
    'If genuine continuity, the founding_problem_status classification of ''contested'' understates a live mandatrophy pattern extending decades beyond this story''s interval. If independent origination, this story''s interval-bounded classification (1923-1948) is analytically self-contained and should not be read as pre-authorizing later territorial claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(post_1948_persistence_and_zombie_founding_problem, conceptual, 'Whether later Greater Israel politics are genealogically continuous with this doctrine or independently originated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__revisionist_zionism_reading, 1923, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1923, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1923, 0.1).
narrative_ontology:measurement(jewi_tr_t1929, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1929, 0.13).
narrative_ontology:measurement(jewi_tr_t1936, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1936, 0.16).
narrative_ontology:measurement(jewi_tr_t1939, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1939, 0.18).
narrative_ontology:measurement(jewi_tr_t1944, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1944, 0.2).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1948, 0.22).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1923, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1923, 0.55).
narrative_ontology:measurement(jewi_be_t1929, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1929, 0.62).
narrative_ontology:measurement(jewi_be_t1936, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1936, 0.7).
narrative_ontology:measurement(jewi_be_t1939, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1939, 0.74).
narrative_ontology:measurement(jewi_be_t1944, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1944, 0.78).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1948, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1923, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1923, 0.5).
narrative_ontology:measurement(jewi_su_t1929, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1929, 0.62).
narrative_ontology:measurement(jewi_su_t1936, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1936, 0.74).
narrative_ontology:measurement(jewi_su_t1939, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1939, 0.79).
narrative_ontology:measurement(jewi_su_t1944, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1944, 0.83).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1948, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__revisionist_zionism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jewish_territorial_claim__revisionist_zionism_reading, 0.05).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, cultural_zionism_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the jewish_territorial_claim kernel. Each reading (political, labor, cultural, revisionist) instantiates a structurally distinct constraint with its own ε, beneficiary/victim structure, and classification — they are linked here rather than merged, per the ε-invariance principle. The revisionist reading carries the highest authored extractiveness and suppression among the siblings because its own stated doctrine (force-before-consent, non-negotiable maximalism) is the most explicitly coercive; the political Zionist reading is expected to show lower suppression (negotiated majority, partition-acceptant) and the cultural Zionist reading lower still (no sovereignty claim at all).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
