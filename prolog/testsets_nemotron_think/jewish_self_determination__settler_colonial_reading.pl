% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__settler_colonial_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: jewish_self_determination__settler_colonial_reading
 *   human_readable: Zionism as European Settler-Colonial Project (Settler-Colonial Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the settler-colonial reading of the
 *   'jewish_self_determination' kernel. It reads Zionism not as a national
 *   liberation movement but as a European settler-colonial project that
 *   arrived in Palestine in the late 19th century, systematically displaced
 *   the indigenous Palestinian population (1948 Nakba, 1967 occupation,
 *   ongoing settlement expansion), and maintains a structure of legal
 *   exclusion (Law of Return for Jews only, military law for Palestinians,
 *   citizenship stratification) that extracts land, water, and sovereignty
 *   from Palestinians to Jewish settlers and the Israeli state. The
 *   constraint operates as a snare: its coordination function (Jewish
 *   state-building) is inseparable from its extraction function (Palestinian
 *   dispossession), and it persists only through active military enforcement
 *   and legal architecture.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, 0.85).
domain_priors:suppression_score(jewish_self_determination__settler_colonial_reading, 0.9).
domain_priors:theater_ratio(jewish_self_determination__settler_colonial_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_self_determination__settler_colonial_reading, "Zionism as European Settler-Colonial Project (Settler-Colonial Reading)").
narrative_ontology:topic_domain(jewish_self_determination__settler_colonial_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__settler_colonial_reading, 'bdf7f52e-740a-4969-8b93-25cbb8cdde03').
narrative_ontology:cs_kernel_codification('bdf7f52e-740a-4969-8b93-25cbb8cdde03', formalized).
narrative_ontology:cs_authority_grounding('bdf7f52e-740a-4969-8b93-25cbb8cdde03', extraction).
narrative_ontology:cs_interpretation_layer_present('bdf7f52e-740a-4969-8b93-25cbb8cdde03').
narrative_ontology:cs_reading_relation('bdf7f52e-740a-4969-8b93-25cbb8cdde03', jewish_self_determination__liberal_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('bdf7f52e-740a-4969-8b93-25cbb8cdde03', jewish_self_determination__indigenous_return_reading, forecloses).
narrative_ontology:cs_reading_relation('bdf7f52e-740a-4969-8b93-25cbb8cdde03', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('bdf7f52e-740a-4969-8b93-25cbb8cdde03', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_axiom('bdf7f52e-740a-4969-8b93-25cbb8cdde03', foundational, zionism_is_european_settler_colonialism).
narrative_ontology:cs_axiom_status(zionism_is_european_settler_colonialism, holdable).
narrative_ontology:cs_axiom_grounding('bdf7f52e-740a-4969-8b93-25cbb8cdde03', zionism_is_european_settler_colonialism, empirically_contingent).
narrative_ontology:cs_axiom('bdf7f52e-740a-4969-8b93-25cbb8cdde03', foundational, palestinian_displacement_is_structural_not_incidental).
narrative_ontology:cs_axiom_status(palestinian_displacement_is_structural_not_incidental, holdable).
narrative_ontology:cs_axiom_grounding('bdf7f52e-740a-4969-8b93-25cbb8cdde03', palestinian_displacement_is_structural_not_incidental, empirically_contingent).
narrative_ontology:cs_axiom('bdf7f52e-740a-4969-8b93-25cbb8cdde03', secondary, law_of_return_asymmetry_constitutes_apartheid).
narrative_ontology:cs_axiom_status(law_of_return_asymmetry_constitutes_apartheid, holdable).
narrative_ontology:cs_axiom_grounding('bdf7f52e-740a-4969-8b93-25cbb8cdde03', law_of_return_asymmetry_constitutes_apartheid, empirically_contingent).
narrative_ontology:cs_reference_frame('bdf7f52e-740a-4969-8b93-25cbb8cdde03', european_colonial_settlement_project).
narrative_ontology:cs_drift_state('bdf7f52e-740a-4969-8b93-25cbb8cdde03', post_oslo_accords_settler_colonial_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bdf7f52e-740a-4969-8b93-25cbb8cdde03', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__settler_colonial_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, european_jewish_settlers).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, israeli_state).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_arabs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, liberal_zionist_ngos).
narrative_ontology:constraint_vindicates(jewish_self_determination__settler_colonial_reading, settler_colonial_elimination_logic).
narrative_ontology:constraint_vindicates(jewish_self_determination__settler_colonial_reading, indigenous_displacement_as_structural).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% European Jewish immigrants and their descendants who arrived in Palestine/Israel from the late 19th century onward. They receive land, resources, citizenship rights, and state protection through the Zionist project. Their exit options include emigration to other countries (many hold dual citizenship), but the structural benefits of settlement create strong incentives to remain.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, european_jewish_settlers, beneficiary,
    organized, biographical, mobile, regional).

% The sovereign state apparatus (government, military, courts, bureaucracy) that administers the Zionist project. It sets immigration policy (Law of Return), controls land allocation (Jewish National Fund, state land), enforces military occupation, and collects taxes/resources from the controlled territory. It benefits from the extraction of Palestinian land, water, and labor while framing the arrangement as national self-determination.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, israeli_state, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__settler_colonial_reading, israeli_state, beneficiary).

% The indigenous Palestinian population (including citizens of Israel, residents of East Jerusalem, West Bank, and Gaza). They bear the costs of displacement (1948, 1967, ongoing), land expropriation, differential legal status (military law vs. civil law), resource extraction (water, agricultural land), movement restrictions, and political exclusion. Exit is structurally blocked: refugees cannot return; those under occupation cannot leave; citizens face systemic discrimination.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_arabs, payer,
    powerless, generational, trapped, local).

% Descendants of Palestinians displaced in 1948 and 1967, now living in refugee camps across the Middle East and globally. They are denied the right of return guaranteed by UN Resolution 194 while Jewish immigrants receive automatic citizenship. Their voices are excluded from the political framework that determines the territory's future.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_refugees_diaspora, excluded,
    powerless, generational, trapped, global).

% UN bodies, ICC, ICJ, human rights organizations that document and adjudicate violations. They recognize the occupation, settlements, and apartheid findings but lack enforcement power against the Israeli state due to geopolitical protection (US veto, etc.). Their analytical seat sees the full structure but cannot alter it.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, international_law_institutions, observer,
    institutional, generational, analytical, global).

% Organizations (e.g., J Street, Peace Now) that support Israel's existence as a Jewish state but oppose occupation/settlements. They benefit from the Jewish state's existence (cultural, political, psychological) while criticizing its extractive edges. Their exit is constrained by identity commitment to the Zionist project.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, liberal_zionist_ngos, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__settler_colonial_reading, liberal_zionist_ngos, observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement coordinates Jewish immigration, settlement, and state-building on a territory already inhabited by Palestinians, solving the 'Jewish question' in Europe by transplanting it onto Palestine — a coordination achieved through the removal and subordination of the existing population.
% TRANSFER_FUNCTION: Moves land, water, political sovereignty, and demographic majority from Palestinian Arabs to European Jewish settlers and the Israeli state, through laws (Absentees' Property Law, Law of Return), military orders, and settlement infrastructure.
% ABSENT_VOICES: Palestinian refugees in diaspora (denied return and political voice), Palestinians in Gaza (under blockade, no political representation in the system controlling their lives), and Bedouin communities in the Naqab/Negev (facing ongoing dispossession). They are structurally excluded by the Law of Return asymmetry, military occupation, and citizenship laws.
% DISAPPEARANCE_RATIONALE: If the Zionist settler-colonial structure vanished overnight, the Law of Return would collapse, military occupation would end, land would revert to Palestinian owners or be subject to new negotiated arrangements, and the demographic engineering maintaining a Jewish majority would cease — the entire political geography of Palestine/Israel would be fundamentally reorganized.
% FOUNDING_PROBLEM: The founding problem, from this reading's perspective, was not Jewish persecution per se but the European colonial answer to it: transplanting a European nationalist project onto an inhabited land, solving Europe's 'Jewish question' at Palestinian expense. The arrangement was built to establish a Jewish demographic majority and sovereign control in Palestine through the displacement of its indigenous inhabitants.
% FOUNDING_PROBLEM_CORROBORATION: The 'founding problem' of creating a Jewish majority state in a majority-Arab land is attested as dead by Palestinian historians (e.g., Walid Khalidi, Rashid Khalidi), Israeli 'new historians' (Benny Morris, Ilan Pappé — though Morris disputes the 'colonial' framing, he documents the displacement), and international legal scholars (Francesca Albanese, John Dugard) who document the ongoing nature of the displacement. The Israeli state and Zionist institutions attest the problem remains 'live' (security, demographic balance), but this is the beneficiary's self-justification.
narrative_ontology:disappearance_verdict(jewish_self_determination__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__settler_colonial_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__settler_colonial_reading, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.85) is high because the structure continuously transfers land, water, and political rights from Palestinians to Jewish settlers/state — settlement expansion, resource allocation, and demographic engineering are ongoing. Suppression (0.9) is near-maximum because the constraint's persistence depends on military occupation, blockade, permit regimes, and legal bars to return — alternatives (one state, right of return, binationalism) are actively suppressed by force. Theater ratio (0.3) reflects that the 'security' and 'self-determination' framings perform a coordination cover for what is structurally eliminationist. Accessibility collapse (0.8) is high because the legal and physical architecture (separation wall, Areas A/B/C, citizenship laws) makes alternatives nearly unimaginable within the system. Resistance (0.7) is substantial: Palestinian sumud, intifadas, BDS, legal challenges, and international pressure represent active contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the Israeli state/settler seat, the constraint appears as a rope or mountain — a legitimate national project coordinating Jewish survival. From the Palestinian seat, it appears as a snare — an eliminationist structure extracting their land and rights. From the refugee seat, it appears as a total foreclosure — the constraint that defines their nonexistence in their homeland. The engine computes this divergence from the structural data: power asymmetry (institutional vs. powerless), exit asymmetry (mobile/analytical vs. trapped), and scope asymmetry (national/global vs. local).
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli state (agenda_setter) and European Jewish settlers (beneficiary) sit at the beneficiary end of directionality (d ~ 0.1-0.2): they collect the extraction (land, resources, sovereignty) and control the rules. Palestinian Arabs (payer) sit at the full-target end (d ~ 0.95): they bear the costs, have no exit, and are subject to the constraint's full coercive force. Palestinian refugees (excluded) are even more extremely targeted (d ~ 1.0) — they are the structural outside, denied even the minimal rights of those inside. International law institutions (observer) sit at analytical (d = 0.5, analytical seat). Liberal Zionist NGOs (beneficiary/observer) sit in a conflicted position: they benefit from the Jewish state's existence (d ~ 0.3) but critique its extractive edges.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (creating a Jewish majority state in Palestine) is dead in the sense that the demographic and military facts are established — but the arrangement persists and intensifies (settlement expansion, judicial overhaul, annexation moves) because the mandate has metastasized: the structure now exists to maintain the extraction itself, not to solve the original problem. This is classic mandatrophy: the coordination justification (Jewish self-determination) has become a cover for the extraction machinery (settlements, resource control, demographic engineering).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_foreclosure_boundary,
    'Does the settler_colonial_reading''s core premise (Zionism as European colonial project) logically foreclose the indigenous_return_reading (Jews as indigenous) within a single framework, or can a framework hold both as contested narratives?',
    'Analyze whether any political/legal framework currently operates with both premises simultaneously (e.g., Israeli courts citing both historical connection and international law). If no framework can stably hold both, foreclosure is structural.',
    'If foreclosure holds, the kernel cannot be a single commitment system with multiple live readings — it fractures into incompatible kernels. If coexistence holds, the kernel remains a site of genuine contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_boundary, conceptual, 'Whether settler-colonial and indigenous-return framings are logically incompatible within one framework').

omega_variable(
    extraction_coordination_inseparability,
    'Is the coordination function (Jewish state-building) structurally inseparable from the extraction function (Palestinian dispossession), or could a Jewish self-determination project exist without eliminationist logic?',
    'Historical counterfactual analysis: examine pre-1948 binationalist proposals (Brit Shalom, Ihud), the 1947 UN partition (which the Zionist leadership accepted tactically but expanded beyond), and post-1967 opportunities for two-state solution. If every historical juncture where coordination without extraction was possible was rejected by the Zionist movement/state, inseparability is structural.',
    'If inseparable, the constraint is a pure snare — no rope component exists to salvage. If separable, a tangled_rope classification might apply to a hypothetical non-eliminationist version, and the current snare is a contingent deformation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_coordination_inseparability, empirical, 'Whether Jewish self-determination structurally requires Palestinian dispossession').

omega_variable(
    suppression_mechanism_internalized,
    'Is the high suppression measured (0.9) primarily structural (military occupation, legal bars) or partially internalized (Palestinian political fragmentation, collaboration, normalization of occupation)?',
    'Post-exit suppression trajectory: if Palestinian Authority security coordination with Israel persists even under reduced occupation, internalized suppression is operative. Compare suppression levels in Gaza (direct military) vs. West Bank (PA-mediated) vs. ''48 citizens (legal).',
    'If internalized suppression is significant, the constraint''s effective suppression exceeds the structural measure — the target carries the constraint''s logic internally. This would increase χ for Palestinian seats beyond the structural calculation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Structural vs. internalized suppression in the Palestinian condition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__settler_colonial_reading, 1882, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jsscr_tr_t1882, jewish_self_determination__settler_colonial_reading, theater_ratio, 1882, 0.1).
narrative_ontology:measurement(jsscr_tr_t1917, jewish_self_determination__settler_colonial_reading, theater_ratio, 1917, 0.15).
narrative_ontology:measurement(jsscr_tr_t1948, jewish_self_determination__settler_colonial_reading, theater_ratio, 1948, 0.25).
narrative_ontology:measurement(jsscr_tr_t1967, jewish_self_determination__settler_colonial_reading, theater_ratio, 1967, 0.28).
narrative_ontology:measurement(jsscr_tr_t1993, jewish_self_determination__settler_colonial_reading, theater_ratio, 1993, 0.3).
narrative_ontology:measurement(jsscr_tr_t2024, jewish_self_determination__settler_colonial_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(jsscr_be_t1882, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1882, 0.3).
narrative_ontology:measurement(jsscr_be_t1917, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1917, 0.45).
narrative_ontology:measurement(jsscr_be_t1948, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1948, 0.75).
narrative_ontology:measurement(jsscr_be_t1967, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1967, 0.8).
narrative_ontology:measurement(jsscr_be_t1993, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1993, 0.82).
narrative_ontology:measurement(jsscr_be_t2024, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jsscr_su_t1882, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1882, 0.4).
narrative_ontology:measurement(jsscr_su_t1917, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1917, 0.55).
narrative_ontology:measurement(jsscr_su_t1948, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1948, 0.85).
narrative_ontology:measurement(jsscr_su_t1967, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1967, 0.88).
narrative_ontology:measurement(jsscr_su_t1993, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1993, 0.89).
narrative_ontology:measurement(jsscr_su_t2024, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__settler_colonial_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__settler_colonial_reading, 0.15).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__diasporist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, palestinian_right_of_return).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, israeli_settlement_expansion).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, law_of_return_asymmetry).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, military_occupation_west_bank).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, gaza_blockade).

% DUAL FORMULATION NOTE:
% This reading decomposes the 'jewish_self_determination' kernel by extracting the empirically_contingent historical claim (European colonial origin, Palestinian displacement) from the normative self-determination claim. The liberal_nationalist_reading takes the normative claim as primary and the history as secondary/justified; this reading takes the historical claim as primary and the normative claim as ideological cover. The ε values diverge because the referent (the standing arrangement) is assessed differently: this reading sees ongoing extraction (ε=0.85); the liberal_nationalist_reading sees coordination with incidental friction (ε≈0.2).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_self_determination__settler_colonial_reading, organized, 0.15).
constraint_indexing:directionality_override(jewish_self_determination__settler_colonial_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
