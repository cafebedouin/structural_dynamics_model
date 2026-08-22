% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__post_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__post_zionist_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__post_zionist_reading
 *   human_readable: Post-Zionist Reading: Ethnic-National Framework as Obstruction to Civic Equality
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint story captures the post-Zionist reading of the Jewish
 *   sovereignty in Palestine kernel. The reading holds that the Zionist
 *   project successfully achieved statehood (1948) but the ethnic-national
 *   framework that enabled that achievement — Law of Return, Jewish National
 *   Fund land regime, nation-state law, military occupation — now functions
 *   as an obstruction to civic equality for Palestinian citizens and regional
 *   integration with the Arab world. The constraint is structurally a tangled
 *   rope: it retains a genuine coordination function (Jewish collective
 *   security, cultural autonomy) while extracting asymmetrically from
 *   Palestinian citizens and occupied populations through active enforcement
 *   (military rule, planning law, demographic engineering). The reading calls
 *   for de-Zionization of state institutions — transforming the state from an
 *   ethnic nation-state to a civic polity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, 0.68).
domain_priors:suppression_score(jewish_sovereignty_palestine__post_zionist_reading, 0.72).
domain_priors:theater_ratio(jewish_sovereignty_palestine__post_zionist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__post_zionist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__post_zionist_reading, "Post-Zionist Reading: Ethnic-National Framework as Obstruction to Civic Equality").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__post_zionist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__post_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__post_zionist_reading, 'c37b9b59-f857-4725-aca1-76e9033354f7').
narrative_ontology:cs_kernel_codification('c37b9b59-f857-4725-aca1-76e9033354f7', formalized).
narrative_ontology:cs_authority_grounding('c37b9b59-f857-4725-aca1-76e9033354f7', lineage).
narrative_ontology:cs_interpretation_layer_present('c37b9b59-f857-4725-aca1-76e9033354f7').
narrative_ontology:cs_reading_relation('c37b9b59-f857-4725-aca1-76e9033354f7', jewish_sovereignty_palestine__religious_zionist_reading, forecloses).
narrative_ontology:cs_reading_relation('c37b9b59-f857-4725-aca1-76e9033354f7', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c37b9b59-f857-4725-aca1-76e9033354f7', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_reading_relation('c37b9b59-f857-4725-aca1-76e9033354f7', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_axiom('c37b9b59-f857-4725-aca1-76e9033354f7', foundational, ethnic_national_framework_obstructs_civic_equality).
narrative_ontology:cs_axiom_status(ethnic_national_framework_obstructs_civic_equality, holdable).
narrative_ontology:cs_axiom_grounding('c37b9b59-f857-4725-aca1-76e9033354f7', ethnic_national_framework_obstructs_civic_equality, deontological).
narrative_ontology:cs_axiom('c37b9b59-f857-4725-aca1-76e9033354f7', secondary, de_zonization_required_for_regional_integration).
narrative_ontology:cs_axiom_status(de_zonization_required_for_regional_integration, holdable).
narrative_ontology:cs_axiom_grounding('c37b9b59-f857-4725-aca1-76e9033354f7', de_zonization_required_for_regional_integration, instrumental).
narrative_ontology:cs_reference_frame('c37b9b59-f857-4725-aca1-76e9033354f7', zionist_achievement_1948).
narrative_ontology:cs_drift_state('c37b9b59-f857-4725-aca1-76e9033354f7', post_oslo_nation_state_law_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c37b9b59-f857-4725-aca1-76e9033354f7', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens_law_of_return).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, jewish_national_institutions).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, israeli_palestinian_citizens).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, occupied_palestinian_population).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, palestinian_refugees_diaspora).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__post_zionist_reading, civic_equality_principle).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__post_zionist_reading, de_zonization_of_state_institutions).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__post_zionist_reading, postcolonial_critique_of_ethnic_nationalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive automatic citizenship, land access, and resource allocation through Law of Return and Jewish National Fund structures. Benefit from demographic engineering that maintains Jewish majority. Can emigrate but retain return rights; exit is easy and reversible.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens_law_of_return, beneficiary,
    powerful, generational, mobile, national).

% World Zionist Organization, Jewish Agency, Jewish National Fund administer land, immigration, and settlement policy. Control 93% of land via state lease system. Set the ethnic-national framework that defines state purpose. Can shift strategy but institutional survival depends on maintaining the framework.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, jewish_national_institutions, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Formal citizens but subject to 65+ discriminatory laws (Adalah database). Land access restricted, planning permissions denied, resource allocation unequal. Identity as Palestinian citizens of Israel makes exit unthinkable — leaving means abandoning homeland and community. Political representation exists but constrained by Jewish-state definition.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, israeli_palestinian_citizens, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__post_zionist_reading, israeli_palestinian_citizens, excluded).

% Live under military occupation without citizenship or political rights. Subject to permit regime, settlement expansion, land expropriation. No exit: Gaza blockade, West Bank fragmentation, revoked residency. The constraint extracts land, labor, and autonomy while denying political voice.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, occupied_palestinian_population, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__post_zionist_reading, occupied_palestinian_population, excluded).

% Denied return under Law of Return's ethnic asymmetry — Jewish citizenship available globally, Palestinian return denied. Hold keys and deeds to 1948 lands but no mechanism for restitution. The constraint's demographic logic requires their permanent exclusion.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, palestinian_refugees_diaspora, excluded,
    powerless, generational, trapped, global).

% Israeli scholars (Pappé, Sand, Ram, Shlaim) and Palestinian citizens (Jabareen, Rouhana) who articulate the reading. Analyze the ethnic-national framework as obstacle to equality. Have institutional positions but face marginalization, funding cuts, loyalty oaths. Their analysis is the reading itself.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, post_zionist_intellectuals, observer,
    analytical, generational, analytical, national).

% HRW, Amnesty, B'Tselem, Adalah document apartheid findings. Their reports structure international legal discourse. Not party to the constraint but their documentation shapes external pressure. Exit is analytical — they observe from outside.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, international_human_rights_ngos, observer,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish collective self-determination through state institutions: immigration, land allocation, defense, and symbolic nationhood. Solves the problem of Jewish statelessness and persecution by creating a sovereign framework with demographic engineering.
% TRANSFER_FUNCTION: Moves land (93% state-controlled via JNF), residency rights, citizenship access, water resources, and state budgets from Palestinian citizens and occupied population to Jewish citizens and national institutions. Transfers political sovereignty from universal franchise to ethno-national franchise.
% ABSENT_VOICES: Palestinian refugees in Lebanon, Syria, Jordan, and diaspora — 5-7 million people whose return would alter the demographic balance the constraint maintains. They are structurally excluded from any negotiation; their voices appear only as 'demographic threat' in Israeli discourse. Also absent: Mizrahi Jewish voices critical of Ashkenazi-dominated Zionist framework.
% DISAPPEARANCE_RATIONALE: If the ethnic-national framework vanished overnight: Law of Return would equalize with Palestinian return rights; JNF land would revert to state for equal allocation; military occupation would lose its ideological justification; 65+ discriminatory laws would fall; the state would reconstitute as a civic polity. The rearrangement would be profound — this is the constraint's function.
% FOUNDING_PROBLEM: The founding problem was Jewish statelessness and vulnerability to persecution in Europe, culminating in the Holocaust. The Zionist project proposed ethno-national statehood in Palestine as the solution: a Jewish demographic majority in a sovereign state as guarantee of security.
% FOUNDING_PROBLEM_CORROBORATION: Post-Zionist historians (Pappé, Shlaim, Morris's later work) and Palestinian historians (Khalidi, Masalha) attest the founding problem was real but the solution created new statelessness. Liberal Zionist scholars (Oz-Salzberger, Gans) argue the founding problem remains live (rising antisemitism) and the ethnic framework is still necessary. No consensus outside the beneficiary set — the status is genuinely contested.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__post_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__post_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__post_zionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(jewish_sovereignty_palestine__post_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__post_zionist_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__post_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__post_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the ongoing transfer of land, resources, and political rights from Palestinian populations to Jewish citizens and institutions. The 1967 occupation intensified extraction; Oslo (1993) briefly reduced it through partial Palestinian autonomy; Second Intifada (2000) and Nation-State Law (2018) increased it; current moment shows slight decline as the framework's contradictions become more visible. Theater ratio (0.42) captures the gap between 'Jewish and democratic' rhetoric and the ethnic-national practice — the democratic performance is real but increasingly hollow. Suppression (0.72) is high because the constraint requires active military enforcement (occupation), legal enforcement (discriminatory laws), and demographic enforcement (denial of return). Accessibility collapse (0.58) and resistance (0.64) reflect that alternatives (one-state, two-state, confederation) are politically visible but structurally blocked.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (Jewish national institutions) experiences the constraint as coordination — it built the state and maintains Jewish collective security. The payer seats (Palestinian citizens, occupied population) experience it as extraction — the same institutions that coordinate Jewish life extract from them. The observer seats see the full structure. This divergence is the point: the engine computes different types for different seats from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish citizens and national institutions are structural beneficiaries (d ~0.15-0.25): they collect citizenship rights, land access, and state resources through the ethnic framework. Their exit options are mobile/arbitrage — they can leave and return. Israeli Palestinian citizens are payers with identity_locked exit (d ~0.75): they pay through discriminatory laws but cannot exit without abandoning homeland. Occupied Palestinians and refugees are trapped payers (d ~0.9-1.0): they bear extraction with no exit. Post-Zionist intellectuals and NGOs are analytical observers (d ~0.5). The engine will compute per-seat types from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish statelessness/persecution) is contested as live or resolved. If live, the ethnic framework retains coordination justification (tangled rope). If resolved, the framework persists by inertia and extraction (piton/snare). The post-Zionist reading argues the framework now obstructs the deeper coordination problem: regional integration and civic equality. The constraint has not undergone mandatrophy resolution — its mandate has expanded (occupation) rather than sunset.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_resolution,
    'Is the founding problem (Jewish statelessness/persecution) resolved such that the ethnic-national framework''s coordination function is exhausted, or does it remain live requiring the framework''s maintenance?',
    'Historical analysis of antisemitism trends, Jewish security in diaspora vs. Israel, and whether civic equality in a binational state would satisfy the original security imperative.',
    'If resolved, the constraint''s coordination justification collapses — it becomes snare or piton. If live, tangled rope classification holds. The engine''s per-seat computation will diverge: beneficiaries experience it as live coordination; victims experience it as resolved extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_resolution, conceptual, 'Whether the founding problem''s status is live, dead, or contested determines the constraint''s structural classification.').

omega_variable(
    coordination_extraction_separability,
    'Can the genuine coordination function (Jewish cultural autonomy, collective security) be separated from the ethnic-national framework that extracts from Palestinians, or are they structurally inseparable?',
    'Constitutional design analysis: could a civic state with robust minority protections for Jewish culture replace the ethnic nation-state? Historical precedents (post-apartheid South Africa, Northern Ireland, Belgium).',
    'If separable, the extraction is not necessary for coordination — the constraint is a tangled rope where extraction is a policy choice. If inseparable, the ethnic framework IS the coordination mechanism — the constraint approaches snare (coordination story is cover).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the ethnic-national form is essential to Jewish collective self-determination or a contingent historical choice.').

omega_variable(
    palestinian_citizen_agency,
    'Do Palestinian citizens of Israel have meaningful agency to transform the constraint from within (parliamentary politics, Supreme Court litigation, civil society), or is their position structurally excluded from effective power?',
    'Empirical analysis of Palestinian parliamentary influence (Joint List, Ra''am), Supreme Court rulings on discrimination, and civil society impact on policy.',
    'If meaningful agency exists, the constraint''s suppression is lower and resistance higher — the engine computes different d for this seat. If structurally excluded, identity_locked is confirmed and effective extraction is near-maximal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(palestinian_citizen_agency, empirical, 'Whether the ''payer'' seat of Israeli Palestinian citizens has structural leverage or is purely extractive.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the kernel ''Jewish sovereignty in Palestine'' admit a single authoritative framing, or do the five declared readings represent genuinely distinct kernels that only share a label?',
    'Genealogical analysis: do all readings trace to the same founding commitment (Herzl, Basel Program, Balfour) or do they posit different founding moments? The ε-invariance principle suggests different ε values may indicate different constraints.',
    'If the kernel is underdetermined, the sibling readings are not readings of ONE constraint but distinct constraints linked by network.affects_constraints. This story''s ε (0.68) and the liberal_nationalist_reading''s ε (likely ~0.3) would be different constraints, not observer variance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the committer frame''s kernel_id refers to one stabilized commitment or a family of related constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__post_zionist_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1948, 0.25).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1967, 0.3).
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1993, 0.35).
narrative_ontology:measurement(jewi_tr_t2000, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(jewi_tr_t2018, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2018, 0.45).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1948, 0.55).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1967, 0.62).
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1993, 0.58).
narrative_ontology:measurement(jewi_be_t2000, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(jewi_be_t2018, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2018, 0.72).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1948, 0.5).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1967, 0.65).
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1993, 0.6).
narrative_ontology:measurement(jewi_su_t2000, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(jewi_su_t2018, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2018, 0.75).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__post_zionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__post_zionist_reading, 0.12).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, israeli_palestinian_citizenship_laws).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, west_bank_settlement_regime).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, law_of_return_asymmetry).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_national_fund_land_regime).

% DUAL FORMULATION NOTE:
% This post-Zionist reading and the liberal_nationalist_reading share the kernel 'Jewish sovereignty in Palestine' but diverge on whether the ethnic-national framework remains functionally necessary. The liberal_nationalist_reading claims the framework IS the coordination mechanism (lower ε); this reading claims the framework now obstructs deeper coordination (civic equality, regional integration) and extracts asymmetrically (higher ε). The settler_colonial_reading shares this reading's structural analysis of extraction but differs on the founding moment's legitimacy — it sees the founding AS displacement; this reading sees founding as achievement that has become obstruction. The cultural_zionist_reading pre-figures this reading's conclusion (statehood not necessary) but from a pre-state vantage. The religious_zionist_reading is foreclosed by this reading's secular civic framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_sovereignty_palestine__post_zionist_reading, moderate, 0.75).
constraint_indexing:directionality_override(jewish_sovereignty_palestine__post_zionist_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
