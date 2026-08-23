% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__political_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__political_zionism_reading, []).

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
 *   constraint_id: jewish_territorial_claim__political_zionism_reading
 *   human_readable: Political Zionism: Jewish Statehood as Solution to Antisemitism Requiring Territorial Sovereignty with Jewish Majority
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   Political Zionism (Herzl, Weizmann, Ben-Gurion mainstream) frames Jewish
 *   statehood as the necessary solution to the 'Jewish Question' —
 *   antisemitism as an ineradicable feature of diaspora existence requiring
 *   sovereign territorial solution with a Jewish demographic majority. This
 *   reading prioritizes state-building over cultural content (contra cultural
 *   Zionism), treats the Arab population as a demographic obstacle to be
 *   managed rather than a partner (contra labor Zionism's 'conquest of labor'
 *   idealism), and considers population transfer a legitimate mechanism
 *   (explicit in Herzl's diary, Ben-Gurion's 1937-48 statements, and
 *   implemented in 1948). The constraint operates through Law of Return,
 *   Absentee Property Law, military governance, and settlement enterprise —
 *   active enforcement mechanisms that maintain Jewish majority control over
 *   territory and resources. The claim/metric gap is structural: the
 *   constraint is CLAIMED as rope/coordination (Jewish self-determination,
 *   refuge) while authored metrics describe substantial extraction
 *   (displacement, land theft, military rule) requiring active suppression —
 *   the engine measures this divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__political_zionism_reading, 0.82).
domain_priors:suppression_score(jewish_territorial_claim__political_zionism_reading, 0.85).
domain_priors:theater_ratio(jewish_territorial_claim__political_zionism_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__political_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__political_zionism_reading, "Political Zionism: Jewish Statehood as Solution to Antisemitism Requiring Territorial Sovereignty with Jewish Majority").
narrative_ontology:topic_domain(jewish_territorial_claim__political_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__political_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__political_zionism_reading, '23518837-ed97-4356-9992-535cc11d65d3').
narrative_ontology:cs_kernel_codification('23518837-ed97-4356-9992-535cc11d65d3', formalized).
narrative_ontology:cs_authority_grounding('23518837-ed97-4356-9992-535cc11d65d3', extraction).
narrative_ontology:cs_interpretation_layer_present('23518837-ed97-4356-9992-535cc11d65d3').
narrative_ontology:cs_reading_relation('23518837-ed97-4356-9992-535cc11d65d3', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('23518837-ed97-4356-9992-535cc11d65d3', jewish_territorial_claim__labor_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('23518837-ed97-4356-9992-535cc11d65d3', jewish_territorial_claim__revisionist_zionism_reading, influences).
narrative_ontology:cs_axiom('23518837-ed97-4356-9992-535cc11d65d3', foundational, sovereign_jewish_state_solves_antisemitism).
narrative_ontology:cs_axiom_status(sovereign_jewish_state_solves_antisemitism, holdable).
narrative_ontology:cs_axiom_grounding('23518837-ed97-4356-9992-535cc11d65d3', sovereign_jewish_state_solves_antisemitism, empirically_contingent).
narrative_ontology:cs_axiom('23518837-ed97-4356-9992-535cc11d65d3', foundational, jewish_majority_required_for_sovereign_security).
narrative_ontology:cs_axiom_status(jewish_majority_required_for_sovereign_security, holdable).
narrative_ontology:cs_axiom_grounding('23518837-ed97-4356-9992-535cc11d65d3', jewish_majority_required_for_sovereign_security, empirically_contingent).
narrative_ontology:cs_axiom('23518837-ed97-4356-9992-535cc11d65d3', secondary, population_transfer_legitimate_for_demographic_majority).
narrative_ontology:cs_axiom_status(population_transfer_legitimate_for_demographic_majority, overridden).
narrative_ontology:cs_axiom_grounding('23518837-ed97-4356-9992-535cc11d65d3', population_transfer_legitimate_for_demographic_majority, instrumental).
narrative_ontology:cs_reference_frame('23518837-ed97-4356-9992-535cc11d65d3', herzlian_diplomatic_zionism).
narrative_ontology:cs_drift_state('23518837-ed97-4356-9992-535cc11d65d3', post_1967_occupation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('23518837-ed97-4356-9992-535cc11d65d3', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, jewish_settlers).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, israeli_state_institutions).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, diaspora_jews_seeking_refuge).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, palestinian_arabs_pre1948).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, palestinian_refugees).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, palestinians_under_occupation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, jewish_settlers).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__political_zionism_reading, jewish_self_determination).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__political_zionism_reading, sovereign_state_solves_antisemitism).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__political_zionism_reading, jewish_majority_required_for_security).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the constraint through immigration law (Law of Return), land administration (Israel Land Authority), military governance (COGAT), and demographic engineering. Sets the rules of Jewish majority maintenance. Collects sovereignty, territory, resources, and international legitimacy as the primary beneficiary of the arrangement.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive subsidized land, housing, infrastructure, and military protection in settlements. Pay through military service, tax burden, and social militarization. Exit is mobile (can leave Israel) but identity-locked for ideological settlers. Benefit from the constraint's demographic engineering while bearing costs of perpetual conflict.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, jewish_settlers, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__political_zionism_reading, jewish_settlers, payer).

% Hold a latent option of immigration (aliyah) under Law of Return as insurance against antisemitism. Do not bear daily costs of the constraint unless they immigrate. Their beneficiary status is potential rather than actualized; the constraint's existence provides psychological and legal security without requiring participation in its enforcement.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, diaspora_jews_seeking_refuge, beneficiary,
    moderate, biographical, mobile, global).

% Indigenous majority population prior to 1948. Subject to displacement (Nakba), land expropriation, and denial of return. Had no representation in Zionist congresses, British Mandate decisions, or UN partition. Exit was physically blocked (surrounded by war, no state of their own). Bear the foundational extraction of the constraint.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, palestinian_arabs_pre1948, payer,
    powerless, generational, trapped, regional).

% Displaced in 1948 and 1967, denied right of return, stateless in host countries (Lebanon, Syria, Jordan, Gaza). The constraint's demographic logic requires their permanent exclusion. Bear intergenerational extraction: loss of property, nationality, political rights, and developmental possibility. No exit from refugee status without constraint's dissolution.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Formal citizens with voting rights but subject to 65+ discriminatory laws (Adalah database), land restrictions, and structural inequality. Benefit from Israeli welfare/education but pay the 'demographic tax' of second-class citizenship. Exit constrained: leaving means losing residency, family unification blocked, return uncertain. Identity-locked to Palestinian collective.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, palestinian_citizens_of_israel, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__political_zionism_reading, palestinian_citizens_of_israel, beneficiary).

% West Bank and Gaza populations under military rule (COGAT) since 1967. Subject to permit regime, settlement expansion, land confiscation, and no political representation in the sovereign that governs them. Exit physically blocked (checkpoints, wall, blockade). Bear the ongoing extraction of land, water, labor, and autonomy.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, palestinians_under_occupation, payer,
    powerless, biographical, trapped, regional).

% Produces resolutions (242, 338, 194, 2334), funds UNRWA, provides diplomatic cover. Documents violations but lacks enforcement will. Experiences the constraint as a persistent failure of international law. Exit is analytical: can change policy but structurally incentivized toward status quo management.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, international_community_un, observer,
    institutional, generational, analytical, global).

% Host refugees, fought wars, signed peace treaties (Egypt, Jordan), normalized (Abraham Accords). Would object to constraint's demographic logic but are bought off or coerced into acceptance. Excluded from core sovereignty decisions over Palestine. Bear regional destabilization costs.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, arab_states, excluded,
    organized, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves Jewish statelessness and provides a guaranteed refuge from antisemitism through a sovereign state with a Jewish demographic majority, centralized immigration control, and exclusive sovereignty over territory.
% TRANSFER_FUNCTION: Moves land (from 7% Jewish ownership in 1947 to 93% state land), water resources, political sovereignty, and demographic control from the Palestinian Arab population to Jewish settlers and Israeli state institutions. Transfers the cost of security (military service, taxation) onto Jewish citizens while transferring the cost of displacement (statelessness, land loss, military rule) onto Palestinians.
% ABSENT_VOICES: The Palestinian Arab population at the founding moment (1897-1948) had zero representation in Zionist congresses, British imperial decisions, or the UN partition vote. The 750,000 refugees of 1948 were excluded from the new state's constitution. Current Palestinians under occupation have no vote in the sovereign that governs their movement, resources, and political future. Their absence is structural: the constraint's logic requires their exclusion to maintain the Jewish majority.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, the Law of Return would collapse, Palestinian right of return would be implemented, the Jewish majority would dissolve within a generation, and the entire legal architecture of land allocation, military governance, and demographic engineering would require replacement. A single democratic state with equal rights would emerge, fundamentally reorganizing sovereignty, citizenship, and resource distribution across historic Palestine.
% FOUNDING_PROBLEM: The 'Jewish Question' in Europe: whether Jews could achieve security and equality as minorities in emerging nation-states, given persistent antisemitism culminating in pogroms, Dreyfus Affair, and ultimately the Holocaust. Herzl's diagnosis: antisemitism is inevitable in diaspora; only sovereign statehood with Jewish majority provides permanent security.
% FOUNDING_PROBLEM_CORROBORATION: Herzl, Nordau, and early Zionist congresses attested the founding problem from within the beneficiary set. Palestinian leadership (Husseini, Nashashibi) and Arab states contested it from 1919 onward, arguing antisemitism is a European problem Palestinians should not pay for. Post-WWII UNSCOP majority report corroborated the refugee dimension but not the demographic engineering; minority report proposed federal state. Historians (Morris, Pappé, Shlaim) document that transfer thinking was present at founding. No consensus outside the Zionist movement validates the founding problem as requiring this specific solution.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__political_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__political_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__political_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_territorial_claim__political_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__political_zionism_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__political_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__political_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__political_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the constraint's core mechanism — establishing and maintaining a Jewish majority in a land with a Palestinian Arab majority — inherently requires transferring land, rights, and sovereignty from one people to another. Suppression (0.85) is higher still because the constraint's persistence depends on active military enforcement (occupation, blockade, permit regime), legal exclusion (denial of return, discriminatory laws), and narrative control (censorship, education). Theater ratio (0.52) reflects the growing gap between Israel's self-presentation as 'only democracy in Middle East' and the reality of military rule over 5 million Palestinians without rights. The measurement series tracks three phases: pre-state (0-20, rising extraction as immigration grows), state-founding/Nakba (40, extraction spikes), occupation (60, suppression peaks), Oslo-era managed suppression (80, slight dip), current annexation-phase (100, all metrics at maximum).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (Israeli state) experiences the constraint as successful coordination: it built a sovereign state, absorbed refugees, created a Hebrew-speaking society, and maintains security. The payer seats (Palestinians in all categories) experience the same structure as snare-like extraction: denial of return, land confiscation, military rule, demographic engineering. The engine computes this divergence from the structural data — the beneficiaries' coordination function is real for them, the victims' extraction is real for them. The tangled_rope classification captures this structural asymmetry: genuine coordination for one people, asymmetric extraction from another, held together by active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli state institutions are the structural agenda-setter and primary beneficiary (d ~ 0.1) — they write the rules, collect the territory, and control enforcement. Jewish settlers are beneficiaries with secondary payer costs (d ~ 0.25) — they gain subsidized land but bear militarization costs. Diaspora Jews are pure potential beneficiaries (d ~ 0.05) — optional insurance policy. Palestinian Arabs pre-1948 were full targets (d ~ 0.95) — total extraction, zero exit. Refugees remain trapped targets (d ~ 0.9). Palestinian citizens of Israel are constrained payers (d ~ 0.7) — formal inclusion masking structural exclusion. Palestinians under occupation are trapped targets (d ~ 0.95). Arab states are excluded (not coordinated, bear costs). International community is analytical observer (d ~ 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (antisemitism/Jewish statelessness) is contested as live: Zionists argue antisemitism persists (rising in Europe/US, Oct 7 as proof), critics argue Jewish integration in Western states and universal human rights frameworks have substantially solved the original problem. The constraint persists despite contested founding problem because: (1) Israeli institutions extract massive benefits (territory, resources, US aid, international impunity) from maintaining it; (2) Palestinian resistance is suppressed but not eliminated; (3) the constraint has developed its own institutional inertia (settler constituency, security apparatus, legal framework). This is not a scaffold (no sunset) and not a piton (active enforcement, concentrated beneficiaries). The mandatrophy is unresolved — the constraint's original justification is contested but its extraction machinery is fully operational.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Is the Jewish majority requirement structurally inseparable from Palestinian displacement, or could a Jewish-majority state exist with full equality for Palestinian citizens and right of return for refugees?',
    'Counterfactual analysis: if Israel annexed West Bank and granted citizenship to all Palestinians, Jewish majority would end. If Israel maintained 1967 borders with full equality and symbolic return, Jewish majority might persist but constraint''s demographic logic would be violated. Empirical test: no historical case of settler colony maintaining demographic dominance without suppression.',
    'If inseparable, the constraint is fundamentally snare-like — the coordination function for Jews requires extraction from Palestinians as a structural necessity, not policy choice. If separable, the current extraction is contingent policy, not kernel requirement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether Jewish demographic majority and Palestinian rights are structurally compatible within the kernel.').

omega_variable(
    transfer_necessity,
    'Was the 1948 Palestinian expulsion (Nakba) militarily necessary for state survival, or a political choice to achieve demographic engineering?',
    'Archival research (Benny Morris, Ilan Pappé, Yoav Gelber) on 1948 war decisions; comparison of military orders vs. political directives; analysis of whether Palestinian communities that stayed (e.g., Nazareth) posed military threat.',
    'If militarily necessary, the extraction has a coordination-defense justification (tangled_rope). If political choice, the extraction is the constraint''s core purpose (snare). The engine''s Tangled Rope gate requires both coordination AND extraction — this omega determines which dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_necessity, empirical, 'Whether the foundational extraction was forced by circumstance or chosen by design.').

omega_variable(
    antisemitism_solution_efficacy,
    'Does a Jewish state with Jewish majority actually solve antisemitism, or does it relocate/transform it into anti-Zionism and regional conflict that endangers Jews?',
    'Longitudinal data on antisemitic incidents globally vs. Israeli security situation; analysis of whether Jewish safety correlates with Israeli policies or with liberal democratic integration; counterfactual of Jewish safety in a binational state.',
    'If the state fails to solve antisemitism (or worsens Jewish security), the constraint''s claimed coordination function is empirically falsified, shifting classification toward snare. If it succeeds, the coordination function is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(antisemitism_solution_efficacy, empirical, 'Whether the constraint achieves its stated coordination purpose.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the jewish_territorial_claim kernel refer to the 1947 UN partition lines, the 1967 lines, the biblical ''Eretz Yisrael'', or a metaphysical claim — and does this reading''s authority depend on keeping the kernel ambiguously framed?',
    'Analyze Israeli Basic Laws, Supreme Court rulings, settlement policy, and diplomatic positions for consistent territorial referent. Track how ambiguity enables expansion (settlements) while claiming defensive posture.',
    'If the kernel''s territorial referent is strategically ambiguous, the constraint''s coordination function is a moving target that always justifies more extraction. This would support snare classification. If the referent is fixed (e.g., 1967 lines), the constraint could be a stable tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel''s territorial ambiguity is a feature enabling extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__political_zionism_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jtcpzr_tr_t0, jewish_territorial_claim__political_zionism_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(jtcpzr_tr_t20, jewish_territorial_claim__political_zionism_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(jtcpzr_tr_t40, jewish_territorial_claim__political_zionism_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(jtcpzr_tr_t60, jewish_territorial_claim__political_zionism_reading, theater_ratio, 60, 0.48).
narrative_ontology:measurement(jtcpzr_tr_t80, jewish_territorial_claim__political_zionism_reading, theater_ratio, 80, 0.52).
narrative_ontology:measurement(jtcpzr_tr_t100, jewish_territorial_claim__political_zionism_reading, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(jtcpzr_be_t0, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(jtcpzr_be_t20, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(jtcpzr_be_t40, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement(jtcpzr_be_t60, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 60, 0.82).
narrative_ontology:measurement(jtcpzr_be_t80, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 80, 0.83).
narrative_ontology:measurement(jtcpzr_be_t100, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jtcpzr_su_t0, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(jtcpzr_su_t20, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(jtcpzr_su_t40, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 40, 0.85).
narrative_ontology:measurement(jtcpzr_su_t60, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 60, 0.88).
narrative_ontology:measurement(jtcpzr_su_t80, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 80, 0.78).
narrative_ontology:measurement(jtcpzr_su_t100, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 100, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__political_zionism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jewish_territorial_claim__political_zionism_reading, 0.12).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the 'Zionism' label into four structurally distinct readings of the jewish_territorial_claim kernel. Political Zionism (this reading) prioritizes sovereign statehood with Jewish majority via diplomatic/legal means. Cultural Zionism rejects sovereignty as necessary. Labor Zionism frames sovereignty as byproduct of socialist settlement. Revisionist Zionism demands maximal territory via military force. Their ε values differ widely: cultural ~0.1, labor ~0.6, political ~0.82, revisionist ~0.9. They share the kernel (Jewish territorial claim) but instantiate different constraints with different beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_territorial_claim__political_zionism_reading, moderate, 0.35).
constraint_indexing:directionality_override(jewish_territorial_claim__political_zionism_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
