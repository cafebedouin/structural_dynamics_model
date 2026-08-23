% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__national_liberation_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: zionist_legitimacy_basis__national_liberation_reading
 *   human_readable: Zionism as National Liberation of Persecuted Indigenous People
 *   domain: political/nationalism/settler-colonialism
 *
 * SUMMARY:
 *   This constraint instantiates the national liberation reading of the
 *   Zionist legitimacy kernel: the claim that Zionism constitutes a
 *   legitimate national liberation movement through which a persecuted
 *   indigenous people returns to its ancestral homeland. The reading frames
 *   Jewish historical connection and European persecution as together
 *   generating an irreversible right to self-determination in Palestine.
 *   Structurally, this narrative coordinates Jewish collective survival and
 *   state-building while asymmetrically extracting territory, sovereignty,
 *   and political voice from the Palestinian Arab population already resident
 *   in the territory. The constraint operates across institutional,
 *   diasporic, and military registers, and its persistence requires active
 *   suppression of Palestinian opposition and alternatives such as
 *   binationalism or refugee return.
 *
 * KEY AGENTS:
 *   - zionist_state_institutions: Primary agenda-setter (institutional/arbitrage) â administers territorial, legal, and military framework
 *   - jewish_diaspora_communities: Primary beneficiary (organized/mobile) â receives collective insurance, identity anchoring, and return rights
 *   - palestinian_arabs_in_palestine: Primary target (powerless/trapped) â bears displacement, occupation, and political exclusion
 *   - palestinian_refugee_populations: Secondary target (powerless/trapped) â bears denial of return and protracted statelessness
 *   - anti_zionist_jewish_dissidents: Excluded voice (moderate/constrained) â structurally absent from legitimacy discourse
 *   - postcolonial_scholars: Analytical observer (analytical/analytical) â documents structural divergence between claims and outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__national_liberation_reading, 0.72).
domain_priors:suppression_score(zionist_legitimacy_basis__national_liberation_reading, 0.78).
domain_priors:theater_ratio(zionist_legitimacy_basis__national_liberation_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__national_liberation_reading, "Zionism as National Liberation of Persecuted Indigenous People").
narrative_ontology:topic_domain(zionist_legitimacy_basis__national_liberation_reading, "political/nationalism/settler-colonialism").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__national_liberation_reading, 'eac82788-5686-43d0-950b-b99810517171').
narrative_ontology:cs_kernel_codification('eac82788-5686-43d0-950b-b99810517171', fixed_text).
narrative_ontology:cs_authority_grounding('eac82788-5686-43d0-950b-b99810517171', lineage).
narrative_ontology:cs_interpretation_layer_present('eac82788-5686-43d0-950b-b99810517171').
narrative_ontology:cs_reading_relation('eac82788-5686-43d0-950b-b99810517171', zionist_legitimacy_basis__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('eac82788-5686-43d0-950b-b99810517171', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('eac82788-5686-43d0-950b-b99810517171', foundational, persecution_generates_return_right).
narrative_ontology:cs_axiom_status(persecution_generates_return_right, holdable).
narrative_ontology:cs_axiom_grounding('eac82788-5686-43d0-950b-b99810517171', persecution_generates_return_right, deontological).
narrative_ontology:cs_axiom('eac82788-5686-43d0-950b-b99810517171', foundational, jewish_indigeneity_to_palestine).
narrative_ontology:cs_axiom_status(jewish_indigeneity_to_palestine, holdable).
narrative_ontology:cs_axiom_grounding('eac82788-5686-43d0-950b-b99810517171', jewish_indigeneity_to_palestine, empirically_contingent).
narrative_ontology:cs_reference_frame('eac82788-5686-43d0-950b-b99810517171', persecuted_indigenous_return).
narrative_ontology:cs_drift_state('eac82788-5686-43d0-950b-b99810517171', post_1967_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('eac82788-5686-43d0-950b-b99810517171', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, zionist_state_institutions).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, jewish_diaspora_communities).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, palestinian_arabs_in_palestine).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, palestinian_refugee_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise sovereignty over the territory of Israel/Palestine, administer immigration policy through the Law of Return, manage military occupation, and expand settlements. Justify these policies as necessary for Jewish collective security and self-determination. Maintain diplomatic, military, and discursive apparatus to defend the national liberation narrative against competing claims.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, zionist_state_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Provide political support, funding, and immigration to the Zionist project. Benefit from the existence of a Jewish state as an insurance policy against persecution and as a center of cultural and religious life. While physically mobile, exit from the Zionist framework is identity-linked for many members.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, jewish_diaspora_communities, beneficiary,
    organized, generational, mobile, global).

% Reside in the territory claimed by the Zionist movement. Subject to displacement, military rule, citizenship restrictions, land expropriation, and movement controls. Their opposition to Zionism is framed within this reading as an illegitimate denial of Jewish rights. Exit is constrained by geography, blockade, checkpoints, and lack of sovereign power.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinian_arabs_in_palestine, payer,
    powerless, generational, trapped, regional).

% Descendants of Palestinians displaced in 1948 and 1967. Denied return to their homes by the sovereignty claims of the national liberation reading. Confined to refugee camps in neighboring states or dispersed in diaspora without citizenship or secure status. Their territorial claim is delegitimized by the narrative of Jewish indigenous return.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinian_refugee_populations, payer,
    powerless, generational, trapped, global).

% Jews who reject the Zionist framing on religious, ethical, or political grounds. Their voices are structurally excluded from mainstream legitimacy discourse; they are frequently accused of self-hatred or betrayal. They face social exclusion within Jewish communities but retain geographic and legal mobility.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, anti_zionist_jewish_dissidents, excluded,
    moderate, biographical, constrained, global).

% Academic analysts who study the structural relationship between Zionism, colonialism, and indigenous rights. They neither pay nor benefit materially from the constraint. They document the divergence between liberation claims and territorial outcomes, comparing the case to other national and colonial formations.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, postcolonial_scholars, observer,
    analytical, civilizational, analytical, global).

narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__national_liberation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for persecuted Jewish populations to achieve collective self-determination and physical security through territorial concentration in what the reading identifies as their ancestral homeland.
% TRANSFER_FUNCTION: Transfers land, political sovereignty, and demographic majority from Palestinian Arab inhabitants to Jewish immigrants and institutions, justified by historical persecution and claimed indigenous status.
% ABSENT_VOICES: Palestinian refugees and anti-Zionist Jewish dissidents are structurally excluded from the legitimacy conversation; their objections are pre-emptively framed as denial of Jewish rights or as self-hatred rather than engaged as legitimate political claims.
% DISAPPEARANCE_RATIONALE: If the national liberation reading vanished as a legitimating framework, the political justification for Jewish sovereign statehood in Palestine would lose its primary ethical anchoring. This would likely lead to binational or single-state reconfiguration, refugee return claims gaining institutional traction, and the dissolution of the current Zionist political structure's moral defense.
% FOUNDING_PROBLEM: Systematic persecution of Jews in Europe and the existential vulnerability of stateless Jewish minorities to genocide, pogroms, and legal discrimination.
% FOUNDING_PROBLEM_CORROBORATION: Jewish historiography and Holocaust documentation corroborate the persecution from outside the immediate beneficiary set. Palestinian historians and postcolonial scholars contest that territorial dispossession was the only or just remedy, and argue the founding problem has been transformed into a reverse-persecution dynamic. International legal scholars offer mixed corroboration, affirming the historical persecution while disputing the legitimacy of the specific territorial solution.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__national_liberation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__national_liberation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__national_liberation_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__national_liberation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__national_liberation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__national_liberation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint transfers land, sovereignty, and demographic control from Palestinians to Jewish institutions. Suppression (0.78) is high because Palestinian opposition is delegitimized as denial of Jewish rights and suppressed by military, legal, and discursive means. Theater ratio (0.48) reflects significant performative maintenance: the liberation narrative is invoked to justify ongoing settlement and occupation that exceed the original security and persecution-remedy rationale. Accessibility collapse (0.75) is high because alternatives such as binationalism, secular democracy, or refugee return are treated as existential threats rather than political options. Resistance (0.80) is high due to sustained Palestinian opposition, international solidarity movements, and sustained academic contestation. The temporal series show extraction peaking at state formation and occupation, with theater rising as the liberation narrative is increasingly invoked to cover expansionist practice.
 *
 * PERSPECTIVAL GAP:
 *   Jewish state institutions and diaspora communities experience the constraint as protective coordination â a necessary refuge from genocide and a legitimate expression of indigenous self-determination. Palestinian seats experience the identical structure as extraction enforced through displacement and military occupation. Anti-Zionist Jewish dissidents experience moderate suppression of voice without territorial extraction. The engine computes this divergence from the structural data: identical constraint, opposite directionalities, divergent computed types per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist state institutions and diaspora communities are structural beneficiaries: the constraint subsidizes their collective security, state sovereignty, and identity continuity (low d). Palestinian Arabs in Palestine and refugees are structural targets: the constraint extracts land, political rights, and return entitlement from them (high d, amplified by trapped exit and regional or global scope). Anti-Zionist Jewish dissidents are excluded rather than targeted, experiencing moderate suppression of voice but not territorial extraction. Postcolonial scholars occupy the analytical seat with negligible d.
 *
 * MANDATROPHY ANALYSIS:
 *   The national liberation framework originated to solve the genuine problem of Jewish persecution and statelessness. The mandatrophy question is whether the framework has outlived this founding problem. The reading's adherents argue the problem persists through antisemitism and security threats, keeping the mandate live. Critics argue the founding problem is either solved (a sovereign state exists with a powerful military) or transformed into a reverse-persecution dynamic, suggesting the mandate is dead and the constraint persists through inertia and extraction. The authored founding_problem_status of contested captures this ambiguity, preventing automatic classification as either live coordination (Rope) or atrophied extraction (Piton/Snare). The Tangled Rope classification is warranted because the coordination function (Jewish collective security) and extraction function (Palestinian displacement) are structurally coupled through the same territorial mechanism and require active enforcement to maintain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigenous_status_contest,
    'Are Jewish communities properly classified as indigenous to Palestine under the political-scientific criteria applied to other indigenous groups, or does this framing instrumentalize indigeneity discourse?',
    'Comparative indigenous studies analysis using UN/ILO criteria for indigenous status, examining historical continuity, territorial connection, and non-dominance relative to other groups in the same territory.',
    'If the indigenous claim fails, the coordination narrative loses its primary ethical anchoring and shifts toward conventional or resource-based justification; if it holds, part of the measured extraction is reclassified as necessary coordination cost for indigenous recovery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_status_contest, conceptual, 'Whether Jewish indigeneity to Palestine is structurally sound or instrumentalized.').

omega_variable(
    displacement_necessity,
    'Was Palestinian displacement a structurally necessary byproduct of Jewish national liberation under this reading, or an independent injustice separable from the liberation logic?',
    'Historical counterfactual analysis assessing whether binational, federated, or non-territorial arrangements were viable in 1948 and were foreclosed by the national liberation framework itself.',
    'If displacement was necessary to the framework, the coordination and extraction are inseparable (Tangled Rope confirmed). If separable, the constraint may decompose into a coordination element (refuge) and an extractive element (expulsion).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_necessity, empirical, 'Whether Palestinian displacement was structurally entangled with the liberation logic.').

omega_variable(
    opposition_delegitimization,
    'Does the national liberation reading structurally require the delegitimization of Palestinian opposition, or can it theoretically accommodate Palestinian self-determination claims without contradiction?',
    'Discourse analysis of Zionist political texts, diplomatic statements, and policy documents to determine whether Palestinian opposition is framed as inherently illegitimate or as a negotiable competing claim.',
    'If delegitimization is structural, suppression is higher than measured and the constraint leans toward Snare. If accommodable, the reading may permit reform toward Rope or Scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(opposition_delegitimization, conceptual, 'Whether Palestinian opposition delegitimization is built into the reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__national_liberation_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t0, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(zion_tr_t20, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(zion_tr_t40, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(zion_tr_t60, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 60, 0.45).
narrative_ontology:measurement(zion_tr_t80, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 80, 0.55).
narrative_ontology:measurement(zion_tr_t100, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 100, 0.5).
narrative_ontology:measurement(zion_tr_t120, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 120, 0.48).

% Extraction over time
narrative_ontology:measurement(zion_be_t0, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(zion_be_t20, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(zion_be_t40, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 40, 0.75).
narrative_ontology:measurement(zion_be_t60, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 60, 0.78).
narrative_ontology:measurement(zion_be_t80, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 80, 0.76).
narrative_ontology:measurement(zion_be_t100, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 100, 0.74).
narrative_ontology:measurement(zion_be_t120, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 120, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t0, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(zion_su_t20, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(zion_su_t40, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(zion_su_t60, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 60, 0.85).
narrative_ontology:measurement(zion_su_t80, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 80, 0.8).
narrative_ontology:measurement(zion_su_t100, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 100, 0.78).
narrative_ontology:measurement(zion_su_t120, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 120, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__national_liberation_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis__settler_colonial_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis__religious_restoration_reading).

% DUAL FORMULATION NOTE:
% The zionist_legitimacy_basis kernel decomposes into at least three structurally distinct constraints. The national_liberation_reading and settler_colonial_reading share the same historical referent but assign opposite epsilon values and opposite beneficiary-victim structures; they are linked as sibling readings but structurally contradict each other. The national_liberation_reading and religious_restoration_reading share overlapping beneficiary structures but differ in authority grounding (lineage/history versus divine promise).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
