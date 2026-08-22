% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: jewish_self_determination__settler_colonial_reading
 *   human_readable: Zionism as Settler-Colonial Dispossession (Settler-Colonial Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint instantiates the settler-colonial reading of the
 *   jewish_self_determination kernel: it holds that the Zionist project,
 *   whatever its stated motivations, structurally functioned and continues to
 *   function as a European settler-colonial enterprise that dispossessed the
 *   indigenous Palestinian Arab population through village depopulation, land
 *   law, differential citizenship, and military occupation. This is ONE of
 *   five declared readings of the same underlying kernel (Jewish claim to
 *   territorial self-determination in historic Palestine/the Land of Israel);
 *   the liberal_nationalist_reading, indigenous_return_reading,
 *   religious_covenant_reading, and diasporist_reading are separate
 *   constraint stories with their own ε, beneficiary/victim sets, and
 *   classifications, linked via network.affects_constraints. This story does
 *   not adjudicate between readings, average across them, or hedge ε toward
 *   any other reading's value — per DP-001 ε-invariance, it treats the
 *   settler-colonial account as ε-fixed and internally coherent, evaluated by
 *   its own analytical lights.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, 0.82).
domain_priors:suppression_score(jewish_self_determination__settler_colonial_reading, 0.85).
domain_priors:theater_ratio(jewish_self_determination__settler_colonial_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_self_determination__settler_colonial_reading, "Zionism as Settler-Colonial Dispossession (Settler-Colonial Reading)").
narrative_ontology:topic_domain(jewish_self_determination__settler_colonial_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__settler_colonial_reading, 'caaa2ccf-ef91-4627-9df9-af610a167150').
narrative_ontology:cs_kernel_codification('caaa2ccf-ef91-4627-9df9-af610a167150', distributed).
narrative_ontology:cs_authority_grounding('caaa2ccf-ef91-4627-9df9-af610a167150', extraction).
narrative_ontology:cs_interpretation_layer_present('caaa2ccf-ef91-4627-9df9-af610a167150').
narrative_ontology:cs_reading_relation('caaa2ccf-ef91-4627-9df9-af610a167150', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('caaa2ccf-ef91-4627-9df9-af610a167150', jewish_self_determination__indigenous_return_reading, forecloses).
narrative_ontology:cs_reading_relation('caaa2ccf-ef91-4627-9df9-af610a167150', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('caaa2ccf-ef91-4627-9df9-af610a167150', jewish_self_determination__diasporist_reading, influences).
narrative_ontology:cs_axiom('caaa2ccf-ef91-4627-9df9-af610a167150', foundational, settler_colonial_framework_applies_to_zionist_settlement).
narrative_ontology:cs_axiom_status(settler_colonial_framework_applies_to_zionist_settlement, holdable).
narrative_ontology:cs_axiom_grounding('caaa2ccf-ef91-4627-9df9-af610a167150', settler_colonial_framework_applies_to_zionist_settlement, empirically_contingent).
narrative_ontology:cs_axiom('caaa2ccf-ef91-4627-9df9-af610a167150', secondary, structural_dispossession_effect_grounds_illegitimacy_independent_of_founding_intent).
narrative_ontology:cs_axiom_status(structural_dispossession_effect_grounds_illegitimacy_independent_of_founding_intent, holdable).
narrative_ontology:cs_axiom_grounding('caaa2ccf-ef91-4627-9df9-af610a167150', structural_dispossession_effect_grounds_illegitimacy_independent_of_founding_intent, deontological).
narrative_ontology:cs_reference_frame('caaa2ccf-ef91-4627-9df9-af610a167150', pre_1917_ottoman_demographic_status_quo).
narrative_ontology:cs_drift_state('caaa2ccf-ef91-4627-9df9-af610a167150', post_oslo_settlement_expansion_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('caaa2ccf-ef91-4627-9df9-af610a167150', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__settler_colonial_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, european_jewish_settler_population).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, jewish_national_fund_land_institutions).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_arab_indigenous_population).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_refugees_1948).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_residents_under_occupation).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_vindicates(jewish_self_determination__settler_colonial_reading, settler_colonial_theory_applicability_to_zionism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers land allocation, citizenship law (Law of Return granting automatic citizenship to any Jew worldwide while denying Palestinian refugees the right of return), military governance over occupied territory, and settlement expansion policy. Sets and enforces the legal architecture that this reading identifies as the extraction mechanism. Frames its own founding and continued expansion as security necessity and national self-determination.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Received land, housing, and citizenship rights through state-administered programs (some on land depopulated in 1948, some in settlements built on occupied territory after 1967) that Palestinians displaced from the same land cannot access. Holds full civil and property rights unavailable to the indigenous population under military law or in refugee status.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, european_jewish_settler_population, beneficiary,
    organized, generational, mobile, national).

% Administers land acquired through pre-state purchase, post-1948 absentee property law, and ongoing settlement development, allocating it under statutes and internal charters that this reading holds restrict long-term leasehold benefit to Jewish nationals. Functions as both a partial rule-setter within the land regime and a direct recipient of the land transferred by it.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, jewish_national_fund_land_institutions, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__settler_colonial_reading, jewish_national_fund_land_institutions, agenda_setter).

% The population present on the land prior to and during Zionist settlement, subject under this reading to village depopulation, land confiscation, and exclusion from the citizenship and property regime built on the same territory. Bears the structural cost of the arrangement across generations with no institutional path to reverse it.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_arab_indigenous_population, payer,
    powerless, generational, trapped, national).

% Displaced in 1948 and its aftermath, dispersed across neighboring states and camps, formally barred from return by Israeli law while the Law of Return simultaneously grants automatic entry and citizenship to Jews with no prior residence in the territory. This asymmetry is, under this reading, the clearest legal expression of the extraction structure.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_refugees_1948, payer,
    powerless, generational, trapped, regional).

% Live under military law in the West Bank and Gaza, subject to movement restriction, settlement expansion onto adjacent land, and a dual legal system in which settlers nearby are governed by civil law. Cannot access citizenship, cannot freely relocate, and have no vote in the state administering their daily life.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_residents_under_occupation, payer,
    powerless, biographical, trapped, national).

% Hold formal citizenship but, under this reading, experience structural exclusion from land allocated through Jewish-national institutions, from full political legitimacy in a state whose Basic Law defines it as the nation-state of the Jewish people, and from equal municipal and budgetary treatment.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_citizens_of_israel, payer,
    powerless, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__settler_colonial_reading, palestinian_citizens_of_israel, excluded).

% Jews outside Israel who reject the settler-colonial framing, hold competing readings of the same kernel (liberal-nationalist, covenantal, diasporist), or are otherwise not represented in the extraction/beneficiary account this reading gives. Their objections to being cast as structural beneficiaries are not adjudicated within this constraint.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, diaspora_jewish_communities_ambivalent, excluded,
    organized, generational, mobile, global).

% Apply comparative settler-colonial frameworks (drawing on Algeria, South Africa, Australia, and the Americas) to classify the founding and maintenance of the Israeli state. Produce the analytical apparatus this reading relies on; their classification is itself contested by scholars applying the sibling readings to the same historical record.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, postcolonial_and_settler_colonial_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__settler_colonial_reading, israeli_state_apparatus).
narrative_ontology:fixing_cost_class(jewish_self_determination__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, the arrangement coordinates the transfer of land, citizenship rights, and physical security from an indigenous population to an incoming settler population and its state, using nationalist ideology (self-determination) as the organizing narrative that recruits settlers and sustains institutional cohesion among beneficiaries.
% TRANSFER_FUNCTION: Moves land, water and resource access, freedom of movement, citizenship rights, and physical safety from Palestinian Arabs to the Jewish settler population and the Israeli state, via mechanisms this reading identifies as village depopulation, absentee property law, differential citizenship (Law of Return vs. denied right of return), and military administration of occupied territory.
% ABSENT_VOICES: Palestinian refugees barred from return, and residents under military occupation without the franchise, are structurally absent from the political process that produces and revises the arrangement; sibling readings held by other Jewish communities and by religious and liberal-nationalist framings are also excluded from this reading's own internal account, since this constraint speaks only for the settler-colonial framing.
% DISAPPEARANCE_RATIONALE: If the legal and institutional architecture this reading identifies (Law of Return asymmetry, military governance, differential land access) were dismantled overnight, the demographic, legal, and territorial arrangement of the entire region would be renegotiated — refugee return claims, land restitution claims, and citizenship equality claims would all become immediately live, which is precisely why this reading treats the arrangement as constructed rather than natural.
% FOUNDING_PROBLEM: Under this reading, the founding problem as officially stated was Jewish physical safety and self-determination after centuries of European antisemitism and, decisively, the Holocaust; this reading holds that the actual operative problem being solved on the ground was how to establish and secure a demographic and territorial majority on land already inhabited by another people.
% FOUNDING_PROBLEM_CORROBORATION: Israeli state historiography and most of the settler-beneficiary population attest the founding problem (Jewish safety) is still live and justifies the current arrangement. Independent corroboration from outside the beneficiary set — UN human rights bodies, B'Tselem, Amnesty International, and comparative settler-colonial scholarship — attests instead that the operative mechanism (land transfer and demographic control) persists and has expanded well past any framing of Jewish physical safety as the sole or primary driver, which is the basis for this reading's contested status.
narrative_ontology:disappearance_verdict(jewish_self_determination__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__settler_colonial_reading, 0.82, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.82 at 2024) because, under this reading's own terms, the arrangement transfers land, water, freedom of movement, and citizenship rights from an indigenous population to a settler population and its state on an ongoing and expanding basis (settlement growth in the West Bank, differential legal status for Palestinian citizens of Israel, continued refugee exclusion). Suppression is authored even higher (0.85) because this reading holds the arrangement depends on active military, legal, and bureaucratic enforcement — not on the voluntary participation of the excluded population — to persist; the sharp jump in suppression_requirement at 1948 and 1967 marks, in this reading's account, the transition from settlement-building to state-enforced territorial control. Theater ratio is moderate (0.4): this reading holds that some genuine internal coordination function exists (a state does administer real services for its citizens) but that a substantial share of the justificatory apparatus (framing all expansion as security necessity) is performative cover for the extractive function.
 *
 * DIRECTIONALITY LOGIC:
 *   European Jewish settlers and the Israeli state apparatus are coded as structural beneficiaries under this reading: they hold the land, citizenship rights, and physical security that the arrangement transfers, and derive their exit and mobility from institutions the arrangement itself built. Palestinian Arabs across all four named payer groups are coded near the full-target end: trapped exit (refugees barred from return, occupied residents without free movement, citizens facing structural exclusion despite formal citizenship), generational time horizon, and no institutional path to renegotiate the arrangement from within. Jewish National Fund institutions carry a dual role because, under this reading, they simultaneously administer part of the land-allocation rule set and are themselves the direct recipient of transferred land — a genuinely dual-positioned agent rather than a modeling artifact.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading holds that the founding problem (Jewish physical safety after European persecution and genocide) was real and the arrangement's coordination function for that specific population is not being denied — what this reading disputes is that the coordination function required, or now requires, the specific extractive architecture (land transfer without compensation, differential citizenship, ongoing settlement expansion) that the arrangement in fact deployed and continues to deploy. The founding_problem_status is authored as contested rather than dead, because from inside the beneficiary account the safety problem remains live (regional threat perception, historical trauma), while from outside corroborating sources (international human rights bodies, comparative colonial scholarship) the mechanism has evidently exceeded any narrow safety-preserving function. This is precisely the kind of divergence the mismatch consumer (founding_problem_status x disappearance_verdict) is built to surface, rather than something this story resolves on its own authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    settler_vs_indigenous_classification_ambiguity,
    'Does the Jewish population that settled in Ottoman/Mandate Palestine in the late 19th and 20th centuries constitute a colonial-settler population (this reading) or a diaspora population returning to indigenous ancestral territory (the indigenous_return_reading)? The classification is foundational and the two readings cannot both be correct as accounts of the same historical population''s structural position.',
    'There is no purely empirical resolution: both readings draw on the same historical record (continuous Jewish presence in the region alongside majority Arab population; European origin of the primary Zionist settlement waves; length and nature of prior sovereign control) and weight the criteria for ''indigenous'' and ''settler'' differently. Comparative genocide and settler-colonial studies scholarship, international law doctrine on self-determination and indigeneity, and historical demographic research all bear on it without fully settling it.',
    'If the indigenous framing is accepted, this constraint''s entire beneficiary/victim assignment inverts and the settler_colonial_reading dissolves as a coherent account of the same events. If the settler framing holds, the reading''s snare classification and high ε are supported. This is the single highest-stakes unresolved question the kernel contains.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(settler_vs_indigenous_classification_ambiguity, conceptual, 'Whether the founding population is classified as settler-colonial or indigenous-returning is the pivot point between this reading and the indigenous_return_reading.').

omega_variable(
    founding_intent_vs_structural_function,
    'Does this reading''s classification depend on demonstrating that dispossession was the intended purpose of Zionist settlement, or only that it was the structural effect regardless of stated intent (safety, national self-determination)?',
    'Historical archival research on Zionist institutional planning documents (Jewish Agency, Jewish National Fund internal records), demographic planning records, and comparison with acknowledged settler-colonial projects elsewhere where intent and effect diverge or converge.',
    'If intent is required and cannot be established, this reading''s snare classification weakens toward tangled_rope (genuine coordination function for a real safety need, with extraction as an unintended but real byproduct) rather than snare (extraction as the primary designed function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_intent_vs_structural_function, conceptual, 'Whether the reading requires demonstrated extractive intent or only extractive structural effect.').

omega_variable(
    diaspora_jewish_beneficiary_status_contested,
    'Are diaspora Jewish communities who do not emigrate, do not hold Israeli citizenship, and in many cases reject this reading''s framing properly classified as beneficiaries of the arrangement (via the Law of Return''s standing offer) or as excluded from the beneficiary account entirely?',
    'Survey and self-report data on diaspora political identification with the Israeli state; legal analysis of whether unclaimed eligibility constitutes structural benefit.',
    'Narrowing the beneficiary class to settlers and the state apparatus (rather than all Jews globally) changes the scope and scale of extraction attributed to the constraint and affects how sweeping the snare classification should be read as.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_jewish_beneficiary_status_contested, conceptual, 'Scope ambiguity in who counts as a structural beneficiary versus merely eligible non-participant.').

omega_variable(
    cs_framing_state_vs_ideology,
    'Should the kernel-reading''s authority structure be modeled as grounded in the Israeli state''s institutional and legal apparatus (extraction-grounded, since the state administers and benefits from the arrangement) or as grounded in the diffuse ideological tradition of Zionist political thought (distributed, since no single body adjudicates what Zionism ''really'' requires)?',
    'Compare the degree to which state institutions (Knesset legislation, Supreme Court rulings on Law of Return and land law) versus diffuse civil-society and diaspora ideological debate actually determine the arrangement''s ongoing shape.',
    'Choosing extraction grounding supports treating this reading as a fixed_text/formalized CS structure with a strong interpretation layer (the state as interpreter); choosing distributed grounding would remove the interpretation_layer_present claim and treat the kernel as under-specified with no single adjudicator, changing how contamination and drift propagate to sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_state_vs_ideology, conceptual, 'Alternative CS framings (state-institutional vs. ideological-distributed) would change the cs_structure classification for this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__settler_colonial_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1917, jewish_self_determination__settler_colonial_reading, theater_ratio, 1917, 0.2).
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__settler_colonial_reading, theater_ratio, 1948, 0.25).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__settler_colonial_reading, theater_ratio, 1967, 0.28).
narrative_ontology:measurement(jewi_tr_t1993, jewish_self_determination__settler_colonial_reading, theater_ratio, 1993, 0.45).
narrative_ontology:measurement(jewi_tr_t2000, jewish_self_determination__settler_colonial_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(jewi_tr_t2010, jewish_self_determination__settler_colonial_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__settler_colonial_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1917, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1917, 0.35).
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1948, 0.68).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1967, 0.74).
narrative_ontology:measurement(jewi_be_t1993, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1993, 0.7).
narrative_ontology:measurement(jewi_be_t2000, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2000, 0.76).
narrative_ontology:measurement(jewi_be_t2010, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1917, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1917, 0.4).
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1948, 0.75).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1967, 0.82).
narrative_ontology:measurement(jewi_su_t1993, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1993, 0.78).
narrative_ontology:measurement(jewi_su_t2000, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2000, 0.83).
narrative_ontology:measurement(jewi_su_t2010, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2010, 0.85).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__settler_colonial_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% This story is one of five siblings decomposing the natural-language label 'Zionism' / 'Jewish self-determination' per the ε-invariance principle: the settler-colonial reading, liberal-nationalist reading, indigenous-return reading, religious-covenant reading, and diasporist reading each authors a structurally distinct claim about the same underlying kernel (the legitimacy and structure of Jewish territorial self-determination in historic Palestine/the Land of Israel), with different ε, different beneficiary/victim sets, and in several cases inverted victim assignments (this reading and indigenous_return_reading assign beneficiary/victim status in directly opposing directions over the same population). None of the five stories average, blend, or hedge across each other; each is internally ε-fixed and evaluated by its own reading's lights, linked here for contamination-propagation and family-tracking purposes only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
