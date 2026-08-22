% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Zionism as European Settler-Colonial Dispossession (Settler-Colonial Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   Beginning with organized Jewish immigration to Ottoman and later British
 *   Mandate Palestine in the late 19th and early 20th centuries, and
 *   accelerating through the 1948 war, the 1967 occupation, and the ongoing
 *   settlement project in the West Bank, this reading holds that a
 *   European-originated national movement established sovereignty over a
 *   territory with a pre-existing indigenous Arab majority through land
 *   acquisition law, military expulsion, and a permanent legal asymmetry (the
 *   Law of Return granting automatic Jewish citizenship worldwide against the
 *   indefinite denial of Palestinian refugee return) that continues to
 *   structure the region's demographic and property order today.
 *
 * KEY AGENTS:
 *   - european_jewish_settlers: primary beneficiary demographic (organized/mobile) — gained land, citizenship, and sovereignty
 *   - israeli_state_apparatus: primary agenda_setter (institutional/arbitrage) — administers the legal-military architecture of exclusion
 *   - jewish_national_fund_and_land_authorities: administrative beneficiary (institutional) — operationalizes land transfer and exclusion
 *   - palestinian_arab_refugees: primary target (powerless/trapped) — bear permanent displacement
 *   - palestinians_under_occupation: primary target (powerless/trapped) — bear ongoing military administration and land loss
 *   - palestinian_citizens_of_israel: secondary target (moderate/constrained) — bear structural inequality within formal citizenship
 *   - international_human_rights_bodies: analytical observer — documents without enforcement capacity
 *   - global_zionist_movement_and_diaspora_funders: excluded structural resourcer — sustains the arrangement from outside the territorial dispute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, 0.86).
domain_priors:suppression_score(jewish_self_determination__settler_colonial_reading, 0.82).
domain_priors:theater_ratio(jewish_self_determination__settler_colonial_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_self_determination__settler_colonial_reading, "Zionism as European Settler-Colonial Dispossession (Settler-Colonial Reading)").
narrative_ontology:topic_domain(jewish_self_determination__settler_colonial_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__settler_colonial_reading, '6a8d914a-47ba-4ab4-a0ba-0dd02781f851').
narrative_ontology:cs_kernel_codification('6a8d914a-47ba-4ab4-a0ba-0dd02781f851', distributed).
narrative_ontology:cs_authority_grounding('6a8d914a-47ba-4ab4-a0ba-0dd02781f851', distributed).
narrative_ontology:cs_reading_relation('6a8d914a-47ba-4ab4-a0ba-0dd02781f851', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('6a8d914a-47ba-4ab4-a0ba-0dd02781f851', jewish_self_determination__indigenous_return_reading, forecloses).
narrative_ontology:cs_reading_relation('6a8d914a-47ba-4ab4-a0ba-0dd02781f851', jewish_self_determination__religious_covenant_reading, influences).
narrative_ontology:cs_reading_relation('6a8d914a-47ba-4ab4-a0ba-0dd02781f851', jewish_self_determination__diasporist_reading, influences).
narrative_ontology:cs_axiom('6a8d914a-47ba-4ab4-a0ba-0dd02781f851', foundational, territorial_sovereignty_achieved_through_indigenous_displacement_is_illegitimate).
narrative_ontology:cs_axiom_status(territorial_sovereignty_achieved_through_indigenous_displacement_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('6a8d914a-47ba-4ab4-a0ba-0dd02781f851', territorial_sovereignty_achieved_through_indigenous_displacement_is_illegitimate, deontological).
narrative_ontology:cs_axiom('6a8d914a-47ba-4ab4-a0ba-0dd02781f851', foundational, european_origin_political_movement_cannot_constitute_indigenous_return).
narrative_ontology:cs_axiom_status(european_origin_political_movement_cannot_constitute_indigenous_return, holdable).
narrative_ontology:cs_axiom_grounding('6a8d914a-47ba-4ab4-a0ba-0dd02781f851', european_origin_political_movement_cannot_constitute_indigenous_return, empirically_contingent).
narrative_ontology:cs_reference_frame('6a8d914a-47ba-4ab4-a0ba-0dd02781f851', pre_1917_multiethnic_ottoman_palestine).
narrative_ontology:cs_drift_state('6a8d914a-47ba-4ab4-a0ba-0dd02781f851', post_oslo_two_state_framework_collapse, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('6a8d914a-47ba-4ab4-a0ba-0dd02781f851', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__settler_colonial_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, european_jewish_settlers).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, jewish_national_fund_and_land_authorities).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_arab_refugees).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinians_under_occupation).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_citizens_of_israel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Arrived in waves from late 19th century onward under the political and financial backing of Zionist organizations, acquiring land through purchase, concession, and later through military conquest and legal transfer mechanisms. On this reading, they functioned as the colonizing demographic whose in-migration was structured to establish a Jewish-majority polity on land already inhabited by Palestinian Arabs.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, european_jewish_settlers, beneficiary,
    organized, generational, mobile, national).

% Administers land law, citizenship law, military occupation, and settlement expansion. Sets and enforces the legal architecture — the Law of Return granting automatic citizenship to Jews worldwide alongside permanent denial of return to Palestinian refugees and their descendants — that this reading treats as the mechanism of ongoing dispossession. Controls military and administrative force sufficient to suppress resistance and foreclose alternatives.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, israeli_state_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Manages land acquisition, transfer, and afforestation historically used to consolidate Jewish landholding and obscure destroyed Palestinian villages. Administers land trusts structured to be leased only to Jewish nationals, operationalizing exclusion at the property-law level rather than through visible violence alone.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, jewish_national_fund_and_land_authorities, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__settler_colonial_reading, jewish_national_fund_and_land_authorities, beneficiary).

% Displaced during 1947-49 and 1967 and barred by law from returning to homes and land now held by the Israeli state or private Jewish ownership. Hold refugee status across multiple host states with limited citizenship rights, generational statelessness, and no legal path of return despite UN General Assembly Resolution 194 affirming that right on paper.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_arab_refugees, payer,
    powerless, civilizational, trapped, regional).

% Live under military administration, checkpoint regimes, settlement expansion onto agricultural land, and a dual legal system in which Jewish settlers in the same territory are subject to civil law while Palestinians are subject to military law. Exit is foreclosed by border control, permit regimes, and the absence of sovereign passage.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinians_under_occupation, payer,
    powerless, generational, trapped, local).

% Hold formal citizenship but, on this reading, experience structurally unequal access to state land allocation, budgetary resources, and political legitimacy within a state that legally defines itself as the nation-state of the Jewish people. Can vote and litigate but cannot alter the foundational legal definition of the state's demographic purpose through ordinary politics.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_citizens_of_israel, payer,
    moderate, generational, constrained, national).

% UN agencies, human rights organizations (Amnesty International, B'Tselem, Human Rights Watch) and international courts document land seizure, settlement expansion, and differential legal treatment. Produce reports and rulings but possess no independent enforcement capacity against a state backed by powerful allies.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% Provides ongoing financial, political, and diplomatic support that sustains settlement infrastructure and state legitimacy narratives. Not physically present in the territorial dispute but structurally implicated as a resourcing party; largely absent from Palestinian-centered accounts of the dispossession mechanism despite being causally central to its persistence.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, global_zionist_movement_and_diaspora_funders, excluded,
    organized, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__settler_colonial_reading, israeli_state_apparatus).
narrative_ontology:fixing_cost_class(jewish_self_determination__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: On this reading, the arrangement coordinates the political, financial, and military resources of a transnational Jewish national movement and allied Western powers to establish and defend Jewish sovereign control over a specific territory — solving, for its beneficiaries, the problem of statelessness and vulnerability to antisemitic violence by creating a state with a demographic majority secured through law and force.
% TRANSFER_FUNCTION: Moves land, water rights, agricultural capital, residency and citizenship status, and physical security from the indigenous Palestinian Arab population to the incoming and incumbent Jewish-Israeli population, formalized through land law, the Law of Return/Citizenship Law asymmetry, military administration, and permanent refugee exclusion.
% ABSENT_VOICES: Palestinian refugees dispersed since 1948 and 1967 have no seat in Israeli domestic political processes that determine land and citizenship law; their claims are adjudicated, if at all, in international fora with no enforcement power. Palestinian citizens of Israel vote but cannot revise the state's foundational demographic self-definition through the ballot.
% DISAPPEARANCE_RATIONALE: If the legal-military architecture of differential citizenship, land control, and occupation dissolved overnight, refugee return claims would activate, land and property title would become contestable on a mass scale, and the demographic and political basis of the current state would be fundamentally renegotiated — this is not a background fact of nature but an actively administered arrangement whose removal would visibly reorganize the territory.
% FOUNDING_PROBLEM: Widespread European antisemitism, culminating in pogroms and the Holocaust, generated an urgent movement to secure a sovereign Jewish homeland where Jewish physical safety would not depend on the tolerance of host nations.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historiography and the Israeli state attest the founding problem (Jewish existential vulnerability) remains live given continued antisemitism and regional hostility. Palestinian historians, the Nakba archival record, and independent scholars (e.g., work drawing on declassified Israeli state archives) attest that whatever the founding problem's validity, its resolution was operationalized through the dispossession of a population that had no role in causing it — corroboration for the dispossession mechanism itself comes from Israeli state archives and UN documentation, sources external to the Palestinian national movement.
narrative_ontology:disappearance_verdict(jewish_self_determination__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__settler_colonial_reading, 0.86, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored high (0.86 at 2024) because this reading treats land, water, residency, and citizenship status as continuously transferred from the indigenous population to the settler-descended population through an active and evolving legal-military apparatus, not a settled historical event. Suppression is authored high (0.82) because the arrangement's persistence depends on military occupation, checkpoint and permit regimes, and legal foreclosure of the refugee right of return — coercion and alternative-suppression are structural to the reading, not incidental. Theater ratio rises after 1993 (0.45 at Oslo) reflecting this reading's view that the peace-process architecture substituted negotiation theater for the underlying territorial and demographic transfer, which continued via settlement expansion during and after the nominal negotiation period. Accessibility collapse is moderate (0.6) rather than near-total because Palestinian citizens of Israel retain formal legal and electoral avenues, however constrained, and international legal fora remain nominally open even without enforcement teeth. Resistance is authored high (0.85), reflecting sustained Palestinian political mobilization, armed and unarmed resistance, and international solidarity movements documented across the interval.
 *
 * DIRECTIONALITY LOGIC:
 *   European Jewish settlers and the Israeli state apparatus sit at the beneficiary end: they set the land and citizenship rules and their exit options are effectively arbitrage-grade — the state can adjust its own legal exposure and the settler population can rely on the state's protective and resourcing apparatus. Palestinian refugees and residents under occupation sit at the full-target end: trapped exit, no legal path to alter the arrangement, and the extraction (land, residency, mobility) flows directly from their position to the beneficiary seats. Palestinian citizens of Israel are directionally intermediate — moderate power and constrained rather than trapped exit reflects formal citizenship, but the story does not treat this as symmetry; the structural extraction (differential land and budgetary allocation, inability to revise the state's constitutional self-definition) still runs from this seat outward.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as snare rather than tangled_rope turns on whether a genuine, non-extractive coordination function exists that is merely burdened by extraction, versus a coordination story that is cover for extraction from the outset. This reading holds the latter: the 'safe national home' coordination function for Jewish settlers is treated as real for the beneficiary population but structurally inseparable from, and historically implemented through, the dispossession mechanism itself — there is no version of the arrangement in this reading's own account where the coordination benefit was available without the extraction. This is why victims are required and enforcement is required: the schema's snare gate (victims present) is satisfied, and the schema's tangled_rope gate (both a genuine coordination function AND asymmetric extraction, holding as two separable strands) is deliberately NOT claimed for this reading — that dual-strand reading belongs more naturally to liberal_nationalist_reading, a sibling file, not to this one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    settler_colonial_categorical_fit,
    'Does the settler-colonial theoretical framework (developed primarily from Anglophone settler states like the US, Canada, Australia) map cleanly onto a movement substantially composed of refugees fleeing genocide and expulsion from their countries of origin, rather than colonizers acting on behalf of an extant metropole?',
    'Comparative historical analysis of settler-colonial theory''s core criteria (metropole sponsorship, elimination of the native, replacement logic) against the specific demographic composition and political motivations of Zionist immigration waves, particularly post-1933 and post-1945 refugee flows with no state of origin to return to.',
    'If the categorical fit is poor, this reading''s classification as snare may still hold on the extraction/displacement evidence but the ''European settler-colonial project'' framing specifically would need qualification — the structural harm to Palestinians would remain regardless of the answer, but the causal-political narrative explaining WHY it happened would shift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(settler_colonial_categorical_fit, conceptual, 'Whether settler-colonial theory''s categorical apparatus, built from other cases, transfers cleanly to this one.').

omega_variable(
    kernel_reading_incommensurability,
    'Is the disagreement between this reading and indigenous_return_reading resolvable by historical evidence, or does it rest on an irreducibly contested premise (who counts as indigenous to a territory with multi-millennial layered habitation and displacement history) that no additional archival evidence can settle?',
    'Distinguish empirically resolvable sub-claims (land acquisition mechanisms, expulsion events, demographic statistics, documented in both Israeli state archives and Palestinian oral history/UNRWA records) from the normative question of which historical connection grounds a valid indigeneity claim, which is a conceptual/political question not settled by archives alone.',
    'If largely conceptual, no amount of archival work will converge this reading with indigenous_return_reading — they will remain coexisting rather than one being empirically vindicated over the other, which is why they are authored as siblings rather than one being treated as this constraint''s rebuttal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the settler-colonial/indigenous-return disagreement is empirical or normative-conceptual at its core.').

omega_variable(
    beneficiary_internal_heterogeneity,
    'European Jewish settlers are treated here as a beneficiary class, but Mizrahi and Sephardi Jewish immigrants (many themselves displaced from Arab and Muslim countries under coercive conditions) do not fit the ''European settler'' description structurally — does folding them into the same beneficiary category obscure a distinct directionality position?',
    'Disaggregate beneficiary analysis by immigration wave and countries of origin; compare land allocation, class position, and political power of European-origin versus Middle Eastern/North African-origin Jewish Israeli populations across the interval.',
    'If Mizrahi/Sephardi Jewish Israelis occupy a materially different directionality position (lower power, less land capital, historically marginalized within Israeli society itself), the beneficiary group as currently authored may overstate homogeneity — a finer-grained stakeholder split could be warranted in a future revision of this story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_internal_heterogeneity, empirical, 'Whether the single beneficiary category ''european_jewish_settlers'' masks internal heterogeneity among Jewish Israeli populations of different origins.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__settler_colonial_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1917, jewish_self_determination__settler_colonial_reading, theater_ratio, 1917, 0.2).
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__settler_colonial_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__settler_colonial_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(jewi_tr_t1993, jewish_self_determination__settler_colonial_reading, theater_ratio, 1993, 0.45).
narrative_ontology:measurement(jewi_tr_t2005, jewish_self_determination__settler_colonial_reading, theater_ratio, 2005, 0.42).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__settler_colonial_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1917, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1917, 0.35).
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1948, 0.75).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1967, 0.8).
narrative_ontology:measurement(jewi_be_t1993, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1993, 0.7).
narrative_ontology:measurement(jewi_be_t2005, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2005, 0.78).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2024, 0.86).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1917, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1917, 0.4).
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1948, 0.75).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1967, 0.8).
narrative_ontology:measurement(jewi_su_t1993, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1993, 0.72).
narrative_ontology:measurement(jewi_su_t2005, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2005, 0.78).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2024, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__settler_colonial_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling constraints decomposing the natural-language label 'Zionism'/'Jewish self-determination' per the ε-invariance principle: measuring this claim through a settler-colonial-theory lens yields high, actively-suppressed extraction (ε=0.86, snare), while measuring the same underlying kernel through a liberal-nationalist lens or an indigenous-return lens yields structurally different ε and different victim sets. These are not the same constraint viewed from different angles — they are five distinct constraints sharing one kernel identifier (jewish_self_determination), linked here rather than merged. Each carries its own beneficiaries, victims, and classification; none is privileged as the 'true' measurement of the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
