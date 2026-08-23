% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__indigenous_return_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__indigenous_return_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: jewish_self_determination__indigenous_return_reading
 *   human_readable: Jewish Indigenous Return as Decolonization
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'indigenous_return_reading' of the
 *   'jewish_self_determination' kernel. The reading asserts that Jewish
 *   people are indigenous to the land of Israel/Palestine with unbroken
 *   connection, making Zionism a decolonial project of return rather than a
 *   colonial project of settlement. It claims mountain status (historical
 *   fact) but operates in a field of competing indigenous claims that
 *   functionally makes it a rope (coordination among claimants). The reading
 *   is advanced by Zionist institutions as the primary legitimacy framework
 *   in international law and discourse, while structurally excluding or
 *   subordinating Palestinian indigenous claims. The claimed_type (mountain)
 *   diverges from the operational metrics (moderate extractiveness,
 *   significant suppression, high resistance) — this divergence is the
 *   measurement, not an error.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__indigenous_return_reading, 0.42).
domain_priors:suppression_score(jewish_self_determination__indigenous_return_reading, 0.38).
domain_priors:theater_ratio(jewish_self_determination__indigenous_return_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__indigenous_return_reading, mountain).
narrative_ontology:human_readable(jewish_self_determination__indigenous_return_reading, "Jewish Indigenous Return as Decolonization").
narrative_ontology:topic_domain(jewish_self_determination__indigenous_return_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__indigenous_return_reading).
domain_priors:emerges_naturally(jewish_self_determination__indigenous_return_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__indigenous_return_reading, 'efe8bc0a-1260-4c25-ba0d-99bb26deb292').
narrative_ontology:cs_kernel_codification('efe8bc0a-1260-4c25-ba0d-99bb26deb292', formalized).
narrative_ontology:cs_authority_grounding('efe8bc0a-1260-4c25-ba0d-99bb26deb292', lineage).
narrative_ontology:cs_interpretation_layer_present('efe8bc0a-1260-4c25-ba0d-99bb26deb292').
narrative_ontology:cs_reading_relation('efe8bc0a-1260-4c25-ba0d-99bb26deb292', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('efe8bc0a-1260-4c25-ba0d-99bb26deb292', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('efe8bc0a-1260-4c25-ba0d-99bb26deb292', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('efe8bc0a-1260-4c25-ba0d-99bb26deb292', jewish_self_determination__diasporist_reading, forecloses).
narrative_ontology:cs_axiom('efe8bc0a-1260-4c25-ba0d-99bb26deb292', foundational, jewish_people_are_indigenous_to_land_of_israel).
narrative_ontology:cs_axiom_status(jewish_people_are_indigenous_to_land_of_israel, holdable).
narrative_ontology:cs_axiom_grounding('efe8bc0a-1260-4c25-ba0d-99bb26deb292', jewish_people_are_indigenous_to_land_of_israel, empirically_contingent).
narrative_ontology:cs_axiom('efe8bc0a-1260-4c25-ba0d-99bb26deb292', foundational, indigenous_status_confers_decolonial_legitimacy_under_international_law).
narrative_ontology:cs_axiom_status(indigenous_status_confers_decolonial_legitimacy_under_international_law, holdable).
narrative_ontology:cs_axiom_grounding('efe8bc0a-1260-4c25-ba0d-99bb26deb292', indigenous_status_confers_decolonial_legitimacy_under_international_law, conventional).
narrative_ontology:cs_reference_frame('efe8bc0a-1260-4c25-ba0d-99bb26deb292', pre_state_jewish_indigenous_continuity).
narrative_ontology:cs_drift_state('efe8bc0a-1260-4c25-ba0d-99bb26deb292', post_1967_occupation_and_settlement_project, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('efe8bc0a-1260-4c25-ba0d-99bb26deb292', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__indigenous_return_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, jewish_people_as_indigenous).
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, zionist_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_self_determination__indigenous_return_reading, jewish_diasporist_communities).
narrative_ontology:constraint_vindicates(jewish_self_determination__indigenous_return_reading, indigenous_peoples_right_to_self_determination).
narrative_ontology:constraint_vindicates(jewish_self_determination__indigenous_return_reading, un_declaration_on_rights_of_indigenous_peoples).
narrative_ontology:constraint_vindicates(jewish_self_determination__indigenous_return_reading, historical_continuity_confers_land_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims indigenous status based on archaeological, genetic, textual, and continuous presence evidence spanning three millennia. Seeks recognition under international indigenous rights frameworks (UNDRIP). The claim is constitutive of collective identity — exit means abandoning the core self-understanding of the people. Gains moral legitimacy, legal standing, and decolonial framing for sovereignty.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, jewish_people_as_indigenous, beneficiary,
    organized, generational, identity_locked, national).

% State of Israel, World Zionist Organization, Jewish Agency, and allied advocacy networks. Actively construct, litigate, and diplomatically advance the indigenous return narrative in international forums, courts, and public discourse. Control the institutional machinery that translates the claim into policy (Law of Return, settlement policy, hasbara). Can pivot to alternative framings (liberal nationalist, security) if this reading loses traction.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, zionist_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% The reading explicitly reframes Palestinian presence as either later arrival (post-7th century) or co-indigenous with subordinate claim. In either framing, Palestinian voices that assert their own indigenous status or reject the subordinate position are structurally excluded from the constraint's internal logic. They bear the material consequences (dispossession, military occupation, denied return) without recognition in this framework.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, palestinian_people, excluded,
    organized, generational, trapped, national).

% UN committees (CERD, CESCR), ICJ, ICC, human rights treaty bodies. Adjudicate competing indigenous claims under UNDRIP and other instruments. Their rulings determine whether the indigenous return reading gains formal legal force or remains contested advocacy. They do not collect rents from the constraint but their recognition is the prize the agenda_setters pursue.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, international_legal_bodies, observer,
    institutional, biographical, analytical, global).

% Advocates of settler_colonial_reading, liberal_nationalist_reading, diasporist_reading, and religious_covenant_reading who contest the indigenous return framing. They operate in academia, media, diplomacy, and activism. Their exclusion is not physical but epistemic: the indigenous return reading's internal logic treats their frameworks as either incomplete (liberal nationalist), false (settler colonial), or supplementary (religious covenant, diasporist).
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, competing_narrative_proponents, excluded,
    organized, biographical, mobile, global).

% Jewish communities and organizations (e.g., Jewish Voice for Peace, Satmar Hasidim, Bundist descendants) who reject the indigenous return framework as the basis for Jewish collective survival. They bear reputational and communal costs when the dominant Zionist institutions equate Jewishness with this reading — facing accusations of self-hatred, antisemitism, or betrayal. Their exit from the consensus is constrained by communal boundaries and identity policing.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, jewish_diasporist_communities, payer,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a morally and legally legible framework under international law (UNDRIP, ILO 169) that converts Jewish historical connection into a recognized right to sovereignty, bypassing the 'colonialism' charge that attaches to other Zionist framings. Coordinates diaspora advocacy, state diplomacy, and legal strategy around a single authoritative claim: we are not settlers, we are returnees.
% TRANSFER_FUNCTION: Transfers the moral burden of proof from Jewish claimants (who would otherwise need to justify displacement of Palestinians) to the international system (which must recognize indigenous rights). Moves legitimacy capital from colonial/settler-colonial frameworks to decolonial frameworks. Materially, translates into land allocation, settlement rights, and military protection for Jewish communities in contested areas — resources extracted from Palestinian territorial contiguity and sovereignty.
% ABSENT_VOICES: Palestinian refugees and their descendants who assert continuous presence and indigenous status; Jewish diasporists who reject territorial sovereignty as the solution to Jewish vulnerability; Bedouin and other non-Jewish indigenous groups in the region whose claims are rendered invisible by the binary Jewish/Palestinian framing. They are absent because the constraint's logic either subsumes them (co-indigenous subordinate) or erases them (later arrival).
% DISAPPEARANCE_RATIONALE: If the indigenous return reading vanished overnight, the primary legal-moral shield for Israeli settlement policy and the Law of Return would collapse. The state would revert to liberal nationalist or security justifications, which lack the decolonial immunity this reading provides. International legal proceedings (ICJ, ICC) would lose the indigenous rights defense. Palestinian claims would gain uncontested standing as the sole indigenous framework. The entire architecture of 'Judea and Samaria' vs 'West Bank' nomenclature would lose its semantic anchor.
% FOUNDING_PROBLEM: The problem of Jewish statelessness and genocidal vulnerability in diaspora, combined with the post-WWII delegitimization of European colonialism — which made classic nationalist colonization frameworks morally and legally untenable. The indigenous return reading was constructed to solve: how can Jewish sovereignty be established without replicating the colonial crime the world just condemned?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (Jewish vulnerability + anti-colonial legitimacy crisis) is attested by: (1) Zionist archival records (Ben-Gurion, Jabotinsky, Labor Zionist debates 1930s-40s) showing explicit awareness of the colonialism charge; (2) UN debate records 1947-49 where Arab states framed Zionism as colonialism and Jewish representatives countered with 'return' rhetoric; (3) UNDRIP drafting history (1980s-2007) where indigenous representatives explicitly debated whether Jews qualify. No single external arbiter corroborates the reading's claim that the problem is *solved* by indigenous status — the corroboration is for the problem's existence, not the reading's solution.
narrative_ontology:disappearance_verdict(jewish_self_determination__indigenous_return_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__indigenous_return_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__indigenous_return_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__indigenous_return_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__indigenous_return_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__indigenous_return_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, ExtMetricName, E),
    domain_priors:suppression_score(jewish_self_determination__indigenous_return_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(jewish_self_determination__indigenous_return_reading),
    narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(jewish_self_determination__indigenous_return_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the reading's operational function: it extracts legitimacy, land, and legal recognition from the international system while externalizing costs onto Palestinians (subordinate claim) and Jewish diasporists (identity policing). The reading claims epsilon ≈ 0 (indigenous status is binary, a fact), but the contested field raises effective extraction. Suppression (0.38) is not direct violence but epistemic and legal: the reading requires active enforcement through hasbara, legal lobbying, archaeological narrative control, and the exclusion of competing historiographies. Theater ratio (0.45) is high because the 'indigenous' framing performs decolonial virtue while the material settlement project expands — the performance is the coordination mechanism. Accessibility collapse (0.62) is moderate: alternative framings (liberal nationalist, religious covenant) remain viable for many Jewish actors, but the indigenous return reading has achieved hegemonic status in official Israeli discourse. Resistance (0.71) is high from Palestinian national movement, international human rights regime, and Jewish diasporist critics.
 *
 * PERSPECTIVAL GAP:
 *   The mountain claim (historical fact) and the operational reality (contested coordination with extraction) produce seat divergence: from the Jewish indigenous seat, the constraint IS the mountain — the fact of origin. From the Palestinian seat, it is a snare — a narrative that erases their indigeneity to extract their land. From the Zionist institutional seat, it is a rope — a coordination mechanism that solves the legitimacy problem. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish people as indigenous: identity_locked beneficiaries (d ≈ 0.1) — the claim constitutes their collective self-understanding; exit means identity dissolution. Zionist institutions: institutional agenda_setters with arbitrage exit (d ≈ 0.15) — they control the narrative machinery and can pivot framings. Palestinian people: excluded/trapped (d ≈ 0.85) — they bear material costs (land, sovereignty, mobility) without recognition in this framework; exit is physically prevented. International legal bodies: analytical observers (d = 0.5) — they adjudicate but don't collect. Competing narrative proponents: excluded/mobile (d ≈ 0.6) — they contest but are structurally locked out of the constraint's internal logic. Jewish diasporists: payers/constrained (d ≈ 0.55) — they bear identity costs for dissenting.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading was founded to solve the Jewish vulnerability + anti-colonial legitimacy crisis (1940s-60s). That problem is contested: statehood achieved (1948) but vulnerability persists (security threats, antisemitism), and the anti-colonial legitimacy crisis has mutated (BDS, ICJ, ICC). The reading now extracts more than it coordinates: it coordinates Jewish return but extracts Palestinian land and Jewish diasporist dissent. The mandatrophy is unresolved — the founding problem's status is contested, and the reading persists as the dominant legitimacy framework despite the shift in conditions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigenous_status_historical_verification,
    'Is the Jewish claim to indigenous status historically and archaeologically established to the standard required by UNDRIP and international law, or is it a constructed narrative serving political ends?',
    'Consensus among archaeologists, geneticists, historians of the ancient Near East, and international legal scholars on the criteria for indigenous status — applied symmetrically to Jewish and Palestinian claims.',
    'If verified, the reading''s mountain claim gains empirical grounding and epsilon drops toward 0. If contested or denied, the reading''s extractiveness is confirmed as political construction, epsilon rises, and classification shifts toward tangled_rope or snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_status_historical_verification, empirical, 'Empirical verification of the core historical claim underlying the mountain classification.').

omega_variable(
    palestinian_indigenous_status_ambiguity,
    'Does the reading''s framing of Palestinian presence as ''later arrival'' or ''co-indigenous with subordinate claim'' reflect historical reality, or is it an epistemic extraction that denies Palestinian indigeneity to secure Jewish primacy?',
    'Symmetrical application of indigenous status criteria (self-identification, historical continuity, distinct culture, connection to territory, non-dominance) to both peoples by an impartial international body.',
    'If Palestinians are co-indigenous with equal claim, the reading''s extraction is asymmetric (Jewish primacy over Palestinian equality) — classification moves to tangled_rope. If Palestinians are not indigenous, the reading''s mountain claim is strengthened but at the cost of historical erasure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(palestinian_indigenous_status_ambiguity, conceptual, 'Whether the reading''s treatment of Palestinian presence is descriptive or extractive.').

omega_variable(
    dual_indigeneity_coordination_possible,
    'Can the indigenous rights framework accommodate two indigenous peoples with overlapping territorial claims, or does it structurally require a single indigenous sovereign?',
    'Comparative analysis of UNDRIP implementation in contexts of competing indigenous claims (e.g., Sami/Norwegian, Maori/Pakeha, Indigenous nations in Canada/USA) — does the framework generate coordination (rope) or zero-sum exclusion (snare)?',
    'If dual indigeneity is structurally impossible under current international law, the reading''s coordination function is illusory — it cannot be a genuine rope. If possible, the reading''s current exclusionary operation is a choice, not a necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_indigeneity_coordination_possible, conceptual, 'Whether the indigenous rights framework can function as a genuine coordination mechanism for competing claims.').

omega_variable(
    committer_kernel_reading_identity,
    'This constraint is one reading (indigenous_return_reading) of the contested kernel ''jewish_self_determination''. What structural elements would change if a sibling reading were instantiated instead?',
    'Comparative constraint story generation for each sibling reading, mapping differences in beneficiaries, victims, epsilon, claimed_type, and axioms.',
    'Documents the committer-frame structure: this reading forecloses settler_colonial_reading and diasporist_reading, coexists_with liberal_nationalist_reading and religious_covenant_reading, influences all siblings through legitimacy competition. The kernel_id and reading_id are structural facts, not authorial choices.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_identity, conceptual, 'Commiter-frame metadata: this constraint''s identity as a kernel reading and its structural relations to siblings.').

omega_variable(
    suppression_mechanism_epistemic_vs_material,
    'Is the suppression measured (0.38) primarily epistemic (narrative control, legal framing, archaeological discourse) or material (military enforcement, settlement expansion, house demolitions), and does the distinction matter for classification?',
    'Disaggregate suppression measurements by mechanism type across the interval; correlate epistemic suppression spikes with material enforcement spikes.',
    'If suppression is primarily epistemic, the reading''s mountain claim (discursive fact) is more plausible. If primarily material, the reading is a cover for snare-like extraction — classification shifts toward tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_epistemic_vs_material, empirical, 'Mechanism of suppression: narrative/legal vs. physical/coercive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__indigenous_return_reading, 1897, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewish_self_determination__indigenous_return_reading_tr_t1897, jewish_self_determination__indigenous_return_reading, theater_ratio, 1897, 0.1).
narrative_ontology:measurement(jewish_self_determination__indigenous_return_reading_tr_t1917, jewish_self_determination__indigenous_return_reading, theater_ratio, 1917, 0.18).
narrative_ontology:measurement(jewish_self_determination__indigenous_return_reading_tr_t1948, jewish_self_determination__indigenous_return_reading, theater_ratio, 1948, 0.3).
narrative_ontology:measurement(jewish_self_determination__indigenous_return_reading_tr_t1967, jewish_self_determination__indigenous_return_reading, theater_ratio, 1967, 0.52).
narrative_ontology:measurement(jewish_self_determination__indigenous_return_reading_tr_t1993, jewish_self_determination__indigenous_return_reading, theater_ratio, 1993, 0.38).
narrative_ontology:measurement(jewish_self_determination__indigenous_return_reading_tr_t2000, jewish_self_determination__indigenous_return_reading, theater_ratio, 2000, 0.48).
narrative_ontology:measurement(jewish_self_determination__indigenous_return_reading_tr_t2024, jewish_self_determination__indigenous_return_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(jewish_self_determination__indigenous_return_reading_be_t1897, jewish_self_determination__indigenous_return_reading, base_extractiveness, 1897, 0.15).
narrative_ontology:measurement(jewish_self_determination__indigenous_return_reading_be_t1917, jewish_self_determination__indigenous_return_reading, base_extractiveness, 1917, 0.22).
narrative_ontology:measurement(jewish_self_determination__indigenous_return_reading_be_t1948, jewish_self_determination__indigenous_return_reading, base_extractiveness, 1948, 0.35).
narrative_ontology:measurement(jewish_self_determination__indigenous_return_reading_be_t1967, jewish_self_determination__indigenous_return_reading, base_extractiveness, 1967, 0.48).
narrative_ontology:measurement(jewish_self_determination__indigenous_return_reading_be_t1993, jewish_self_determination__indigenous_return_reading, base_extractiveness, 1993, 0.41).
narrative_ontology:measurement(jewish_self_determination__indigenous_return_reading_be_t2000, jewish_self_determination__indigenous_return_reading, base_extractiveness, 2000, 0.46).
narrative_ontology:measurement(jewish_self_determination__indigenous_return_reading_be_t2024, jewish_self_determination__indigenous_return_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(jewish_self_determination__indigenous_return_reading_su_t1897, jewish_self_determination__indigenous_return_reading, suppression_requirement, 1897, 0.05).
narrative_ontology:measurement(jewish_self_determination__indigenous_return_reading_su_t1917, jewish_self_determination__indigenous_return_reading, suppression_requirement, 1917, 0.12).
narrative_ontology:measurement(jewish_self_determination__indigenous_return_reading_su_t1948, jewish_self_determination__indigenous_return_reading, suppression_requirement, 1948, 0.28).
narrative_ontology:measurement(jewish_self_determination__indigenous_return_reading_su_t1967, jewish_self_determination__indigenous_return_reading, suppression_requirement, 1967, 0.55).
narrative_ontology:measurement(jewish_self_determination__indigenous_return_reading_su_t1993, jewish_self_determination__indigenous_return_reading, suppression_requirement, 1993, 0.35).
narrative_ontology:measurement(jewish_self_determination__indigenous_return_reading_su_t2000, jewish_self_determination__indigenous_return_reading, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement(jewish_self_determination__indigenous_return_reading_su_t2024, jewish_self_determination__indigenous_return_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__indigenous_return_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__indigenous_return_reading, 0.08).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__diasporist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, palestinian_self_determination__indigenous_claim).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, israeli_settlement_policy__legal_framework).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, international_law__undrip_implementation).

% DUAL FORMULATION NOTE:
% This constraint is the indigenous_return_reading of the jewish_self_determination kernel. It claims mountain status (historical fact) but operates in a field of competing indigenous claims that functionally makes it a coordination mechanism (rope) with asymmetric extraction (tangled_rope tendencies). The sibling readings instantiate different constraints from the same kernel: liberal_nationalist_reading (rope, lower epsilon), settler_colonial_reading (snare from Palestinian seat, mountain from anti-Zionist seat), religious_covenant_reading (mountain from theological seat), diasporist_reading (scaffold or piton). The epsilon values differ structurally: this reading's epsilon is contested (0.42) because its mountain claim is the coordination mechanism itself — the claim *is* the rope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_self_determination__indigenous_return_reading, institutional, 0.15).
constraint_indexing:directionality_override(jewish_self_determination__indigenous_return_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
