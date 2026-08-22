% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   contested kernel 'jewish_self_determination.' It models Zionism as a
 *   European settler-colonial project that arrived in Palestine (First Aliyah
 *   1882, accelerated by Balfour 1917), established a demographic and
 *   institutional foothold, and through 1948 and 1967 created a structure of
 *   systematic extraction from the indigenous Palestinian population: land,
 *   water, mobility, political rights, and the right of return. The
 *   constraint is actively enforced by the Israeli state's military, legal,
 *   and bureaucratic apparatus. The high extractiveness (0.87) and
 *   suppression (0.91) reflect ongoing settlement expansion, the permit
 *   regime, the blockade, and the differential legal architecture. Theater
 *   ratio is low (0.23) because the extraction is not performative — it is
 *   the operating logic. The measurement series tracks the trajectory from
 *   early colonization (low extraction, low suppression) through state
 *   establishment (sharp jump) to occupation and matrix of control (sustained
 *   high extraction).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, 0.87).
domain_priors:suppression_score(jewish_self_determination__settler_colonial_reading, 0.91).
domain_priors:theater_ratio(jewish_self_determination__settler_colonial_reading, 0.23).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, theater_ratio, 0.23).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_self_determination__settler_colonial_reading, "Zionism as European Settler-Colonial Project (Settler-Colonial Reading)").
narrative_ontology:topic_domain(jewish_self_determination__settler_colonial_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__settler_colonial_reading, '7c707476-3e32-41e6-93eb-346c57ca5e7a').
narrative_ontology:cs_kernel_codification('7c707476-3e32-41e6-93eb-346c57ca5e7a', distributed).
narrative_ontology:cs_authority_grounding('7c707476-3e32-41e6-93eb-346c57ca5e7a', extraction).
narrative_ontology:cs_interpretation_layer_present('7c707476-3e32-41e6-93eb-346c57ca5e7a').
narrative_ontology:cs_reading_relation('7c707476-3e32-41e6-93eb-346c57ca5e7a', jewish_self_determination__indigenous_return_reading, forecloses).
narrative_ontology:cs_reading_relation('7c707476-3e32-41e6-93eb-346c57ca5e7a', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c707476-3e32-41e6-93eb-346c57ca5e7a', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c707476-3e32-41e6-93eb-346c57ca5e7a', jewish_self_determination__religious_covenant_reading, influences).
narrative_ontology:cs_axiom('7c707476-3e32-41e6-93eb-346c57ca5e7a', foundational, zionism_as_european_settler_colonialism).
narrative_ontology:cs_axiom_status(zionism_as_european_settler_colonialism, holdable).
narrative_ontology:cs_axiom_grounding('7c707476-3e32-41e6-93eb-346c57ca5e7a', zionism_as_european_settler_colonialism, empirically_contingent).
narrative_ontology:cs_axiom('7c707476-3e32-41e6-93eb-346c57ca5e7a', foundational, palestinian_indigeneity_and_nakba_as_founding_crime).
narrative_ontology:cs_axiom_status(palestinian_indigeneity_and_nakba_as_founding_crime, holdable).
narrative_ontology:cs_axiom_grounding('7c707476-3e32-41e6-93eb-346c57ca5e7a', palestinian_indigeneity_and_nakba_as_founding_crime, empirically_contingent).
narrative_ontology:cs_reference_frame('7c707476-3e32-41e6-93eb-346c57ca5e7a', european_colonial_settlement_project).
narrative_ontology:cs_drift_state('7c707476-3e32-41e6-93eb-346c57ca5e7a', post_oslo_matrix_of_control, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('7c707476-3e32-41e6-93eb-346c57ca5e7a', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__settler_colonial_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, european_jewish_settlers).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, israeli_state).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, zionist_institutions).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_arabs).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_refugees).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_citizens_of_israel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, western_governments).
narrative_ontology:constraint_vindicates(jewish_self_determination__settler_colonial_reading, settler_colonial_elimination_logic).
narrative_ontology:constraint_vindicates(jewish_self_determination__settler_colonial_reading, demographic_engineering_as_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Early Zionist immigrants from Europe who acquired land, established settlements, and built institutions that became the foundation of the Israeli state. They benefited from land acquisition, demographic transformation, and the creation of a Jewish-majority polity. Their exit was mobile — they chose to migrate and could have chosen other destinations (US, Argentina, etc.) — but once invested, exit became costly.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, european_jewish_settlers, beneficiary,
    powerful, biographical, mobile, regional).

% The sovereign state established in 1948 that administers the legal, military, and bureaucratic apparatus maintaining differential legal status (Law of Return vs. Nakba denial, military occupation, settlement expansion). It sets the agenda, enforces the constraint, and extracts resources (land, water, labor) from the Palestinian population. Its exit options are arbitrage-grade — it operates in the international system with significant diplomatic, economic, and military leverage.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, israeli_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Pre-state organizations (Jewish Agency, JNF, WZO) and post-state bodies that coordinate settlement, land acquisition, immigration, and diaspora mobilization. They benefit from institutional continuity, funding streams, and political influence. Their exit is mobile — they could redirect activities — but their identity and donor base are fused to the project.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, zionist_institutions, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__settler_colonial_reading, zionist_institutions, agenda_setter).

% The indigenous population of historic Palestine subjected to displacement (1948, 1967), military occupation, land expropriation, and differential legal status. They bear the extraction: loss of land, restricted movement, denial of return, resource diversion to settlements. Exit is trapped — no right of return, limited emigration options, statelessness for refugees. The constraint is designed to eliminate their collective presence.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_arabs, payer,
    powerless, generational, trapped, national).

% Palestinians displaced in 1948 and 1967 and their descendants, denied return under UNGA 194 while Jewish immigration is unrestricted. They bear the most acute extraction: permanent exile, statelessness, camp conditions, dependency on UNRWA. Exit is trapped — no citizenship, no return, host state restrictions.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Palestinians who remained in 1948 areas and hold Israeli citizenship but face systematic legal discrimination (Adalah documents 65+ discriminatory laws), land confiscation, planning restrictions, and exclusion from the national self-definition of the state. They pay through second-class citizenship while being excluded from the polity's defining mythology.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_citizens_of_israel, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__settler_colonial_reading, palestinian_citizens_of_israel, excluded).

% UN bodies, ICJ, ICC, human rights organizations that document violations (apartheid findings, settlement illegality, occupation law) but lack enforcement power. They observe, adjudicate, and declare but cannot compel compliance. Their exit is analytical — they can reinterpret but not escape the structural contradiction.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, international_law_community, observer,
    institutional, generational, analytical, global).

% States (US, EU, UK, Germany) that provide diplomatic cover, military aid, trade preferences, and institutional legitimacy. They benefit from a strategic ally, intelligence cooperation, and domestic political alignment. They set agendas through vetoes, funding conditions, and diplomatic frameworks. Their exit is arbitrage-grade — they could condition aid but choose not to.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, western_governments, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__settler_colonial_reading, western_governments, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates the establishment and maintenance of a Jewish-majority state in historic Palestine through demographic engineering, land acquisition, and legal architecture that privileges Jewish immigration and settlement over Palestinian presence.
% TRANSFER_FUNCTION: Moves land, water, resources, political rights, and demographic weight from the indigenous Palestinian population to Jewish settlers and the Israeli state — through expropriation, military orders, planning law, and the Law of Return asymmetry.
% ABSENT_VOICES: Palestinian refugees in exile (denied return and representation), Palestinians in Gaza under blockade (denied political agency), pre-1948 Palestinian leadership (eliminated or exiled), and anti-Zionist Jewish voices (marginalized within Jewish communal institutions and Israeli politics). They would object to the elimination logic but are structurally excluded from the arrangement's decision-making.
% DISAPPEARANCE_RATIONALE: If the settler-colonial constraint vanished overnight — Law of Return repealed, occupation ended, settlements dismantled, return implemented — the demographic, legal, and territorial order would fundamentally reorganize. The Jewish-majority state would cease to exist in its current form; a single democratic state or binational arrangement would emerge. The world rearranges because the constraint IS the arrangement.
% FOUNDING_PROBLEM: The founding problem, from this reading's perspective, was NOT a Jewish problem but a European one: how to solve the 'Jewish Question' in Europe by exporting it to Palestine — transferring European antisemitism's victims onto Palestinian land, making Palestinians pay for Europe's crimes.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by: Benny Morris and 'New Historians' (Israeli archives on 1948 expulsion), Rashid Khalidi (The Hundred Years' War on Palestine), Ilan Pappé (The Ethnic Cleansing of Palestine), UNRWA records (5.9M registered refugees), ICJ 2024 advisory opinion (occupation illegality), Human Rights Watch and Amnesty International (apartheid findings), and Palestinian oral history (Nakba testimony). No corroborating source outside the beneficiary set treats the founding problem as live.
narrative_ontology:disappearance_verdict(jewish_self_determination__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__settler_colonial_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__settler_colonial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(jewish_self_determination__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__settler_colonial_reading, 0.87, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is high because the constraint transfers land, resources, and rights from Palestinians to Jewish settlers/state continuously and increasingly. Suppression is very high because the constraint's persistence depends on military occupation, legal denial of return, permit systems, and the active prevention of alternatives (one state, return, binationalism). Theater ratio is low because the security discourse does not mask the extraction — the extraction IS the security logic. Accessibility collapse is high (0.78) because the two-state solution has been rendered non-viable by settlement facts on the ground, and return is legally foreclosed. Resistance is high (0.82) because Palestinians have mounted continuous resistance (intifadas, BDS, legal challenges, sumud) despite overwhelming force.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (Israeli state), the constraint appears as necessary self-defense and realization of self-determination — a coordination function. From the payer seats (Palestinians), it is pure elimination and extraction — a snare. The engine computes this divergence from the structural data: the same legal-military apparatus that coordinates Jewish sovereignty extracts from Palestinian existence. The claimed type (snare) reflects the payer seat's reality; the beneficiary seats would claim rope or mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   European Jewish settlers and the Israeli state are structural beneficiaries (d ~0.1-0.2): they collect land, sovereignty, resources, and demographic dominance. The Israeli state as agenda_setter has arbitrage exit. Zionist institutions benefit institutionally with mobile exit. Palestinian Arabs, refugees, and citizens of Israel are structural payers/victims (d ~0.9-1.0): they bear extraction with trapped or constrained exit. Palestinian citizens of Israel are doubly positioned — payers through discrimination, excluded from the polity's self-definition. Western governments are indirect beneficiaries with arbitrage exit — they could condition but don't. International law community is analytical observer.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (solving Europe's Jewish Question by colonizing Palestine) is dead — the Holocaust ended, European Jewry was largely destroyed or emigrated elsewhere, antisemitism persists but the demographic logic of 1882-1948 is obsolete. Yet the arrangement persists and intensifies (settlement expansion, Nation-State Law, judicial overhaul). This is not mandatrophy in the sense of a degraded function — it is a snare whose extraction logic has become self-sustaining. The 'self-determination' mandate has been captured by the elimination logic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigeneity_claim_contestation,
    'Does the Jewish historical connection to the land constitute indigeneity that reframes the project as return rather than colonization, or is indigeneity a later legitimating narrative for a European colonial project?',
    'Comparative analysis of indigeneity criteria (UNPFII framework: self-identification, historical continuity, distinct institutions, territorial connection) applied to Jewish and Palestinian claims; historical analysis of when indigeneity language entered Zionist discourse (post-1967 vs. pre-1948).',
    'If Jewish indigeneity is structurally established, the settler_colonial_reading''s core premise (European colonizers vs. indigenous Palestinians) is falsified — the constraint becomes a contested sovereignty dispute between two indigenous groups. If indigeneity is a later narrative, the snare classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigeneity_claim_contestation, conceptual, 'Whether Jewish historical connection constitutes indigeneity that reframes the colonial relation.').

omega_variable(
    security_vs_extraction_boundary,
    'How much of the measured suppression (0.91) and extraction (0.87) is structurally necessary for Israeli Jewish physical security vs. how much is the elimination logic of settler colonialism?',
    'Counterfactual analysis: what suppression would remain under a genuine two-state settlement on 1967 lines with security guarantees? Compare to actual suppression matrix (permits, checkpoints, blockade, administrative detention, settlement protection).',
    'If most suppression is security-necessary, the constraint has a genuine coordination core (tangled_rope). If suppression exceeds security requirements and tracks settlement/expansion, it is pure snare. Current measurement cannot cleanly separate them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_vs_extraction_boundary, empirical, 'Boundary between security coordination and colonial elimination in the suppression apparatus.').

omega_variable(
    committer_frame_underdetermination,
    'This constraint is one reading of the kernel ''jewish_self_determination.'' Does the kernel itself have a stable referent, or is ''Jewish self-determination'' an essentially contested concept that different readings instantiate as different constraints with different ε?',
    'Analyze whether the five declared readings share a common structural core (a kernel) or are five distinct constraints linked only by the label. Test: if each reading''s ε, beneficiaries, victims, and type are stable and non-overlapping, the kernel is a linguistic overlay, not a structural entity.',
    'If the kernel has no structural unity, the committer frame is a category error — each reading should stand alone without kernel_context. If the kernel is real, the reading_relations and axioms capture genuine structural relationships.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_underdetermination, conceptual, 'Whether the kernel ''jewish_self_determination'' is a structural entity or a linguistic overlay over five distinct constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__settler_colonial_reading, 1882, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewish_self_determination__settler_colonial_reading_tr_t1882, jewish_self_determination__settler_colonial_reading, theater_ratio, 1882, 0.05).
narrative_ontology:measurement(jewish_self_determination__settler_colonial_reading_tr_t1917, jewish_self_determination__settler_colonial_reading, theater_ratio, 1917, 0.12).
narrative_ontology:measurement(jewish_self_determination__settler_colonial_reading_tr_t1948, jewish_self_determination__settler_colonial_reading, theater_ratio, 1948, 0.18).
narrative_ontology:measurement(jewish_self_determination__settler_colonial_reading_tr_t1967, jewish_self_determination__settler_colonial_reading, theater_ratio, 1967, 0.21).
narrative_ontology:measurement(jewish_self_determination__settler_colonial_reading_tr_t1993, jewish_self_determination__settler_colonial_reading, theater_ratio, 1993, 0.22).
narrative_ontology:measurement(jewish_self_determination__settler_colonial_reading_tr_t2000, jewish_self_determination__settler_colonial_reading, theater_ratio, 2000, 0.23).
narrative_ontology:measurement(jewish_self_determination__settler_colonial_reading_tr_t2024, jewish_self_determination__settler_colonial_reading, theater_ratio, 2024, 0.23).

% Extraction over time
narrative_ontology:measurement(jewish_self_determination__settler_colonial_reading_be_t1882, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1882, 0.15).
narrative_ontology:measurement(jewish_self_determination__settler_colonial_reading_be_t1917, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1917, 0.35).
narrative_ontology:measurement(jewish_self_determination__settler_colonial_reading_be_t1948, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1948, 0.72).
narrative_ontology:measurement(jewish_self_determination__settler_colonial_reading_be_t1967, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1967, 0.81).
narrative_ontology:measurement(jewish_self_determination__settler_colonial_reading_be_t1993, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1993, 0.84).
narrative_ontology:measurement(jewish_self_determination__settler_colonial_reading_be_t2000, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2000, 0.86).
narrative_ontology:measurement(jewish_self_determination__settler_colonial_reading_be_t2024, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2024, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(jewish_self_determination__settler_colonial_reading_su_t1882, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1882, 0.25).
narrative_ontology:measurement(jewish_self_determination__settler_colonial_reading_su_t1917, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1917, 0.55).
narrative_ontology:measurement(jewish_self_determination__settler_colonial_reading_su_t1948, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1948, 0.88).
narrative_ontology:measurement(jewish_self_determination__settler_colonial_reading_su_t1967, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1967, 0.9).
narrative_ontology:measurement(jewish_self_determination__settler_colonial_reading_su_t1993, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1993, 0.89).
narrative_ontology:measurement(jewish_self_determination__settler_colonial_reading_su_t2000, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2000, 0.9).
narrative_ontology:measurement(jewish_self_determination__settler_colonial_reading_su_t2024, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2024, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__settler_colonial_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__settler_colonial_reading, 0.08).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, israeli_palestinian_peace_process).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, palestinian_right_of_return).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jerusalem_status).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, gaza_blockade).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, west_bank_settlement_enterprise).

% DUAL FORMULATION NOTE:
% Part of the jewish_self_determination constraint family. This reading (settler_colonial_reading) has ε=0.87, type=snare, victims=Palestinians. The indigenous_return_reading claims ε≈0.1 (coordination of return), type=rope/mountain, beneficiaries=Jewish indigenous collective. The liberal_nationalist_reading claims ε≈0.3 (some extraction but justified coordination), type=tangled_rope. The diasporist_reading claims the constraint itself is the problem (negative coordination). The religious_covenant_reading operates on theological ε. These are distinct constraints linked by the shared label.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_self_determination__settler_colonial_reading, institutional, 0.15).
constraint_indexing:directionality_override(jewish_self_determination__settler_colonial_reading, powerless, 0.95).
constraint_indexing:directionality_override(jewish_self_determination__settler_colonial_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
