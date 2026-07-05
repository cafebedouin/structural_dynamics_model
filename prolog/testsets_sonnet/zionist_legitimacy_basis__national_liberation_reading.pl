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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: zionist_legitimacy_basis__national_liberation_reading
 *   human_readable: Zionism as National Liberation Movement (National Liberation Reading)
 *   domain: political_history/nationalism/settler_colonialism_studies
 *
 * SUMMARY:
 *   This story instantiates the national-liberation reading of the contested
 *   Zionist legitimacy kernel: Zionism as the political mobilization of a
 *   persecuted, historically-connected diaspora nation returning to and
 *   re-establishing sovereignty in its ancestral homeland, with
 *   early-20th-century antisemitic persecution and the Holocaust as the
 *   central justifying facts. This reading is analytically distinct from the
 *   settler-colonial reading (which treats the same events as a European
 *   colonial implantation) and the religious-restoration reading (which
 *   treats the same events as fulfillment of covenantal promise, active
 *   chiefly among religious Zionists especially post-1967). Each reading is
 *   authored as its own constraint with its own epsilon; this file does not
 *   average across them or hedge between them. The genuine coordination
 *   function — organizing a stateless, persecuted population around
 *   collective self-determination and physical safety — is real and is the
 *   load-bearing claim of this reading. The extraction component is the
 *   displacement of, and continued denial of return to, the pre-existing Arab
 *   population, whose competing national claim this reading structurally
 *   treats as illegitimate resistance rather than as symmetric grievance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__national_liberation_reading, 0.58).
domain_priors:suppression_score(zionist_legitimacy_basis__national_liberation_reading, 0.62).
domain_priors:theater_ratio(zionist_legitimacy_basis__national_liberation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__national_liberation_reading, "Zionism as National Liberation Movement (National Liberation Reading)").
narrative_ontology:topic_domain(zionist_legitimacy_basis__national_liberation_reading, "political_history/nationalism/settler_colonialism_studies").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__national_liberation_reading, 'a79a9213-e923-4850-80c0-258e262f4b99').
narrative_ontology:cs_kernel_codification('a79a9213-e923-4850-80c0-258e262f4b99', distributed).
narrative_ontology:cs_authority_grounding('a79a9213-e923-4850-80c0-258e262f4b99', distributed).
narrative_ontology:cs_reading_relation('a79a9213-e923-4850-80c0-258e262f4b99', zionist_legitimacy_basis__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('a79a9213-e923-4850-80c0-258e262f4b99', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('a79a9213-e923-4850-80c0-258e262f4b99', foundational, persecution_and_historical_connection_ground_return_right).
narrative_ontology:cs_axiom_status(persecution_and_historical_connection_ground_return_right, holdable).
narrative_ontology:cs_axiom_grounding('a79a9213-e923-4850-80c0-258e262f4b99', persecution_and_historical_connection_ground_return_right, deontological).
narrative_ontology:cs_axiom('a79a9213-e923-4850-80c0-258e262f4b99', foundational, arab_opposition_constitutes_denial_of_jewish_self_determination).
narrative_ontology:cs_axiom_status(arab_opposition_constitutes_denial_of_jewish_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('a79a9213-e923-4850-80c0-258e262f4b99', arab_opposition_constitutes_denial_of_jewish_self_determination, conventional).
narrative_ontology:cs_reference_frame('a79a9213-e923-4850-80c0-258e262f4b99', diaspora_persecution_and_ancestral_return_legitimation).
narrative_ontology:cs_drift_state('a79a9213-e923-4850-80c0-258e262f4b99', post_1993_oslo_and_contemporary_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a79a9213-e923-4850-80c0-258e262f4b99', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, jewish_immigrant_settlers).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, yishuv_institutions).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, israeli_state_successor_institutions).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, palestinian_arab_residents_1917_1948).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, palestinian_refugees_post_1948).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__national_liberation_reading, jewish_indigeneity_to_land_of_israel).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__national_liberation_reading, self_determination_right_of_persecuted_diaspora_nation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Arrive fleeing pogroms, legal exclusion, and later genocide in Europe and expulsion from Arab lands; organize land purchase, agricultural settlement, and paramilitary self-defense under the reading that this is return to an ancestral homeland after millennia of persecution elsewhere, not colonization of a foreign land. Their exit from Europe was itself often foreclosed by quota systems and extermination, making the land the only available refuge in their own account.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, jewish_immigrant_settlers, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__national_liberation_reading, jewish_immigrant_settlers, agenda_setter).

% The pre-state Jewish self-governing bodies (Jewish Agency, Haganah, National Council) that negotiate with the British Mandate, organize immigration, and build state-like institutions, treating international recognition (Balfour Declaration, League of Nations Mandate, UN Partition) as vindication of a pre-existing national right rather than as license granted by outside powers.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, yishuv_institutions, agenda_setter,
    institutional, generational, mobile, regional).

% Long-resident Arab population whose land purchases, displacement, and political marginalization under Mandate-era Zionist institution-building are read, from this constraint's own vantage, as regrettable friction incidental to a just national return rather than as primary dispossession. Their opposition to Jewish immigration and statehood is characterized within this reading as illegitimate resistance to Jewish self-determination, foreclosing recognition of their own competing land claim as a symmetric national one.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinian_arab_residents_1917_1948, payer,
    powerless, biographical, trapped, local).

% Those displaced in and after the 1948 war and denied return under Israeli law; within the national-liberation reading their displacement is attributed to Arab state rejection of partition and the war Arab armies initiated, not to a program of ethnic displacement, which forecloses their claim to a right of return as a matter internal to the constraint's own legitimating logic.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinian_refugees_post_1948, payer,
    powerless, generational, trapped, regional).

% The state apparatus founded in 1948 that inherits and formalizes the national-liberation legitimation, using it in law (Law of Return), diplomacy, and civic education to ground sovereignty claims, military actions, and immigration policy in the persecution-and-return narrative rather than in conquest or mandate-derived title alone.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, israeli_state_successor_institutions, beneficiary,
    institutional, civilizational, arbitrage, national).

% Rejected partition and fought the 1948 and subsequent wars framing Zionism as foreign colonial imposition; within this reading their position is treated not as a legitimate competing national claim to be negotiated but as denial of Jewish national rights, which structurally excludes their framing from the constraint's own account of what happened.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, arab_states_and_pan_arab_nationalist_movements, excluded,
    powerful, generational, constrained, regional).

% Assess archival records, demographic data, and legal instruments (Mandate texts, UN resolutions, expulsion and land-transfer records) to evaluate whether the persecution-and-return account, the settler-colonial account, or some synthesis best fits the documentary record; findings are contested and cited selectively by all parties.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, historians_and_international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diaspora Jewish communities facing sustained persecution around a single, organized project of collective self-determination and physical safety through territorial concentration and eventual sovereignty, solving a genuine problem of statelessness and vulnerability to pogroms, expulsion, and genocide that no diaspora status had solved.
% TRANSFER_FUNCTION: Moves land, political sovereignty, and demographic majority status from the pre-existing Arab population of Mandate Palestine to Jewish immigrant and later Israeli state institutions, financed by land purchase, international diplomatic support, and, from 1947-49 onward, military force and post-war property law.
% ABSENT_VOICES: Palestinian Arab residents and refugees are structurally present as opponents to be answered rather than as parties whose own national claim is evaluated on equal footing within this reading; their historiography, displacement testimony, and land records are treated as secondary to the persecution narrative that grounds Jewish claims.
% DISAPPEARANCE_RATIONALE: If the national-liberation legitimation collapsed as the dominant self-understanding, Israeli constitutional law (Law of Return), diplomatic argument, and civic education would lose their primary grounding narrative and would have to justify sovereignty and immigration policy on other bases (conquest, mandate succession, or negotiated recognition), materially changing legal and political arguments in ongoing territorial and refugee disputes.
% FOUNDING_PROBLEM: Sustained, escalating persecution of Jews in Europe (pogroms, legal exclusion, the Holocaust) and later expulsion from Arab and Muslim states, combined with the absence of any state willing to guarantee Jewish safety, created a need for a sovereign refuge understood as return to a historically and religiously attested homeland.
% FOUNDING_PROBLEM_CORROBORATION: Holocaust historiography and refugee-flow documentation from independent historians corroborate that persecution was real and severe and that statelessness was a genuine, unsolved problem prior to 1948. However, historians outside the Zionist institutional tradition (including Israeli 'new historians' using Israeli state archives) dispute whether the specific mode of resolution — displacement of the existing Arab population rather than, e.g., a binational or partitioned state achieved without mass displacement — was necessitated by the founding problem or was a separate political choice; this corroboration is therefore partial, not full.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__national_liberation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__national_liberation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__national_liberation_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises sharply around 1948 (land transfer, refugee displacement, denial of return under law) and moderates afterward as the constraint stabilizes into an established state's legal and diplomatic architecture rather than an active land-transfer program, though it never returns to pre-1948 levels because occupation, settlement, and refugee-status denial continue to operate through this same legitimating logic post-1967. Suppression tracks the same arc: rising sharply through the Mandate period and the 1948 war as competing claims are actively delegitimized and militarily contested, moderating somewhat after formal statehood but remaining substantial because the reading requires continuously answering, rather than accommodating, the rival national claim. Theater ratio is modest and slowly rising, reflecting increasing use of commemorative and educational framing (Holocaust memorialization tied explicitly to state legitimation, civic ceremony) layered onto the functional core over time.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary/agenda-setter seats (Jewish immigrant settlers, yishuv institutions, Israeli state institutions), the structure is experienced as vindicated national self-determination correcting historic injustice; from the payer seats (Palestinian Arab residents and refugees), the identical structural events are experienced as dispossession justified after the fact by a narrative that treats their own competing claim as illegitimate. The engine computes these as different seat classifications from the same structural data; this story does not adjudicate which seat is 'correct' — that adjudication is exactly what the settler_colonial_reading sibling constraint exists to carry from the opposing structural premise.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish immigrant settlers and successor state institutions are declared beneficiaries because the constraint's operation transfers land, sovereignty, and demographic security to them; their exit options were often genuinely foreclosed in Europe, which is itself the persecution history that grounds the reading's own legitimating force, and is reflected in constrained rather than fully mobile exit for the settler generation. Palestinian Arab residents and refugees are declared victims because the same operation transfers land and political standing away from them and, in the post-1948 case, forecloses return; their powerless, trapped structural position is what the reading's own internal logic treats as an unfortunate but justified cost of a prior, more urgent claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the absence of any state willing to guarantee Jewish safety amid documented, severe persecution — was genuinely live through 1948 and arguably remains partially live given continued antisemitism globally; this keeps the founding_problem_status at 'contested' rather than 'dead,' since a straightforward mandatrophy verdict (problem solved, arrangement now purely legacy extraction) is not clearly supportable from outside sources. What IS contested, per historians working from Israeli state archives, is whether the specific extractive component — displacement of the existing population rather than a negotiated non-displacing settlement — was necessitated by the founding problem or represents a separable political choice layered onto the coordination function. This is the tangled_rope signature: a real coordination problem (Jewish physical safety) coexisting with an asymmetric extraction (Palestinian displacement and continued refugee-status denial) sustained by active enforcement (land law, immigration law, military and legal apparatus denying return).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigeneity_versus_colonial_settlement_framing,
    'Does the historical and religious connection of the Jewish diaspora to the land of Israel constitute indigeneity sufficient to ground a right of return that supersedes the continuous residence claims of the Arab population present in 1917-1948, or does 19th/20th century Jewish immigration to Palestine structurally resemble other settler-colonial movements regardless of the immigrants'' own historical connection and persecution?',
    'This is not resolvable by additional historical data alone — it is a genealogically named contest between two entire readings of the kernel (this constraint versus settler_colonial_reading). Comparative analysis of other diaspora-return and settler movements, together with international-law standards for indigeneity claims by non-continuously-resident populations, would narrow but not close the disagreement.',
    'If the indigeneity claim is accepted as sufficient, this reading''s legitimation holds and the displacement is read as incidental cost of a justified return. If rejected, the same historical events are better modeled by the settler_colonial_reading sibling constraint, which would carry a substantially higher extractiveness and a snare-leaning rather than tangled-rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigeneity_versus_colonial_settlement_framing, conceptual, 'Whether historical/religious connection plus persecution constitutes indigeneity sufficient to ground displacement, versus the settler-colonial counter-framing.').

omega_variable(
    necessity_of_displacement_to_founding_problem,
    'Was the displacement of the Palestinian Arab population a necessary consequence of solving Jewish statelessness and persecution, or was it a separable political and military choice (e.g., rejection of binational or non-displacing partition alternatives) layered onto a genuine coordination need?',
    'Archival work on 1930s-1940s binational proposals (e.g., Brit Shalom, the 1937 Peel Commission minority proposals), Israeli ''new historian'' archival research on 1948 population transfer policy and intent, and counterfactual institutional analysis of whether Jewish safety could have been achieved via non-displacing settlement.',
    'If displacement was structurally necessary to the founding problem, the tangled_rope classification''s coordination component is stronger relative to its extraction component. If displacement was a separable choice, the extraction is less excusable within this reading''s own logic and the case for treating this as closer to a snare (extraction wearing coordination as cover) strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_of_displacement_to_founding_problem, empirical, 'Whether Palestinian displacement was necessitated by Jewish safety needs or was a separable political choice.').

omega_variable(
    symmetric_claim_delegitimization,
    'Is Arab and Palestinian opposition to Jewish statehood correctly characterized as denial of Jewish national rights (this reading''s framing), or as an ordinary assertion of a competing, structurally symmetric national claim to the same territory that deserves equal standing rather than delegitimization?',
    'Comparative political theory on competing national claims to shared territory (cf. other partition and dual-national-claim cases) and closer reading of Palestinian national historiography as an independent tradition rather than solely as a response to Zionism.',
    'If the opposition is a symmetric national claim, the founding_problem_corroboration weakens further and the absent_voices gap becomes a structural flaw in the reading rather than a minor omission. If genuinely a denial-of-rights framing holds, the reading''s treatment of Arab opposition as illegitimate is internally consistent rather than a delegitimizing move.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(symmetric_claim_delegitimization, conceptual, 'Whether Arab/Palestinian opposition constitutes a symmetric competing national claim or a denial of Jewish rights, as this reading holds.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__national_liberation_reading, 1897, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1897, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1897, 0.1).
narrative_ontology:measurement(zion_tr_t1917, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1917, 0.14).
narrative_ontology:measurement(zion_tr_t1936, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1936, 0.18).
narrative_ontology:measurement(zion_tr_t1948, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1967, 0.24).
narrative_ontology:measurement(zion_tr_t1993, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1993, 0.26).
narrative_ontology:measurement(zion_tr_t2024, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(zion_be_t1897, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1897, 0.2).
narrative_ontology:measurement(zion_be_t1917, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1917, 0.32).
narrative_ontology:measurement(zion_be_t1936, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1936, 0.42).
narrative_ontology:measurement(zion_be_t1948, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1948, 0.68).
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1967, 0.62).
narrative_ontology:measurement(zion_be_t1993, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1993, 0.55).
narrative_ontology:measurement(zion_be_t2024, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1897, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1897, 0.15).
narrative_ontology:measurement(zion_su_t1917, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1917, 0.28).
narrative_ontology:measurement(zion_su_t1936, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1936, 0.45).
narrative_ontology:measurement(zion_su_t1948, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1948, 0.72).
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1967, 0.65).
narrative_ontology:measurement(zion_su_t1993, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1993, 0.58).
narrative_ontology:measurement(zion_su_t2024, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__national_liberation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(zionist_legitimacy_basis__national_liberation_reading, 0.1).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, settler_colonial_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, religious_restoration_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, law_of_return_israeli_citizenship_policy).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, palestinian_right_of_return_claim).

% DUAL FORMULATION NOTE:
% This constraint, settler_colonial_reading, and religious_restoration_reading are three readings of the single contested zionist_legitimacy_basis kernel. Each reading is generated as a separate, ε-invariant constraint with its own beneficiary/victim structure and classification rather than as a single hedged story: this reading is authored as tangled_rope (genuine coordination for a persecuted stateless population, coexisting with asymmetric extraction from the pre-existing Arab population, sustained by active legal and military enforcement). The settler_colonial_reading sibling is expected to carry a higher extractiveness and weaker coordination-function finding from the opposed structural premise that Jewish immigration constituted external colonization rather than indigenous return. The religious_restoration_reading sibling shares much of this reading's beneficiary set but grounds legitimacy in covenantal/messianic claims rather than persecution-and-return, and is expected to diverge sharply after 1967 as it becomes the dominant framing for settlement expansion in occupied territories, which this reading does not itself claim to justify.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
