% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__indigenous_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__indigenous_continuity_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: territorial_legitimacy__indigenous_continuity_reading
 *   human_readable: Territorial Legitimacy via Indigenous Continuity and Anti-Colonial Self-Determination
 *   domain: political/legal/territorial
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of the territorial
 *   legitimacy kernel: the indigenous continuity reading, which holds that
 *   Palestinian presence and habitation in the territory for centuries
 *   creates inalienable rights to self-determination and territorial
 *   sovereignty over the whole of historic Palestine, and that the 1948
 *   partition and creation of the Israeli state constitute a settler-colonial
 *   dispossession (Nakba in Arabic, 'catastrophe') rather than a legitimate
 *   partition. This reading rejects the partition reading's claim that
 *   international legal recognition of two states resolves the legitimacy
 *   question, and rejects the security necessity reading's claim that Israeli
 *   control serves defensive purposes that override Palestinian rights. Under
 *   this reading, the constraint operates as a structural snare: Palestinian
 *   territorial rights are systematically suppressed through military
 *   occupation, settlement expansion, legal denial of return, and refugee
 *   camp containment, with no exit available except abandonment of land
 *   claims or national identity. The beneficiary is the Israeli state and
 *   Jewish Israeli population; the victims are Palestinians across all three
 *   geographical groups (refugees, occupied territories, diaspora). The claim
 *   and metrics are intentionally independent: the constraint is CLAIMED as a
 *   snare (the indigenous continuity reading's own assessment) and the
 *   metrics confirm high extraction and suppression.
 *
 * KEY AGENTS:
 *   - Palestinian refugees displaced 1948 (powerless; structurally denied return and restitution)
 *   - Palestinian population in West Bank and Gaza (powerless; under military occupation and blockade)
 *   - Palestinian diaspora (moderate power; identity-locked to unsolved claim)
 *   - Israeli state (institutional power; beneficiary and agenda-setter; maintains constraint through military and legal apparatus)
 *   - Jewish Israeli population (organized power; structured beneficiary of Palestinian dispossession)
 *   - International legal authorities (institutional observer; recognize Palestinian rights in principle but lack enforcement machinery independent of state power)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, 0.87).
domain_priors:suppression_score(territorial_legitimacy__indigenous_continuity_reading, 0.91).
domain_priors:theater_ratio(territorial_legitimacy__indigenous_continuity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__indigenous_continuity_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy__indigenous_continuity_reading, "Territorial Legitimacy via Indigenous Continuity and Anti-Colonial Self-Determination").
narrative_ontology:topic_domain(territorial_legitimacy__indigenous_continuity_reading, "political/legal/territorial").

domain_priors:requires_active_enforcement(territorial_legitimacy__indigenous_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__indigenous_continuity_reading, 'dd021f23-f577-4c5f-9c31-43869e04c1ec').
narrative_ontology:cs_kernel_codification('dd021f23-f577-4c5f-9c31-43869e04c1ec', distributed).
narrative_ontology:cs_authority_grounding('dd021f23-f577-4c5f-9c31-43869e04c1ec', distributed).
narrative_ontology:cs_reading_relation('dd021f23-f577-4c5f-9c31-43869e04c1ec', territorial_legitimacy__partition_reading, forecloses).
narrative_ontology:cs_reading_relation('dd021f23-f577-4c5f-9c31-43869e04c1ec', territorial_legitimacy__security_necessity_reading, coexists_with).
narrative_ontology:cs_axiom('dd021f23-f577-4c5f-9c31-43869e04c1ec', foundational, continuous_indigenous_habitation_creates_inalienable_self_determination_rights).
narrative_ontology:cs_axiom_status(continuous_indigenous_habitation_creates_inalienable_self_determination_rights, holdable).
narrative_ontology:cs_axiom_grounding('dd021f23-f577-4c5f-9c31-43869e04c1ec', continuous_indigenous_habitation_creates_inalienable_self_determination_rights, deontological).
narrative_ontology:cs_axiom('dd021f23-f577-4c5f-9c31-43869e04c1ec', foundational, settler_colonial_dispossession_cannot_be_legitimized_by_international_recognition_absent_restitution).
narrative_ontology:cs_axiom_status(settler_colonial_dispossession_cannot_be_legitimized_by_international_recognition_absent_restitution, holdable).
narrative_ontology:cs_axiom_grounding('dd021f23-f577-4c5f-9c31-43869e04c1ec', settler_colonial_dispossession_cannot_be_legitimized_by_international_recognition_absent_restitution, deontological).
narrative_ontology:cs_reference_frame('dd021f23-f577-4c5f-9c31-43869e04c1ec', pre_partition_continuous_palestinian_habitation).
narrative_ontology:cs_drift_state('dd021f23-f577-4c5f-9c31-43869e04c1ec', contemporary_unresolved_dispossession, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('dd021f23-f577-4c5f-9c31-43869e04c1ec', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, palestinian_refugees_displaced_1948).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, palestinian_population_in_west_bank_gaza).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, palestinian_diaspora_outside_levant).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, israeli_state).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, jewish_israeli_population).
narrative_ontology:constraint_vindicates(territorial_legitimacy__indigenous_continuity_reading, indigenous_peoples_right_to_self_determination).
narrative_ontology:constraint_vindicates(territorial_legitimacy__indigenous_continuity_reading, anti_colonial_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(territorial_legitimacy__indigenous_continuity_reading, right_of_return_for_dispossessed_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Forcibly expelled from their homes and land in 1948 during the formation of the Israeli state, branded as 'refugees' by international law and denied return under the claim that return would compromise Israeli Jewish majority. Live in camps and diaspora across Lebanon, Syria, Jordan, and globally. Their legal status is stateless; their land claims are contested by the Israeli state claim that Palestinians cannot hold pre-state property rights. No legal mechanism for restitution or return exists within the partition framework.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_refugees_displaced_1948, payer,
    powerless, generational, trapped, global).

% Live under military occupation (in West Bank) or blockade (Gaza), with restricted movement, settlement, and self-governance. Settlements expand into Palestinian-designated areas; land expropriations are routine. They lack full sovereignty; final status is deferred indefinitely. Under the indigenous continuity reading, they retain rights to all of historic Palestine and the constraint denies them the exercise of those rights. Exit from the territory means permanent loss of land claim.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_population_in_west_bank_gaza, payer,
    powerless, generational, trapped, regional).

% Dispersed globally as economic migrants and political refugees, maintaining Palestinian identity and kinship ties to the homeland. Legally resident in host states but politically constituted by the unfulfilled right of return. The constraint denies them access to ancestral property and collective sovereignty. Exit from Palestinian identity claims requires severing ties to family land and national political struggle.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_diaspora_outside_levant, payer,
    moderate, generational, identity_locked, global).

% Established in 1948 on disputed territory, maintains control over Mandatory Palestine territory through military force and legal sovereignty claims. Interprets the constraint as illegitimate under international law; contests the claim that continuous Palestinian habitation creates any right to territory. Under the indigenous continuity reading, the Israeli state is structurally a settler-colonial entity whose very existence extracts from Palestinian territorial rights and self-determination. The state apparatus enforces settlement expansion, military occupation, and exclusion of Palestinians from claimed lands.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, israeli_state, beneficiary,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__indigenous_continuity_reading, israeli_state, agenda_setter).

% Constitutes the majority population within Israeli territory. Benefits from the land claims and military security apparatus that the constraint produces. Their position as beneficiary is structurally dependent on Palestinian dispossession. Under the indigenous continuity reading, their territorial security depends on suppressing Palestinian return and sovereignty claims. Exit from the territory would mean loss of the state structure that exists to benefit them; exit from support for the state means abandonment of the collective security project.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, jewish_israeli_population, beneficiary,
    organized, generational, constrained, regional).

% UN bodies, International Court of Justice, human rights bodies: interpret the constraint through different readings (partition, security necessity, indigenous continuity). Under the indigenous continuity reading, these authorities acknowledge Palestinian rights to self-determination in principle but decline to enforce the reading's implications. They observe the constraint's operation but lack enforcement machinery independent of state power.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, international_legal_authorities, observer,
    institutional, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__indigenous_continuity_reading, israeli_state).
narrative_ontology:fixing_cost_class(territorial_legitimacy__indigenous_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. The indigenous continuity reading rejects the claim that there is a legitimate coordination problem this constraint solves. The partition reading claims it coordinates Jewish and Palestinian self-determination; the indigenous continuity reading denies that any partition of Palestinian land can coordinate self-determination when one party has indigenous rights to the whole. Under this reading, the constraint frames territorial control and population suppression as a coordination solution when it is actually structural dispossession.
% TRANSFER_FUNCTION: Transfers Palestinian territorial sovereignty, land ownership, and rights of return to the Israeli state and Jewish Israeli population. Moves Palestinian populations into refugee camps, occupied territories, and diaspora status. Converts Palestinian territorial claims into legal statelessness and political exclusion. Extracts the capacity for Palestinian national self-determination and redirects that capacity toward Israeli state security and Jewish Israeli population growth.
% ABSENT_VOICES: The voices most completely absent are those of Palestinian populations within the territory itself — 1948 Palestinians inside what became Israel, Palestinians expelled in 1948 and their descendants, Palestinians living under occupation who are denied any role in defining legitimacy. The constraint exists precisely to exclude their claim to define territorial legitimacy via indigenous continuity. International legal authorities are nominally present but structurally excluded from enforcing this reading's implications. Palestinian political movements outside the reading's own advocacy are excluded by institutional power asymmetries.
% DISAPPEARANCE_RATIONALE: If this constraint (the claim that Palestinian territorial rights flow from indigenous continuity and anti-colonial self-determination) disappeared from political and legal discourse, territorial legitimacy would reorganize entirely: Israeli state claims would lose the counter-claim they must suppress; Palestinian refugees' legal status would shift from stateless to indigenous-rights claimants with enforceability; the occupation would become either explicitly colonial administration or require renegotiation of territorial boundaries and return rights. The entire geopolitical structure of the Levant depends on this constraint remaining suppressed or contested rather than recognized.
% FOUNDING_PROBLEM: Colonial displacement and denial of indigenous self-determination: European and global Zionist movement established a settler state on territory inhabited by Palestinians for centuries, displacing the majority population in 1948 and subsequently through occupation. The founding problem this reading identifies is the structural illegitimacy of dispossession without consent or restitution.
% FOUNDING_PROBLEM_CORROBORATION: Palestinian national movements, post-colonial scholarship (Edward Said, Avi Shlaim, Rashid Khalidi), human rights organizations (Amnesty International, Human Rights Watch findings on displacement and settlement expansion), and UN bodies (General Assembly resolutions on Palestinian self-determination and right of return) attest the founding problem is live. Israeli state narratives and Western legal authorities contest this reading entirely, treating partition and state recognition as having resolved the founding problem. Corroboration exists outside the reading's own advocate base only partially — academic and human rights sectors provide substantial witness; official Western and Israeli authorities provide none.
narrative_ontology:disappearance_verdict(territorial_legitimacy__indigenous_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__indigenous_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__indigenous_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy__indigenous_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__indigenous_continuity_reading, 0.87, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.87) and rising over the interval because the constraint persistently denies Palestinian territorial claims and rights of return with no compensatory mechanism or pathway to restitution. The measurement series tracks the 1948 partition forward through occupation, settlement expansion (1967 borders, subsequent expansion), and contemporary stasis: the core extraction (denial of rights and territory to Palestinians) remains constant while suppression mechanisms intensify (settlement security, wall construction, permit systems, land law). Suppression is highest (0.91) because the constraint's persistence depends entirely on active military and legal suppression of Palestinian claims and movement — there is no passive equilibrium where Palestinians accept dispossession. Theater ratio (0.41, moderate-low) reflects that some enforcement activity genuinely addresses security concerns (Israeli population protection), but a substantial and growing share of enforcement activity is purely extractive (settlement expansion, land expropriation, legal denial of return rights). The measurement grid shares one time axis spanning 1948 (t=0) to 2024 (t=76), with snapshots every 8 years to track institutional hardening and settlement expansion phases.
 *
 * PERSPECTIVAL GAP:
 *   From the Palestinian payer seats, this is clearly a snare: their territorial rights are extracted from them, suppressed through force, with no exit except surrender or diaspora identity loss. From the Israeli state and Jewish Israeli beneficiary seats, the same constraint is framed as security necessity and partition legitimacy (the other two readings) — not as extraction but as self-defense and lawful state formation. The engine computes these divergences from the structural data: powerless victims with trapped exit compute very high effective extraction; powerful beneficiaries with arbitrage options compute lower effective extraction (or subsidy). The authored claim (snare) matches the victim-seat computation; the other readings' claims (rope for partition, mountain for security necessity) would match beneficiary-seat computations. This is the normal state of contested constraint readings: different seats compute different types from the same structural arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   See analysis above — the directionality chain runs from beneficiary/victim declarations through power and exit options to d values. Palestinian refugees are trapped powerless, so d approaches 1.0 (full target). Israeli state is institutional with arbitrage (can reshape its borders if necessary), so d approaches 0.0 (full beneficiary). No overrides are needed; the standard derivation captures the structural reality.
 *
 * MANDATROPHY ANALYSIS:
 *   The indigenous continuity reading explicitly denies that the founding problem (colonial displacement without restitution) has been solved. The constraint's persistence depends on continued suppression, not on achieving the original coordination goal. This is a hallmark mandatrophy signature: the founding problem is live (Palestinian statelessness and displacement are ongoing), but the arrangement persists by pure suppression. The reading rejects the partition reading's claim that UN Resolution 181 and state recognition solved the problem; it rejects the security necessity reading's claim that military control serves defensive purposes. Under this reading, what persists is pure extraction with theatrical coordination framing (security review, state necessity) that masks land seizure and rights denial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigenous_rights_doctrine_applicability,
    'Does the international indigenous rights doctrine (UNDRIP, ILO 169) apply to Palestinians as an indigenous population, or is the indigenous/settler-colonial framing a constructed political reading rather than a legal category?',
    'International legal authorities (ICJ, UN bodies) explicitly adjudicating whether Palestinians are an indigenous people under international law, and whether settler-colonial doctrine applies to the Israeli state.',
    'If indigenous rights doctrine applies, the constraint''s legitimacy is substantially weakened and Palestinian claims to territorial sovereignty become legally grounded. If the doctrine does not apply or is deemed inapplicable by dominant legal authorities, the indigenous continuity reading remains a political rather than legal claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_rights_doctrine_applicability, conceptual, 'Whether indigenous rights law applies to Palestinian territorial claims.').

omega_variable(
    settler_colonial_category_applicability,
    'Is the Israeli state structurally a settler-colonial project (an external population displacing indigenous inhabitants), or is the Zionist claim that Jews constitute an indigenous people to the territory the correct historical and legal framing?',
    'Post-colonial scholarship, archaeological evidence, and legal determinations about indigenous status and historical continuity would resolve this. The crux is whether Jewish presence in antiquity creates indigenous status versus Palestinian continuous habitation in modernity.',
    'If settler-colonial framing is correct, the indigenous continuity reading''s extraction assessment stands. If the Zionist indigenous claim is correct, the constraint operates differently (not dispossession but conflicting indigenous claims, which shifts the type and requires different resolutions).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(settler_colonial_category_applicability, conceptual, 'Whether the historical and legal category of settler-colonialism applies to the Israeli state.').

omega_variable(
    right_of_return_feasibility_and_coexistence,
    'Is the right of return for 1948 Palestinian refugees compatible with the existence and security of an Israeli state, or are these claims mutually foreclosing?',
    'Demographic analysis, political negotiation outcomes, and judicial determinations about whether return and coexistence can be operationalized (limited return, compensation in lieu, regional compensation, population exchanges, binational state).',
    'If return and Israeli statehood are mutually foreclosing, the indigenous continuity reading forecloses the partition reading (they cannot coexist in one framework). If return is operationalizable with Israeli state survival, both readings could coexist (different parties accepting different terms). This determines whether the reading_relations should be ''forecloses'' or ''coexists_with''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(right_of_return_feasibility_and_coexistence, preference, 'Whether Palestinian return rights and Israeli state existence are mutually exclusive or operationally compatible.').

omega_variable(
    international_legal_authority_capacity,
    'What is the role of international legal authorities in enforcing this reading''s implications? Do they recognize the reading''s legitimacy but lack enforcement machinery, or do they actively reject the reading itself?',
    'Analysis of UN General Assembly resolutions, ICJ advisory opinions, and the actual enforcement capacity of international bodies to adjudicate territorial disputes where a powerful state (Israel) contests the reading.',
    'If authorities recognize the reading but lack enforcement (current state), the constraint persists through state power despite legal recognition of Palestinian rights. If authorities actively reject the reading, the constraint is reinforced by international legal authority. This affects the theater_ratio interpretation: is performative activity in international bodies a form of lip service (high theater) or genuine recognition constrained by enforcement gaps (low theater)?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_legal_authority_capacity, empirical, 'Whether international legal authorities recognize or reject indigenous continuity reading legitimacy.').

omega_variable(
    colonial_displacement_temporality_cutoff,
    'Does the indigenous continuity reading apply only to 1948 displacement, or to the entire historical period of Jewish immigration and settlement before 1948? Where is the temporal boundary of indigenous claim?',
    'Legal and political clarification of whether the claim reaches back to the 1880s (First Aliyah), the 1920s (British Mandate), or only to 1948 (partition and Nakba). Post-colonial scholarship and Palestinian national narratives offer different temporal boundaries.',
    'A narrower temporal frame (1948 only) makes the claim more focused and potentially more operationalizable; a wider frame (entire settlement period) strengthens the dispossession narrative but becomes harder to reverse retroactively. The extractiveness measurement could shift based on the temporal scope of claimed rights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_displacement_temporality_cutoff, conceptual, 'The temporal boundary of colonial displacement that the indigenous continuity reading applies to.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__indigenous_continuity_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(terr_tr_t0, observed).
narrative_ontology:measurement(terr_tr_t8, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement_basis(terr_tr_t8, observed).
narrative_ontology:measurement(terr_tr_t16, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement_basis(terr_tr_t16, observed).
narrative_ontology:measurement(terr_tr_t24, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement_basis(terr_tr_t24, observed).
narrative_ontology:measurement(terr_tr_t32, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 32, 0.37).
narrative_ontology:measurement_basis(terr_tr_t32, observed).
narrative_ontology:measurement(terr_tr_t40, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement_basis(terr_tr_t40, observed).
narrative_ontology:measurement(terr_tr_t48, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 48, 0.4).
narrative_ontology:measurement_basis(terr_tr_t48, observed).
narrative_ontology:measurement(terr_tr_t56, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 56, 0.41).
narrative_ontology:measurement_basis(terr_tr_t56, observed).
narrative_ontology:measurement(terr_tr_t64, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 64, 0.41).
narrative_ontology:measurement_basis(terr_tr_t64, observed).
narrative_ontology:measurement(terr_tr_t76, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 76, 0.41).
narrative_ontology:measurement_basis(terr_tr_t76, observed).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 0, 0.79).
narrative_ontology:measurement_basis(terr_be_t0, observed).
narrative_ontology:measurement(terr_be_t8, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 8, 0.81).
narrative_ontology:measurement_basis(terr_be_t8, observed).
narrative_ontology:measurement(terr_be_t16, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 16, 0.83).
narrative_ontology:measurement_basis(terr_be_t16, observed).
narrative_ontology:measurement(terr_be_t24, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 24, 0.84).
narrative_ontology:measurement_basis(terr_be_t24, observed).
narrative_ontology:measurement(terr_be_t32, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 32, 0.85).
narrative_ontology:measurement_basis(terr_be_t32, observed).
narrative_ontology:measurement(terr_be_t40, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 40, 0.86).
narrative_ontology:measurement_basis(terr_be_t40, observed).
narrative_ontology:measurement(terr_be_t48, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 48, 0.86).
narrative_ontology:measurement_basis(terr_be_t48, observed).
narrative_ontology:measurement(terr_be_t56, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 56, 0.87).
narrative_ontology:measurement_basis(terr_be_t56, observed).
narrative_ontology:measurement(terr_be_t64, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 64, 0.87).
narrative_ontology:measurement_basis(terr_be_t64, observed).
narrative_ontology:measurement(terr_be_t76, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 76, 0.87).
narrative_ontology:measurement_basis(terr_be_t76, observed).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 0, 0.83).
narrative_ontology:measurement_basis(terr_su_t0, observed).
narrative_ontology:measurement(terr_su_t8, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 8, 0.85).
narrative_ontology:measurement_basis(terr_su_t8, observed).
narrative_ontology:measurement(terr_su_t16, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 16, 0.87).
narrative_ontology:measurement_basis(terr_su_t16, observed).
narrative_ontology:measurement(terr_su_t24, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 24, 0.88).
narrative_ontology:measurement_basis(terr_su_t24, observed).
narrative_ontology:measurement(terr_su_t32, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 32, 0.89).
narrative_ontology:measurement_basis(terr_su_t32, observed).
narrative_ontology:measurement(terr_su_t40, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 40, 0.9).
narrative_ontology:measurement_basis(terr_su_t40, observed).
narrative_ontology:measurement(terr_su_t48, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 48, 0.9).
narrative_ontology:measurement_basis(terr_su_t48, observed).
narrative_ontology:measurement(terr_su_t56, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 56, 0.91).
narrative_ontology:measurement_basis(terr_su_t56, observed).
narrative_ontology:measurement(terr_su_t64, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 64, 0.91).
narrative_ontology:measurement_basis(terr_su_t64, observed).
narrative_ontology:measurement(terr_su_t76, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 76, 0.91).
narrative_ontology:measurement_basis(terr_su_t76, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__indigenous_continuity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy__indigenous_continuity_reading, 0.15).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy__partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy__security_necessity_reading).

% DUAL FORMULATION NOTE:
% The territorial legitimacy kernel has three structurally distinct readings, each producing a different constraint with different ε, beneficiary/victim structures, and classification. The indigenous_continuity_reading (this story) holds that continuous Palestinian habitation creates inalienable self-determination rights over all of historic Palestine, treating the 1948 Israeli state as illegitimate settler-colonial dispossession. The partition_reading treats UN Resolution 181 and state recognition as legitimate allocation of territory to two peoples, treating partition as solving the legitimacy question. The security_necessity_reading treats Israeli military control as justified by defensive necessity and majority Jewish population self-determination. These are not three perspectives on a single constraint — they are three different constraints with different referents (what legitimacy IS) and different measurements (what extraction THE CONSTRAINT produces). All three stories should link via network.affects_constraints to indicate kernel membership and causal influence relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
