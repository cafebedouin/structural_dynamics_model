% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__two_state_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__two_state_coexistence_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__two_state_coexistence_reading
 *   human_readable: Two-State Coexistence Reading: Mutual Recognition on 1967 Lines
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This story instantiates the two_state_coexistence_reading of the
 *   territorial_legitimacy_dual kernel: mutual recognition of both peoples'
 *   1948-era legitimacy, the 1967 lines as the basis of partition, refugee
 *   return limited to the Palestinian state, and security cooperation
 *   replacing zero-sum competition. The standing arrangement under contest is
 *   the Oslo-lineage framework as it actually operates — recognition letters,
 *   the Palestinian Authority, joint security machinery, donor structures,
 *   and a final-status promise repeatedly deferred — assessed by this
 *   reading's own lights, which price settler evacuation and refugee
 *   renunciation as the compromise's intended costs. The claim/metric gap is
 *   deliberate: the framework is CLAIMED as tangled_rope (genuine
 *   coordination carrying asymmetric extraction) while the metrics describe
 *   the operated arrangement's actual burdens, including the drift of the
 *   interim regime into permanence. Sibling readings are separate
 *   constraints, not hedges inside this one: zionist_refuge_reading and
 *   palestinian_autochthony_reading instantiate different victim sets and
 *   different epsilon values, and are linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - - israeli_civic_mainstream: Primary beneficiary (organized/constrained) — collects recognition, defined borders, and security cooperation
 *   - - palestinian_governing_authority_elite: Dual-positioned beneficiary-administrator (institutional/constrained) — collects mandate, aid, and standing; administers the security machinery
 *   - - west_bank_settler_communities: Primary target (organized/trapped) — bears the framework's most concentrated physical cost
 *   - - palestinian_refugee_diaspora: Primary target (powerless/identity_locked) — bears renunciation of individual return
 *   - - east_jerusalem_residents: Secondary target-beneficiary (moderate/constrained) — faces jurisdictional change in a contested city
 *   - - mediating_international_powers: Agenda-setter (institutional/arbitrage) — brokers, funds, and guarantees; lowest-cost exit
 *   - - hamas_and_rejectionist_factions: Excluded spoiler (organized/identity_locked) — outside the architecture the framework built
 *   - - regional_normalization_states: Secondary beneficiary (institutional/mobile) — collects stability without bearing partition's costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, 0.66).
domain_priors:suppression_score(territorial_legitimacy_dual__two_state_coexistence_reading, 0.72).
domain_priors:theater_ratio(territorial_legitimacy_dual__two_state_coexistence_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__two_state_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__two_state_coexistence_reading, "Two-State Coexistence Reading: Mutual Recognition on 1967 Lines").
narrative_ontology:topic_domain(territorial_legitimacy_dual__two_state_coexistence_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__two_state_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__two_state_coexistence_reading, 'b60ebbea-eaba-4b67-a05a-5831398c5d65').
narrative_ontology:cs_kernel_codification('b60ebbea-eaba-4b67-a05a-5831398c5d65', distributed).
narrative_ontology:cs_authority_grounding('b60ebbea-eaba-4b67-a05a-5831398c5d65', lineage).
narrative_ontology:cs_interpretation_layer_present('b60ebbea-eaba-4b67-a05a-5831398c5d65').
narrative_ontology:cs_reading_relation('b60ebbea-eaba-4b67-a05a-5831398c5d65', territorial_legitimacy_dual__zionist_refuge_reading, influences).
narrative_ontology:cs_reading_relation('b60ebbea-eaba-4b67-a05a-5831398c5d65', territorial_legitimacy_dual__palestinian_autochthony_reading, forecloses).
narrative_ontology:cs_axiom('b60ebbea-eaba-4b67-a05a-5831398c5d65', foundational, dual_national_legitimacy_recognized).
narrative_ontology:cs_axiom_status(dual_national_legitimacy_recognized, holdable).
narrative_ontology:cs_axiom_grounding('b60ebbea-eaba-4b67-a05a-5831398c5d65', dual_national_legitimacy_recognized, conventional).
narrative_ontology:cs_axiom('b60ebbea-eaba-4b67-a05a-5831398c5d65', foundational, return_subordinate_to_partition).
narrative_ontology:cs_axiom_status(return_subordinate_to_partition, holdable).
narrative_ontology:cs_axiom_grounding('b60ebbea-eaba-4b67-a05a-5831398c5d65', return_subordinate_to_partition, instrumental).
narrative_ontology:cs_axiom('b60ebbea-eaba-4b67-a05a-5831398c5d65', secondary, security_cooperation_replaces_zero_sum).
narrative_ontology:cs_axiom_status(security_cooperation_replaces_zero_sum, holdable).
narrative_ontology:cs_axiom_grounding('b60ebbea-eaba-4b67-a05a-5831398c5d65', security_cooperation_replaces_zero_sum, instrumental).
narrative_ontology:cs_reference_frame('b60ebbea-eaba-4b67-a05a-5831398c5d65', mutual_recognition_partition_baseline).
narrative_ontology:cs_drift_state('b60ebbea-eaba-4b67-a05a-5831398c5d65', contemporary_post_2023, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('b60ebbea-eaba-4b67-a05a-5831398c5d65', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_civic_mainstream).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_governing_authority_elite).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, mediating_international_powers).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, regional_normalization_states).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, west_bank_settler_communities).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugee_diaspora).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, east_jerusalem_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, east_jerusalem_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The majority of Israeli society that trades maximal territorial claims for recognized borders, regional normalization, and a reduced security burden. They receive the framework's recognition dividend and the security-cooperation services delivered under it. Leaving the conflict itself is not available to them short of emigration, which only a minority exercises.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_civic_mainstream, beneficiary,
    organized, generational, constrained, national).

% Holds the governing mandate, donor funding, and international standing that the framework confers on a Palestinian state-in-preparation. Administers the security-coordination machinery on the framework's behalf, including containment of armed opposition. Their constituents bear the open-ended interim burdens while the elite's position depends on the framework's continuation; abandoning it would cost them the mandate and the funding that sustain their institutions.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_governing_authority_elite, beneficiary,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_governing_authority_elite, agenda_setter).

% Live beyond the 1967 lines the framework designates as the basis of the Palestinian state's territory. Partition requires their evacuation or transfer out of the areas designated for Palestinian sovereignty. Their homes, communities, and ideological investment are fixed in place; relocation is the framework's most concentrated physical cost, borne by them and compensated, if at all, by state programs they do not control.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, west_bank_settler_communities, payer,
    organized, biographical, trapped, regional).

% Hold inherited refugee status across Jordan, Lebanon, Syria, the Gulf, the Americas, and Europe, with claims to homes and property lost in 1948. The framework extinguishes individual return into Israel, offering citizenship in the new Palestinian state plus compensation instead. The claim being negotiated away is constitutive of family identity across generations, and the leadership negotiating it does not bear their specific loss.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugee_diaspora, payer,
    powerless, generational, identity_locked, continental).

% Live in the city both movements claim as capital. Framework scenarios divide sovereignty or share it, changing their residency status, municipal services, and citizenship options. They stand to gain guaranteed access and possible Palestinian citizenship while giving up the established, if precarious, rights they have adapted to under the existing arrangement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, east_jerusalem_residents, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, east_jerusalem_residents, beneficiary).

% The United States, Quartet members, and major donor states convene negotiations, condition aid, staff monitoring missions, and guarantee the framework's terms. They collect diplomatic order and regional stability from its operation and can disengage or redirect attention at far lower cost than the regional parties, who cannot walk away from the territory itself.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, mediating_international_powers, agenda_setter,
    institutional, generational, arbitrage, global).

% Armed factions and their constituencies deny the other people's legitimacy and the partition line outright. They sit outside the negotiating architecture the framework built — excluded by the channel's design on one side, by their own refusal on the other. The framework's security machinery exists in substantial part to contain them, and their continued mobilization is the standing proof that the framework's premise is not universally shared.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, hamas_and_rejectionist_factions, excluded,
    organized, generational, identity_locked, regional).

% Arab states that endorsed the 2002 initiative trade recognition and normal relations for the framework's terms. They collect stabilization, trade, and strategic-alignment benefits from its operation without bearing any of partition's direct costs, and can recalibrate their commitment as their interests shift.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, regional_normalization_states, beneficiary,
    institutional, generational, mobile, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_governing_authority_elite).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__two_state_coexistence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts a mutually exclusive legitimacy contest over one territory into two recognized sovereignties on agreed lines: mutual recognition replaces non-recognition, a defined border replaces open-ended territorial contest, joint security mechanisms replace competitive deterrence, and a single negotiating channel replaces recurring bilateral violence.
% TRANSFER_FUNCTION: Moves land and administrative control in the West Bank and Jerusalem from Israeli to Palestinian sovereignty; moves recognized borders and security assurances to Israel through demilitarization and cooperation clauses; moves compensation and resettlement resources to refugee households; moves donor funds and diplomatic standing to Palestinian governing institutions. The concentrated costs land on settlers (homes and communities), refugees (individual return claims), and Jerusalem border-zone residents (jurisdictional change).
% ABSENT_VOICES: Armed rejectionists on both sides are outside the architecture — Hamas and allied factions by the Oslo channel's design, the settler vanguard by its own refusal. The refugee diaspora is represented only indirectly, by leaderships that signed interim terms without a mandate for the final renunciation of return. Future generations will inherit boundaries drawn without their voice. Each would object from outside the room the framework built.
% DISAPPEARANCE_RATIONALE: If the framework vanished overnight, the institutions built on it — the Palestinian Authority, joint security coordination, donor structures, the recognition exchange — lose their legal substrate. The legitimacy contest reverts to an open bilateral claim over the whole territory, regional normalization tracks unravel, and both societies rearrange around unilateralism and deterrence rather than negotiated division.
% FOUNDING_PROBLEM: After 1948, two peoples each held mass presence and mutually exclusive legitimacy claims to the same territory, with roughly seven hundred thousand Palestinians displaced and no mutually accepted border or sovereignty formula. The framework was built to convert mutual non-recognition into mutual recognition on partitioned lines.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: UNSC Resolutions 242 and 338, framed by non-party great powers, treat both claims as live objects of settlement; the 2002 Arab Peace Initiative, offered by states bearing none of partition's direct costs, attests both the problem's persistence and the framework's terms; ICJ advisory-process records document both peoples' claims; and the continued armed and electoral mobilization of rejectionist constituencies on both sides is behavioral evidence that neither legitimacy claim has dissolved.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__two_state_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__two_state_coexistence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__two_state_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__two_state_coexistence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__two_state_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.66 because the framework's costs are concentrated and identity-laden — settler displacement, refugee claim-renunciation, Jerusalem jurisdictional change — while its benefits are diffuse majoritarian goods, and the interim regime whose relief was promised became indefinite. Suppression is 0.72 because persistence requires active coercive maintenance: freezing settlement expansion against a mobilized settler movement, containing armed rejectionist factions, and running security coordination that has included detention of opposition figures. Theater_ratio is 0.58: the early interval was substantially functional (recognition exchanged, institutions built, cooperation real), but after the 2000 collapse much of the framework's activity became performative — summits, roadmaps, and donor pledges that maintained the appearance of process while facts on the ground diverged. Accessibility_collapse is 0.40 because alternatives remain visibly live: binational one-state, confederal, and annexationist arrangements are all actively advocated, so understanding the framework does not close the option space. Resistance is 0.70: rejectionist constituencies on both sides contest the framework continuously, up to armed insurrection and legislative obstruction. The measurement series run on one shared eight-point grid (T=0..32, mapping 1993..2025) so every metric is authored at every examined time point; the monotonic rise in base_extractiveness models extraction accumulating as the interim hardened into permanence, and the step-change at T=7 marks the second-intifada rupture, after which enforcement intensity and performative maintenance both ratcheted up and stayed up.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/administrator seats compute differently from the same structure. From the settler and diaspora seats the framework operates as confiscation — it prices their identity claims out of the settlement and hands the bill to those with the least leverage. From the two civic-mainstream seats it operates as salvation — the deal that ends the existential contest. From the governing-elite seat it is simultaneously mandate and trap: the framework supplies their standing and funding while requiring them to police their own constituency's objections to it. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. The settler communities (trapped exit amplifies) and the refugee diaspora (identity_locked, powerless) sit nearest the full-target end — the framework extracts from them specifically and they cannot arbitrage away. East Jerusalem residents sit high but not maximal, since the framework also delivers them rights they currently lack. The two civic mainstreams sit near the beneficiary end: constrained exit, but net gainers. The mediating powers sit nearest the beneficiary pole of any seat — arbitrage-grade exit plus collected order. The governing-elite seat derives low-to-moderate d from its beneficiary role, but the derivation understates one nuance recorded here rather than overridden: the elite also administers extraction against its own constituency and captures the framework's operational rents, pulling its effective position toward the middle. No directionality_overrides entries are authored because the role-plus-exit derivation captures every seat's relationship adequately; the elite-capture correction is a commentary-grade observation pending corpus evidence.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents mislabeling in both directions. Reading the framework as pure coordination (rope) erases the concentrated victims — the diaspora whose return is extinguished and the settlers whose homes are priced out — and launders extraction as peace. Reading it as pure extraction (snare) erases the real coordination achievement — mutual recognition happened, signatory violence ended, security cooperation functioned for years — and would mispredict its persistence, which rests on genuine delivered benefits, not only coercion. The founding problem remains live (corroborated outside the beneficiary set), so this is not yet a resolved mandate; but the rising theater_ratio series is the Goodhart signal to watch: if process-maintenance fully substitutes for implementation, the framework drifts toward piton — a dead arrangement kept alive by diplomatic performance — and the post_2023_implementability omega tracks exactly that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is the two_state_coexistence_reading of the territorial_legitimacy_dual kernel; what structurally different constraints do the zionist_refuge_reading and palestinian_autochthony_reading instantiate, and where exactly does the disagreement bite?',
    'Comparative classification of the sibling stories: author each sibling as its own epsilon-invariant constraint and diff victim sets, epsilon, and enforcement structure against this file.',
    'The autochthony reading centers return-denial as the extraction surface (diaspora as primary target, higher epsilon, different enforcement object); the zionist-refuge reading treats the 1967 limitation itself as the illegitimate imposition (settler and annexationist constituencies as targets). Classification of this reading does not transfer to either sibling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: one of three readings of the dual-legitimacy kernel; sibling readings change the victim set and the location of extraction.').

omega_variable(
    boundary_basis_variant,
    'Does ''1967 boundaries as basis'' mean the strict armistice lines, or lines with equivalent territorial swaps — and do the variants produce the same constraint?',
    'Decomposition test per epsilon-invariance: if modeling strict lines versus swap-variants changes who bears displacement (settlement-bloc residents inside versus outside the swapped area) and therefore epsilon, author separate stories for each variant and link them.',
    'Strict lines maximize the settler victim set; swap variants shrink it while adding swap-area Palestinians to it. Treating the variants as one constraint would average two different victim structures into one unstable epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_basis_variant, conceptual, 'Ambiguity in the boundary basis observable; candidate decomposition if victim sets diverge materially.').

omega_variable(
    return_limit_scope,
    'Does ''right of return limited to the Palestinian state'' admit a token humanitarian quota into Israel, and does the answer change the extraction magnitude borne by the diaspora?',
    'Negotiated-formula analysis: compare authored frameworks (Geneva Initiative-style quotas versus absolute limitation) on the diaspora seat''s residual claim burden.',
    'A token quota partially restores individual return and lowers the diaspora seat''s effective extraction; absolute limitation maximizes it. The reading''s epsilon is indexed to the absolute-limitation formulation authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(return_limit_scope, preference, 'Scope ambiguity in the return-limitation axiom; preference-dependent victim burden.').

omega_variable(
    referent_designed_vs_operated,
    'Is the epsilon referent the framework as designed (paper terms: finite interim, full withdrawal, compensation) or the framework as operated (Oslo-lineage arrangement with indefinite interim and deferred final status)?',
    'Author a sibling story for the framework-as-designed with its own stakeholder surface and epsilon; compare. The designed version prices the same concentrated costs but promises bounded duration, lowering measured extraction.',
    'This file authors epsilon 0.66 for the operated arrangement, which is the standing arrangement under contest. A designed-framework story would author materially lower epsilon; conflating the two would corrupt both.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(referent_designed_vs_operated, empirical, 'Referent disambiguation between the negotiated terms and the operated regime; decomposition candidate.').

omega_variable(
    post_2023_implementability,
    'Does the framework retain implementability at all after the 2023-2025 war, or has its maintenance become purely theatrical?',
    'Track the theater_ratio series past T=32: sustained values above roughly 0.7 with no implementation events would indicate performance-only maintenance; any renewed final-status track would falsify the piton-drift hypothesis.',
    'If maintenance is purely theatrical, the constraint reclassifies toward piton — an inertial shell administered by parties who could change it but bear less cost than fixing would require — and the founding-problem mismatch flag fires.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_2023_implementability, empirical, 'Lifecycle question: live tangled_rope versus theatrical remnant; monitored through the theater_ratio trajectory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__two_state_coexistence_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(terr_tr_t5, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(terr_tr_t7, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 7, 0.36).
narrative_ontology:measurement(terr_tr_t12, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 12, 0.44).
narrative_ontology:measurement(terr_tr_t17, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 17, 0.49).
narrative_ontology:measurement(terr_tr_t22, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 22, 0.52).
narrative_ontology:measurement(terr_tr_t27, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 27, 0.55).
narrative_ontology:measurement(terr_tr_t32, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 32, 0.58).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(terr_be_t5, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(terr_be_t7, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 7, 0.53).
narrative_ontology:measurement(terr_be_t12, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 12, 0.59).
narrative_ontology:measurement(terr_be_t17, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 17, 0.62).
narrative_ontology:measurement(terr_be_t22, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 22, 0.64).
narrative_ontology:measurement(terr_be_t27, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 27, 0.65).
narrative_ontology:measurement(terr_be_t32, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 32, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(terr_su_t5, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(terr_su_t7, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 7, 0.61).
narrative_ontology:measurement(terr_su_t12, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(terr_su_t17, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 17, 0.68).
narrative_ontology:measurement(terr_su_t22, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 22, 0.69).
narrative_ontology:measurement(terr_su_t27, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 27, 0.71).
narrative_ontology:measurement(terr_su_t32, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 32, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__two_state_coexistence_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual__zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual__palestinian_autochthony_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'two-state solution / dual legitimacy' decomposes into three structurally distinct readings of the territorial_legitimacy_dual kernel. This story (two_state_coexistence_reading) carries the mutual-recognition partition constraint with epsilon 0.66 over the operated Oslo-lineage arrangement. The zionist_refuge_reading sibling instantiates a constraint whose legitimacy premise is Israel's refuge-historical title, with the 1967 limitation appearing as external imposition; the palestinian_autochthony_reading sibling instantiates a constraint whose legitimacy premise is habitation and return, with partition itself appearing as the extraction. The upstream/downstream structure differs per edge: this reading influences the zionist sibling (grants legitimacy conditionally, changing its operating environment) and forecloses the autochthony sibling (the return-limit axiom directly contradicts the inalienable-return premise within any single framework). Each member links the others via affects_constraints; epsilon values are authored independently per file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
