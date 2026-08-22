% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__settler_colonial_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__settler_colonial_reading
 *   human_readable: Zionist Settlement as Settler-Colonial Displacement Regime
 *   domain: political_philosophy/postcolonial_theory/nationalism_studies
 *
 * SUMMARY:
 *   This story instantiates the settler-colonial reading of the
 *   jewish_sovereignty_palestine kernel: a structural analysis holding that
 *   the pattern of Jewish immigration and state-building in Palestine matches
 *   the comparative settler-colonial form (metropole sponsorship, land
 *   acquisition displacing an indigenous population, legal differentiation,
 *   demographic engineering) regardless of the subjective motive of
 *   individual settlers, including flight from persecution and genocide. This
 *   is one of five sibling readings of a single contested kernel; the
 *   liberal_nationalist_reading, religious_zionist_reading,
 *   cultural_zionist_reading, and post_zionist_reading are separate
 *   constraint stories with their own epsilon values, beneficiary/victim
 *   structures, and classifications. This story does not adjudicate between
 *   them — it authors the settler-colonial reading cleanly, on its own
 *   structural terms, per the epsilon-invariance principle.
 *
 * KEY AGENTS:
 *   - palestinian_indigenous_population: primary structural victim (powerless/trapped) — bears displacement
 *   - palestinian_refugees: continuing victim class (powerless/trapped) — bears non-return
 *   - jewish_settler_population: structural beneficiary of land/sovereignty transfer (organized/constrained), though individually often fleeing persecution
 *   - british_mandate_administration: initial agenda-setter and beneficiary (institutional/arbitrage) — sponsored the framework, exited its costs
 *   - us_strategic_imperial_interests: successor beneficiary (institutional/arbitrage) — sustains the arrangement for strategic value
 *   - settler_colonial_studies_scholars: analytical observer applying comparative pattern-matching
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, 0.82).
domain_priors:suppression_score(jewish_sovereignty_palestine__settler_colonial_reading, 0.78).
domain_priors:theater_ratio(jewish_sovereignty_palestine__settler_colonial_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__settler_colonial_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__settler_colonial_reading, "Zionist Settlement as Settler-Colonial Displacement Regime").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__settler_colonial_reading, "political_philosophy/postcolonial_theory/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__settler_colonial_reading, '701e05a1-de06-41b4-845e-5d82326717e8').
narrative_ontology:cs_kernel_codification('701e05a1-de06-41b4-845e-5d82326717e8', distributed).
narrative_ontology:cs_authority_grounding('701e05a1-de06-41b4-845e-5d82326717e8', distributed).
narrative_ontology:cs_reading_relation('701e05a1-de06-41b4-845e-5d82326717e8', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('701e05a1-de06-41b4-845e-5d82326717e8', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('701e05a1-de06-41b4-845e-5d82326717e8', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('701e05a1-de06-41b4-845e-5d82326717e8', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_axiom('701e05a1-de06-41b4-845e-5d82326717e8', foundational, structural_position_overrides_settler_intent).
narrative_ontology:cs_axiom_status(structural_position_overrides_settler_intent, holdable).
narrative_ontology:cs_axiom_grounding('701e05a1-de06-41b4-845e-5d82326717e8', structural_position_overrides_settler_intent, conventional).
narrative_ontology:cs_axiom('701e05a1-de06-41b4-845e-5d82326717e8', foundational, comparative_settler_colonial_pattern_is_decisive_classifier).
narrative_ontology:cs_axiom_status(comparative_settler_colonial_pattern_is_decisive_classifier, holdable).
narrative_ontology:cs_axiom_grounding('701e05a1-de06-41b4-845e-5d82326717e8', comparative_settler_colonial_pattern_is_decisive_classifier, empirically_contingent).
narrative_ontology:cs_reference_frame('701e05a1-de06-41b4-845e-5d82326717e8', pre_mandate_indigenous_land_tenure).
narrative_ontology:cs_drift_state('701e05a1-de06-41b4-845e-5d82326717e8', post_1993_oslo_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('701e05a1-de06-41b4-845e-5d82326717e8', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, british_mandate_administration).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, jewish_settler_population).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, us_strategic_imperial_interests).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_indigenous_population).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_refugees).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_citizens_under_military_law).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, jewish_settler_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held land, village, and agrarian ties in Palestine prior to and during the Mandate period. Faces progressive land purchase and later expropriation, village destruction, and displacement through war and administrative policy. Has no sovereign vehicle of its own during the formative period and cannot exit the territory without becoming a refugee.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_indigenous_population, payer,
    powerless, generational, trapped, regional).

% Displaced in 1947-49 and subsequent waves, denied return under a right-of-return framework the emergent state does not recognize for them while recognizing an analogous right for world Jewry. Lives in camps or diaspora with UNRWA administration standing in for sovereignty. Exit from refugee status is blocked by both host-state policy and the receiving state's demographic calculus.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Palestinians remaining within the territory who become subject to military administration, land requisition, and differential citizenship or residency status. Formally present but structurally subordinated within the legal architecture that consolidates the settlement project.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_citizens_under_military_law, payer,
    powerless, biographical, constrained, regional).

% Arrives under Zionist organizational auspices (Jewish Agency, land purchase companies, later state institutions), acquiring land, labor markets, and eventually sovereignty structured to secure and expand Jewish demographic and territorial presence. Benefits directly from the displacement regime's outcomes even where individual motive is escape from persecution rather than colonial intent; the reading holds structural position as decisive over stated motive. Some settlers, particularly refugees fleeing genocide, also bear real costs of statelessness prior to arrival — hence the secondary payer role — but the constraint's operation in Palestine positions them as net beneficiaries of the land and sovereignty transfer.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, jewish_settler_population, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__settler_colonial_reading, jewish_settler_population, payer).

% Administers the Mandate, issues and enforces the Balfour Declaration's commitments, controls immigration quotas, land registries, and policing. Uses the Zionist settlement project instrumentally to secure regional strategic position (Suez access, oil routes) while managing Arab opposition through coercive administration. Exits the arrangement in 1948 having shaped its structure without bearing its long-term costs.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, british_mandate_administration, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__settler_colonial_reading, british_mandate_administration, beneficiary).

% Inherits and sustains the regional benefit structure post-1948 through military aid, diplomatic cover at the UN, and basing/alliance value, treating the resulting state as a strategic asset in a wider regional order. Bears none of the direct territorial costs and can adjust the relationship without existential risk to itself.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, us_strategic_imperial_interests, beneficiary,
    institutional, civilizational, arbitrage, global).

% UN bodies, human rights organizations, and international law scholars document dispossession, occupation, and refugee status but lack enforcement power against the settlement project's core sponsors; their findings are contested by state and allied institutions and rarely translate into binding remedy.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, international_human_rights_bodies, excluded,
    institutional, generational, analytical, global).

% Analyze the case comparatively against Algeria, South Africa, Australia, and the Americas, arguing structural pattern-matching (land acquisition, demographic replacement, legal differentiation) is decisive regardless of the settlers' own history of persecution or the absence of a metropole after 1948.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, settler_colonial_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, the arrangement coordinates the transfer of land, sovereignty, and demographic control from an indigenous population to an incoming settler population, backed initially by an imperial administrator and later by the settler state's own institutions and allied great-power patronage — a genuine coordination problem for the settling population (organizing immigration, land purchase, defense, statehood) solved at the structural expense of the indigenous population.
% TRANSFER_FUNCTION: Moves land title, water rights, residency and citizenship status, and ultimately sovereign control from the Palestinian indigenous population to the Jewish settler population and the state apparatus built around it, with strategic and economic benefit flowing onward to the successive imperial patrons (Britain, then the United States).
% ABSENT_VOICES: Palestinian refugees and their descendants are the primary excluded voice — displaced from the territory and from the negotiating table in most formative decisions (the 1917 Balfour Declaration, the 1947 UN partition vote conducted without Palestinian Arab consent as a co-equal party, the 1948-49 war's territorial outcomes). International law bodies document but cannot bind the arrangement's sponsors.
% DISAPPEARANCE_RATIONALE: If the settlement and displacement regime were reversed or undone, land and sovereignty arrangements across the entire territory would need to be renegotiated from a substantially different demographic and legal baseline; refugee return, citizenship structures, and regional strategic alignments would all restructure. Under this reading the current state's existence is constitutively bound up with the displacement it produced, not incidental to it.
% FOUNDING_PROBLEM: From the settler-colonial reading's perspective, the arrangement was built to resolve the settling population's need for territorial refuge and sovereignty by establishing demographic and legal control over land already inhabited, using the organizational and coercive resources of a sponsoring imperial power to overcome indigenous resistance.
% FOUNDING_PROBLEM_CORROBORATION: Palestinian historians, UN human rights rapporteurs, and comparative settler-colonial studies scholars (writing from outside both the Zionist movement and Israeli state institutions) attest that the founding displacement dynamic continues through ongoing settlement expansion, refugee non-return, and differential legal status — this is not self-asserted by a beneficiary party but is contested vigorously by liberal-nationalist and religious-Zionist readings, which deny the settler-colonial framing describes the arrangement's founding problem at all.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__settler_colonial_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__settler_colonial_reading, 0.82, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.82) reflecting the reading's zero-sum territorial logic: land, water, and sovereign control are treated as a fixed quantity transferred from the indigenous population to the settling population, not as a positive-sum arrangement. Suppression is high (0.78) because the reading holds that maintaining the demographic and territorial outcome requires ongoing active enforcement (military administration, land law, permit regimes, refugee non-return policy) rather than voluntary participant consent. Theater ratio is moderate-low (0.25): most of the machinery (land registries, military courts, settlement administration) performs a real function within this reading's own terms — sustaining the displacement outcome — rather than being merely performative, though some diplomatic and legal process is read as theater covering settled facts on the ground. Accessibility collapse is moderate (0.6): from within this reading, alternative arrangements (binational state, confederation, restitution) remain conceivable and are actively argued for, so collapse is not total. Resistance is very high (0.88), consistent with the reading's own account that Palestinian resistance, international legal challenge, and academic contestation are continuous and substantial — this is not a settled mountain but a heavily contested structure.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute markedly different seat classifications here: from the palestinian_indigenous_population and palestinian_refugees seats (powerless, trapped, regional) the structure should compute as extractive and enforcement-dependent; from the british_mandate_administration and us_strategic_imperial_interests seats (institutional, arbitrage, global) the same structure should compute as low-cost, high-exit beneficiary position. The jewish_settler_population seat is the most structurally interesting divergence point: this reading insists that even where individual settlers' own antecedent history is one of severe victimization (statelessness, genocide), their position WITHIN this specific constraint is structurally beneficiary — the dual role captures that tension without resolving it by fiat.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian populations are coded as targets (d near 1.0): they bear the displacement, have no meaningful exit from either the territory (trapped) or refugee status (trapped), and possess no organized sovereign vehicle in the formative period. The jewish_settler_population is coded structurally as beneficiary despite frequently being individually a payer in the antecedent European persecution context — the reading's core methodological move is to hold structural position, not stated motive or biography, as decisive for directionality. This is why the population carries both beneficiary (primary) and payer (secondary) roles: individual settlers may have fled genocide, yet the reading holds that within the Palestine arrangement specifically, their position is one of net structural gain (land, sovereignty, legal status) relative to the indigenous population's net structural loss. The british_mandate_administration and us_strategic_imperial_interests are coded as high-exit beneficiaries (arbitrage) — they shape or sustain the arrangement's terms while bearing none of its territorial costs and can disengage without existential consequence to themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored as 'live' rather than 'dead' because, from within this reading, the structural mechanism (settlement expansion, differential legal status, refugee non-return) is understood to be continuing rather than a a legacy arrangement that has outlived its function. This blocks a mandatrophy misreading in the other direction: a reading that called the founding problem 'dead' while the disappearance_verdict remained 'world_rearranges' would flag as a capture/zombie mismatch under the R5 consumption rule — this story avoids that mismatch by keeping status and verdict coherent with each other under its own terms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_vs_structure_decisiveness,
    'Is structural position (land/sovereignty transfer outcome) or subjective intent/circumstance (flight from persecution, absence of a metropole after 1948) the correct basis for classifying the Jewish settler population''s directionality?',
    'No empirical resolution exists; this is a conceptual/framing dispute between comparative settler-colonial methodology (which brackets intent) and liberal or humanitarian frameworks (which weight intent and circumstance heavily). Adjudication depends on which normative framework for assessing historical injustice is adopted.',
    'If intent/circumstance is decisive, much of the beneficiary coding of jewish_settler_population would need to shift toward a more symmetric or even payer-weighted position, substantially lowering the reading''s authored extractiveness; if structure is decisive (as this reading holds), the current coding stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intent_vs_structure_decisiveness, conceptual, 'Whether settler status is structurally or intentionally determined — the central methodological fork between this reading and its liberal-nationalist sibling.').

omega_variable(
    post_1948_metropole_absence,
    'Does the settler-colonial framework require a continuing external metropole to remain applicable after 1948, when the sponsoring British Mandate ended and the settling population became a sovereign state in its own right?',
    'Comparative case analysis against other settler-colonial states (US, Australia, South Africa) that also lost or transformed their metropole relationship yet are still classified as settler-colonial in the comparative literature; alternatively, a framework that treats metropole-sponsorship as necessary only at the founding moment, not continuously.',
    'If a continuing metropole is required and the US relationship post-1948 does not qualify as one, the reading''s post-1948 beneficiary chain (us_strategic_imperial_interests) becomes weaker and the classification could shift toward one with a more internally-generated extraction structure rather than an externally-sponsored one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_1948_metropole_absence, conceptual, 'Whether the settler-colonial pattern requires an ongoing external sponsor or is self-sustaining once sovereignty is achieved.').

omega_variable(
    kernel_framing_underdetermination,
    'Given the kernel context (jewish_sovereignty_palestine with five declared readings), is the settler-colonial framing and the liberal-nationalist framing genuinely addressing the same kernel, or do they diverge so completely in their premises (structural pattern-matching vs. rights-based self-determination) that they are better modeled as addressing different objects entirely?',
    'This is inherent to the committer-frame methodology itself: the schema treats them as sibling readings of one kernel by design (Rule 1-4), routing the disagreement to omega variables and reading_relations rather than resolving it. No empirical test resolves which framing is ''correct'' — the frameworks differ in what counts as evidence.',
    'Determines whether the reading_relations edges to sibling readings should be coded as coexists_with (both frameworks remain live, unresolved) or forecloses (one framework''s premises are logically incompatible with the other''s) — this story codes the relationship to liberal_nationalist_reading as coexists_with rather than forecloses because both remain actively held positions by different scholarly and political communities without one being logically derivable as false from the other''s axioms alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the settler-colonial and liberal-nationalist readings share a kernel or are better modeled as addressing structurally different questions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__settler_colonial_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1917, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1917, 0.1).
narrative_ontology:measurement(jewi_tr_t1936, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1936, 0.12).
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1967, 0.18).
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1993, 0.35).
narrative_ontology:measurement(jewi_tr_t2005, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1917, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1917, 0.45).
narrative_ontology:measurement(jewi_be_t1936, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1936, 0.55).
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1948, 0.82).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1967, 0.85).
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1993, 0.78).
narrative_ontology:measurement(jewi_be_t2005, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 2005, 0.8).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1917, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1917, 0.4).
narrative_ontology:measurement(jewi_su_t1936, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1936, 0.55).
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1948, 0.85).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1967, 0.9).
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1993, 0.7).
narrative_ontology:measurement(jewi_su_t2005, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 2005, 0.82).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__settler_colonial_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% This story is one of five constraint stories decomposing the natural-language label 'Zionism/Jewish sovereignty in Palestine' per the epsilon-invariance principle. Each sibling reading (liberal_nationalist, religious_zionist, cultural_zionist, post_zionist) authors its own epsilon, beneficiary/victim structure, and classification from its own premises. This settler_colonial_reading authors the highest epsilon (0.82) among the family, reflecting its zero-sum structural methodology; the liberal_nationalist_reading is expected to author substantially lower epsilon since it treats the arrangement as a legitimate exercise of a collective right rather than a displacement mechanism. The post_zionist_reading is coded as influenced-by (not coexists_with) because the settler-colonial critique's persistence as an intellectual and political force is read as one of the structural pressures the post-Zionist reading responds to in re-examining the founding narrative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
