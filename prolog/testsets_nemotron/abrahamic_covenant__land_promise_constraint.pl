% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__land_promise_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__land_promise_constraint, []).

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
 *   constraint_id: abrahamic_covenant__land_promise_constraint
 *   human_readable: Abrahamic Covenant Land Promise as Territorial Legitimacy Mechanism
 *   domain: religious/institutional/political
 *
 * SUMMARY:
 *   The Genesis covenant (Genesis 12, 15, 17) contains a territorial grant of
 *   the Land of Canaan to Abraham and his descendants. Three major reading
 *   traditions diverge on the promise's scope, conditionality, and
 *   fulfillment: (1) the Isaac-line reading restricts the covenant to Isaac's
 *   descendants (Jewish people), (2) the Ishmael-line reading extends it
 *   through Ishmael to Muhammad and the Muslim ummah, (3) the land-promise
 *   reading treats the territorial grant as perpetual, unconditional, and
 *   currently binding — this third reading structures the modern
 *   Israeli-Palestinian conflict by providing divine legitimation for
 *   exclusive Jewish territorial sovereignty. This constraint story models
 *   the land-promise reading as a high-extraction snare: the reading's
 *   persistence depends on active enforcement (military, legal, diplomatic)
 *   that suppresses Palestinian alternatives and extracts territory,
 *   resources, and sovereignty from displaced and occupied populations. The
 *   beneficiaries are the Israeli state, the settler movement, and the
 *   religious Zionist institutions that administer and profit from the
 *   territorial regime. The victims are Palestinian populations across three
 *   categories: the displaced, refugees, and those under occupation.
 *
 * KEY AGENTS:
 *   - israeli_state_actor: Primary agenda-setter and beneficiary (institutional/arbitrage) — administers the territorial regime grounded in the covenant reading
 *   - settler_movement: Primary beneficiary (organized/identity_locked) — receives material benefits; identity fused with the reading
 *   - religious_zionist_institutions: Beneficiary (organized/identity_locked) — provides theological infrastructure; authority depends on the reading
 *   - palestinian_displaced_populations: Primary victim (powerless/trapped) — bears dispossession and denial of return
 *   - palestinian_refugees: Victim (powerless/trapped) — intergenerational bearers of displacement cost
 *   - palestinian_residents_under_occupation: Victim (powerless/constrained) — lives under military law and land confiscation
 *   - international_diplomatic_community: Observer (institutional/analytical) — holds contesting legal framework but lacks enforcement leverage
 *   - palestinian_leadership_factions: Excluded (moderate/constrained) — would contest but structurally filtered out
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, 0.82).
domain_priors:suppression_score(abrahamic_covenant__land_promise_constraint, 0.78).
domain_priors:theater_ratio(abrahamic_covenant__land_promise_constraint, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, extractiveness, 0.82).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__land_promise_constraint, snare).
narrative_ontology:human_readable(abrahamic_covenant__land_promise_constraint, "Abrahamic Covenant Land Promise as Territorial Legitimacy Mechanism").
narrative_ontology:topic_domain(abrahamic_covenant__land_promise_constraint, "religious/institutional/political").

domain_priors:requires_active_enforcement(abrahamic_covenant__land_promise_constraint).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__land_promise_constraint, '98da72d6-fd15-4412-96a9-4d85a7daff2f').
narrative_ontology:cs_kernel_codification('98da72d6-fd15-4412-96a9-4d85a7daff2f', fixed_text).
narrative_ontology:cs_authority_grounding('98da72d6-fd15-4412-96a9-4d85a7daff2f', lineage).
narrative_ontology:cs_interpretation_layer_present('98da72d6-fd15-4412-96a9-4d85a7daff2f').
narrative_ontology:cs_reading_relation('98da72d6-fd15-4412-96a9-4d85a7daff2f', abrahamic_covenant__isaac_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('98da72d6-fd15-4412-96a9-4d85a7daff2f', abrahamic_covenant__ishmael_covenant_reading, coexists_with).
narrative_ontology:cs_axiom('98da72d6-fd15-4412-96a9-4d85a7daff2f', foundational, land_promise_perpetual_unconditional).
narrative_ontology:cs_axiom_status(land_promise_perpetual_unconditional, holdable).
narrative_ontology:cs_axiom_grounding('98da72d6-fd15-4412-96a9-4d85a7daff2f', land_promise_perpetual_unconditional, deontological).
narrative_ontology:cs_axiom('98da72d6-fd15-4412-96a9-4d85a7daff2f', foundational, isaac_line_exclusive_heir).
narrative_ontology:cs_axiom_status(isaac_line_exclusive_heir, holdable).
narrative_ontology:cs_axiom_grounding('98da72d6-fd15-4412-96a9-4d85a7daff2f', isaac_line_exclusive_heir, deontological).
narrative_ontology:cs_reference_frame('98da72d6-fd15-4412-96a9-4d85a7daff2f', biblical_grant_frame).
narrative_ontology:cs_drift_state('98da72d6-fd15-4412-96a9-4d85a7daff2f', contemporary_settler_state, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('98da72d6-fd15-4412-96a9-4d85a7daff2f', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__land_promise_constraint, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, israeli_state_actor).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, settler_movement).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, religious_zionist_institutions).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, palestinian_displaced_populations).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, palestinian_refugees).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, palestinian_residents_under_occupation).
narrative_ontology:constraint_vindicates(abrahamic_covenant__land_promise_constraint, divine_land_grant_to_abrahamic_line).
narrative_ontology:constraint_vindicates(abrahamic_covenant__land_promise_constraint, perpetual_territorial_covenant).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the territorial regime grounded in the covenant reading; controls settlement policy, military enforcement, legal frameworks, and diplomatic framing. Derives territorial legitimacy and international diplomatic cover from the reading. Exit means abandoning the foundational legitimacy claim of the state.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, israeli_state_actor, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__land_promise_constraint, israeli_state_actor, beneficiary).

% Receives material benefits (land, housing, subsidies, legal protection) directly from the territorial regime the reading legitimates. Identity is fused with the covenant reading — exit would dissolve the self-concept and communal belonging that animates the movement.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, settler_movement, beneficiary,
    organized, biographical, identity_locked, local).

% Provide theological infrastructure and interpretive authority that translates the covenant reading into political legitimacy. Collect status, funding, and institutional power from maintaining the reading. Exit means surrendering the interpretive monopoly that constitutes their authority.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, religious_zionist_institutions, beneficiary,
    organized, generational, identity_locked, regional).

% Bear the material costs of the territorial regime: dispossession, denial of return, military occupation, house demolitions, movement restrictions. No effective exit — return is blocked by the regime the reading legitimates; integration elsewhere leaves statelessness.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, palestinian_displaced_populations, payer,
    powerless, biographical, trapped, local).

% Intergenerational bearers of the displacement cost; right of return denied by the same legitimacy structure. Host-state restrictions compound the trap. The covenant reading is the legitimating layer above the exclusion mechanism.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Live under military law, permit regimes, land confiscation, and settlement expansion — all justified by the territorial legitimacy claim. Constrained exit: some mobility within fragmented enclaves; emigration is possible but means permanent exile.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, palestinian_residents_under_occupation, payer,
    powerless, biographical, constrained, local).

% Holds the legal and diplomatic framework (UN resolutions, ICJ opinions, Geneva Conventions) that contests the territorial legitimacy claim. Can impose costs (sanctions, non-recognition) but lacks enforcement leverage against the agenda-setter. Analytical seat: observes the structural dynamics without collecting or paying.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, international_diplomatic_community, observer,
    institutional, generational, analytical, global).

% Would contest the territorial legitimacy claim if structurally admitted to the legitimating framework; instead managed through negotiated surrender frameworks (Oslo) or military containment. Their exclusion is what the covenant reading's enforcement requires.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, palestinian_leadership_factions, excluded,
    moderate, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a trans-historical legitimating narrative that converts a contested territorial claim into a divine mandate, coordinating Jewish collective action (immigration, settlement, defense) around a fixed geographic focus across centuries of diaspora.
% TRANSFER_FUNCTION: Transfers territorial control, resources, water, legal rights, and political sovereignty from Palestinian populations to Israeli state and settler beneficiaries, mediated by the covenant reading's claim that the land was granted exclusively to Abraham's line through Isaac/Jacob.
% ABSENT_VOICES: Palestinian refugees in diaspora (denied right of return), Palestinian citizens of Israel (subject to separate legal regime), Palestinian voices in international forums (structurally filtered through state-centric diplomacy), and Jewish anti-Zionist/non-Zionist traditions (excluded from the 'Jewish consensus' the reading claims to represent).
% DISAPPEARANCE_RATIONALE: If the covenant reading's legitimating force vanished overnight, the legal basis for settlements, the Law of Return's territorial scope, the military occupation's ideological foundation, and the diplomatic immunity the claim provides would collapse — the territorial regime would lose its primary justification and face immediate structural crisis.
% FOUNDING_PROBLEM: How to ground Jewish national return in a legitimacy claim that transcends modern colonialism accusations and connects diaspora longing to a specific, bounded territory — the Land of Canaan — in a way that mobilizes collective action and withstands competing claims.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by early Zionist founders (Herzl, Ben-Gurion) and religious Zionist theologians (Kook) as the core challenge the covenant reading solves. Palestinian historians (Khalidi, Masalha) and international lawyers (Hijab, Lynk) corroborate that the founding problem was real but the solution chosen — exclusive territorial claim grounded in divine grant — structurally required the displacement and ongoing subjugation of the indigenous population, a cost the founding narrative does not acknowledge.
narrative_ontology:disappearance_verdict(abrahamic_covenant__land_promise_constraint, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__land_promise_constraint, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__land_promise_constraint, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(abrahamic_covenant__land_promise_constraint, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__land_promise_constraint, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__land_promise_constraint_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(abrahamic_covenant__land_promise_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is very high (0.82) because the covenant reading transfers territorial sovereignty, water rights, legal personhood, and political agency from Palestinian populations to Israeli beneficiaries — a near-total transfer of the land's value. Suppression is high (0.78) because the regime's persistence requires continuous military enforcement, legal exclusion (denial of return, permit regimes), diplomatic pressure, and the active suppression of alternative legitimacy frameworks (international law, Palestinian self-determination). Theater ratio is moderate (0.42): the religious-national infrastructure (yeshivas, settlement councils, theological education) performs genuine coordination for beneficiaries while increasingly serving as extraction machinery — the coordination function (Jewish collective survival) is real but the territorial exclusivity component is the extractive edge. Accessibility collapse is high (0.71): once the divine grant frame is accepted, alternatives (binational state, equal rights, international law) appear as betrayal of the covenant itself. Resistance is substantial (0.65): Palestinian resistance (intifadas, legal campaigns, BDS, sumud) and international legal contestation are real and sustained, but the regime's enforcement capacity absorbs them.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seats (Israeli state, settlers, religious Zionist institutions), the constraint appears as a rope or mountain — genuine coordination of Jewish return, divine mandate, natural law of history. From the payer seats (Palestinian populations), it computes as a snare — pure extraction enforced by overwhelming power, with no coordination benefit for them. From the observer seat (international diplomacy), it reads as a contested claim with substantial extraction and suppression. The engine computes this seat divergence from the structural data: beneficiaries collect, payers pay, enforcement is active and asymmetrical.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli state actor is structural beneficiary (d near 0.0) — collects territorial legitimacy, diplomatic cover, settlement revenue; arbitrage-grade exit (could theoretically abandon the reading but would lose foundational legitimacy). Settler movement and religious Zionist institutions are identity-locked beneficiaries (d ~0.1-0.2) — collect material/status benefits; exit dissolves identity. Palestinian displaced, refugees, and occupied residents are structural targets (d ~0.9-1.0) — bear full extraction; exit options range from trapped (refugees, displaced) to constrained (occupied residents). International diplomacy is analytical (d = 0.5). Palestinian leadership factions are excluded — would be payers if admitted.
 *
 * MANDATROPHY ANALYSIS:
 *   The covenant reading's coordination function (mobilizing Jewish return and survival) was live in 1948-1967 but has attenuated as the territorial regime shifted from survival to expansion. The extraction component (settlements, resource transfer, denial of Palestinian rights) now exceeds the coordination benefit for all but the most identity-locked beneficiaries. The mandate has outlived its founding problem — the constraint persists by inertia and identity fusion, not by solving a live collective-action problem. This is a snare whose coordination story is cover for ongoing extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conditionality_of_promise,
    'Is the Genesis land promise conditional (dependent on covenant fidelity) or unconditional (irreversible grant)?',
    'Exegetical consensus across Jewish, Christian, and Islamic interpretive traditions on Genesis 17:7-8 (''everlasting covenant'') vs. Deuteronomic conditionality (Deut 28-30); historical reception history of the conditionality question.',
    'If conditional, the reading''s claim to perpetual territorial legitimacy collapses when covenant fidelity fails — the snare''s legitimating core dissolves. If unconditional, the extraction structure is theologically armored against internal critique.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conditionality_of_promise, conceptual, 'Whether the territorial grant''s perpetuity survives covenant breach — the theological hinge point for the reading''s legitimacy.').

omega_variable(
    fulfillment_timing,
    'Has the land promise been fulfilled (in Joshua, in 1948, in 1967), or does it remain an unfulfilled eschatological claim?',
    'Comparative theology of fulfillment: Christian supersessionist readings (fulfilled in Christ/church), Jewish messianic readings (future), Zionist readings (fulfilled in 1948/1967), Islamic readings (fulfilled in Muhammad/umma).',
    'If fulfilled, the reading''s ongoing extraction lacks theological warrant — it becomes a pure political claim using exhausted theological capital. If unfulfilled, the extraction is theologically mandated as preparation for completion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fulfillment_timing, conceptual, 'Whether the promise''s fulfillment status authorizes or delegitimizes current territorial extraction.').

omega_variable(
    beneficiary_lineage_exclusivity,
    'Does ''Abraham''s descendants'' in the land promise refer exclusively to Isaac/Jacob''s line, or inclusively to Ishmael''s line as well?',
    'Philological and reception-historical analysis of Genesis 17:19-21 (''Isaac... I will establish my covenant with him'') vs. 17:20 (''Ishmael... I will make him a great nation'') vs. 21:13 (''son of the slave woman I will make a nation''); Quranic and hadith reception.',
    'If exclusive, the reading forecloses Palestinian/Muslim claims theologically — the snare''s victim structure is theologically necessary. If inclusive, the reading''s exclusive territorial claim is a theological distortion — the extraction is not just political but hermeneutically violent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_lineage_exclusivity, conceptual, 'Whether the covenant''s beneficiary scope is exclusive or inclusive — the structural hinge for victim/beneficiary assignment.').

omega_variable(
    theological_vs_political_causality,
    'Does the covenant reading causally drive the territorial regime, or does the regime selectively deploy the reading as post-hoc legitimation?',
    'Historical sociology of Zionist thought: compare pre-1948 secular Zionist territorial claims (Herzl, Borochov) with religious Zionist theology (Kook); trace the adoption of covenant language by the secular state; measure correlation between theological commitment and settlement policy.',
    'If theological causality, the snare is structurally armored — extraction is inseparable from the reading''s truth claim. If political instrumentalism, the reading is a detachable legitimating layer — extraction could persist under a different cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_vs_political_causality, empirical, 'Whether the covenant reading is the engine or the exhaust of the territorial regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__land_promise_constraint, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abrahamic_covenant__land_promise_constraint_tr_t1948, abrahamic_covenant__land_promise_constraint, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(abrahamic_covenant__land_promise_constraint_tr_t1967, abrahamic_covenant__land_promise_constraint, theater_ratio, 1967, 0.28).
narrative_ontology:measurement(abrahamic_covenant__land_promise_constraint_tr_t1987, abrahamic_covenant__land_promise_constraint, theater_ratio, 1987, 0.35).
narrative_ontology:measurement(abrahamic_covenant__land_promise_constraint_tr_t1993, abrahamic_covenant__land_promise_constraint, theater_ratio, 1993, 0.38).
narrative_ontology:measurement(abrahamic_covenant__land_promise_constraint_tr_t2000, abrahamic_covenant__land_promise_constraint, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(abrahamic_covenant__land_promise_constraint_tr_t2024, abrahamic_covenant__land_promise_constraint, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(abrahamic_covenant__land_promise_constraint_be_t1948, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1948, 0.55).
narrative_ontology:measurement(abrahamic_covenant__land_promise_constraint_be_t1967, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1967, 0.68).
narrative_ontology:measurement(abrahamic_covenant__land_promise_constraint_be_t1987, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1987, 0.72).
narrative_ontology:measurement(abrahamic_covenant__land_promise_constraint_be_t1993, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1993, 0.7).
narrative_ontology:measurement(abrahamic_covenant__land_promise_constraint_be_t2000, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(abrahamic_covenant__land_promise_constraint_be_t2024, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(abrahamic_covenant__land_promise_constraint_su_t1948, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1948, 0.6).
narrative_ontology:measurement(abrahamic_covenant__land_promise_constraint_su_t1967, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1967, 0.7).
narrative_ontology:measurement(abrahamic_covenant__land_promise_constraint_su_t1987, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1987, 0.75).
narrative_ontology:measurement(abrahamic_covenant__land_promise_constraint_su_t1993, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1993, 0.73).
narrative_ontology:measurement(abrahamic_covenant__land_promise_constraint_su_t2000, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2000, 0.77).
narrative_ontology:measurement(abrahamic_covenant__land_promise_constraint_su_t2024, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__land_promise_constraint, identity_coordination).
narrative_ontology:boltzmann_floor_override(abrahamic_covenant__land_promise_constraint, 0.08).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, israeli_settlement_enterprise).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, palestinian_right_of_return_denial).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, jerusalem_status_quota).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, west_bank_area_c_control).

% DUAL FORMULATION NOTE:
% This constraint is one member of the abrahamic_covenant constraint family. The isaac_covenant_reading and ishmael_covenant_reading are sibling constraints with different beneficiary/victim structures and different epsilon values. This reading (land_promise_constraint) is the only one that maps the covenant onto a modern territorial sovereignty claim with active enforcement against a resident population — hence its distinctively high epsilon and snare classification. The sibling readings operate at the doctrinal/theological level without the same material extraction structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(abrahamic_covenant__land_promise_constraint, organized, 0.15).
constraint_indexing:directionality_override(abrahamic_covenant__land_promise_constraint, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
