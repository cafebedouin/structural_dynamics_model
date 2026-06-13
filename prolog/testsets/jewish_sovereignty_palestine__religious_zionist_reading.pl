% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__religious_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__religious_zionist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jewish_sovereignty_palestine__religious_zionist_reading
 *   human_readable: Divine Promise of Eretz Yisrael — Religious Zionist Reading
 *   domain: political_philosophy/nationalism/theology
 *
 * SUMMARY:
 *   The religious Zionist reading positions divine covenant as grounding an
 *   inalienable Jewish territorial claim to Eretz Yisrael and interprets
 *   statehood as theological fulfillment of redemptive promise. Under this
 *   reading, the Jewish people constitute a covenant community whose return
 *   to the land restores their rightful historical and spiritual place.
 *   Palestinian Arab presence is positioned as non-covenantal and
 *   historically contingent — a demographic fact that does not generate
 *   equivalent territorial legitimacy. The constraint enforces this reading
 *   through state institutions, settlement policy, demographic maintenance,
 *   and suppression of competing interpretations (both Jewish and Muslim).
 *   The claim/metric independence principle applies: the reading claims this
 *   is rope-like coordination (solving the Jewish redemption problem through
 *   statehood), while the authored metrics describe substantially extractive,
 *   actively enforced operation with high accessibility collapse and
 *   significant suppression requirement. The engine measures the divergence.
 *
 * KEY AGENTS:
 *   - Jewish people as covenant community: beneficiary of the territorial claim; identity is fused with the claim such that exit is identity-foreclosing
 *   - Religious Zionist movement: agenda-setter and institutional curator of the reading; controls settlement policy and theological authority
 *   - Palestinian Arabs: structurally positioned as non-covenantal payers; dispossessed and subordinated by the constraint's operation
 *   - Secular Jewish Israelis: beneficiary of statehood but not dependent on the religious reading's theological frame
 *   - International liberal order: excluded from decision-making; would object to territorial maximalism under human-rights and self-determination frameworks
 *   - Theological traditionalists (Jewish and Muslim): excluded from institutional authority; interpret their covenants as incompatible with Zionist statehood
 *   - Observer scholars: analytical seat measuring how the reading's enforceability depends on theological authority and how alternatives would alter the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, 0.89).
domain_priors:suppression_score(jewish_sovereignty_palestine__religious_zionist_reading, 0.76).
domain_priors:theater_ratio(jewish_sovereignty_palestine__religious_zionist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__religious_zionist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__religious_zionist_reading, "Divine Promise of Eretz Yisrael — Religious Zionist Reading").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__religious_zionist_reading, "political_philosophy/nationalism/theology").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__religious_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__religious_zionist_reading, 'd8f64e0b-4c16-4b57-b3bc-a1fe0f68fd50').
narrative_ontology:cs_kernel_codification('d8f64e0b-4c16-4b57-b3bc-a1fe0f68fd50', fixed_text).
narrative_ontology:cs_authority_grounding('d8f64e0b-4c16-4b57-b3bc-a1fe0f68fd50', lineage).
narrative_ontology:cs_interpretation_layer_present('d8f64e0b-4c16-4b57-b3bc-a1fe0f68fd50').
narrative_ontology:cs_reading_relation('d8f64e0b-4c16-4b57-b3bc-a1fe0f68fd50', jewish_sovereignty_palestine__cultural_zionist_reading, forecloses).
narrative_ontology:cs_reading_relation('d8f64e0b-4c16-4b57-b3bc-a1fe0f68fd50', jewish_sovereignty_palestine__liberal_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('d8f64e0b-4c16-4b57-b3bc-a1fe0f68fd50', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('d8f64e0b-4c16-4b57-b3bc-a1fe0f68fd50', jewish_sovereignty_palestine__post_zionist_reading, forecloses).
narrative_ontology:cs_axiom('d8f64e0b-4c16-4b57-b3bc-a1fe0f68fd50', foundational, divine_covenant_grants_inalienable_title).
narrative_ontology:cs_axiom_status(divine_covenant_grants_inalienable_title, holdable).
narrative_ontology:cs_axiom_grounding('d8f64e0b-4c16-4b57-b3bc-a1fe0f68fd50', divine_covenant_grants_inalienable_title, theological).
narrative_ontology:cs_axiom('d8f64e0b-4c16-4b57-b3bc-a1fe0f68fd50', foundational, territorial_maximalism_covenantally_mandated).
narrative_ontology:cs_axiom_status(territorial_maximalism_covenantally_mandated, holdable).
narrative_ontology:cs_axiom_grounding('d8f64e0b-4c16-4b57-b3bc-a1fe0f68fd50', territorial_maximalism_covenantally_mandated, theological).
narrative_ontology:cs_axiom('d8f64e0b-4c16-4b57-b3bc-a1fe0f68fd50', secondary, palestinian_non_covenantal_status).
narrative_ontology:cs_axiom_status(palestinian_non_covenantal_status, holdable).
narrative_ontology:cs_axiom_grounding('d8f64e0b-4c16-4b57-b3bc-a1fe0f68fd50', palestinian_non_covenantal_status, theological).
narrative_ontology:cs_reference_frame('d8f64e0b-4c16-4b57-b3bc-a1fe0f68fd50', jewish_return_and_covenantal_redemption).
narrative_ontology:cs_drift_state('d8f64e0b-4c16-4b57-b3bc-a1fe0f68fd50', contemporary_post_oslo_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d8f64e0b-4c16-4b57-b3bc-a1fe0f68fd50', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, jewish_people_as_covenant_community).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_arabs_displaced_and_subordinated).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__religious_zionist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__religious_zionist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 (1880, incipient movement) to 0.89 (2026, mature institutional state). The trajectory reflects accumulated territorial claims, demographic displacement, subordination of Palestinian political standing, and territorial maximalism (no partition accepted as legitimate). Suppression rises from 0.22 to 0.76, measuring the active enforcement machinery required to maintain the reading's institutional dominance against competing interpretations and Palestinian resistance. Theater ratio rises to 0.42 by 2026, indicating an increasing share of enforcement activity devoted to defending the reading's theological monopoly and territorial claim rather than solving genuine security problems. The accessibility_collapse metric is very high (0.91) because once the covenant frame is accepted, alternatives become cognitively incoherent to believers — the reading colonizes the entire possibility space for Jewish identity. Resistance remains substantial (0.78) because Palestinian movements and international liberal institutions continuously challenge the reading's legitimacy. All measurements are authored on a single time grid so temporal analysis captures the constraint's lifecycle across all four metrics.
 *
 * PERSPECTIVAL GAP:
 *   The religious Zionist agenda-setter (organized power, civilizational horizon, identity-locked exit) and the Palestinian payer (powerless, generational horizon, trapped exit) should compute entirely different constraint types. From the agenda-setter's seat, the arrangement solves a genuine redemptive problem (coordinate_type: identity_coordination, fulfilling covenantal belonging). From the payer's seat, the same arrangement is pure territorial extraction enforced by superior military and institutional power. The engine computes per-seat classifications from directionality derived from beneficiary/victim and exit asymmetry. The authored claim (tangled_rope) reflects the reading's own framing: it solves the Jewish problem (coordination) while imposing asymmetric costs on Palestinians (extraction). This is structurally correct from within the reading; other readings would dispute the problem definition itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish people as covenant community occupy d~0.05 (strong beneficiary position): they collect the territorial claim, define the reading's terms, control the enforcement machinery through state institutions, and have identity fused to the claim. Palestinian Arabs occupy d~0.95 (strong target position): they bear the primary costs (dispossession, subordination, political powerlessness), have no voice in the reading's definition, and exit is entirely foreclosed (return denied, remaining means subordination, external displacement is marginal). Secular Jewish Israelis occupy d~0.35 (moderate beneficiary with constrained exit): they benefit from statehood security but are not identity-locked to the religious reading. Religious Zionist movement occupies d~0.08 (strong beneficiary): they set the reading, control settlement policy, and are identity-locked to the redemptive mission. International liberal order occupies d~0.70 (moderate target): they bear reputational/legitimacy costs from hosting or defending the reading, and their frameworks are suppressed rather than integrated. Theological traditionalists occupy d~0.85 (strong target): their interpretations are excluded and delegitimized. Directionality overrides are not necessary here; the structural data (beneficiary/victim declarations, power atoms, exit options) produce the right d values through automatic derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem at t0 (1880) was Jewish vulnerability in diaspora and the need for secure territorial belonging. By t2026, this founding problem is substantially solved from a Jewish demographic security perspective: Israeli statehood exists, Jewish population has grown, military capability is substantial, and diaspora Jewish communities have greater security than in pre-WWII Europe. Yet the constraint persists with rising extractiveness (0.89) and theater_ratio (0.42). The mismatch between founding_problem_status: 'contested' and disappearance_verdict: 'world_rearranges' signals potential mandatrophy: the founding mandate is dead or sufficiently transformed that its original justification no longer applies, but the territorial arrangement persists as institutional inertia. Theater_ratio rising faster than base_extractiveness suggests increasing performative maintenance of theological authority while material security rationales fade. This is consistent with a zombie constraint: the institutional apparatus maintains the reading not because the founding problem drives it, but because the beneficiary organizations are locked into the territorial claim through identity, institutional capture, and settler interests. A mandatrophy resolution would require either (a) the religious Zionist movement acknowledging the founding problem as solved and renegotiating the mandate's scope, or (b) institutional transition to a different reading (secular nationalist, post-Zionist) that justifies the state differently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_theological_authority,
    'Is the religious Zionist reading''s claim to theological authority over competing Jewish and Muslim interpretations justified by textual tradition, rabbinic consensus, or institutional power?',
    'Comparative theological analysis of Jewish law (Halakha), Quranic interpretation, and rabbinic tradition; examination of how authority shifted from pre-modern consensus (messianic restraint) to modern religious Zionist reinterpretation; documentation of institutional gatekeeping that marginalizes dissenting voices.',
    'If authority is institutional rather than textually grounded, the reading becomes a strategic choice rather than a revealed claim, which would reclassify the constraint from divinely-mandated sovereignty to politically-enforced ideology. That would shift extractiveness measurement focus from covenant vindication to power consolidation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_theological_authority, empirical, 'Whether religious Zionist theological claims rest on text-immanent authority or institutional power.').

omega_variable(
    covenant_as_container_frame,
    'Does the covenant container necessarily require territorial exclusivity and demographic majority, or is the covenant container separable from the territorial configuration?',
    'Examination of alternative Jewish theological readings (cultural Zionist, liberal nationalist, post-Zionist, diaspora-positive) that affirm covenant belonging without requiring the religious Zionist territorial reading. Study of how Jewish law addresses minority status, non-Jewish residents, and shared land under pre-modern conditions.',
    'If the covenant frame is separable from territorial maximalism, then the high extractiveness (0.89) is measuring a choice about how to instantiate covenant, not a structural necessity of covenant itself. This would open space for alternative readings to claim equal theological legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_as_container_frame, conceptual, 'Whether divine covenant grounds only this reading or is compatible with competing readings.').

omega_variable(
    palestinian_structural_absence,
    'Is the Palestinian absence from the religious Zionist beneficiary/victim calculus a structural feature of the reading''s theology, or a contingent choice about how to apply it?',
    'Textual analysis of how Palestinian humanity, land attachment, and historical presence are positioned within the reading''s framework. Comparison with alternative readings that include Palestinian self-determination in the beneficiary set or reframe the victim set. Examination of whether the reading''s core theological commitments logically require Palestinian subordination or merely permit it.',
    'If Palestinian absence is contingent, a non-subordinating reading of the same covenant theology is logically possible, which would open the constraint to alternative formulations. If structural, the reading''s extractiveness (0.89) is justified by theological necessity from within the reading''s frame, though competing frames would classify the same facts as predatory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(palestinian_structural_absence, conceptual, 'Whether Palestinian exclusion follows from the covenant theology or is a political choice.').

omega_variable(
    mandate_obsolescence_contest,
    'Has the founding problem (Jewish exile, persecution, vulnerability) that initially grounded the statehood mandate been substantially solved by Israeli statehood itself, rendering the territorial mandate subject to renegotiation?',
    'Demographic analysis of Jewish security in diaspora vs. within statehood; comparison of persecution risks pre-1950 and post-2000; examination of whether the reading''s proponents acknowledge any condition under which the mandate would be complete or renegotiable; study of how secular Israeli leadership frames the founding problem vs. religious Zionist framing.',
    'If the founding problem is substantially solved, the constraint would face a mandatrophy verdict: the founding mandate is dead, but territorial maximalism persists theatrically (rising theater_ratio, measuring increasing decoupling). If contested, the mismatch between founding_problem_status and disappearance_verdict signals a zombie constraint maintained by institutional inertia rather than structural need.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_obsolescence_contest, empirical, 'Whether the founding problem still justifies the territorial mandate.').

omega_variable(
    reading_vs_settler_colonial_frame,
    'Is the religious Zionist reading genuinely incompatible with the settler-colonial reading, or do they describe the same territorial process through different legitimacy lenses?',
    'Structural comparison of land acquisition, demographic displacement, and institutional architecture across the two readings. Examination of whether the religious reading''s theological commitments logically foreclose settler-colonial analysis, or whether settler colonialism is a description of facts the religious reading reinterprets theologically.',
    'If incompatible (forecloses), the readings occupy different ontological spaces and cannot compete. If compatible (coexists_with or influences), the settler-colonial reading describes structural facts the religious reading reframes, which means the theological authority does not settle the structural question about displacement and subordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_settler_colonial_frame, conceptual, 'Whether the religious reading''s theology forecloses structural/historical analysis of colonialism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__religious_zionist_reading, 1880, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1880, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1880, 0.08).
narrative_ontology:measurement(jewi_tr_t1920, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1920, 0.12).
narrative_ontology:measurement(jewi_tr_t1945, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1945, 0.25).
narrative_ontology:measurement(jewi_tr_t1970, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1970, 0.38).
narrative_ontology:measurement(jewi_tr_t2000, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2000, 0.41).
narrative_ontology:measurement(jewi_tr_t2026, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1880, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1880, 0.15).
narrative_ontology:measurement(jewi_be_t1920, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1920, 0.42).
narrative_ontology:measurement(jewi_be_t1945, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1945, 0.68).
narrative_ontology:measurement(jewi_be_t1970, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1970, 0.81).
narrative_ontology:measurement(jewi_be_t2000, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2000, 0.87).
narrative_ontology:measurement(jewi_be_t2026, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2026, 0.89).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1880, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1880, 0.22).
narrative_ontology:measurement(jewi_su_t1920, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1920, 0.38).
narrative_ontology:measurement(jewi_su_t1945, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1945, 0.61).
narrative_ontology:measurement(jewi_su_t1970, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1970, 0.72).
narrative_ontology:measurement(jewi_su_t2000, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(jewi_su_t2026, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2026, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__religious_zionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__religious_zionist_reading, 0.12).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__post_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_self_determination_right).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, international_human_rights_framework).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'jewish_sovereignty_palestine'. Five constraint files collectively model the readings: religious_zionist_reading (highest extractiveness, covenant theology, territorial maximalism), liberal_nationalist_reading (moderate extractiveness, collective self-determination right, partition legitimacy), settler_colonial_reading (high extractiveness, colonial structure, displacement focus), cultural_zionist_reading (lower extractiveness, cultural center without political sovereignty), post_zionist_reading (critique of ethnic-national framework, transition to civic form). Each reading has distinct beneficiary/victim structure, ε value, and temporal trajectory. The readings coexist as live positions in contemporary dispute; none logically forecloses the others within the contested kernel frame. The religious reading influences downstream constraints (Palestinian self-determination, international human-rights framework) by subordinating them to covenant claims. Sibling readings structure the kernel's decomposition — ε-invariance requires separate files because the readings instantiate substantially different constraints even though they reference the same historical territory.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
