% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__land_promise_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: abrahamic_covenant__land_promise_constraint
 *   human_readable: Abrahamic Covenant Land Promise Constraint (Conditional/Fulfilled/Ongoing Readings)
 *   domain: religious/theological/political
 *
 * SUMMARY:
 *   The Genesis covenant includes a territorial grant (Land of Canaan) whose
 *   interpretation determines material control over disputed territory in the
 *   modern Israeli-Palestinian conflict. This constraint instantiates ONE
 *   READING of the contested Abrahamic covenant kernel: the land-promise
 *   reading that interprets the covenant as granting permanent divine title
 *   to a specific territory, applicable in perpetuity, binding on all who
 *   inherit the Abrahamic lineage through Isaac. Sibling readings
 *   (isaac_covenant_reading, ishmael_covenant_reading) offer alternative
 *   theological framings that would yield different territorial and
 *   population consequences. This reading treats the promise as ONGOING
 *   rather than conditional or fulfilled-in-history, making it a live
 *   justification for territorial claims and settlement expansion. The
 *   constraint persists because state institutions embed it in law, military
 *   practice, and settlement infrastructure; it extracts by displacing
 *   Palestinian populations and foreclosing their territorial claims. Theater
 *   rises over the interval (1948–2024) as performative maintenance of the
 *   reading (religious services, ceremonial settlements, theo-political
 *   rhetoric) grows relative to the functional core (territorial
 *   consolidation completed). Suppression intensifies throughout as the cost
 *   to maintain the reading against competing interpretations escalates.
 *
 * KEY AGENTS:
 *   - jewish_state_institutional_apparatus: agenda-setter (institutional) — sets and enforces the land-promise reading; uses it to legitimize territorial claims and settlement expansion
 *   - settlement_expansion_networks: beneficiary (organized) — receives land, security, ideological validation; their physical presence consolidates the reading
 *   - palestinian_displaced_populations: victim (powerless, trapped) — bears the primary cost: dispossession, legal subordination, geographic confinement
 *   - palestinian_territorial_claimants: payer + excluded (moderate, constrained) — maintain alternative readings but are excluded from the authority apparatus that makes this reading operative
 *   - jewish_religious_authorities: agenda-setter + beneficiary (organized, identity-locked) — produce interpretive authority for the reading; benefit from centrality in legitimation narratives
 *   - islamic_religious_authorities: excluded payer (organized, identity-locked) — hold directly contradictory readings (Ishmael-inclusive, Muhammad-succession) but are structurally excluded from the state enforcement apparatus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, 0.82).
domain_priors:suppression_score(abrahamic_covenant__land_promise_constraint, 0.78).
domain_priors:theater_ratio(abrahamic_covenant__land_promise_constraint, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, extractiveness, 0.82).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__land_promise_constraint, snare).
narrative_ontology:human_readable(abrahamic_covenant__land_promise_constraint, "Abrahamic Covenant Land Promise Constraint (Conditional/Fulfilled/Ongoing Readings)").
narrative_ontology:topic_domain(abrahamic_covenant__land_promise_constraint, "religious/theological/political").

domain_priors:requires_active_enforcement(abrahamic_covenant__land_promise_constraint).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__land_promise_constraint, '4ff25128-6e60-4510-aa55-ca12a85fc645').
narrative_ontology:cs_kernel_codification('4ff25128-6e60-4510-aa55-ca12a85fc645', fixed_text).
narrative_ontology:cs_authority_grounding('4ff25128-6e60-4510-aa55-ca12a85fc645', extraction).
narrative_ontology:cs_interpretation_layer_present('4ff25128-6e60-4510-aa55-ca12a85fc645').
narrative_ontology:cs_reading_relation('4ff25128-6e60-4510-aa55-ca12a85fc645', abrahamic_covenant__isaac_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('4ff25128-6e60-4510-aa55-ca12a85fc645', abrahamic_covenant__ishmael_covenant_reading, forecloses).
narrative_ontology:cs_axiom('4ff25128-6e60-4510-aa55-ca12a85fc645', foundational, perpetual_territorial_covenant_binding).
narrative_ontology:cs_axiom_status(perpetual_territorial_covenant_binding, holdable).
narrative_ontology:cs_axiom_grounding('4ff25128-6e60-4510-aa55-ca12a85fc645', perpetual_territorial_covenant_binding, deontological).
narrative_ontology:cs_axiom('4ff25128-6e60-4510-aa55-ca12a85fc645', foundational, isaac_exclusive_covenant_transmission).
narrative_ontology:cs_axiom_status(isaac_exclusive_covenant_transmission, holdable).
narrative_ontology:cs_axiom_grounding('4ff25128-6e60-4510-aa55-ca12a85fc645', isaac_exclusive_covenant_transmission, deontological).
narrative_ontology:cs_axiom('4ff25128-6e60-4510-aa55-ca12a85fc645', secondary, unfulfilled_promise_ongoing_obligation).
narrative_ontology:cs_axiom_status(unfulfilled_promise_ongoing_obligation, holdable).
narrative_ontology:cs_axiom_grounding('4ff25128-6e60-4510-aa55-ca12a85fc645', unfulfilled_promise_ongoing_obligation, empirically_contingent).
narrative_ontology:cs_reference_frame('4ff25128-6e60-4510-aa55-ca12a85fc645', perpetual_territorial_promise_isaac_line).
narrative_ontology:cs_drift_state('4ff25128-6e60-4510-aa55-ca12a85fc645', contemporary_post_1948_state_implementation, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('4ff25128-6e60-4510-aa55-ca12a85fc645', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__land_promise_constraint, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, jewish_state_institutional_apparatus).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, settlement_expansion_networks).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, palestinian_displaced_populations).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, palestinian_territorial_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, jewish_religious_authorities).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, islamic_religious_authorities).
narrative_ontology:constraint_vindicates(abrahamic_covenant__land_promise_constraint, jewish_historical_continuity_doctrine).
narrative_ontology:constraint_vindicates(abrahamic_covenant__land_promise_constraint, chosen_people_hermeneutics).
narrative_ontology:constraint_vindicates(abrahamic_covenant__land_promise_constraint, redemptive_geography_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the covenant reading that interprets the land promise as ongoing, divinely mandated, and applicable to contemporary territorial claims. Uses the reading to legitimize state sovereignty over disputed territories, settlement expansion, and control of land designated by international law as Palestinian. The institutional apparatus frames security concerns and demographic maintenance as corollaries of covenant obligation, not as separate policy choices.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, jewish_state_institutional_apparatus, agenda_setter,
    institutional, generational, arbitrage, regional).

% Settlers occupy territory claimed under the land-promise reading. They receive state infrastructure, military protection, legal privileges, and ideological validation through covenant theology. Their physical presence and institutional consolidation depend on the constraint's active maintenance — the reading transforms territorial occupation into religious duty.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, settlement_expansion_networks, beneficiary,
    organized, biographical, constrained, regional).

% Carry the primary material cost of the covenant reading: displacement from land, inability to claim ancestral property, legal subordination in contested territory. They have no voice in the covenant reading's authority structure and no mechanism to contest the territorial interpretation. Exit from Palestinian identity/kinship is identity_locked; geographic exit is administratively restricted.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, palestinian_displaced_populations, payer,
    powerless, generational, trapped, regional).

% Maintain alternative covenant readings (Ishmael-inclusive, conditional promise, fulfilled-in-ancient-history readings) that would annul or reframe territorial claims. They are structurally excluded from the authority apparatus that adjudicates the land-promise reading in state law and settlement practice. Their competing readings carry no institutional force.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, palestinian_territorial_claimants, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__land_promise_constraint, palestinian_territorial_claimants, excluded).

% Orthodox and religious-nationalist interpretive communities produce and defend the ongoing land-promise reading as authentic covenant theology. They hold institutional authority over textual interpretation, religious legitimacy, and seminary training. The reading benefits them by centering their interpretive tradition as the authoritative voice on covenant meaning.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, jewish_religious_authorities, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__land_promise_constraint, jewish_religious_authorities, beneficiary).

% Hold Ishmael-inclusive and Muhammad-succession readings of the Abrahamic covenant that explicitly contest the territorial monopoly claimed by the land-promise reading. They are excluded from the state-enforcement apparatus that makes the land-promise reading operative in territory, law, and settlement. Their textual authority is not recognized in the institutional mechanisms that consolidate the constraint.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, islamic_religious_authorities, payer,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__land_promise_constraint, islamic_religious_authorities, excluded).

% Recognizes the constraint's operation through UN resolutions, human rights bodies, and humanitarian law frameworks. Observes the reading as a legitimation claim rather than a binding legal covenant. Can document the constraint's effect on displacement and territorial control but cannot adjudicate the religious covenant itself.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, international_law_order, observer,
    institutional, generational, analytical, global).

% Holds diverse readings: supersessionist (covenant transferred to church), Zionist (supports land promise), Palestinian-solidarity (reads covenant as conditional and forfeited). Christian voices are structurally outside the Jewish-state and Islamic-authority apparatus that enforces the constraint operationally, though Christian Zionists provide geopolitical support.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, christian_theological_tradition, excluded,
    organized, generational, identity_locked, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(abrahamic_covenant__land_promise_constraint, jewish_state_institutional_apparatus).
narrative_ontology:fixing_cost_class(abrahamic_covenant__land_promise_constraint, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: NONE. The constraint does not solve a collective-action problem between Jewish and Palestinian parties. Theologically, it solves an INTERNAL Jewish problem: reconciling diaspora dispersion with election narrative. This internal problem is solved by transforming it into an external territorial claim, which creates a NEW coordination problem (how do multiple peoples share one territory?) that the constraint does NOT solve — it unilaterally forecloses Palestinian voices from the solution. A genuine rope would coordinate both parties' interests in a shared outcome; this constraint excludes one party from voice and outcome both.
% TRANSFER_FUNCTION: Transfers land, administrative jurisdiction, population rights, security guarantees, and future settlement opportunity from Palestinian claimants (both displaced populations and territorial claimants) to the Jewish state apparatus and settlement networks. The transfer is enforced by state military power and justified by a theological reading. What flows: territorial control, property rights, autonomous governance, physical security. From: Palestinians. To: Jewish state institutions and settler networks. The theological reading provides legitimation cover; the military/administrative enforcement provides the mechanism.
% ABSENT_VOICES: Islamic religious authorities who hold Ishmael-inclusive readings (their theological position directly contradicts the land-promise reading but they are excluded from state-apparatus adjudication). Palestinian Christian theologians who read the covenant as superseded or transferred (they are structurally outside the Jewish-state enforcement apparatus and their reading is institutionally inert). Secular Jewish critics who argue the theological framing obscures a real-estate and security dispute that could be negotiated without divine-right language. Diaspora Jewish communities (particularly Haredi and Jewish diasporist traditions) that read Zionism itself as a heretical innovation or unwelcome to Jewish tradition. Palestinian voices claiming THEIR theological reading of the covenant (Ishmael-inclusive, or covenant-transferred-beyond-land) — not merely secular objections but theological alternatives.
% DISAPPEARANCE_RATIONALE: If the land-promise reading and its institutional enforcement vanished overnight, the territorial dispute would revert to secular frameworks: historical presence, security needs, population demographics, international law, negotiated borders. The institution of settlement expansion would cease (no theological justification). Israeli legal claims to Palestinian land would convert to claims based on security or occupation (different legal posture, more contestable). Palestinian claims to return would convert from 'covenantally excluded' to 'dispossessed by force' — different moral vocabulary, same material injury but different remedy framework (return vs. reparations). The theological apparatus (religious authorities, ceremonial settlements, state-sanctioned theology, seminary curriculum) would lose legitimation cover. All parties would reorganize around different legal and political theories.
% FOUNDING_PROBLEM: Jewish statelessness following Roman diaspora (70–135 CE onward, formalized in medieval dispersion). The theological problem: how can a people chosen by God (election theology) be without political sovereignty and homeland for 1,800+ years? The promise of land in the covenant text becomes the solution: the election narrative entails a right to the land, and the land's restoration becomes the vindication of Jewish identity. The constraint emerges to answer: on what basis does the Jewish people have a moral and legal right to return to and control a specific territory?
% FOUNDING_PROBLEM_CORROBORATION: Jewish Zionist theologians and state institutional leaders attest the founding problem is live: Jewish people require sovereign territory for security and self-determination after 2,000 years of persecution and powerlessness. Palestinian scholars and Islamic authorities attest the founding problem is real as a theological and political question but contest whether its solution is the displacement of an existing population (alternative: secure return negotiated with inhabitants). Secular Jewish historians attest the founding problem is real but the covenant-reading solution is one choice among several (alternative: historical connection + modern need warrants negotiation, not divine-right claims). International humanitarian scholars attest that regardless of the founding problem's validity, its solution via displacement of existing populations violates post-WWII international law norms. NO corroboration from outside the benefiting parties (Jewish state, religious authorities) attests that the COVENANT READING is the appropriate solution — international law, Palestinians, and Islamic authorities all contest the solution's validity even if they acknowledge the founding problem's reality.
narrative_ontology:disappearance_verdict(abrahamic_covenant__land_promise_constraint, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__land_promise_constraint, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__land_promise_constraint, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(abrahamic_covenant__land_promise_constraint, 'none', 1).

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
 *   Extractiveness is high (0.82 at 2024) because the constraint transfers material resources (land, jurisdiction, population security) from Palestinian claimants to Israeli state and settlement actors, and this transfer is defended by threat of force (military, administrative). The constraint persists not because all parties benefit but because one party has institutional power to enforce it. Suppression is nearly as high (0.78) because the cost to maintain the reading against competing interpretations is steep: suppression includes military occupation, settlement security infrastructure, legal discrimination, media control of narratives. Theater rises from 0.25 (1948, when territorial consolidation was the active task) to 0.41 (2024, when the reading is institutionalized and performative maintenance dominates: ceremonial settlements, religious rhetoric, state-sponsored theology). This drift from functional extraction to theater-heavy extraction marks the constraint as piton-trajectory (inertia-driven persistence) EXCEPT that the constraint remains highly extractive (0.82), which disqualifies it from piton status — a piton is one where the primary benefit to the agenda-setter has atrophied; here the settlement networks and state apparatus continue to extract real territorial advantage. The accessibility_collapse metrics reflect the trap mechanism: individual Palestinians have collapsed alternatives (0.78 at 2024); organizational alternatives also collapse (0.72); class-level solidarity is constrained (0.75); structural alternatives are foreclosed (0.82) — there is no institutional framework available to Palestinians that recognizes their territorial reading. The resistance metrics show declining resistance over time (0.72 → 0.58 at individual level; 0.68 → 0.62 at organizational level), reflecting either fatigue, asymmetric power hardening, or the normalization of the constraint in institutional structures. The stakes_inflation metrics show how the cost of noncompliance escalates across all levels: individual Palestinians face deportation/legal punishment (0.88 at 2024); organizations face disbanding/de-legitimation (0.84); classes face structural dispossession (0.86); the system faces existential competition from alternative readings (0.90 — the highest inflation, reflecting how existential the theological contest is).
 *
 * PERSPECTIVAL GAP:
 *   From the Jewish state and settlement apparatus position, this is a divinely mandated return to ancestral land, justified by both theology and historical connection. The constraint appears as restoration of rightful order. From the Palestinian displaced-population position, this is enforced exile from ancestral land, justified by a competing theological reading that is structurally excluded from the state apparatus. The constraint appears as theft + occupation + displacement. From the Islamic authority position, this is a false reading of a covenant that explicitly includes Ishmael and transmits through Muhammad, making Palestinian (Ishmael-descendant) claims equally valid. The constraint appears as heretical appropriation. From the international law perspective, regardless of theological validity, the constraint's operation violates humanitarian law and self-determination norms. The constraint appears as breach of post-WWII order. From the secular Jewish perspective (internal divergence), the constraint's theological framing muddies policy by mixing divinity claims with real-estate disputes that could be negotiated on secular grounds. Each seat computes a different effective extraction (χ) because directionality differs: beneficiary seats see low χ (subsidy), payer seats see high χ (extraction). The engine computes this per-seat divergence from the structural data; the constraint's single ε describes its structural extractiveness independent of observer position.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish state apparatus sits at d ≈ 0.1 (full beneficiary): it sets the rule, collects the territorial and strategic benefit, controls enforcement, holds arbitrage options (can negotiate away pieces while keeping core). Settlement networks sit at d ≈ 0.15 (substantial beneficiary): they receive land and security but are somewhat dependent on state backing and vulnerable to demographic pressure. Palestinian displaced populations sit at d ≈ 0.95 (near-total target): they bear the primary cost of dispossession, lack exit options (identity_locked to place/kinship), have no voice in the apparatus that enforces the reading. Palestinian territorial claimants sit at d ≈ 0.88 (high target): they maintain counter-readings but are structurally excluded from the enforcement apparatus, so their reading is operationally inert. Jewish religious authorities sit at d ≈ 0.08 (beneficiary): they produce the reading and gain institutional centrality from doing so, but do not directly bear dispossession costs. Islamic religious authorities sit at d ≈ 0.85 (high target): they hold directly contradictory readings that would annul the constraint, but are excluded from the authority apparatus, so their reading is operationally inert — they bear the reputational/institutional cost of being identified with the losing side of the theological contest without having recourse to enforcement. International law observers sit at d ≈ 0.5 (symmetric): they observe the operation without benefiting from extraction or bearing direct costs, though they carry diplomatic costs of documenting violations. Christian theological traditions sit at d ≈ 0.6 (moderate target): they are partly excluded from the contested apparatus and partly supporting the constraint (Christian Zionism) or opposing it (Palestinian-solidarity Christianity), creating divided institutional positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is real: Jewish statelessness and the need for a legitimate territorial basis for return after diaspora. The problem's validity is independent of whether the covenant reading is the right solution to it. The founding_problem_status = contested captures exactly this split: Jewish authorities attest the problem is live (Jewish people still need a state); international and Palestinian authorities attest the problem was theological but its solution via displacement violates post-WWII norms (alternative: historical connection warrants negotiation, not divine-right claims). The disappearance_verdict = world_rearranges correctly forecasts that if the land-promise reading disappeared, territorial negotiations would revert to secular frameworks (security, historical presence, population needs) rather than divine grant. The mandatrophy surface is therefore UNRESOLVED (base_properties.mandatrophy_resolved: false): the founding problem motivates the constraint but has outlived the condition of the constraint's original justification (Jewish need for secure territory → Covenant reading → territorial displacement). The constraint persists as pure extraction because (1) institutional inertia: the reading is embedded in law, settlement infrastructure, military training; (2) distributed institutional benefit: no single seat could unilaterally change the reading without losing power; (3) theological entrenchment: the reading has become constitutive of Jewish institutional identity, so delegitimizing the reading is experienced as delegitimizing the people. This is the mandatrophy condition: the constraint's founding mandate (secure Jewish territorial return) is arguably achieved, but the constraint persists because the institutional and theological apparatus cannot disengage without ontological crisis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conditional_vs_unconditional_reading,
    'Is the covenant promise conditional on the covenantee''s compliance with the covenant obligations (as several Torah passages suggest), or is it unconditional as later rabbinic interpretation emphasizes?',
    'Textual analysis comparing Torah conditional language (Deuteronomy covenantal curses for disobedience) with prophetic tradition; historical study of how conditions were interpreted in Second Temple and medieval periods.',
    'If conditional, territorial dispossession of the target population could be read as covenant breach (forfeiture of promise); if unconditional, no breach is possible and the promise is perpetually binding regardless of historical/political circumstances.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conditional_vs_unconditional_reading, conceptual, 'Whether the covenant''s land promise is conditional or unconditional.').

omega_variable(
    fulfilled_vs_ongoing_promise,
    'Was the land promise fulfilled in the ancient Israelite kingdom period (c. 1000 BCE), or is it an ONGOING promise still awaiting fulfillment in modern time?',
    'Theological/hermeneutical analysis: Jewish Zionist theology reads the promise as perpetually binding (ongoing); Christian supersessionist theology reads the promise as fulfilled-in-ancient-times (hence inapplicable to modern claims); Islamic theology reads the promise as transferred/redirected to other Abrahamic peoples.',
    'If fulfilled-in-history, territorial claims grounded in the promise are anachronistic and lose theological validity. If ongoing, the promise justifies contemporary territorial claims. This is the PRIMARY theological pivot point enabling or disabling the constraint''s extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fulfilled_vs_ongoing_promise, conceptual, 'Whether the land promise is a historical event (fulfilled) or a perpetual obligation (ongoing).').

omega_variable(
    exclusive_vs_inclusive_covenant_lineage,
    'Does the Abrahamic covenant apply EXCLUSIVELY to descendants of Isaac (as Genesis 17:19-21 emphasizes), or INCLUSIVELY to all Abraham''s descendants including Ishmael (as Genesis 21:13 suggests)?',
    'Textual exegesis of Genesis 17 vs. 21; comparison with Qur''anic emphasis on Ishmael as also covenanted; Islamic jurisprudence on Abrahamic inheritance vs. Jewish halakhic tradition on covenant transmission.',
    'If exclusive to Isaac, Palestinian Muslim claimants (Ishmael descendants, per Islamic tradition) have no covenant claim to the land. If inclusive, both Jewish and Muslim claims flow from the same covenant, making the territorial contest intra-covenantal rather than between covenant and non-covenant populations. This determines whether the constraint''s victim/beneficiary structure can frame Palestinians as ''outside'' the covenantal promise (delegitimizing their claims) or ''also within'' it (requiring different theological justification for displacement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusive_vs_inclusive_covenant_lineage, conceptual, 'Whether the Abrahamic covenant lineage is exclusive (Isaac only) or inclusive (all Abraham''s descendants).').

omega_variable(
    institutional_enforcement_vs_natural_emergence,
    'Is the land-promise reading''s persistence primarily an institutional achievement (state law, military enforcement, settlement infrastructure, theological education) or a ''natural'' consequence of the reading''s theological truth for believers?',
    'Comparative institutional analysis: how much enforcement capacity would be required if the reading did NOT have institutional backing? Would the reading persist among diaspora Jews without state power to operationalize it? Ethnographic study of settlement communities: what percentage explicitly ground their occupation in covenant theology vs. security or economic motivation?',
    'If persistence is primarily institutional, the constraint is snare-class (requires active enforcement, victims are real, alternatives are suppressed). If persistence is primarily natural/theological, the reading might appear more as rope-class (coordinate, not enforced). The measured suppression (0.78) suggests institutional enforcement is heavy; this omega questions whether that enforcement measures the reading''s theological weakness or the state''s power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_enforcement_vs_natural_emergence, empirical, 'Whether the land-promise reading persists through institutional enforcement or theological persuasion.').

omega_variable(
    identity_fusion_mechanism,
    'To what degree is the land-promise reading fused with Jewish identity and institutional legitimacy such that questioning the reading is experienced as delegitimizing Jewish peoplehood itself?',
    'Sociological study of Jewish institutional resistance to alternative covenant readings; analysis of how questioning the reading is framed in institutional discourse (as theological debate vs. existential threat); ethnographic evidence from Jewish communities that maintain alternative readings (Haredi anti-Zionism, Jewish diasporist tradition, etc.).',
    'If highly fused (high identity-lock), the reading''s institutional persistence is nearly inevitable because institutional leaders face existential delegitimization by allowing alternative readings. If loosely fused, the reading could be revised without institutional collapse. This affects whether the constraint is fixable through negotiation or requires deeper identity-reconstruction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_mechanism, empirical, 'Degree to which the land-promise reading is fused with Jewish institutional and personal identity.').

omega_variable(
    theological_sibling_reading_foreclosure,
    'Does the land-promise reading (ONGOING promise to Isaac''s line) FORECLOSE the Ishmael-inclusive reading (covenant continues through Ishmael to Muhammad) within any single theological framework, or do the readings merely COEXIST as held by different parties?',
    'Comparative theology: do Jewish interpretive traditions that hold the Isaac-exclusive reading LOGICALLY ENTAIL rejection of Islamic Qur''anic reading of Ishmael-inclusion? Or are they simply different traditions held by different communities with no internal logical clash?',
    'If foreclosure (one reading logically contradicts the other), then neither can be institutionally integrated; the constraint necessarily reflects one tradition''s dominance. If coexistence (readings held by different parties), then the constraint reflects power asymmetry rather than logical incompatibility. This determines whether the readings could be negotiated toward a synthesis or whether institutional dominance is the only possible outcome.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theological_sibling_reading_foreclosure, conceptual, 'Whether the land-promise reading logically forecloses the Ishmael-inclusive reading or merely coexists with it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__land_promise_constraint, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(covenant_land_tr_t1948, abrahamic_covenant__land_promise_constraint, theater_ratio, 1948, 0.25).
narrative_ontology:measurement_basis(covenant_land_tr_t1948, observed).
narrative_ontology:measurement(covenant_land_tr_t1967, abrahamic_covenant__land_promise_constraint, theater_ratio, 1967, 0.29).
narrative_ontology:measurement_basis(covenant_land_tr_t1967, observed).
narrative_ontology:measurement(covenant_land_tr_t1982, abrahamic_covenant__land_promise_constraint, theater_ratio, 1982, 0.34).
narrative_ontology:measurement_basis(covenant_land_tr_t1982, observed).
narrative_ontology:measurement(covenant_land_tr_t2000, abrahamic_covenant__land_promise_constraint, theater_ratio, 2000, 0.38).
narrative_ontology:measurement_basis(covenant_land_tr_t2000, observed).
narrative_ontology:measurement(covenant_land_tr_t2012, abrahamic_covenant__land_promise_constraint, theater_ratio, 2012, 0.4).
narrative_ontology:measurement_basis(covenant_land_tr_t2012, observed).
narrative_ontology:measurement(covenant_land_tr_t2024, abrahamic_covenant__land_promise_constraint, theater_ratio, 2024, 0.41).
narrative_ontology:measurement_basis(covenant_land_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(covenant_land_be_t1948, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1948, 0.45).
narrative_ontology:measurement_basis(covenant_land_be_t1948, observed).
narrative_ontology:measurement(covenant_land_be_t1967, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1967, 0.62).
narrative_ontology:measurement_basis(covenant_land_be_t1967, observed).
narrative_ontology:measurement(covenant_land_be_t1982, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1982, 0.71).
narrative_ontology:measurement_basis(covenant_land_be_t1982, observed).
narrative_ontology:measurement(covenant_land_be_t2000, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2000, 0.77).
narrative_ontology:measurement_basis(covenant_land_be_t2000, observed).
narrative_ontology:measurement(covenant_land_be_t2012, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2012, 0.8).
narrative_ontology:measurement_basis(covenant_land_be_t2012, observed).
narrative_ontology:measurement(covenant_land_be_t2024, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2024, 0.82).
narrative_ontology:measurement_basis(covenant_land_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(covenant_land_su_t1948, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1948, 0.52).
narrative_ontology:measurement_basis(covenant_land_su_t1948, observed).
narrative_ontology:measurement(covenant_land_su_t1967, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1967, 0.64).
narrative_ontology:measurement_basis(covenant_land_su_t1967, observed).
narrative_ontology:measurement(covenant_land_su_t1982, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1982, 0.7).
narrative_ontology:measurement_basis(covenant_land_su_t1982, observed).
narrative_ontology:measurement(covenant_land_su_t2000, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement_basis(covenant_land_su_t2000, observed).
narrative_ontology:measurement(covenant_land_su_t2012, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2012, 0.77).
narrative_ontology:measurement_basis(covenant_land_su_t2012, observed).
narrative_ontology:measurement(covenant_land_su_t2024, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2024, 0.78).
narrative_ontology:measurement_basis(covenant_land_su_t2024, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1948, tn=2024
narrative_ontology:measurement(covenant_land_grid_01, abrahamic_covenant__land_promise_constraint, accessibility_collapse(class), 1948, 0.52).
narrative_ontology:measurement(covenant_land_grid_02, abrahamic_covenant__land_promise_constraint, accessibility_collapse(class), 2024, 0.75).
narrative_ontology:measurement(covenant_land_grid_03, abrahamic_covenant__land_promise_constraint, accessibility_collapse(individual), 1948, 0.55).
narrative_ontology:measurement(covenant_land_grid_04, abrahamic_covenant__land_promise_constraint, accessibility_collapse(individual), 2024, 0.78).
narrative_ontology:measurement(covenant_land_grid_05, abrahamic_covenant__land_promise_constraint, accessibility_collapse(organizational), 1948, 0.48).
narrative_ontology:measurement(covenant_land_grid_06, abrahamic_covenant__land_promise_constraint, accessibility_collapse(organizational), 2024, 0.72).
narrative_ontology:measurement(covenant_land_grid_07, abrahamic_covenant__land_promise_constraint, accessibility_collapse(structural), 1948, 0.58).
narrative_ontology:measurement(covenant_land_grid_08, abrahamic_covenant__land_promise_constraint, accessibility_collapse(structural), 2024, 0.82).
narrative_ontology:measurement(covenant_land_grid_09, abrahamic_covenant__land_promise_constraint, resistance(class), 1948, 0.7).
narrative_ontology:measurement(covenant_land_grid_10, abrahamic_covenant__land_promise_constraint, resistance(class), 2024, 0.64).
narrative_ontology:measurement(covenant_land_grid_11, abrahamic_covenant__land_promise_constraint, resistance(individual), 1948, 0.72).
narrative_ontology:measurement(covenant_land_grid_12, abrahamic_covenant__land_promise_constraint, resistance(individual), 2024, 0.58).
narrative_ontology:measurement(covenant_land_grid_13, abrahamic_covenant__land_promise_constraint, resistance(organizational), 1948, 0.68).
narrative_ontology:measurement(covenant_land_grid_14, abrahamic_covenant__land_promise_constraint, resistance(organizational), 2024, 0.62).
narrative_ontology:measurement(covenant_land_grid_15, abrahamic_covenant__land_promise_constraint, resistance(structural), 1948, 0.65).
narrative_ontology:measurement(covenant_land_grid_16, abrahamic_covenant__land_promise_constraint, resistance(structural), 2024, 0.68).
narrative_ontology:measurement(covenant_land_grid_17, abrahamic_covenant__land_promise_constraint, stakes_inflation(class), 1948, 0.44).
narrative_ontology:measurement(covenant_land_grid_18, abrahamic_covenant__land_promise_constraint, stakes_inflation(class), 2024, 0.86).
narrative_ontology:measurement(covenant_land_grid_19, abrahamic_covenant__land_promise_constraint, stakes_inflation(individual), 1948, 0.42).
narrative_ontology:measurement(covenant_land_grid_20, abrahamic_covenant__land_promise_constraint, stakes_inflation(individual), 2024, 0.88).
narrative_ontology:measurement(covenant_land_grid_21, abrahamic_covenant__land_promise_constraint, stakes_inflation(organizational), 1948, 0.38).
narrative_ontology:measurement(covenant_land_grid_22, abrahamic_covenant__land_promise_constraint, stakes_inflation(organizational), 2024, 0.84).
narrative_ontology:measurement(covenant_land_grid_23, abrahamic_covenant__land_promise_constraint, stakes_inflation(structural), 1948, 0.48).
narrative_ontology:measurement(covenant_land_grid_24, abrahamic_covenant__land_promise_constraint, stakes_inflation(structural), 2024, 0.9).
narrative_ontology:measurement(covenant_land_grid_25, abrahamic_covenant__land_promise_constraint, suppression(class), 1948, 0.42).
narrative_ontology:measurement(covenant_land_grid_26, abrahamic_covenant__land_promise_constraint, suppression(class), 2024, 0.79).
narrative_ontology:measurement(covenant_land_grid_27, abrahamic_covenant__land_promise_constraint, suppression(individual), 1948, 0.38).
narrative_ontology:measurement(covenant_land_grid_28, abrahamic_covenant__land_promise_constraint, suppression(individual), 2024, 0.82).
narrative_ontology:measurement(covenant_land_grid_29, abrahamic_covenant__land_promise_constraint, suppression(organizational), 1948, 0.35).
narrative_ontology:measurement(covenant_land_grid_30, abrahamic_covenant__land_promise_constraint, suppression(organizational), 2024, 0.76).
narrative_ontology:measurement(covenant_land_grid_31, abrahamic_covenant__land_promise_constraint, suppression(structural), 1948, 0.48).
narrative_ontology:measurement(covenant_land_grid_32, abrahamic_covenant__land_promise_constraint, suppression(structural), 2024, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__land_promise_constraint, identity_coordination).
narrative_ontology:boltzmann_floor_override(abrahamic_covenant__land_promise_constraint, 0.12).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, isaac_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, ishmael_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, israeli_settlement_legitimation_apparatus).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, palestinian_territorial_dispossession).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, jewish_diaspora_return_theology).

% DUAL FORMULATION NOTE:
% The land-promise constraint is one member of a constraint family grounded in the Abrahamic covenant kernel. All members instantiate READINGS of the same kernel text, each generating different structural outcomes. isaac_covenant_reading emphasizes Ishmael's exclusion (legitimates Jewish-exclusive claims). ishmael_covenant_reading emphasizes covenant continuation through Ishmael (would delegitimate Jewish-monopoly reading). This constraint (land_promise_constraint) treats the promise as ONGOING and operationally determinative. The readings are NOT observable-dependent variations of a single constraint — they are genuinely different constraints because their ε values (extractiveness) differ by an order of magnitude, their beneficiary/victim structures are inverted, and their institutional power bases are disjoint. Each reading, if operationally dominant, would instantiate a different constraint family with different victims and beneficiaries. They are linked not because they describe the same phenomenon but because they are competing interpretations of the SAME KERNEL TEXT, each instantiating the constraint structure as the respective reading's institutional apparatus enforces it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(abrahamic_covenant__land_promise_constraint, powerless, 0.95).
constraint_indexing:directionality_override(abrahamic_covenant__land_promise_constraint, moderate, 0.88).
constraint_indexing:directionality_override(abrahamic_covenant__land_promise_constraint, organized, 0.15).
constraint_indexing:directionality_override(abrahamic_covenant__land_promise_constraint, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
