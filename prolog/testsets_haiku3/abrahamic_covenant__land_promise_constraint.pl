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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: abrahamic_covenant__land_promise_constraint
 *   human_readable: Genesis Territorial Covenant: Land Promise Reading
 *   domain: religious/political/territorial
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the Abrahamic covenant
 *   kernel: the interpretation that Genesis 17 and 12 establish an
 *   unconditional, perpetual territorial grant of Canaan to Abraham's
 *   descendants through the Jewish line. This reading grounds Israeli state
 *   legitimacy and justifies territorial administration, settlement
 *   expansion, and military occupation. The reading is contested by
 *   alternative kernel readings (Ishmael/Islamic inheritance, secular
 *   Palestinian nationalist claims) and by international humanitarian law
 *   frameworks that reject religious texts as territorial bases. The
 *   constraint's operation extracts territorial control and demographic
 *   hegemony from Palestinian and Arab populations while distributing
 *   legitimacy and security benefits to Jewish diaspora and Israeli state
 *   institutions. This is ONE constraint generated from ONE reading of the
 *   kernel; the sibling readings (isaac_covenant_reading,
 *   ishmael_covenant_reading) are separate constraints with different ε
 *   values and beneficiary structures.
 *
 * KEY AGENTS:
 *   - Israeli state apparatus — agenda setter, institutional power, enforces the reading through law, settlement policy, and military administration; collects territorial control and state legitimacy
 *   - Palestinian residents and Arab-displaced populations — payers, powerless, trapped in occupied territory or permanent diaspora, suppressed by the constraint's enforcement
 *   - Jewish diaspora — beneficiaries, organized power, mobile exit, receive Law of Return citizenship and security benefits without bearing direct occupation costs
 *   - Religious authority structures (Jewish) — secondary agenda setters, beneficiaries, organized power, validate and perpetuate the reading through textual interpretation and theological justification
 *   - Palestinian Authority and Arab states — excluded from the constraint's enforcement machinery, powerful but constrained, their counter-readings suppressed
 *   - International legal authorities — observers, institutional power, document the reading's operation as contradicting international humanitarian law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, 0.82).
domain_priors:suppression_score(abrahamic_covenant__land_promise_constraint, 0.76).
domain_priors:theater_ratio(abrahamic_covenant__land_promise_constraint, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, extractiveness, 0.82).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__land_promise_constraint, snare).
narrative_ontology:human_readable(abrahamic_covenant__land_promise_constraint, "Genesis Territorial Covenant: Land Promise Reading").
narrative_ontology:topic_domain(abrahamic_covenant__land_promise_constraint, "religious/political/territorial").

domain_priors:requires_active_enforcement(abrahamic_covenant__land_promise_constraint).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__land_promise_constraint, 'c741ecd9-c753-4492-9c14-35bec0b7cf38').
narrative_ontology:cs_kernel_codification('c741ecd9-c753-4492-9c14-35bec0b7cf38', fixed_text).
narrative_ontology:cs_authority_grounding('c741ecd9-c753-4492-9c14-35bec0b7cf38', lineage).
narrative_ontology:cs_interpretation_layer_present('c741ecd9-c753-4492-9c14-35bec0b7cf38').
narrative_ontology:cs_reading_relation('c741ecd9-c753-4492-9c14-35bec0b7cf38', abrahamic_covenant__isaac_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('c741ecd9-c753-4492-9c14-35bec0b7cf38', abrahamic_covenant__ishmael_covenant_reading, coexists_with).
narrative_ontology:cs_axiom('c741ecd9-c753-4492-9c14-35bec0b7cf38', foundational, territorial_grant_perpetual_binding).
narrative_ontology:cs_axiom_status(territorial_grant_perpetual_binding, holdable).
narrative_ontology:cs_axiom_grounding('c741ecd9-c753-4492-9c14-35bec0b7cf38', territorial_grant_perpetual_binding, theological).
narrative_ontology:cs_axiom('c741ecd9-c753-4492-9c14-35bec0b7cf38', foundational, jewish_lineage_exclusive_covenant_bearer).
narrative_ontology:cs_axiom_status(jewish_lineage_exclusive_covenant_bearer, holdable).
narrative_ontology:cs_axiom_grounding('c741ecd9-c753-4492-9c14-35bec0b7cf38', jewish_lineage_exclusive_covenant_bearer, theological).
narrative_ontology:cs_reference_frame('c741ecd9-c753-4492-9c14-35bec0b7cf38', abrahamic_covenant_territorial_grant).
narrative_ontology:cs_drift_state('c741ecd9-c753-4492-9c14-35bec0b7cf38', contemporary_post_2024, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c741ecd9-c753-4492-9c14-35bec0b7cf38', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__land_promise_constraint, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, israeli_state_apparatus).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, palestinian_residents).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, arab_populations_displaced_post_1948).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, jewish_diaspora).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, religious_authority_structures_jewish).
narrative_ontology:constraint_vindicates(abrahamic_covenant__land_promise_constraint, divine_territorial_grant_doctrine).
narrative_ontology:constraint_vindicates(abrahamic_covenant__land_promise_constraint, perpetual_jewish_territorial_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces the reading of the Genesis covenant as a binding territorial grant, using this reading to justify state boundaries, settlement policy, and defensive military operations. Legally codifies the reading through the Law of Return (1950), Basic Law: Jerusalem, and territorial administration frameworks. Collects legitimacy benefits from the religious claim (domestic mobilization, diaspora alignment) and control over disputed territory.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, israeli_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, regional).

% Inhabit land the constraint claims as granted territory; experience displacement, settlement expansion, property restrictions, and military administration justified through the covenant reading. Cannot exit the territory without abandoning property and kinship. Their own genealogical and religious claims to the same land are systematically suppressed through the constraint's enforcement.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, palestinian_residents, payer,
    powerless, biographical, trapped, regional).

% Forcibly removed or fled during 1948 war and subsequent conflicts; barred from return by Law of Return's inversion (right to return granted to diaspora Jews, not to Arab refugees). Camps and diaspora communities are maintained as permanent populations; the covenant reading's enforcement through state policy denies their territorial claims and material restitution.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, arab_populations_displaced_post_1948, payer,
    powerless, generational, trapped, regional).

% Granted unconditional Law of Return right to Israeli citizenship (Genesis reading: covenant extended to all Jews as Abrahamic heirs). Benefit from the state's territorial security, subsidized settlement expansion, and religious-national alignment without bearing direct costs of Palestinian displacement or military administration.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, jewish_diaspora, beneficiary,
    organized, generational, mobile, global).

% Validate and perpetuate the covenant reading through textual interpretation, rabbinical rulings, and theological frameworks that bind Israeli state legitimacy to Genesis narrative. Collect influence and institutional resources from state alignment; shape settlement theology and occupation justification through yeshiva networks and religious courts.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, religious_authority_structures_jewish, beneficiary,
    organized, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__land_promise_constraint, religious_authority_structures_jewish, agenda_setter).

% Would argue for nullification of the covenant reading as a basis for territorial claims, advancing counter-readings (Islamic prophetic succession, indigenous Palestinian presence, secular national self-determination). Militarily and politically constrained; their voice on the kernel reading itself is suppressed by the enforcement machinery that the dominant reading sustains.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, palestinian_authority_and_arab_states, excluded,
    powerful, generational, constrained, regional).

% International courts, UN bodies, and human-rights organizations document the constraint's operation through displacement data, settlement expansion metrics, and testimonies. They assess the reading's invocation as a justification for occupation and settlement policies that contravene international humanitarian law, regardless of the reading's religious authority.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, international_legal_authorities, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(abrahamic_covenant__land_promise_constraint, israeli_state_apparatus).
narrative_ontology:fixing_cost_class(abrahamic_covenant__land_promise_constraint, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a collective religious-national identity framework for the Jewish people grounded in textual continuity with Abraham: the covenant reading coordinates diaspora Jews into a single territorial claim and shared historical narrative.
% TRANSFER_FUNCTION: Transfers territorial control, settlement rights, and demographic dominance from Palestinian and Arab populations to the Israeli state apparatus; transfers legitimacy and mobilization benefits to Jewish diaspora and religious institutions; transfers dispossession and permanent statelessness to Palestinian refugees and occupied residents.
% ABSENT_VOICES: Palestinian theological readings of the same Genesis text (Islamic inheritance through Ishmael, Qur'anic reinterpretation of the covenant), indigenous Palestinian historical claims predating Abrahamic narratives, and secular international-law frameworks that reject religious texts as bases for territorial claims are systematically excluded from the enforcement machinery that sustains the reading.
% DISAPPEARANCE_RATIONALE: If the covenant reading's enforcement as territorial justification disappeared overnight, Israeli state legitimacy would lose a foundational religious claim; settlement expansion would require secular nationalist justification; Palestinian territorial rights and refugee repatriation claims would immediately re-enter negotiation. The entire territorial-administrative architecture of the state and occupation is structured around the reading's persistence.
% FOUNDING_PROBLEM: Jewish diaspora needed a unified claim to territorial sovereignty after centuries of statelessness and persecution; a genealogical and textual return narrative (Genesis covenant) provided both religious legitimacy and collective identity for statehood (1948).
% FOUNDING_PROBLEM_CORROBORATION: Israeli state and religious authorities attest the problem is live and ongoing (diaspora security, antisemitism persistence). Palestinian authorities and international human-rights organizations attest the founding problem is substantially resolved (Jewish state exists) but the constraint persists as territorial extraction and occupation justification; independent historians and comparative theologians from outside benefiting parties document that the founding problem (diaspora homelessness) does not logically necessitate the particular reading of permanent territorial grant or its enforcement through displacement.
narrative_ontology:disappearance_verdict(abrahamic_covenant__land_promise_constraint, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__land_promise_constraint, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__land_promise_constraint, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(abrahamic_covenant__land_promise_constraint, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__land_promise_constraint, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.82 at 2024) is high because the constraint transfers territorial control, settlement rights, and permanent residence from Palestinians to Israeli state apparatus — not through market mechanism or negotiated exchange, but through enforcement of a reading that denies Palestinian territorial legitimacy. The transfer is asymmetric: Israeli state and diaspora Jews gain statehood and security; Palestinians lose homeland and right to return. Suppression (0.76) is substantial because the constraint's persistence depends on actively suppressing Palestinian theological readings of the same Genesis text and silencing Palestinian territorial claims through military administration and legal restrictions (Law of Return inversion). Theater ratio (0.41) is moderate: religious legitimacy is genuinely invoked in settlement theology and state ideology, but a growing share of enforcement activity (post-1967 military administration, settlement expansion mechanics, settler security operations) operates independent of the reading's truth and serves territorial extraction directly. The measurement trajectory shows extractiveness, suppression, and theater all rising from 1948 to 2024 — indicating the constraint has become more purely extractive and less reliant on coordination justification as it has matured. The rise in theater_ratio tracks the increasing disconnect between the founding problem (diaspora homelessness, resolved by 1967) and the constraint's continued operation (territorial expansion and occupation, which the reading now must justify through theological reinterpretation rather than practical security).
 *
 * PERSPECTIVAL GAP:
 *   From the Israeli state and religious authority seats, the constraint is justified as fulfilling a divine territorial promise and ensuring Jewish security and collective identity — the reading is experienced as coordination and protection. From the Palestinian resident seat, the same constraint is experienced as expropriation enforced through military power, justified retroactively by a reading that denies Palestinian territorial legitimacy. From the international observer seat, the constraint is classified as territorial appropriation that uses a religious reading as cover for material extraction and occupation. The engine computes these divergences from the structural data: the agent's directionality (beneficiary vs. target), exit options (mobile diaspora vs. trapped residents), power (institutional vs. powerless), and role (agenda-setter vs. payer). The claim/metric independence rule ensures the authored claimed_type (snare) and the authored metrics (high extractiveness, high suppression, rising theater) do not reconcile to each other — they stand as independent facts. The divergence IS what the corpus measures.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli state apparatus sits at d ≈ 0.0 (full beneficiary): collects territorial control, state legitimacy, settlement expansion rights; exercises enforcement power; exits freely through state apparatus continuity. Jewish diaspora sits at d ≈ 0.1-0.2 (near beneficiary): receives Law of Return citizenship and security benefits; mobile exit (can immigrate or not); no direct enforcement responsibility. Palestinian residents sit at d ≈ 0.95 (near-full target): lose territorial control, inhabit administered territory, suppressed from advancing counter-claims; trapped exit (cannot leave without abandoning property and kin). Arab-displaced populations sit at d ≈ 1.0 (full target): completely expropriated, barred from return, permanent diaspora, no exit option. Religious authority structures sit at d ≈ 0.15 (near beneficiary): collect institutional influence and resources from state alignment; constrained exit (ideological commitment to the reading). Palestinian Authority and Arab states sit at d ≈ 0.85 (near target): excluded from the reading's enforcement, constrained militarily and politically, their counter-readings systematically suppressed. International observers sit at d ≈ 0.5 (analytical, symmetric): no material stake, no exit constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows incipient mandatrophy: the founding problem (Jewish diaspora statelessness, 1948) is substantially resolved (state exists, Law of Return implemented). The constraint persists not because the founding coordination problem exists, but because territorial extraction and regional hegemony have become independent justifications maintained through the reading's enforcement machinery. Theater_ratio rising from 0.22 to 0.41 tracks this atrophy — the religious reading that was functionally essential to mobilizing diaspora support and state-building (1948-1967) now operates increasingly as performance: settlement theology rationalizes post-hoc expansion, and military administration sustains the reading's enforcement independent of its truth. The rise in suppression_requirement (0.58 to 0.76) indicates that maintaining the reading requires ever-greater enforcement as Palestinian counter-claims grow louder and international humanitarian law frameworks challenge the reading's legitimacy. This is a snare, not a rope: coordination function (diaspora unity) was real at founding; extraction function (territorial expropriation) has become primary; the coordination justification is now a cover story for what is structurally pure territorial taking backed by military power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_conditionality_ambiguity,
    'Is the Genesis territorial covenant conditional (contingent on Jewish adherence to covenant law) or unconditional (perpetually binding regardless of conduct)?',
    'Textual analysis comparing Genesis 12, 15, 17 with Deuteronomic and prophetic conditionality clauses; rabbinic jurisprudence on covenant breach and restoration; Christian and Islamic reinterpretations of conditionality.',
    'If conditional, occupation of territory violating humanitarian law could constitute covenant breach, delegitimizing the reading; if unconditional, the reading withstands conduct-based challenges and provides perpetual justification for territorial claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_conditionality_ambiguity, conceptual, 'Whether the covenant''s territorial promise is conditional on Jewish observance or unconditionally binding.').

omega_variable(
    fulfillment_vs_ongoing_promise,
    'Has the Genesis territorial promise been fulfilled (Joshua''s conquest, modern state established) or is it ongoing (perpetual expansion and security requirement)?',
    'Comparative theology: Christian supersessionism (promise fulfilled, spiritual inheritance supersedes territorial); Jewish halakhic interpretation (territorial command perpetual until redemption); Islamic eschatology (promise redirected to Islamic ummah).',
    'If fulfilled, the modern state''s existence satisfies the promise and settlement expansion becomes unjustifiable; if ongoing, territorial expansion can be justified as covenant fulfillment-in-progress.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fulfillment_vs_ongoing_promise, conceptual, 'Whether the territorial promise is a completed historical event or an ongoing divine mandate.').

omega_variable(
    lineage_exclusivity_interpretation,
    'Does Genesis 17:19-21 (''through Isaac your descendants will be called'') exclude Ishmael from covenant inheritance or merely establish Isaac''s pre-eminence?',
    'Textual exegesis comparing Genesis 17 with Genesis 21:11-14 (Abraham''s love for Ishmael) and Islamic Qur''anic reinterpretation (Ishmael as progenitor of Arabs and subsequent prophets).',
    'If exclusive, Ishmael and Islamic peoples are covenant-excluded, and land promise is exclusively Jewish-Israeli; if pre-eminence alone, Ishmael inherits covenant through Islamic prophetic succession, and territorial claim is contested between Jewish and Islamic readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lineage_exclusivity_interpretation, conceptual, 'Whether Genesis 17:19-21 establishes exclusive or merely primary Abrahamic lineage through Isaac.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is Palestinian suppression from advancing counter-readings a structural constraint (military administration, legal barriers to speech and assembly) or internalized (Palestinian acceptance of Jewish theological claims as self-evident)?',
    'Post-enforcement trajectory: if Palestinians advanced ishmael_covenant_reading and land_promise_constraint enforcement machinery prevented it (curfews, permit restrictions, military administration), suppression is structural; if Palestinians accept the constraint even when enforcement mechanisms are absent or relaxed, suppression is internalized.',
    'If structural, removing enforcement would enable counter-claims; if internalized, the reading persists even without enforcement, indicating deeper theological-identity capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether Palestinian suppression from counter-covenant readings is maintained through external coercion or internalized acceptance.').

omega_variable(
    kernel_contest_sibling_foreclosure,
    'Do the three covenant readings (land_promise_constraint, isaac_covenant_reading, ishmael_covenant_reading) coexist as live positions or does enforcement of land_promise_constraint functionally foreclose the siblings?',
    'Institutional analysis: are Palestinian ishmael_covenant readings (Islamic inheritance) actively suppressed through military administration, religious authority silencing, or legal barriers? Are Jewish alternative readings (supersessionist, universalist) institutionally marginalized?',
    'If foreclosed operationally (enforcement prevents voice), the constraint functions as territorial totalitarianism suppressing alternative readings; if coexisting (alternative readings persist and spread despite disagreement), the kernel contest remains live and the reading is merely dominant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_sibling_foreclosure, empirical, 'Whether enforcement of the land_promise reading forecloses sibling readings or merely dominates them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__land_promise_constraint, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abrahamic_covenant__land_promise_tr_t1948, abrahamic_covenant__land_promise_constraint, theater_ratio, 1948, 0.22).
narrative_ontology:measurement_basis(abrahamic_covenant__land_promise_tr_t1948, observed).
narrative_ontology:measurement(abrahamic_covenant__land_promise_tr_t1967, abrahamic_covenant__land_promise_constraint, theater_ratio, 1967, 0.28).
narrative_ontology:measurement_basis(abrahamic_covenant__land_promise_tr_t1967, observed).
narrative_ontology:measurement(abrahamic_covenant__land_promise_tr_t1987, abrahamic_covenant__land_promise_constraint, theater_ratio, 1987, 0.33).
narrative_ontology:measurement_basis(abrahamic_covenant__land_promise_tr_t1987, observed).
narrative_ontology:measurement(abrahamic_covenant__land_promise_tr_t2005, abrahamic_covenant__land_promise_constraint, theater_ratio, 2005, 0.37).
narrative_ontology:measurement_basis(abrahamic_covenant__land_promise_tr_t2005, observed).
narrative_ontology:measurement(abrahamic_covenant__land_promise_tr_t2015, abrahamic_covenant__land_promise_constraint, theater_ratio, 2015, 0.39).
narrative_ontology:measurement_basis(abrahamic_covenant__land_promise_tr_t2015, observed).
narrative_ontology:measurement(abrahamic_covenant__land_promise_tr_t2024, abrahamic_covenant__land_promise_constraint, theater_ratio, 2024, 0.41).
narrative_ontology:measurement_basis(abrahamic_covenant__land_promise_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(abrahamic_covenant__land_promise_be_t1948, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1948, 0.65).
narrative_ontology:measurement_basis(abrahamic_covenant__land_promise_be_t1948, observed).
narrative_ontology:measurement(abrahamic_covenant__land_promise_be_t1967, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1967, 0.72).
narrative_ontology:measurement_basis(abrahamic_covenant__land_promise_be_t1967, observed).
narrative_ontology:measurement(abrahamic_covenant__land_promise_be_t1987, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1987, 0.76).
narrative_ontology:measurement_basis(abrahamic_covenant__land_promise_be_t1987, observed).
narrative_ontology:measurement(abrahamic_covenant__land_promise_be_t2005, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2005, 0.79).
narrative_ontology:measurement_basis(abrahamic_covenant__land_promise_be_t2005, observed).
narrative_ontology:measurement(abrahamic_covenant__land_promise_be_t2015, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2015, 0.81).
narrative_ontology:measurement_basis(abrahamic_covenant__land_promise_be_t2015, observed).
narrative_ontology:measurement(abrahamic_covenant__land_promise_be_t2024, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2024, 0.82).
narrative_ontology:measurement_basis(abrahamic_covenant__land_promise_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(abrahamic_covenant__land_promise_su_t1948, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1948, 0.58).
narrative_ontology:measurement_basis(abrahamic_covenant__land_promise_su_t1948, observed).
narrative_ontology:measurement(abrahamic_covenant__land_promise_su_t1967, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1967, 0.64).
narrative_ontology:measurement_basis(abrahamic_covenant__land_promise_su_t1967, observed).
narrative_ontology:measurement(abrahamic_covenant__land_promise_su_t1987, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1987, 0.7).
narrative_ontology:measurement_basis(abrahamic_covenant__land_promise_su_t1987, observed).
narrative_ontology:measurement(abrahamic_covenant__land_promise_su_t2005, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2005, 0.74).
narrative_ontology:measurement_basis(abrahamic_covenant__land_promise_su_t2005, observed).
narrative_ontology:measurement(abrahamic_covenant__land_promise_su_t2015, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2015, 0.75).
narrative_ontology:measurement_basis(abrahamic_covenant__land_promise_su_t2015, observed).
narrative_ontology:measurement(abrahamic_covenant__land_promise_su_t2024, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2024, 0.76).
narrative_ontology:measurement_basis(abrahamic_covenant__land_promise_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__land_promise_constraint, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(abrahamic_covenant__land_promise_constraint, 0.12).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, isaac_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, ishmael_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, palestinian_indigenous_territorial_claim).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, right_of_return_constraint).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, settler_expansion_mechanism).

% DUAL FORMULATION NOTE:
% The abrahamic_covenant kernel produces three structurally distinct constraints corresponding to three kernel readings. The land_promise_constraint (THIS story) instantiates the reading that Genesis 17 grants unconditional, perpetual territorial rights to Jewish descendants, and it justifies modern Israeli territorial administration. The isaac_covenant_reading instantiates the interpretation that the covenant is exclusive to Isaac's line, excluding Ishmael. The ishmael_covenant_reading instantiates the Islamic prophetic succession reading, where Ishmael inherits the covenant through Muhammad. All three readings reference the same Genesis text (the kernel); they differ in genealogical interpretation and theological framework. Each reading produces different beneficiary/victim structures and different ε values: land_promise_constraint (THIS one) produces high extraction (0.82) with Israeli state as beneficiary and Palestinians as victims; isaac_covenant_reading produces similar structure with added emphasis on exclusion; ishmael_covenant_reading produces inverted structure with Palestinians/Muslims as potential beneficiaries and Israeli territorial claim as victim of theologically illegitimate exclusion. The three constraints are linked via network.affects_constraints so the corpus can model the kernel contest structurally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(abrahamic_covenant__land_promise_constraint, powerless, 0.95).
constraint_indexing:directionality_override(abrahamic_covenant__land_promise_constraint, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
